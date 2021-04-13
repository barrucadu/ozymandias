{-# LANGUAGE OverloadedStrings #-}

module Ozymandias.Podman
  ( -- * The Podman handle
    Podman,
    initPodman,

    -- * Pod types
    Pod (..),
    IsManaged (..),

    -- * Interacting with Podman
    getAllPods,
  )
where

import Control.Exception (catch)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Types (hAccept)
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS
import Ozymandias.Problem

-------------------------------------------------------------------------------

-- | A handle to the Podman API.
newtype Podman = Podman Manager

-- | Create a new 'Podman' handle.
initPodman ::
  -- | Path to the Podman UNIX socket file.
  FilePath ->
  IO Podman
initPodman socketPath = Podman <$> newManager defaultManagerSettings {managerRawConnection = pure openUnixSocket}
  where
    -- from https://kseo.github.io/posts/2017-01-23-custom-connection-manager-for-http-client.html
    openUnixSocket _ _ _ = do
      sock <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
      S.connect sock (S.SockAddrUnix socketPath)
      makeConnection (SBS.recv sock 8096) (SBS.sendAll sock) (S.close sock)

-------------------------------------------------------------------------------

-- | A pod, which is a collection of containers.
data Pod = Pod
  { -- | Internal identifier of the pod, used to manipulate it.
    podId :: Text,
    -- | Human-friendly (somewhat) name of the pod.
    podName :: Text,
    -- | Whether this pod is managed by Ozymandias or not.
    podIsManaged :: IsManaged
  }
  deriving (Show)

instance FromJSON Pod where
  parseJSON = withObject "Pod" $ \v ->
    Pod
      <$> v .: "Id"
      <*> v .: "Name"
      <*> ((\labels -> if M.member "managed-by-ozymandias" (labels :: Object) then IsManaged else IsNotManaged) <$> (v .: "Labels"))

-- | Whether a pod is managed by Ozymandias or not.
data IsManaged = IsNotManaged | IsManaged
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

-------------------------------------------------------------------------------

-- | List all pods, managed and unmanaged.
getAllPods :: Podman -> IO (Either Problem [Pod])
getAllPods = podmanApiRequestJSON "/v3.0.0/libpod/pods/json"

-------------------------------------------------------------------------------

-- | Make a request to the Podman API.
podmanApiRequest :: String -> Podman -> IO (Either Problem (Response BL.ByteString))
podmanApiRequest uriPath (Podman manager) = (Right <$> doRequest) `catch` (pure . Left . PodmanHttpError)
  where
    doRequest = do
      req <- parseUrlThrow ("http://127.0.0.1" <> uriPath)
      httpLbs req {requestHeaders = (hAccept, "application/json") : requestHeaders req} manager

-- | Make a request to the Podman API and decode it as JSON.
podmanApiRequestJSON :: FromJSON a => String -> Podman -> IO (Either Problem a)
podmanApiRequestJSON uriPath podman = decodeJson <$> podmanApiRequest uriPath podman
  where
    decodeJson (Right resp) = case eitherDecode (responseBody resp) of
      Right a -> Right a
      Left err -> Left (PodmanJsonError err)
    decodeJson (Left err) = Left err
