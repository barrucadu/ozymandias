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

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Types (hAccept)
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS

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
getAllPods :: Podman -> IO [Pod]
getAllPods podman = decodeWithError . responseBody <$> podmanApiRequest podman "/v3.0.0/libpod/pods/json"

-------------------------------------------------------------------------------

-- | A request to the Podman API
podmanApiRequest :: Podman -> String -> IO (Response BL.ByteString)
podmanApiRequest (Podman manager) uriPath =
  let req = parseRequest_ ("http://127.0.0.1" <> uriPath)
   in httpLbs req {requestHeaders = (hAccept, "application/json") : requestHeaders req} manager

-- | Decode JSON, and throw an error if it can't be decoded.
decodeWithError :: FromJSON a => BL.ByteString -> a
decodeWithError = either error id . eitherDecode
