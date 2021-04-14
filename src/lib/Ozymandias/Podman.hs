{-# LANGUAGE OverloadedStrings #-}

module Ozymandias.Podman
  ( -- * The Podman handle
    Podman,
    initPodman,

    -- * Pod types
    IdObj (..),
    Pod (..),
    IsManaged (..),

    -- * Interacting with Podman
    createAndLaunchPod,
    destroyPod,
    getAllPods,
  )
where

import Control.Monad (void)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import qualified Data.HashMap.Strict as M
import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Types
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS
import Ozymandias.Job
import Ozymandias.Monad
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

-- | A type for Podman endpoints which return an id.
newtype IdObj = IdObj Text
  deriving (Show)

instance FromJSON IdObj where
  parseJSON = withObject "IdObj" $ \v -> IdObj <$> v .: "Id"

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

-- | Create a pod and launch containers inside it.
createAndLaunchPod :: NormalisedJobSpec -> Podman -> Oz IdObj
createAndLaunchPod njobspec podman = do
  pod <- createPod jobspec podman
  launchContainers pod (normalisedJobSpecToLaunchOrder njobspec)
  where
    jobspec = normalisedJobSpecToJobSpec njobspec

    launchContainers :: IdObj -> [[Text]] -> Oz IdObj
    launchContainers pod = go
      where
        go [] = pure pod
        go (cs : css) = launchContainersInGroup pod cs >> go css

    launchContainersInGroup :: IdObj -> [Text] -> Oz ()
    launchContainersInGroup pod = go
      where
        go [] = pure ()
        go (c : cs) = do
          let container = jobspecContainers jobspec M.! c
          pullImage (containerspecImage container) podman
          cid <- createContainer pod container podman
          initContainer cid podman
          startContainer cid podman
          go cs

-- | Kill and delete a pod and all its containers.
destroyPod :: IdObj -> Podman -> Oz ()
destroyPod (IdObj pid) = void . podmanApiRequest' methodDelete uriPath Nothing
  where
    uriPath = "/v3.0.0/libpod/pods/" <> unpack pid <> "?force=true"

-- | List all pods, managed and unmanaged.
getAllPods :: Podman -> Oz [Pod]
getAllPods = podmanApiRequest methodGet "/v3.0.0/libpod/pods/json" Nothing

-------------------------------------------------------------------------------

-- Lower-level podman operations used by the functions above.

-- | Create a pod, but not any of its containers.
createPod :: JobSpec -> Podman -> Oz IdObj
createPod jobspec = podmanApiRequest methodPost "/v3.0.0/libpod/pods/create" (Just j)
  where
    j =
      object
        [ "name" .= jobspecName jobspec,
          "labels" .= object ["managed-by-ozymandias" .= ("yes" :: Text)],
          "portmappings" .= (map portMappingToJson . concat $ mapMaybe containerspecPortMappings (M.elems (jobspecContainers jobspec)))
        ]

    portMappingToJson p =
      object
        [ "host_port" .= portmappingFrom p,
          "container_port" .= portmappingTo p,
          "protocol" .= intercalate "," (map (map toLower . show) (fromMaybe [TCP] (portmappingProtocols p)))
        ]

-- | Pull an image from a registry.
pullImage :: Text -> Podman -> Oz ()
pullImage image = void . podmanApiRequest' methodPost uriPath Nothing
  where
    uriPath = unpack . decodeUtf8 $ "/v3.0.0/libpod/images/pull?reference=" <> urlEncode True (encodeUtf8 image)

-- | Create, but do not start, a container.
createContainer :: IdObj -> ContainerSpec -> Podman -> Oz IdObj
createContainer (IdObj pid) c = podmanApiRequest methodPost "/v3.0.0/libpod/containers/create" (Just j)
  where
    j =
      object
        [ "pod" .= pid,
          "image" .= containerspecImage c,
          "restart_policy" .= containerspecRestartPolicy c,
          "restart_tries" .= containerspecRestartTries c,
          "command" .= containerspecCommand c,
          "entrypoint" .= containerspecEntrypoint c,
          "env" .= containerspecEnvironment c,
          "resource_limits" .= object ["memory" .= object ["limit" .= containerspecMemory c]]
        ]

-- | Initialise, but do not start, a container.
initContainer :: IdObj -> Podman -> Oz ()
initContainer (IdObj cid) = void . podmanApiRequest' methodPost uriPath Nothing
  where
    uriPath = "/v3.0.0/libpod/containers/" <> unpack cid <> "/init"

-- | Start a container.
startContainer :: IdObj -> Podman -> Oz ()
startContainer (IdObj cid) = void . podmanApiRequest' methodPost uriPath Nothing
  where
    uriPath = "/v3.0.0/libpod/containers/" <> unpack cid <> "/start"

-------------------------------------------------------------------------------

-- | Make a request to the Podman API and decode the response as JSON.
podmanApiRequest ::
  FromJSON a =>
  -- | Request method
  Method ->
  -- | Request path
  String ->
  -- | JSON request body
  Maybe Value ->
  -- | Podman handle
  Podman ->
  Oz a
podmanApiRequest meth uriPath body manager = decodeJson =<< podmanApiRequest' meth uriPath body manager
  where
    decodeJson = either (problem . PodmanJsonError) pure . eitherDecode . responseBody

-- | Make a request to the Podman API.
podmanApiRequest' ::
  -- | Request method
  Method ->
  -- | Request path
  String ->
  -- | JSON request body
  Maybe Value ->
  -- | Podman handle
  Podman ->
  Oz (Response BL.ByteString)
podmanApiRequest' meth uriPath body (Podman manager) = do
  req <- makeReq <$> liftIO (parseUrlThrow ("http://127.0.0.1" <> uriPath))
  liftIO (httpLbs req manager) `catch` (problem . PodmanHttpError)
  where
    makeReq req =
      req
        { method = meth,
          requestHeaders = [(hAccept, "application/json"), (hContentType, "application/json")],
          requestBody = maybe (requestBody req) (RequestBodyLBS . encode) body
        }
