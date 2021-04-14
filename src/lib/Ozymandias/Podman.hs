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
import Data.Char (toLower)
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as M
import Data.List (intercalate, nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Client (makeConnection, managerRawConnection)
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS
import Ozymandias.Job
import Ozymandias.Monad
import Ozymandias.Problem
import Ozymandias.Util

-------------------------------------------------------------------------------

-- | A handle to the Podman API.
newtype Podman = Podman {podmanHandle :: ApiHandle}

-- | Create a new 'Podman' handle.
initPodman ::
  -- | Path to the Podman UNIX socket file.
  FilePath ->
  IO Podman
initPodman socketPath = do
  manager <- newManager defaultManagerSettings {managerRawConnection = pure openUnixSocket}
  pure
    Podman
      { podmanHandle =
          ApiHandle
            { apiManager = manager,
              apiMakeRequest = \uriPath -> parseUrlThrow ("http://127.0.0.1" <> uriPath),
              apiJsonError = PodmanJsonError,
              apiHttpError = PodmanHttpError
            }
      }
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
createAndLaunchPod :: Podman -> NormalisedJobSpec -> Oz IdObj
createAndLaunchPod podman njobspec = do
  pullImages . map containerspecImage . M.elems $ jobspecContainers jobspec
  pod <- createPod podman jobspec
  launchContainers pod (normalisedJobSpecToLaunchOrder njobspec)
  pure pod
  where
    jobspec = normalisedJobSpecToJobSpec njobspec

    pullImages :: [Text] -> Oz ()
    pullImages = void . mapConcurrently (pullImage podman) . nub

    launchContainers :: IdObj -> [[Text]] -> Oz ()
    launchContainers pod = traverse_ (mapConcurrently go)
      where
        go c = do
          let container = jobspecContainers jobspec M.! c
          cid <- createContainer podman pod container
          initContainer podman cid
          startContainer podman cid

-- | Kill and delete a pod and all its containers.
destroyPod :: Podman -> IdObj -> Oz ()
destroyPod podman (IdObj pid) = apiRequest_ (podmanHandle podman) methodDelete uriPath Nothing
  where
    uriPath = "/v3.0.0/libpod/pods/" <> unpack pid <> "?force=true"

-- | List all pods, managed and unmanaged.
getAllPods :: Podman -> Oz [Pod]
getAllPods podman = apiRequest (podmanHandle podman) methodGet "/v3.0.0/libpod/pods/json" Nothing

-------------------------------------------------------------------------------

-- Lower-level podman operations used by the functions above.

-- | Create a pod, but not any of its containers.
createPod :: Podman -> JobSpec -> Oz IdObj
createPod podman jobspec =
  apiRequest (podmanHandle podman) methodPost "/v3.0.0/libpod/pods/create" . Just $
    object
      [ "name" .= jobspecName jobspec,
        "labels" .= object ["managed-by-ozymandias" .= ("yes" :: Text)],
        "portmappings" .= (map portMappingToJson . concat $ mapMaybe containerspecPortMappings (M.elems (jobspecContainers jobspec)))
      ]
  where
    portMappingToJson p =
      object
        [ "host_port" .= portmappingFrom p,
          "container_port" .= portmappingTo p,
          "protocol" .= intercalate "," (map (map toLower . show) (fromMaybe [TCP] (portmappingProtocols p)))
        ]

-- | Pull an image from a registry.
pullImage :: Podman -> Text -> Oz ()
pullImage podman image = apiRequest_ (podmanHandle podman) methodPost uriPath Nothing
  where
    uriPath = unpack . decodeUtf8 $ "/v3.0.0/libpod/images/pull?reference=" <> urlEncode True (encodeUtf8 image)

-- | Create, but do not start, a container.
createContainer :: Podman -> IdObj -> ContainerSpec -> Oz IdObj
createContainer podman (IdObj pid) c =
  apiRequest (podmanHandle podman) methodPost "/v3.0.0/libpod/containers/create" . Just $
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
initContainer :: Podman -> IdObj -> Oz ()
initContainer podman (IdObj cid) = apiRequest_ (podmanHandle podman) methodPost uriPath Nothing
  where
    uriPath = "/v3.0.0/libpod/containers/" <> unpack cid <> "/init"

-- | Start a container.
startContainer :: Podman -> IdObj -> Oz ()
startContainer podman (IdObj cid) = apiRequest_ (podmanHandle podman) methodPost uriPath Nothing
  where
    uriPath = "/v3.0.0/libpod/containers/" <> unpack cid <> "/start"
