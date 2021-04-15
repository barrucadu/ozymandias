{-# LANGUAGE DeriveGeneric #-}

module Ozymandias.Pod
  ( -- * Pod specifications
    PodSpec (..),
    ContainerSpec (..),
    PortMappingSpec (..),
    RestartPolicySpec (..),
    PortProtocolSpec (..),

    -- * Normalised pod specifications
    NormalisedPodSpec,
    normalisedPodSpecToPodSpec,
    normalisedPodSpecToLaunchOrder,
    normalisePodSpec,
  )
where

import Data.Aeson
import qualified Data.HashMap.Strict as M
import Data.List (partition, sortOn, stripPrefix)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Data.Word (Word16)
import GHC.Generics
import Numeric.Natural (Natural)
import Ozymandias.Problem

-- | A specification of a pod to run.
data PodSpec = PodSpec
  { -- | The name of the pod, to use for DNS.  Must be globally
    -- unique, and valid as a hostname.
    podspecName :: Text,
    -- | Container definitions.  These are run as a single \"pod\",
    -- bound to the same network interface.
    podspecContainers :: M.HashMap Text ContainerSpec
  }
  deriving (Generic, Show)

instance ToJSON PodSpec where
  toJSON = genericToJSON (jsonOptions "podspec")
  toEncoding = genericToEncoding (jsonOptions "podspec")

instance FromJSON PodSpec where
  parseJSON = genericParseJSON (jsonOptions "podspec")

-- | A specification of a container to run.
data ContainerSpec = ContainerSpec
  { -- | Image to run.
    containerspecImage :: Text,
    -- | Restart policy to use.
    containerspecRestartPolicy :: Maybe RestartPolicySpec,
    -- | Number of times to try to restart, if the policy is 'on-failure'.
    containerspecRestartTries :: Maybe Natural,
    -- | Command to use (if not the image's default).
    containerspecCommand :: Maybe (NonEmpty Text),
    -- | Entrypoint to use (if not the image's default).
    containerspecEntrypoint :: Maybe (NonEmpty Text),
    -- | Environment variables.
    containerspecEnvironment :: Maybe (M.HashMap Text Text),
    -- | Port mappings.
    containerspecPortMappings :: Maybe [PortMappingSpec],
    -- | Memory limit, in bytes
    containerspecMemory :: Natural,
    -- | Dependencies on other containers in the same pod.
    containerspecDepends :: Maybe [Text]
  }
  deriving (Generic, Show)

instance ToJSON ContainerSpec where
  toJSON = genericToJSON (jsonOptions "containerspec")
  toEncoding = genericToEncoding (jsonOptions "containerspec")

instance FromJSON ContainerSpec where
  parseJSON = genericParseJSON (jsonOptions "containerspec")

-- | A single container port mapping.
data PortMappingSpec = PortMappingSpec
  { -- | Port to open on the external IP address.
    portmappingFrom :: Word16,
    -- | Port to map to inside the container.
    portmappingTo :: Word16,
    -- | Protocols to use (defaults to tcp)
    portmappingProtocols :: Maybe [PortProtocolSpec]
  }
  deriving (Show, Generic)

instance ToJSON PortMappingSpec where
  toJSON = genericToJSON (jsonOptions "portmapping")
  toEncoding = genericToEncoding (jsonOptions "portmapping")

instance FromJSON PortMappingSpec where
  parseJSON = genericParseJSON (jsonOptions "portmapping")

-- | A container's restart policy.
data RestartPolicySpec = Never | OnFailure | Always
  deriving (Generic, Show)

instance ToJSON RestartPolicySpec where
  toJSON = genericToJSON jsonOptions'
  toEncoding = genericToEncoding jsonOptions'

instance FromJSON RestartPolicySpec where
  parseJSON = genericParseJSON jsonOptions'

-- | A container port mapping protocol.
data PortProtocolSpec = TCP | UDP | SCTP
  deriving (Show, Generic)

instance ToJSON PortProtocolSpec where
  toJSON = genericToJSON jsonOptions'
  toEncoding = genericToEncoding jsonOptions'

instance FromJSON PortProtocolSpec where
  parseJSON = genericParseJSON jsonOptions'

-------------------------------------------------------------------------------

-- | A normalised @PodSpec@ has had all the @containerspecDepends@
-- validated: all dependencies are in the pod, and there is an order
-- to launch the containers which meets all of the dependencies.
data NormalisedPodSpec = NormalisedPodSpec
  { normalisedPodSpec :: PodSpec,
    normalisedContainerLaunchOrder :: [[Text]]
  }
  deriving (Show)

-- | Get the @PodSpec@ from a @NormalisedPodSpec@.
normalisedPodSpecToPodSpec :: NormalisedPodSpec -> PodSpec
normalisedPodSpecToPodSpec = normalisedPodSpec

-- | Get the launch order from a @NormalisedPodSpec@.
normalisedPodSpecToLaunchOrder :: NormalisedPodSpec -> [[Text]]
normalisedPodSpecToLaunchOrder = normalisedContainerLaunchOrder

-- | Validate the @containerspecDepends@ of a @PodSpec@ and compute a
-- container launch order.
normalisePodSpec :: PodSpec -> Either Problem NormalisedPodSpec
normalisePodSpec podspec = go [] . sortOn (length . deps) . M.toList $ podspecContainers podspec
  where
    go launcho [] =
      Right
        NormalisedPodSpec
          { normalisedPodSpec = podspec,
            normalisedContainerLaunchOrder = reverse launcho
          }
    go launcho todo =
      let (launcho', todo') = partition (all (\d -> any (d `elem`) launcho) . deps) todo
       in if null launcho'
            then Left (PodDependencyError (reverse launcho) (map fst todo))
            else go (map fst launcho' : launcho) todo'

    deps = fromMaybe [] . containerspecDepends . snd

-------------------------------------------------------------------------------

-- | JSON encoding / decoding options for record types.
jsonOptions :: String -> Options
jsonOptions prefix = defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix prefix}

-- | JSON encoding / decoding options for sum types.
jsonOptions' :: Options
jsonOptions' = defaultOptions {constructorTagModifier = camelTo2 '-'}
