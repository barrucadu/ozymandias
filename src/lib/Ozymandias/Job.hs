{-# LANGUAGE DeriveGeneric #-}

module Ozymandias.Job
  ( -- * Job specifications
    JobSpec (..),
    ContainerSpec (..),
    PortMappingSpec (..),
    RestartPolicySpec (..),
    PortProtocolSpec (..),

    -- * Normalised job specifications
    NormalisedJobSpec,
    normalisedJobSpecToJobSpec,
    normalisedJobSpecToLaunchOrder,
    normaliseJobSpec,
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

-- | A specification of a job to run.
data JobSpec = JobSpec
  { -- | The name of the job, to use for DNS.  Must be globally
    -- unique, and valid as a hostname.
    jobspecName :: Text,
    -- | Number of instances to run.
    jobspecNumInstances :: Natural,
    -- | Container definitions.  These are run as a single \"pod\",
    -- bound to the same network interface.
    jobspecContainers :: M.HashMap Text ContainerSpec
  }
  deriving (Generic, Show)

instance ToJSON JobSpec where
  toJSON = genericToJSON (jsonOptions "jobspec")
  toEncoding = genericToEncoding (jsonOptions "jobspec")

instance FromJSON JobSpec where
  parseJSON = genericParseJSON (jsonOptions "jobspec")

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

-- | A normalised @JobSpec@ has had all the @containerspecDepends@
-- validated: all dependencies are in the pod, and there is an order
-- to launch the containers which meets all of the dependencies.
data NormalisedJobSpec = NormalisedJobSpec
  { normalisedJobSpec :: JobSpec,
    normalisedContainerLaunchOrder :: [[Text]]
  }
  deriving (Show)

-- | Get the @JobSpec@ from a @NormalisedJobSpec@.
normalisedJobSpecToJobSpec :: NormalisedJobSpec -> JobSpec
normalisedJobSpecToJobSpec = normalisedJobSpec

-- | Get the launch order from a @NormalisedJobSpec@.
normalisedJobSpecToLaunchOrder :: NormalisedJobSpec -> [[Text]]
normalisedJobSpecToLaunchOrder = normalisedContainerLaunchOrder

-- | Validate the @containerspecDepends@ of a @JobSpec@ and compute a
-- container launch order.
normaliseJobSpec :: JobSpec -> Either Problem NormalisedJobSpec
normaliseJobSpec jobspec = go [] . sortOn (length . deps) . M.toList $ jobspecContainers jobspec
  where
    go launcho [] =
      Right
        NormalisedJobSpec
          { normalisedJobSpec = jobspec,
            normalisedContainerLaunchOrder = reverse launcho
          }
    go launcho todo =
      let (launcho', todo') = partition (all (\d -> any (d `elem`) launcho) . deps) todo
       in if null launcho'
            then Left (JobDependencyError (reverse launcho) (map fst todo))
            else go (map fst launcho' : launcho) todo'

    deps = fromMaybe [] . containerspecDepends . snd

-------------------------------------------------------------------------------

-- | JSON encoding / decoding options for record types.
jsonOptions :: String -> Options
jsonOptions prefix = defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix prefix}

-- | JSON encoding / decoding options for sum types.
jsonOptions' :: Options
jsonOptions' = defaultOptions {constructorTagModifier = camelTo2 '-'}
