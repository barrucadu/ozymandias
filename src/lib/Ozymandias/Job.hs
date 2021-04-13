{-# LANGUAGE DeriveGeneric #-}

module Ozymandias.Job
  ( -- * Job specifications
    JobSpec (..),
    ContainerSpec (..),
    PortMappingSpec (..),
    RestartPolicySpec (..),
    PortProtocolSpec (..),
  )
where

import Data.Aeson
import qualified Data.HashMap.Strict as M
import Data.List (stripPrefix)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Word (Word16)
import GHC.Generics
import Numeric.Natural (Natural)

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

-- | JSON encoding / decoding options for record types.
jsonOptions :: String -> Options
jsonOptions prefix = defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix prefix}

-- | JSON encoding / decoding options for sum types.
jsonOptions' :: Options
jsonOptions' = defaultOptions {constructorTagModifier = camelTo2 '-'}
