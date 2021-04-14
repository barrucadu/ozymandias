{-# LANGUAGE OverloadedStrings #-}

module Ozymandias.Etcd
  ( -- * The etcd handle
    Etcd,
    initEtcd,

    -- * Interacting with etcd
    EtcdKey (..),
    fetchJobSpec,
  )
where

import Data.Aeson
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Ozymandias.Job
import Ozymandias.Monad
import Ozymandias.Problem
import Ozymandias.Util

-------------------------------------------------------------------------------

-- | A handle to the etcd API.
newtype Etcd = Etcd {etcdHandle :: ApiHandle}

-- | Create a new 'Etcd' handle.
initEtcd ::
  -- | URL of the etcd host.
  String ->
  IO Etcd
initEtcd etcdHost = do
  manager <- newManager defaultManagerSettings
  pure
    Etcd
      { etcdHandle =
          ApiHandle
            { apiManager = manager,
              apiMakeRequest = \uriPath -> parseUrlThrow (etcdHost <> uriPath),
              apiJsonError = EtcdJsonError,
              apiHttpError = EtcdHttpError
            }
      }

-------------------------------------------------------------------------------

-- | A reference to an object in etcd.
newtype EtcdKey = EtcdKey Text
  deriving (Show)

-- | Fetch a 'JobSpec' from etcd.
fetchJobSpec :: Etcd -> EtcdKey -> Oz JobSpec
fetchJobSpec etcd key = kvValue . NE.head <$> readKey etcd key

-------------------------------------------------------------------------------

-- Lower-level etcd operations used by the functions above.

-- | A collection of keys/values.
newtype KVs a = KVs [KV a]
  deriving (Show)

instance FromJSON a => FromJSON (KVs a) where
  parseJSON = withObject "KVs" $ \v ->
    KVs
      <$> fmap (fromMaybe []) (v .:? "kvs")

-- | A single key/value
data KV a = KV
  { kvKey :: Text,
    kvValue :: a,
    kvVersion :: Text
  }
  deriving (Show)

instance FromJSON a => FromJSON (KV a) where
  parseJSON = withObject "KV" $ \v ->
    KV
      <$> fmap decodeTextFromBase64 (v .: "key")
      <*> (parseJSONFromBase64 =<< (v .: "value"))
      <*> v .: "version"

-- | Fetch the value at a key.
readKey :: FromJSON a => Etcd -> EtcdKey -> Oz (NonEmpty (KV a))
readKey etcd (EtcdKey key) = do
  let j = object ["key" .= encodeTextAsBase64 key]
  KVs values <- apiRequest (etcdHandle etcd) methodPost "/v3alpha/kv/range" (Just j)
  maybe (problem (EtcdKeyNotFoundError key)) pure (NE.nonEmpty values)

-------------------------------------------------------------------------------

-- | Parse some JSON from base64 data
parseJSONFromBase64 :: FromJSON a => Text -> A.Parser a
parseJSONFromBase64 = either fail pure . eitherDecode . BL.fromStrict . B64.decodeLenient . encodeUtf8

-- | Decode some @Text@ from base64 data.
decodeTextFromBase64 :: Text -> Text
decodeTextFromBase64 = decodeUtf8 . B64.decodeLenient . encodeUtf8

-- | Encode some @Text@ into base64 data.
encodeTextAsBase64 :: Text -> Text
encodeTextAsBase64 = decodeUtf8 . B64.encode . encodeUtf8
