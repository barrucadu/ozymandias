{-# LANGUAGE OverloadedStrings #-}

module Ozymandias.Util
  ( ApiHandle (..),
    apiRequest,
    apiRequest_,
    apiRequest',

    -- * Re-exports
    methodGet,
    methodPost,
    methodDelete,
    urlEncode,
    newManager,
    defaultManagerSettings,
    parseUrlThrow,
  )
where

import Control.Monad (void)
import Data.Aeson (FromJSON, Value, eitherDecode, encode)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client
import Network.HTTP.Types
import Ozymandias.Monad
import Ozymandias.Problem

-- | A handle to an API.
data ApiHandle = ApiHandle
  { -- | Connection manager
    apiManager :: Manager,
    -- | Turn a request path into a request
    apiMakeRequest :: String -> IO Request,
    -- | Raise a JSON error
    apiJsonError :: String -> Problem,
    -- | Raise an HTTP error
    apiHttpError :: HttpException -> Problem
  }

-- | Make an HTTP request and decode the response as JSON.
apiRequest ::
  FromJSON a =>
  -- | API handle
  ApiHandle ->
  -- | Request method
  Method ->
  -- | Request path
  String ->
  -- | JSON request body
  Maybe Value ->
  Oz a
apiRequest handle meth uriPath body = decodeJson =<< apiRequest' handle meth uriPath body
  where
    decodeJson = either (problem . apiJsonError handle) pure . eitherDecode . responseBody

-- | Make an HTTP request, discarding the result.
apiRequest_ ::
  -- | API handle
  ApiHandle ->
  -- | Request method
  Method ->
  -- | Request path
  String ->
  -- | JSON request body
  Maybe Value ->
  Oz ()
apiRequest_ handle meth uriPath body = void $ apiRequest' handle meth uriPath body

-- | Make an HTTP request.
apiRequest' ::
  -- | API handle
  ApiHandle ->
  -- | Request method
  Method ->
  -- | Request path
  String ->
  -- | JSON request body
  Maybe Value ->
  Oz (Response ByteString)
apiRequest' handle meth uriPath body = do
  req <- makeReq <$> liftIO (apiMakeRequest handle uriPath)
  liftIO (httpLbs req (apiManager handle)) `catch` (problem . apiHttpError handle)
  where
    makeReq req =
      req
        { method = meth,
          requestHeaders = [(hAccept, "application/json"), (hContentType, "application/json")],
          requestBody = maybe (requestBody req) (RequestBodyLBS . encode) body
        }
