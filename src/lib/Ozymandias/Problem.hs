{-# LANGUAGE OverloadedStrings #-}

module Ozymandias.Problem where

import Data.Aeson
import Data.Text (Text, pack)
import qualified Network.HTTP.Client as HTTP

-- | All the problems that can arise.
data Problem
  = PodmanJsonError {jsonError :: String}
  | PodmanHttpError {httpError :: HTTP.HttpException}
  deriving (Show)

instance ToJSON Problem where
  toJSON = toJSON . toProblemDocument

-- | A formatted problem, with help text and suchlike.
data ProblemDocument = ProblemDocument
  { -- | A URL which holds further information
    problemType :: Text,
    -- | The name of the problem
    problemTitle :: Text,
    -- | A description of the specific issue
    problemDetail :: Text,
    -- | Extra details (optional)
    problemExtraDetails :: [(Text, Text)]
  }
  deriving (Show)

instance ToJSON ProblemDocument where
  toJSON doc =
    object $
      [ "type" .= problemType doc,
        "title" .= problemTitle doc,
        "detail" .= problemDetail doc
      ]
        ++ map (\(label, value) -> ("detail_" <> label) .= value) (problemExtraDetails doc)

-- | Convert a @Problem@ into a @ProblemDocument@.
toProblemDocument :: Problem -> ProblemDocument
toProblemDocument (PodmanJsonError err) =
  ProblemDocument
    { problemType = problemTypeBaseURL <> "#podman-request-returned-invalid-json",
      problemTitle = "Podman request returned invalid JSON",
      problemDetail = pack err,
      problemExtraDetails = []
    }
toProblemDocument (PodmanHttpError (HTTP.HttpExceptionRequest req err)) =
  ProblemDocument
    { problemType = problemTypeBaseURL <> "#podman-request-raised-an-http-error",
      problemTitle = "Podman request raised an HTTP error",
      problemDetail = pack (show err),
      problemExtraDetails = [("request", pack (show req))]
    }
toProblemDocument (PodmanHttpError (HTTP.InvalidUrlException url err)) =
  ProblemDocument
    { problemType = problemTypeBaseURL <> "#invalid-url",
      problemTitle = "Invalid URL",
      problemDetail = pack err,
      problemExtraDetails = [("url", pack url)]
    }

-- | Base URL for problem type help text
problemTypeBaseURL :: Text
problemTypeBaseURL = "https://github.com/barrucadu/ozymandias/master/docs/errors.markdown"
