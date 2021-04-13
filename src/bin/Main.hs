{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Aeson as A
import Data.Foldable (traverse_)
import Data.Text (unpack)
import Options.Applicative
import Ozymandias.Job
import Ozymandias.Podman
import Ozymandias.Problem
import System.Exit (die)

main :: IO ()
main = do
  args <- parseArgs
  podman <- initPodman (argsPodmanSocket args)
  case argsCommand args of
    DebugArgs ListManagedPods -> debugListPods podman IsManaged
    DebugArgs ListUnmanagedPods -> debugListPods podman IsNotManaged
    DebugArgs (ParseJobDefinition fp) -> debugParseJobDefinition fp
    DebugArgs (CreatePodFromJob fp) -> debugCreatePodFromJob podman fp
    _ -> print args >> die "unimplemented"
  where
    parseArgs =
      customExecParser
        (prefs showHelpOnEmpty)
        ( info
            (parser <**> helper)
            (fullDesc <> progDesc "A decentralised container scheduler")
        )

-------------------------------------------------------------------------------

data Args = Args
  { argsCommand :: CommandArgs,
    argsPodmanSocket :: FilePath
  }
  deriving (Show)

newtype CommandArgs = DebugArgs DebugArgs
  deriving (Show)

data DebugArgs
  = ListManagedPods
  | ListUnmanagedPods
  | ParseJobDefinition FilePath
  | CreatePodFromJob FilePath
  | DestroyPod String
  deriving (Show)

parser :: Parser Args
parser =
  Args
    <$> commands
      [ ("debug", "Debugging commands", debugArgs)
      ]
    <*> opt 'P' "podman-socket" "SOCKET" "Path to Podman socket"
  where
    debugArgs =
      DebugArgs
        <$> commands
          [ ("list-managed-pods", "List running pods managed by the cluster", pure ListManagedPods),
            ("list-unmanaged-pods", "List running pods not managed by the cluster", pure ListUnmanagedPods),
            ("parse-job-definition", "Read and dump a job configuration file", ParseJobDefinition <$> opt 'c' "config-file" "FILE" "Path to job configuration file"),
            ("create-pod-from-job", "Create a pod from a job configuration file", CreatePodFromJob <$> opt 'c' "config-file" "FILE" "Path to job configuration file"),
            ("destroy-pod", "Kill and delete a running pod", DestroyPod <$> opt 'p' "pod" "POD" "Identifier of the pod")
          ]

    opt sname lname mvar htext = strOption $ short sname <> long lname <> metavar mvar <> help htext

    commands = hsubparser . mconcat . map (\(name, desc, cmd) -> command name (cmd `info` progDesc desc))

-------------------------------------------------------------------------------

debugListPods :: Podman -> IsManaged -> IO ()
debugListPods podman isManaged =
  getAllPods podman >>= \case
    Right allPods ->
      let pods = filter (\p -> podIsManaged p == isManaged) allPods
       in if null pods
            then putStrLn "No pods."
            else traverse_ print pods
    Left err -> die (formatProblem err)

debugParseJobDefinition :: FilePath -> IO ()
debugParseJobDefinition fp =
  A.eitherDecodeFileStrict fp >>= \case
    Right jobspec -> case normaliseJobSpec jobspec of
      Right njobspec -> print njobspec
      Left err -> die (formatProblem err)
    Left err -> die (formatError "Could not parse JSON" err)

debugCreatePodFromJob :: Podman -> FilePath -> IO ()
debugCreatePodFromJob podman fp =
  A.eitherDecodeFileStrict fp >>= \case
    Right jobspec -> case normaliseJobSpec jobspec of
      Right njobspec ->
        createAndLaunchPod podman njobspec >>= \case
          Right pod -> print pod
          Left err -> die (formatProblem err)
      Left err -> die (formatProblem err)
    Left err -> die (formatError "Could not parse JSON" err)

-------------------------------------------------------------------------------

-- | Pretty-print a problem.
formatProblem :: Problem -> String
formatProblem err =
  unlines . map unpack $
    [ "ERROR: " <> problemTitle doc,
      "",
      problemDetail doc
    ]
      ++ concatMap (\(n, v) -> ["", n <> ": " <> v]) (problemExtraDetails doc)
      ++ [ "",
           "For further information, see " <> problemType doc
         ]
  where
    doc = toProblemDocument err

-- | Pretty-print an error
formatError :: String -> String -> String
formatError title detail =
  unlines
    [ "ERROR: " <> title,
      "",
      detail
    ]
