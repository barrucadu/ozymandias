{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Aeson as A
import Data.Foldable (traverse_)
import Data.Text (pack, unpack)
import Options.Applicative
import Ozymandias.Etcd
import Ozymandias.Job
import Ozymandias.Monad
import Ozymandias.Podman
import Ozymandias.Problem
import System.Exit (die)

main :: IO ()
main = do
  args <- parseArgs
  podman <- initPodman (argsPodmanSocket args)
  etcd <- initEtcd (argsEtcdHost args)
  case argsCommand args of
    DebugArgs ListManagedPods -> debugListPods podman IsManaged
    DebugArgs ListUnmanagedPods -> debugListPods podman IsNotManaged
    DebugArgs (ParseJobDefinition fp) -> debugParseJobDefinition fp
    DebugArgs (CreatePodFromFile fp) -> debugCreatePodFromFile podman fp
    DebugArgs (CreatePodFromEtcd key) -> debugCreatePodFromEtcd etcd podman key
    DebugArgs (DestroyPod pid) -> debugDestroyPod podman pid
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
    argsPodmanSocket :: FilePath,
    argsEtcdHost :: String
  }
  deriving (Show)

newtype CommandArgs = DebugArgs DebugArgs
  deriving (Show)

data DebugArgs
  = ListManagedPods
  | ListUnmanagedPods
  | ParseJobDefinition FilePath
  | CreatePodFromFile FilePath
  | CreatePodFromEtcd String
  | DestroyPod String
  deriving (Show)

parser :: Parser Args
parser =
  Args
    <$> commands
      [ ("debug", "Debugging commands", debugArgs)
      ]
    <*> opt 'P' "podman-socket" "SOCKET" "Path to Podman socket"
    <*> opt 'E' "etcd-host" "URL" "URL of etcd host"
  where
    debugArgs =
      DebugArgs
        <$> commands
          [ ("list-managed-pods", "List running pods managed by the cluster", pure ListManagedPods),
            ("list-unmanaged-pods", "List running pods not managed by the cluster", pure ListUnmanagedPods),
            ("parse-job-definition", "Read and dump a job configuration file", ParseJobDefinition <$> opt 'f' "config-file" "FILE" "Path to job configuration file"),
            ("create-pod-from-file", "Create a pod from a job configuration file", CreatePodFromFile <$> opt 'f' "config-file" "FILE" "Path to job configuration file"),
            ("create-pod-from-etcd", "Create a pod from a job configuration in etcd", CreatePodFromEtcd <$> opt 'k' "key" "KEY" "Key to read job configuration from"),
            ("destroy-pod", "Kill and delete a running pod", DestroyPod <$> opt 'p' "pod" "POD" "Identifier of the pod")
          ]

    opt sname lname mvar htext = strOption $ short sname <> long lname <> metavar mvar <> help htext

    commands = hsubparser . mconcat . map (\(name, desc, cmd) -> command name (cmd `info` progDesc desc))

-------------------------------------------------------------------------------

debugListPods :: Podman -> IsManaged -> IO ()
debugListPods podman isManaged =
  runOz (getAllPods podman) >>= \case
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

debugCreatePodFromFile :: Podman -> FilePath -> IO ()
debugCreatePodFromFile podman fp =
  A.eitherDecodeFileStrict fp >>= \case
    Right jobspec -> debugCreatePod podman jobspec
    Left err -> die (formatError "Could not parse JSON" err)

debugCreatePodFromEtcd :: Etcd -> Podman -> String -> IO ()
debugCreatePodFromEtcd etcd podman key =
  runOz (fetchJobSpec etcd (EtcdKey (pack key))) >>= \case
    Right jobspec -> debugCreatePod podman jobspec
    Left err -> die (formatProblem err)

debugCreatePod :: Podman -> JobSpec -> IO ()
debugCreatePod podman jobspec = case normaliseJobSpec jobspec of
  Right njobspec ->
    runOz (createAndLaunchPod podman njobspec) >>= \case
      Right pod -> print pod
      Left err -> die (formatProblem err)
  Left err -> die (formatProblem err)

debugDestroyPod :: Podman -> String -> IO ()
debugDestroyPod podman pid =
  runOz (destroyPod podman (IdObj (pack pid))) >>= \case
    Right () -> putStrLn ("Pod " <> pid <> " destroyed.")
    Left err -> die (formatProblem err)

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
