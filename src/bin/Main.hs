module Main (main) where

import Data.Foldable (traverse_)
import Options.Applicative
import Ozymandias.Podman
import System.Exit (die)

main :: IO ()
main = do
  args <- parseArgs
  podman <- initPodman (argsPodmanSocket args)
  case argsCommand args of
    DebugArgs ListManagedPods -> debugListPods podman IsManaged
    DebugArgs ListUnmanagedPods -> debugListPods podman IsNotManaged
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
  | CreatePod FilePath
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
            ("create-pod", "Create a pod from a configuration file", CreatePod <$> opt 'c' "config-file" "FILE" "Path to pod configuration file"),
            ("destroy-pod", "Kill and delete a running pod", DestroyPod <$> opt 'p' "pod" "POD" "Identifier of the pod")
          ]

    opt sname lname mvar htext = strOption $ short sname <> long lname <> metavar mvar <> help htext

    commands = hsubparser . mconcat . map (\(name, desc, cmd) -> command name (cmd `info` progDesc desc))

-------------------------------------------------------------------------------

debugListPods :: Podman -> IsManaged -> IO ()
debugListPods podman isManaged = do
  pods <- filter (\p -> podIsManaged p == isManaged) <$> getAllPods podman
  if null pods
    then putStrLn "No pods."
    else traverse_ print pods
