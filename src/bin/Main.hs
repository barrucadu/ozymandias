module Main (main) where

import Options.Applicative
import System.Exit (die)

main :: IO ()
main = parseArgs >>= runCommand
  where
    parseArgs =
      customExecParser
        (prefs showHelpOnEmpty)
        ( info
            (args <**> helper)
            (fullDesc <> progDesc "A decentralised container scheduler")
        )

    runCommand cmd = do
      print cmd
      die "unimplemented"

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

args :: Parser Args
args =
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
