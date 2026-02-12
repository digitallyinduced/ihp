module Test.IDE.PostgresSpec where

import IHP.Prelude
import Test.Hspec
import IHP.IDE.Postgres (withBuiltinOrDevenvPostgres)
import IHP.IDE.Types (Context (..))
import IHP.IDE.PortConfig (PortConfig (..))
import qualified IHP.Log as Log
import Control.Concurrent.MVar (takeMVar)
import qualified Control.Concurrent.Chan.Unagi as Queue
import qualified Control.Exception.Safe as Exception
import qualified Data.Map as Map
import Data.Default (def)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Network.Socket as Socket
import qualified System.Directory as Directory
import qualified System.Environment as Env
import qualified System.Process as Process
import qualified System.Timeout as Timeout

tests :: Spec
tests = describe "IHP.IDE.Postgres" do
    it "runs postgres setup through direnv in wrapped mode" do
        withTemporaryTestDirectory \testDir -> do
            let scriptsDir = testDir <> "/bin"
            let commandLog = testDir <> "/commands.log"
            path <- fromMaybe "" <$> Env.lookupEnv "PATH"

            writeExecutable (scriptsDir <> "/direnv") direnvScript
            writeExecutable (scriptsDir <> "/initdb") initdbScript
            writeExecutable (scriptsDir <> "/createdb") createdbScript
            writeExecutable (scriptsDir <> "/psql") psqlScript
            writeExecutable (scriptsDir <> "/postgres") postgresScript

            withEnvVar "PATH" (scriptsDir <> ":" <> path) do
                withEnvVar "IHP_DEVENV" "0" do
                    withEnvVar "IHP_TEST_COMMAND_LOG" commandLog do
                        withTestContext True do
                            withCurrentDirectory testDir do
                                withBuiltinOrDevenvPostgres \databaseIsReady _ _ -> do
                                    ready <- Timeout.timeout (2 * 1000 * 1000) (takeMVar databaseIsReady)
                                    ready `shouldBe` Just ()

            commandLogContent <- TextIO.readFile commandLog
            let commandLines = Text.lines commandLogContent
            commandLines `shouldSatisfy` any ("exec . initdb " `Text.isPrefixOf`)
            commandLines `shouldSatisfy` any ("exec . createdb " `Text.isPrefixOf`)
            commandLines `shouldSatisfy` any ("exec . psql " `Text.isPrefixOf`)
            commandLines `shouldSatisfy` any ("-f Application/Schema.sql" `Text.isInfixOf`)
            commandLines `shouldSatisfy` any ("-f Application/Fixtures.sql" `Text.isInfixOf`)
            length (filter ("exec . postgres " `Text.isPrefixOf`) commandLines) `shouldSatisfy` (>= 2)

withEnvVar :: String -> String -> IO a -> IO a
withEnvVar key value action = do
    oldValue <- Env.lookupEnv key
    Exception.bracket_
        (Env.setEnv key value)
        (restoreEnvVar oldValue)
        action
    where
        restoreEnvVar (Just oldValue) = Env.setEnv key oldValue
        restoreEnvVar Nothing = Env.unsetEnv key

withTemporaryTestDirectory :: (FilePath -> IO a) -> IO a
withTemporaryTestDirectory callback = do
    currentDirectory <- Directory.getCurrentDirectory
    let testDir = currentDirectory <> "/build/test-postgres-direnv"
    ignoreIOError (Directory.removePathForcibly testDir)
    Directory.createDirectoryIfMissing True (testDir <> "/Application")
    Directory.createDirectoryIfMissing True (testDir <> "/bin")
    TextIO.writeFile (testDir <> "/Application/Schema.sql") "CREATE TABLE test_table (id UUID PRIMARY KEY);\n"
    TextIO.writeFile (testDir <> "/Application/Fixtures.sql") ""
    Exception.finally
        (callback testDir)
        (ignoreIOError (Directory.removePathForcibly testDir))

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory workingDirectory callback = do
    oldDirectory <- Directory.getCurrentDirectory
    Exception.bracket_
        (Directory.setCurrentDirectory workingDirectory)
        (Directory.setCurrentDirectory oldDirectory)
        callback

ignoreIOError :: IO () -> IO ()
ignoreIOError io = io `Exception.catchAny` \_ -> pure ()

withTestContext :: Bool -> ((?context :: Context) => IO a) -> IO a
withTestContext wrapWithDirenv callback = Exception.bracket createContext cleanupContext runCallback
    where
        createContext = do
            logger <- Log.newLogger def
            (ghciInChan, ghciOutChan) <- Queue.newChan
            liveReloadClients <- newIORef Map.empty
            lastSchemaCompilerError <- newIORef Nothing
            (appSocket, helperSocket) <- Socket.socketPair Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
            Socket.close helperSocket
            let portConfig = PortConfig { appPort = 8000, toolServerPort = 8001 }
            let isDebugMode = False
            let context = Context { portConfig, isDebugMode, logger, ghciInChan, ghciOutChan, wrapWithDirenv, liveReloadClients, lastSchemaCompilerError, appSocket }
            pure context

        cleanupContext context = do
            Socket.close context.appSocket
            context.logger.cleanup

        runCallback context = do
            let ?context = context
            callback

writeExecutable :: FilePath -> Text -> IO ()
writeExecutable filePath content = do
    TextIO.writeFile filePath content
    Process.callProcess "chmod" ["+x", filePath]

direnvScript :: Text
direnvScript = Text.unlines
    [ "#!/usr/bin/env bash"
    , "set -euo pipefail"
    , "if [ \"${1:-}\" != \"exec\" ] || [ \"${2:-}\" != \".\" ]; then"
    , "  echo \"unexpected direnv invocation: $*\" >&2"
    , "  exit 1"
    , "fi"
    , "echo \"$*\" >> \"$IHP_TEST_COMMAND_LOG\""
    , "shift 2"
    , "export IHP_TEST_CALLED_VIA_DIRENV=1"
    , "\"$@\""
    ]

initdbScript :: Text
initdbScript = Text.unlines
    [ "#!/usr/bin/env bash"
    , "set -euo pipefail"
    , "if [ \"${IHP_TEST_CALLED_VIA_DIRENV:-0}\" != \"1\" ]; then"
    , "  echo \"initdb called without direnv\" >&2"
    , "  exit 1"
    , "fi"
    , "mkdir -p \"$1\""
    ]

createdbScript :: Text
createdbScript = Text.unlines
    [ "#!/usr/bin/env bash"
    , "set -euo pipefail"
    , "if [ \"${IHP_TEST_CALLED_VIA_DIRENV:-0}\" != \"1\" ]; then"
    , "  echo \"createdb called without direnv\" >&2"
    , "  exit 1"
    , "fi"
    ]

psqlScript :: Text
psqlScript = Text.unlines
    [ "#!/usr/bin/env bash"
    , "set -euo pipefail"
    , "if [ \"${IHP_TEST_CALLED_VIA_DIRENV:-0}\" != \"1\" ]; then"
    , "  echo \"psql called without direnv\" >&2"
    , "  exit 1"
    , "fi"
    ]

postgresScript :: Text
postgresScript = Text.unlines
    [ "#!/usr/bin/env bash"
    , "set -euo pipefail"
    , "if [ \"${IHP_TEST_CALLED_VIA_DIRENV:-0}\" != \"1\" ]; then"
    , "  echo \"postgres called without direnv\" >&2"
    , "  exit 1"
    , "fi"
    , "echo \"database system is ready to accept connections\" >&2"
    , "trap 'exit 0' INT TERM"
    , "while true; do"
    , "  sleep 0.1"
    , "done"
    ]
