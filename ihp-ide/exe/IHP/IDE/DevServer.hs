module Main (main, mainInParentDirectory, mainInAppDirectory) where

import ClassyPrelude
import qualified System.Process as Process
import IHP.HaskellSupport
import qualified Data.ByteString.Char8 as ByteString
import Control.Concurrent (myThreadId)
import System.Exit
import System.Posix.Signals

import IHP.IDE.Types
import IHP.IDE.Postgres
import IHP.IDE.StatusServer
import IHP.IDE.LiveReloadNotificationServer
import IHP.IDE.PortConfig
import IHP.IDE.ToolServer
import IHP.IDE.ToolServer.Types
import qualified IHP.SchemaCompiler as SchemaCompiler
import qualified IHP.EnvVar as EnvVar
import Data.String.Conversions (cs)
import qualified IHP.LibDir as LibDir
import qualified IHP.Telemetry as Telemetry
import qualified IHP.Version as Version

import qualified IHP.Log.Types as Log
import qualified IHP.Log as Log
import Data.Default (def, Default (..))
import qualified IHP.IDE.CodeGen.MigrationGenerator as MigrationGenerator
import Main.Utf8 (withUtf8)
import qualified IHP.FrameworkConfig as FrameworkConfig
import qualified Control.Concurrent.Chan.Unagi as Queue
import IHP.IDE.FileWatcher
import qualified System.Environment as Env
import qualified System.Directory as Directory
import qualified Control.Exception.Safe as Exception

mainInParentDirectory :: IO ()
mainInParentDirectory = do
    cwd <- Directory.getCurrentDirectory
    let projectDir = cwd <> "/../"
    Directory.setCurrentDirectory projectDir

    Env.setEnv "IHP_LIB" (cwd <> "/ihp-ide/lib/IHP")
    Env.setEnv "TOOLSERVER_STATIC" (cwd <> "/ihp-ide/lib/IHP/static")
    Env.setEnv "IHP_STATIC" (cwd <> "/lib/IHP/static")

    mainWithOptions True

main :: IO ()
main = mainWithOptions False

mainWithOptions :: Bool -> IO ()
mainWithOptions wrapWithDirenv = withUtf8 do
    databaseNeedsMigration <- newIORef False
    portConfig <- findAvailablePortConfig
    ensureUserIsNotRoot

    -- Start the dev server in Debug mode by setting the env var DEBUG=1
    -- Like: $ DEBUG=1 devenv up
    isDebugMode <- EnvVar.envOrDefault "DEBUG" False

    bracket (Log.newLogger def) (\logger -> logger.cleanup) \logger -> do
        (ghciInChan, ghciOutChan) <- Queue.newChan
        liveReloadClients <- newIORef mempty
        lastSchemaCompilerError <- newIORef Nothing
        let ?context = Context { portConfig, isDebugMode, logger, ghciInChan, ghciOutChan, wrapWithDirenv, liveReloadClients, lastSchemaCompilerError }

        -- Print IHP Version when in debug mode
        when isDebugMode (Log.debug ("IHP Version: " <> Version.ihpVersion))

        ghciIsLoadingVar <- newIORef False

        withBuiltinOrDevenvPostgres \databaseIsReady postgresStandardOutput postgresErrorOutput -> do
            withStatusServer ghciIsLoadingVar \startStatusServer stopStatusServer statusServerStandardOutput statusServerErrorOutput statusServerClients -> do
                withAppGHCI ghciIsLoadingVar startStatusServer stopStatusServer statusServerStandardOutput statusServerErrorOutput statusServerClients \inputHandle outputHandle errorHandle processHandle reloadGhciVar -> do
                    -- Compile Schema before loading the app
                    tryCompileSchema reloadGhciVar startStatusServer
                    
                    let toolServerApplication = ToolServerApplication
                            { postgresStandardOutput
                            , postgresErrorOutput
                            , appStandardOutput = statusServerStandardOutput
                            , appErrorOutput = statusServerErrorOutput
                            , appPort = portConfig.appPort
                            , databaseNeedsMigration
                            }


                    void $ runConcurrently $ (,,,,)
                        <$> Concurrently (updateDatabaseIsOutdated databaseNeedsMigration databaseIsReady)
                        <*> Concurrently (runToolServer toolServerApplication liveReloadClients)
                        <*> Concurrently (consumeGhciOutput statusServerStandardOutput statusServerErrorOutput statusServerClients)
                        <*> Concurrently Telemetry.reportTelemetry
                        <*> Concurrently (runFileWatcherWithDebounce (fileWatcherParams liveReloadClients databaseNeedsMigration databaseIsReady reloadGhciVar startStatusServer))

            pure ()

fileWatcherParams liveReloadClients databaseNeedsMigration databaseIsReady reloadGhciVar startStatusServer =
    FileWatcherParams
        { onHaskellFileChanged = putMVar reloadGhciVar ()
        , onSchemaChanged = concurrently_ (tryCompileSchema reloadGhciVar startStatusServer) (updateDatabaseIsOutdated databaseNeedsMigration databaseIsReady)
        , onAssetChanged = notifyAssetChange liveReloadClients
        }

isUsingDevenv :: IO Bool
isUsingDevenv = EnvVar.envOrDefault "IHP_DEVENV" False

ghciArguments :: [String]
ghciArguments =
    [ "-threaded"
    , "-fomit-interface-pragmas"
    , "-j"
    , "-O0"
    , "-package-env -" -- Do not load `~/.ghc/arch-os-version/environments/name file`, global packages interfere with our packages
    , "-ignore-dot-ghci" -- Ignore the global ~/.ghc/ghci.conf That file sometimes causes trouble (specifically `:set +c +s`)
    , "-ghci-script", ".ghci" -- Because the previous line ignored default ghci config file locations, we have to manual load our .ghci
    , "+RTS", "-A128m", "-n2m", "-H2m", "--nonmoving-gc", "-N"
    ]

withGHCI :: (?context :: Context) => (Handle -> Handle -> Handle -> Process.ProcessHandle -> IO a) -> IO a
withGHCI callback = do
    let params = (procDirenvAware "ghci" ghciArguments)
            { Process.std_in = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.CreatePipe
            , Process.create_group = True
            }

    Process.withCreateProcess params \(Just input) (Just output) (Just error) processHandle -> callback input output error processHandle

-- | Exit with an error if running as the root user
--
-- When the dev server starts the postgres server, it will fail if run as root:
--
-- > initdb: cannot be run as root
--
-- This is a bit hard to debug, therefore we proactively fail early when run as root
--
ensureUserIsNotRoot :: IO ()
ensureUserIsNotRoot = do
    username <- EnvVar.envOrDefault "USERNAME" ("" :: ByteString)
    when (username == "root") do
        ByteString.hPutStrLn stderr "Cannot be run as root: The IHP dev server cannot be run with the root user because we cannot start the postgres server with a root user.\n\nPlease run this with a normal user.\nIf you need help, join the IHP Slack: https://ihp.digitallyinduced.com/Slack"
        exitFailure

initGHCICommands = 
    [ -- The app is loaded by loading .ghci, which then loads applicationGhciConfig, which triggers a ':l Main.hs'
     ":set prompt \"\"" -- Disable the prompt as this caused output such as '[38;5;208mIHP>[m Ser[v3e8r; 5s;t2a0r8tmedI' instead of 'Server started'
    , "import qualified ClassyPrelude"
    ]

type WithAppGHCICallback a = Handle -> Handle -> Handle -> Process.ProcessHandle -> MVar () -> IO a
withAppGHCI :: (?context :: Context) => IORef Bool -> MVar () -> MVar () -> IORef [ByteString] -> IORef [ByteString] -> Clients -> WithAppGHCICallback a -> IO a
withAppGHCI ghciIsLoadingVar startStatusServer stopStatusServer statusServerStandardOutput statusServerErrorOutput statusServerClients callback = do
    let isDebugMode = ?context.isDebugMode
    -- The app is using the `PORT` env variable for its web server
    let appPort :: Int = fromIntegral ?context.portConfig.appPort
    Env.setEnv "PORT" (show appPort)
    libDirectory <- LibDir.findLibDirectory

    reloadGhciVar :: MVar () <- newEmptyMVar
    isRunningVar <- newIORef False

    let processReloadSignal inputHandle = forever do
            takeMVar reloadGhciVar
            isRunning <- readIORef isRunningVar
            writeIORef ghciIsLoadingVar True
            when isRunning (sendGhciCommand inputHandle "ClassyPrelude.uninterruptibleCancel app")
            writeIORef isRunningVar False
            sendGhciCommand inputHandle ":r"
            clearStatusServer statusServerStandardOutput statusServerErrorOutput statusServerClients
            notifyHaskellChange ?context.liveReloadClients

    
    withGHCI \inputHandle outputHandle errorHandle processHandle -> do
        (result, _, _, _) <- runConcurrently $ (,,,)
                <$> Concurrently ((do
                        sendGhciCommands inputHandle initGHCICommands
                        callback inputHandle outputHandle errorHandle processHandle reloadGhciVar
                    ) :: IO _)
                <*> Concurrently ((processReloadSignal inputHandle) :: IO _)
                <*> Concurrently ((forever (ByteString.hGetLine outputHandle >>= handleAppOutputLine ghciIsLoadingVar isRunningVar startStatusServer stopStatusServer inputHandle)) :: IO _)
                <*> Concurrently ((forever (ByteString.hGetLine errorHandle >>= handleAppErrorLine ghciIsLoadingVar inputHandle)) :: IO _)

        pure result
            

handleAppOutputLine :: (?context :: Context) => IORef Bool -> IORef Bool -> MVar () -> MVar () -> Handle -> ByteString -> IO ()
handleAppOutputLine ghciIsLoadingVar isRunningVar startStatusServer stopStatusServer inputHandle line = do
    unless ?context.isDebugMode (Log.info line)
    putStrLn (cs line)
    if "Server started" `isInfixOf` line
        then notifyHaskellChange ?context.liveReloadClients
        else if "Failed," `isInfixOf` line
                then do
                    _ <- tryPutMVar startStatusServer ()
                    notifyHaskellChange ?context.liveReloadClients
                    writeIORef ghciIsLoadingVar False
                else if "modules loaded." `isInfixOf` line
                    then do
                        writeIORef ghciIsLoadingVar False
                        hasSchemaCompilerError <- isJust <$> readIORef ?context.lastSchemaCompilerError
                        if hasSchemaCompilerError
                            then do
                                _ <- putMVar startStatusServer ()
                                pure ()
                            else do
                                _ <- tryPutMVar stopStatusServer ()
                                writeIORef isRunningVar True
                                sendGhciCommands inputHandle
                                    [ "ClassyPrelude.uninterruptibleCancel app"
                                    , "app <- ClassyPrelude.async (main `catch` \\(e :: SomeException) -> IHP.Prelude.putStrLn (tshow e))"
                                    ]



                    else receiveAppOutput (StandardOutput line)

handleAppErrorLine ghciIsLoadingVar inputHandle line = do
    unless ?context.isDebugMode (Log.info line)
    if "cannot find object file for module" `isInfixOf` line
        then do
            writeIORef ghciIsLoadingVar False
            sendGhciCommands inputHandle initGHCICommands
            receiveAppOutput (ErrorOutput "Linking Issue: Reloading Main")
        else receiveAppOutput (ErrorOutput line)

receiveAppOutput :: (?context :: Context) => OutputLine -> IO ()
receiveAppOutput line = Queue.writeChan ?context.ghciInChan line

checkDatabaseIsOutdated :: IO Bool
checkDatabaseIsOutdated = do
    databaseUrl <- cs <$> FrameworkConfig.defaultDatabaseUrl
    diff <- MigrationGenerator.diffAppDatabase True databaseUrl
    pure (not (isEmpty diff))

updateDatabaseIsOutdated :: (?context :: Context) => IORef Bool -> MVar () -> IO ()
updateDatabaseIsOutdated databaseNeedsMigrationRef databaseIsReady = do
    result <- Exception.tryAny do
            readMVar databaseIsReady
            databaseNeedsMigration <- checkDatabaseIsOutdated
            writeIORef databaseNeedsMigrationRef databaseNeedsMigration

    case result of
        Left exception -> Log.error (tshow exception)
        Right _ -> pure ()

tryCompileSchema :: (?context :: Context) => MVar () -> MVar () -> IO ()
tryCompileSchema reloadGhciVar startStatusServer = do
    result <- Exception.tryAny SchemaCompiler.compile
    
    case result of
        Left exception -> do
            Log.error (tshow exception)
            receiveAppOutput (ErrorOutput (cs $ displayException exception))

            writeIORef ?context.lastSchemaCompilerError (Just exception)
            putMVar reloadGhciVar ()
            putMVar startStatusServer ()

        Right _ -> do
            writeIORef ?context.lastSchemaCompilerError Nothing
            putMVar reloadGhciVar ()
