module Main (main, mainInParentDirectory, mainInProjectDirectory) where

import ClassyPrelude
import qualified System.Process as Process
import IHP.HaskellSupport
import qualified Data.ByteString.Char8 as ByteString
import Control.Concurrent (myThreadId, threadDelay)
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
import qualified Data.ByteString.Builder as ByteString
import qualified Network.Socket as Socket
import qualified System.IO as IO

mainInParentDirectory :: IO ()
mainInParentDirectory = do
    cwd <- Directory.getCurrentDirectory
    mainInProjectDirectory (cwd <> "/../")

mainInProjectDirectory :: FilePath -> IO ()
mainInProjectDirectory projectDir = do
    cwd <- Directory.getCurrentDirectory

    withCurrentWorkingDirectory projectDir do
        Env.setEnv "IHP_LIB" (cwd <> "/ihp-ide/lib/IHP")
        Env.setEnv "TOOLSERVER_STATIC" (cwd <> "/ihp-ide/lib/IHP/static")
        Env.setEnv "IHP_STATIC" (cwd <> "/lib/IHP/static")

        mainWithOptions True

withCurrentWorkingDirectory :: FilePath -> IO result -> IO result
withCurrentWorkingDirectory workingDirectory callback = do
    cwd <- Directory.getCurrentDirectory
    Exception.bracket_
        (Directory.setCurrentDirectory workingDirectory)
        (Directory.setCurrentDirectory cwd)
        callback

main :: IO ()
main = mainWithOptions False

mainWithOptions :: Bool -> IO ()
mainWithOptions wrapWithDirenv = withUtf8 do
    -- https://github.com/digitallyinduced/ihp/issues/2134
    -- devenv will redirect the standard handles to a pipe, causing block buffering by default
    -- We need to override this so that `putStrLn` etc. works as expected
    IO.hSetBuffering IO.stdout IO.LineBuffering
    IO.hSetBuffering IO.stderr IO.LineBuffering

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
        reloadGhciVar :: MVar () <- newEmptyMVar

        withBuiltinOrDevenvPostgres \databaseIsReady postgresStandardOutput postgresErrorOutput -> do
            withStatusServer ghciIsLoadingVar \startStatusServer stopStatusServer statusServerStandardOutput statusServerErrorOutput statusServerClients -> do
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


                void $ runConcurrently $ (,,,,,,)
                        <$> Concurrently (updateDatabaseIsOutdated databaseNeedsMigration databaseIsReady)
                        <*> Concurrently (runToolServer toolServerApplication liveReloadClients)
                        <*> Concurrently (consumeGhciOutput statusServerStandardOutput statusServerErrorOutput statusServerClients)
                        <*> Concurrently Telemetry.reportTelemetry
                        <*> Concurrently (runFileWatcherWithDebounce (fileWatcherParams liveReloadClients databaseNeedsMigration databaseIsReady reloadGhciVar startStatusServer))
                        <*> Concurrently (runAppGhci ghciIsLoadingVar startStatusServer stopStatusServer statusServerStandardOutput statusServerErrorOutput statusServerClients reloadGhciVar)

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

runAppGhci :: (?context :: Context) => IORef Bool -> MVar () -> MVar (MVar ()) -> IORef [ByteString] -> IORef [ByteString] -> Clients -> MVar () -> IO ()
runAppGhci ghciIsLoadingVar startStatusServer stopStatusServer statusServerStandardOutput statusServerErrorOutput statusServerClients reloadGhciVar = do
    let isDebugMode = ?context.isDebugMode
    -- The app is using the `PORT` env variable for its web server
    let appPort :: Int = fromIntegral ?context.portConfig.appPort
    Env.setEnv "PORT" (show appPort)

    let withoutStatusServer callback = Exception.bracket_
            (do isStoppedVar <- newEmptyMVar; putMVar stopStatusServer isStoppedVar; takeMVar isStoppedVar)
            (putMVar startStatusServer ())
            callback

    let processResult inputHandle outputHandle errorHandle processHandle result = do
            hasSchemaCompilerError <- isJust <$> readIORef ?context.lastSchemaCompilerError
            -- This branch blocks until .hs file change happens
            case result of
                Left failed -> takeMVar reloadGhciVar
                Right _ | hasSchemaCompilerError -> takeMVar reloadGhciVar
                Right loaded -> do
                    withoutStatusServer do
                        withRunningApp ?context.portConfig.appPort inputHandle outputHandle errorHandle processHandle receiveAppOutput do
                            takeMVar reloadGhciVar


                    pure ()
            

            writeIORef ghciIsLoadingVar True
            
            -- Clear logs in web ui
            clearStatusServer statusServerStandardOutput statusServerErrorOutput statusServerClients

            result <- refresh inputHandle outputHandle errorHandle receiveAppOutput
            
            -- reload app
            notifyHaskellChange ?context.liveReloadClients

            processResult inputHandle outputHandle errorHandle processHandle result

    
    withGHCI \inputHandle outputHandle errorHandle processHandle -> do
        writeIORef ghciIsLoadingVar True
        withLoadedApp inputHandle outputHandle errorHandle receiveAppOutput \result -> do
            processResult inputHandle outputHandle errorHandle processHandle result

withLoadedApp :: (?context :: Context) => Handle -> Handle -> Handle -> (OutputLine -> IO ()) -> ((Either LByteString LByteString) -> IO a) -> IO a
withLoadedApp inputHandle outputHandle errorHandle logLine callback = do
    outputVar :: MVar ByteString.Builder <- newMVar ""
    resultVar :: MVar Bool <- newEmptyMVar
    let readHandle handle logLine = race_ (readMVar resultVar) $ forever do
            line <- ByteString.hGetLine handle
            modifyMVar_ outputVar (\builder -> pure (builder <> "\n" <> ByteString.byteString line))
            logLine line
            case line of
                line | "Failed," `isInfixOf` line -> putMVar resultVar False
                line | "modules loaded." `isInfixOf` line -> putMVar resultVar True
                _ -> pure ()

    let main = do
            sendGhciCommands inputHandle initGHCICommands

            result <- takeMVar resultVar
            output <- takeMVar outputVar

            let resultArg = if result
                    then Right (ByteString.toLazyByteString output)
                    else Left (ByteString.toLazyByteString output)

            callback resultArg


    (result, _, _) <- runConcurrently $ (,,)
        <$> Concurrently main
        <*> Concurrently (readHandle outputHandle (\line -> logLine (StandardOutput line)))
        <*> Concurrently (readHandle errorHandle (\line -> logLine (ErrorOutput line)))

    pure result

withRunningApp :: (?context :: Context) => Socket.PortNumber -> Handle -> Handle -> Handle -> Process.ProcessHandle -> (OutputLine -> IO ()) -> (IO a) -> IO a
withRunningApp appPort inputHandle outputHandle errorHandle processHandle logLine callback = do
    outputVar :: MVar ByteString.Builder <- newMVar ""
    serverStarted :: MVar () <- newEmptyMVar
    serverStopped :: MVar () <- newEmptyMVar
    let readHandle handle logLine = race_
                (readMVar serverStopped)
                (forever do
                    line <- ByteString.hGetLine handle
                    modifyMVar_ outputVar (\builder -> pure (builder <> "\n" <> ByteString.byteString line))
                    logLine line
                    case line of
                        line | "Server started" `isInfixOf` line -> putMVar serverStarted ()
                        _ -> pure ()
                )

    let startApp = do
            sendGhciCommand inputHandle "stopVar :: ClassyPrelude.MVar () <- ClassyPrelude.newEmptyMVar"
            sendGhciCommand inputHandle "app <- ClassyPrelude.async (ClassyPrelude.race_ (ClassyPrelude.takeMVar stopVar) (main `ClassyPrelude.catch` \\(e :: SomeException) -> IHP.Prelude.putStrLn (tshow e)))"
    let stopApp = do
            sendGhciCommand inputHandle "ClassyPrelude.putMVar stopVar ()"
            sendGhciCommand inputHandle "ClassyPrelude.cancel app"
            waitForPortAvailable appPort
            putMVar serverStopped ()

    (result, _, _) <- runConcurrently $ (,,)
        <$> Concurrently (Exception.bracket_ startApp stopApp (do takeMVar serverStarted; callback))
        <*> Concurrently (readHandle outputHandle (\line -> logLine (StandardOutput line)))
        <*> Concurrently (readHandle errorHandle (\line -> logLine (ErrorOutput line)))

    pure result

waitForPortAvailable :: Socket.PortNumber -> IO ()
waitForPortAvailable port = do
    isAvailable <- isPortAvailable port
    unless isAvailable do
        putStrLn "waitForPortAvailable: wait"
        threadDelay 100000
        waitForPortAvailable port

refresh :: (?context :: Context) => Handle -> Handle -> Handle -> (OutputLine -> IO ()) -> IO (Either LByteString LByteString)
refresh inputHandle outputHandle errorHandle logOutput = do
    outputVar :: MVar ByteString.Builder <- newMVar ""
    resultVar :: MVar Bool <- newEmptyMVar
    let readHandle handle logLine = race_
                (readMVar resultVar)
                (forever do
                    line <- ByteString.hGetLine handle
                    modifyMVar_ outputVar (\builder -> pure (builder <> "\n" <> ByteString.byteString line))
                    logLine line
                    case line of
                        line | "Failed," `isInfixOf` line -> putMVar resultVar False
                        line | "modules loaded." `isInfixOf` line -> putMVar resultVar True
                        line | "cannot find object file for module" `isInfixOf` line -> do
                            -- https://gitlab.haskell.org/ghc/ghc/-/issues/11596
                            sendGhciCommand inputHandle ":l"
                        _ -> pure ()
                )

    let main = do
            sendGhciCommand inputHandle ":r"

            result <- takeMVar resultVar
            output <- takeMVar outputVar

            pure if result
                    then Right (ByteString.toLazyByteString output)
                    else Left (ByteString.toLazyByteString output)

    (result, _, _) <- runConcurrently $ (,,)
        <$> Concurrently main
        <*> Concurrently (readHandle outputHandle (\line -> logOutput (StandardOutput line)))
        <*> Concurrently (readHandle errorHandle (\line -> logOutput (ErrorOutput line)))

    pure result

receiveAppOutput :: (?context :: Context) => OutputLine -> IO ()
receiveAppOutput line = do
    case line of
        StandardOutput output -> ByteString.putStrLn output
        ErrorOutput output -> ByteString.putStrLn output
    Queue.writeChan ?context.ghciInChan line

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
            previouslyHadSchemaError <- isJust <$> readIORef ?context.lastSchemaCompilerError
            writeIORef ?context.lastSchemaCompilerError Nothing

            when previouslyHadSchemaError (putMVar reloadGhciVar ())
