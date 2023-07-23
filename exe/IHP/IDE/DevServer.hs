module Main (main) where

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
import qualified IHP.SchemaCompiler as SchemaCompiler
import qualified System.Environment as Env
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

main :: IO ()
main = withUtf8 do
    actionVar <- newEmptyMVar
    appStateRef <- emptyAppState >>= newIORef
    portConfig <- findAvailablePortConfig
    ensureUserIsNotRoot

    -- Start the dev server in Debug mode by setting the env var DEBUG=1
    -- Like: $ DEBUG=1 devenv up
    isDebugMode <- maybe False (\value -> value == "1") <$> Env.lookupEnv "DEBUG"

    logger <- Log.newLogger def
    (ghciInChan, ghciOutChan) <- Queue.newChan
    liveReloadClients <- newIORef mempty
    let ?context = Context { actionVar, portConfig, appStateRef, isDebugMode, logger, ghciInChan, ghciOutChan, liveReloadClients }

    -- Print IHP Version when in debug mode
    when isDebugMode (Log.debug ("IHP Version: " <> Version.ihpVersion))

    threadId <- myThreadId
    let catchHandler = do
            state <- readIORef appStateRef
            stop state
            throwTo threadId ExitSuccess
    installHandler sigINT (Catch catchHandler) Nothing

    start

    withToolServer do
        withAsync consumeGhciOutput \_ -> do
            withFileWatcher do
                async Telemetry.reportTelemetry
                forever do
                    appState <- readIORef appStateRef
                    when isDebugMode (Log.debug $ " ===> " <> (tshow appState))
                    action <- takeMVar actionVar
                    when isDebugMode (Log.debug $ tshow action)
                    nextAppState <- handleAction appState action
                    writeIORef appStateRef nextAppState


handleAction :: (?context :: Context) => AppState -> Action -> IO AppState
handleAction state@(AppState { appGHCIState }) (UpdatePostgresState postgresState) =
    case postgresState of
        PostgresReady -> onPostgresReady
        PostgresStarted {} -> onPostgresReady
        otherwise -> pure state { postgresState }
    where
      onPostgresReady = do
        async (updateDatabaseIsOutdated state)

        -- If the app is already running before the postgres started up correctly,
        -- we need to trigger a restart, otherwise e.g. background jobs will not start correctly
        case appGHCIState of
            AppGHCIModulesLoaded { .. } -> do
                sendGhciCommand process "ClassyPrelude.uninterruptibleCancel app"
                sendGhciCommand process ":r"
                pure state { appGHCIState = AppGHCILoading { .. }, postgresState }
            otherwise -> pure state { postgresState }
handleAction state (UpdateAppGHCIState appGHCIState) = pure state { appGHCIState }
handleAction state@(AppState { statusServerState = StatusServerNotStarted }) (UpdateStatusServerState statusServerState) = pure state { statusServerState }
handleAction state@(AppState { statusServerState = StatusServerStarted { } }) (UpdateStatusServerState StatusServerNotStarted) = pure state { statusServerState = StatusServerNotStarted }
handleAction state@(AppState { statusServerState = StatusServerPaused { } }) (UpdateStatusServerState statusServerState) = pure state { statusServerState = StatusServerNotStarted }
handleAction state@(AppState { appGHCIState, statusServerState, postgresState }) (AppModulesLoaded { success = True }) = do
    case appGHCIState of
        AppGHCILoading { .. } -> do
            let appGHCIState' = AppGHCIModulesLoaded { .. }
            let startApp = do
                            stopStatusServer statusServerState
                            startLoadedApp appGHCIState


                            let statusServerState' = case statusServerState of
                                    StatusServerStarted { .. } -> StatusServerPaused { .. }
                                    _ -> statusServerState

                            pure state { appGHCIState = appGHCIState', statusServerState = statusServerState' }

            hasSchemaCompilerError <- isJust <$> readIORef state.lastSchemaCompilerError
            case postgresState of
                PostgresStarted {} | not hasSchemaCompilerError -> startApp
                PostgresReady | not hasSchemaCompilerError -> startApp
                _ -> do
                    when ?context.isDebugMode (Log.debug ("AppModulesLoaded but db not in PostgresStarted state, therefore not starting app yet" :: Text))
                    pure state { appGHCIState = appGHCIState' }

        RunningAppGHCI { } -> pure state -- Do nothing as app is already in running state
        AppGHCINotStarted -> error "Unreachable AppGHCINotStarted"
        AppGHCIModulesLoaded { } -> do
            -- You can trigger this case by running: $ while true; do touch test.hs; done;
            when ?context.isDebugMode (Log.debug ("AppGHCIModulesLoaded triggered multiple times. This happens when multiple file change events are detected. Skipping app start as the app is already starting from a previous file change event" :: Text))
            pure state
handleAction state@(AppState { appGHCIState, statusServerState, postgresState }) (AppModulesLoaded { success = False }) = do
    statusServerState' <- case statusServerState of
        s@(StatusServerPaused { .. }) -> do
            async $ continueStatusServer s
            pure StatusServerStarted { .. }
        o -> pure o

    let newAppGHCIState =
            case appGHCIState of
                AppGHCILoading { .. } -> AppGHCIModulesLoaded { .. }
                AppGHCIModulesLoaded { .. } -> AppGHCIModulesLoaded { .. }
                RunningAppGHCI { .. } -> AppGHCIModulesLoaded { .. }
                AppGHCINotStarted {} -> error "Modules cannot be loaded when ghci not in started state"

    notifyHaskellChange

    pure state { statusServerState = statusServerState', appGHCIState = newAppGHCIState }

handleAction state@(AppState { statusServerState, appGHCIState }) AppStarted = do
    notifyHaskellChange
    case appGHCIState of
        AppGHCIModulesLoaded { .. } -> pure state { appGHCIState = RunningAppGHCI { .. } }
        RunningAppGHCI { } -> pure state
        otherwise -> pure state

handleAction state@(AppState { appGHCIState, statusServerState }) HaskellFileChanged = do
    case appGHCIState of
        AppGHCIModulesLoaded { .. } -> do
            -- The app might already have been triggered
            -- but the the "Server started" message might not have been received yet
            sendGhciCommand process "ClassyPrelude.uninterruptibleCancel app"
            sendGhciCommand process ":r"
        RunningAppGHCI { .. } -> do
            sendGhciCommand process "ClassyPrelude.uninterruptibleCancel app"
            sendGhciCommand process ":r"
        AppGHCILoading { .. } -> sendGhciCommand process ":r"
        AppGHCINotStarted -> pure ()

    clearStatusServer statusServerState

    lastSchemaCompilerError <- readIORef state.lastSchemaCompilerError
    case lastSchemaCompilerError of
        Just exception -> receiveAppOutput (ErrorOutput (cs $ displayException exception))
        Nothing -> pure ()

    let appGHCIState' =
            case appGHCIState of
                AppGHCILoading { .. } -> AppGHCILoading { .. }
                AppGHCIModulesLoaded { .. } -> AppGHCILoading { .. }
                RunningAppGHCI { .. } -> AppGHCILoading { .. }
                AppGHCINotStarted -> AppGHCINotStarted
    pure state { appGHCIState = appGHCIState' }

handleAction state SchemaChanged = do
    async tryCompileSchema
    async (updateDatabaseIsOutdated state)
    pure state

handleAction state@(AppState { appGHCIState }) PauseApp =
    case appGHCIState of
        RunningAppGHCI { .. } -> do
            pauseAppGHCI appGHCIState
            pure state { appGHCIState = AppGHCIModulesLoaded { .. } }
        otherwise -> do Log.info ("Could not pause app as it's not in running state" <> tshow otherwise); pure state

start :: (?context :: Context) => IO ()
start = do
    async startStatusServer
    async startAppGHCI
    async startOrWaitPostgres
    pure ()

stop :: (?context :: Context) => AppState -> IO ()
stop AppState { .. } = do
    useDevenv <- isUsingDevenv
    when ?context.isDebugMode (Log.debug ("Stop called" :: Text))
    stopAppGHCI appGHCIState
    when (not useDevenv) $ stopPostgres postgresState
    stopStatusServer statusServerState

isUsingDevenv :: IO Bool
isUsingDevenv = do
    Env.lookupEnv "IHP_DEVENV" >>= \case
        Just "1" -> pure True
        Nothing -> pure False

startOrWaitPostgres :: (?context :: Context) => IO ()
startOrWaitPostgres = do
    useDevenv <- isUsingDevenv
    if useDevenv
    then waitPostgres
    else do
        startPostgres
        pure ()

startGHCI :: IO ManagedProcess
startGHCI = do
    let args =
            [ "-threaded"
            , "-fomit-interface-pragmas"
            , "-j"
            , "-O0"
            , "-package-env -" -- Do not load `~/.ghc/arch-os-version/environments/name file`, global packages interfere with our packages
            , "-ignore-dot-ghci" -- Ignore the global ~/.ghc/ghci.conf That file sometimes causes trouble (specifically `:set +c +s`)
            , "-ghci-script", ".ghci" -- Because the previous line ignored default ghci config file locations, we have to manual load our .ghci
            , "+RTS", "-A128m", "-n2m", "-H2m", "--nonmoving-gc", "-N"
            ]
    createManagedProcess (Process.proc "ghci" args)
            { Process.std_in = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.CreatePipe
            }

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
    username <- fromMaybe "" <$> Env.lookupEnv "USERNAME"
    when (username == "root") do
        ByteString.hPutStrLn stderr "Cannot be run as root: The IHP dev server cannot be run with the root user because we cannot start the postgres server with a root user.\n\nPlease run this with a normal user.\nIf you need help, join the IHP Slack: https://ihp.digitallyinduced.com/Slack"
        exitFailure

startAppGHCI :: (?context :: Context) => IO ()
startAppGHCI = do
    let isDebugMode = ?context.isDebugMode
    -- The app is using the `PORT` env variable for its web server
    let appPort :: Int = fromIntegral ?context.portConfig.appPort
    Env.setEnv "PORT" (show appPort)

    process <- startGHCI

    let ManagedProcess { outputHandle, errorHandle } = process

    libDirectory <- LibDir.findLibDirectory

    let loadAppCommands =
            [ -- The app is loaded by loading .ghci, which then loads applicationGhciConfig, which triggers a ':l Main.hs'
             ":set prompt \"\"" -- Disable the prompt as this caused output such as '[38;5;208mIHP>[m Ser[v3e8r; 5s;t2a0r8tmedI' instead of 'Server started'
            , "import qualified ClassyPrelude"
            ]

    async $ forever $ ByteString.hGetLine outputHandle >>= \line -> do
                unless isDebugMode (Log.info line)
                if "Server started" `isInfixOf` line
                    then dispatch AppStarted
                    else if "Failed," `isInfixOf` line
                            then do
                                dispatch AppModulesLoaded { success = False }
                            else if "modules loaded." `isInfixOf` line
                                then do
                                    dispatch AppModulesLoaded { success = True }
                                else receiveAppOutput (StandardOutput line)

    async $ forever $ ByteString.hGetLine errorHandle >>= \line -> do
        unless isDebugMode (Log.info line)
        if "cannot find object file for module" `isInfixOf` line
            then do
                forEach loadAppCommands (sendGhciCommand process)
                receiveAppOutput (ErrorOutput "Linking Issue: Reloading Main")
            else receiveAppOutput (ErrorOutput line)


    -- Compile Schema before loading the app
    tryCompileSchema

    forEach loadAppCommands (sendGhciCommand process)

    dispatch (UpdateAppGHCIState (AppGHCILoading { .. }))

receiveAppOutput :: (?context :: Context) => OutputLine -> IO ()
receiveAppOutput line = Queue.writeChan ?context.ghciInChan line

startLoadedApp :: (?context :: Context) => AppGHCIState -> IO ()
startLoadedApp (AppGHCIModulesLoaded { .. }) = do
    let commands =
            [ "ClassyPrelude.uninterruptibleCancel app"
            , "app <- ClassyPrelude.async (main `catch` \\(e :: SomeException) -> IHP.Prelude.putStrLn (tshow e))"
            ]
    forEach commands (sendGhciCommand process)
startLoadedApp (RunningAppGHCI { .. }) = error "Cannot start app as it's already in running statstate"
startLoadedApp (AppGHCILoading { .. }) = sendGhciCommand process "app <- ClassyPrelude.async (main `catch` \\(e :: SomeException) -> IHP.Prelude.putStrLn (tshow e))"
startLoadedApp _ = when ?context.isDebugMode (Log.debug ("startLoadedApp: App not running" :: Text))


stopAppGHCI :: AppGHCIState -> IO ()
stopAppGHCI RunningAppGHCI { process } = cleanupManagedProcess process
stopAppGHCI AppGHCIModulesLoaded { process } = cleanupManagedProcess process
stopAppGHCI _ = pure ()

pauseAppGHCI :: (?context :: Context) => AppGHCIState -> IO ()
pauseAppGHCI RunningAppGHCI { process } = sendGhciCommand process "ClassyPrelude.uninterruptibleCancel app"
pauseAppGHCI _ = pure ()

checkDatabaseIsOutdated :: IO Bool
checkDatabaseIsOutdated = do
    databaseUrl <- cs <$> FrameworkConfig.defaultDatabaseUrl
    diff <- MigrationGenerator.diffAppDatabase True databaseUrl
    pure (not (isEmpty diff))

updateDatabaseIsOutdated state = ((do
            let databaseNeedsMigrationRef = state.databaseNeedsMigration
            databaseNeedsMigration <- checkDatabaseIsOutdated
            writeIORef databaseNeedsMigrationRef databaseNeedsMigration
        ) `catch` (\(exception :: SomeException) -> do
            Log.error (tshow exception)
            receiveAppOutput (ErrorOutput (cs $ tshow exception))
        ))

tryCompileSchema :: (?context :: Context) => IO ()
tryCompileSchema =
    (do
        SchemaCompiler.compile
        state <- readIORef ?context.appStateRef
        writeIORef state.lastSchemaCompilerError Nothing
    ) `catch` (\(exception :: SomeException) -> do
            Log.error (tshow exception)
            receiveAppOutput (ErrorOutput (cs $ displayException exception))

            state <- readIORef ?context.appStateRef
            writeIORef state.lastSchemaCompilerError (Just exception)
        )
