module Main (main) where

import ClassyPrelude
import qualified System.Process as Process
import IHP.HaskellSupport
import qualified Data.ByteString.Char8 as ByteString
import Control.Concurrent (threadDelay, myThreadId)
import System.Exit
import System.Posix.Signals
import qualified System.FSNotify as FS

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
import qualified Data.Time.Clock as Clock

import qualified IHP.Log.Types as Log
import qualified IHP.Log as Log
import Data.Default (def, Default (..))
import qualified IHP.IDE.CodeGen.MigrationGenerator as MigrationGenerator
import Main.Utf8 (withUtf8)





import Config
import qualified IHP.Server
import IHP.RouterSupport hiding (get)
import IHP.FrameworkConfig
import IHP.Job.Types
import Web.FrontController
import Web.Types

main :: IO ()
main = withUtf8 do
    actionVar <- newEmptyMVar
    appStateRef <- emptyAppState >>= newIORef
    portConfig <- findAvailablePortConfig

    -- Start the dev server in Debug mode by setting the env var DEBUG=1
    -- Like: $ DEBUG=1 ./start
    isDebugMode <- maybe True (\value -> value == "1") <$> Env.lookupEnv "DEBUG"

    logger <- Log.newLogger def
    let ?context = Context { actionVar, portConfig, appStateRef, isDebugMode, logger }

    -- Print IHP Version when in debug mode
    when isDebugMode (Log.debug ("IHP Version: " <> Version.ihpVersion))

    threadId <- myThreadId
    let catchHandler = do
            state <- readIORef appStateRef
            stop state
            throwTo threadId ExitSuccess
    installHandler sigINT (Catch catchHandler) Nothing

    start
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
        PostgresStarted {} -> do
            async (updateDatabaseIsOutdated state)
            
            -- If the app is already running before the postgres started up correctly,
            -- we need to trigger a restart, otherwise e.g. background jobs will not start correctly
            case appGHCIState of
                AppGHCIModulesLoaded { .. } -> do
                    sendGhciCommand process "ClassyPrelude.uninterruptibleCancel app"
                    sendGhciCommand process ":r"
                    pure state { appGHCIState = AppGHCILoading { .. }, postgresState }
                otherwise -> pure state { postgresState }
        otherwise -> pure state { postgresState }
handleAction state (UpdateAppGHCIState appGHCIState) = pure state { appGHCIState }
handleAction state (UpdateToolServerState toolServerState) = pure state { toolServerState }
handleAction state@(AppState { statusServerState = StatusServerNotStarted }) (UpdateStatusServerState statusServerState) = pure state { statusServerState }
handleAction state@(AppState { statusServerState = StatusServerStarted { } }) (UpdateStatusServerState StatusServerNotStarted) = pure state { statusServerState = StatusServerNotStarted }
handleAction state@(AppState { statusServerState = StatusServerPaused { } }) (UpdateStatusServerState statusServerState) = pure state { statusServerState = StatusServerNotStarted }
handleAction state (UpdateFileWatcherState fileWatcherState) = pure state { fileWatcherState }
handleAction state@(AppState { statusServerState }) ReceiveAppOutput { line } = do
    notifyBrowserOnApplicationOutput statusServerState line
    pure state
handleAction state@(AppState { appGHCIState, statusServerState, postgresState }) (AppModulesLoaded { success = True }) = do
    case appGHCIState of
        AppGHCILoading { .. } -> do
            let appGHCIState' = AppGHCIModulesLoaded { .. }

            case postgresState of
                PostgresStarted {} -> do
                    startLoadedApp appGHCIState
                    let statusServerState' = case statusServerState of
                            StatusServerStarted { .. } -> StatusServerPaused { .. }
                            _ -> statusServerState

                    pure state { appGHCIState = appGHCIState', statusServerState = statusServerState' }
                _ -> do
                    when (get #isDebugMode ?context) (Log.debug ("AppModulesLoaded but db not in PostgresStarted state, therefore not starting app yet" :: Text))
                    pure state { appGHCIState = appGHCIState' }

        RunningAppGHCI { } -> pure state -- Do nothing as app is already in running state
        AppGHCINotStarted -> error "Unreachable AppGHCINotStarted"
        AppGHCIModulesLoaded { } -> do
            -- You can trigger this case by running: $ while true; do touch test.hs; done;
            when (get #isDebugMode ?context) (Log.debug ("AppGHCIModulesLoaded triggered multiple times. This happens when multiple file change events are detected. Skipping app start as the app is already starting from a previous file change event" :: Text))
            pure state
handleAction state@(AppState { appGHCIState, statusServerState, postgresState, liveReloadNotificationServerState }) (AppModulesLoaded { success = False }) = do
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

    notifyHaskellChange liveReloadNotificationServerState

    pure state { statusServerState = statusServerState', appGHCIState = newAppGHCIState }

handleAction state@(AppState { statusServerState, appGHCIState, liveReloadNotificationServerState }) AppStarted = do
    notifyHaskellChange liveReloadNotificationServerState
    case appGHCIState of
        AppGHCIModulesLoaded { .. } -> pure state { appGHCIState = RunningAppGHCI { .. } }
        RunningAppGHCI { } -> pure state
        otherwise -> pure state

handleAction state@(AppState { liveReloadNotificationServerState }) AssetChanged = do
    notifyAssetChange liveReloadNotificationServerState
    pure state

handleAction state SchemaChanged = do
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
    async startToolServer
    async startAppGHCI
    async startPostgres
    async startFileWatcher
    pure ()

stop :: (?context :: Context) => AppState -> IO ()
stop AppState { .. } = do
    when (get #isDebugMode ?context) (Log.debug ("Stop called" :: Text))
    stopAppGHCI appGHCIState
    stopPostgres postgresState
    stopFileWatcher fileWatcherState
    stopToolServer toolServerState

startFileWatcher :: (?context :: Context) => IO ()
startFileWatcher = do
        let fileWatcherDebounceTime = Clock.secondsToNominalDiffTime 0.1 -- 100ms
        let fileWatcherConfig = FS.defaultConfig { FS.confDebounce = FS.Debounce fileWatcherDebounceTime }
        thread <- async $ FS.withManagerConf fileWatcherConfig $ \manager -> do
            FS.watchTree manager "." shouldActOnFileChange handleFileChange
            forever (threadDelay maxBound) `finally` FS.stopManager manager
        dispatch (UpdateFileWatcherState (FileWatcherStarted { thread }))
    where
        handleFileChange event = do
            let filePath = getEventFilePath event
            if "Application/Schema.sql" `isSuffixOf` filePath
                then dispatch SchemaChanged
                else if isAssetFile filePath
                    then dispatch AssetChanged
                    else mempty

        shouldActOnFileChange :: FS.ActionPredicate
        shouldActOnFileChange event =
            let path = getEventFilePath event
            in isAssetFile path || isSQLFile path

        isAssetFile = isSuffixOf ".css"
        isSQLFile = isSuffixOf ".sql"

        getEventFilePath :: FS.Event -> FilePath
        getEventFilePath event = case event of
                FS.Added filePath _ _ -> filePath
                FS.Modified filePath _ _ -> filePath
                FS.Removed filePath _ _ -> filePath
                FS.Unknown filePath _ _ -> filePath

stopFileWatcher :: FileWatcherState -> IO ()
stopFileWatcher FileWatcherStarted { thread } = uninterruptibleCancel thread
stopFileWatcher _ = pure ()

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

startAppGHCI :: (?context :: Context) => IO ()
startAppGHCI = IHP.Server.run config

startLoadedApp :: (?context :: Context) => AppGHCIState -> IO ()
startLoadedApp (AppGHCIModulesLoaded { .. }) = do
    let commands =
            [ "ClassyPrelude.uninterruptibleCancel app"
            , "app <- ClassyPrelude.async (main `catch` \\(e :: SomeException) -> IHP.Prelude.putStrLn (tshow e))"
            ]
    forEach commands (sendGhciCommand process)
startLoadedApp (RunningAppGHCI { .. }) = error "Cannot start app as it's already in running statstate"
startLoadedApp (AppGHCILoading { .. }) = sendGhciCommand process "app <- ClassyPrelude.async (main `catch` \\(e :: SomeException) -> IHP.Prelude.putStrLn (tshow e))"
startLoadedApp _ = when (get #isDebugMode ?context) (Log.debug ("startLoadedApp: App not running" :: Text))


stopAppGHCI :: AppGHCIState -> IO ()
stopAppGHCI RunningAppGHCI { process } = cleanupManagedProcess process
stopAppGHCI AppGHCIModulesLoaded { process } = cleanupManagedProcess process
stopAppGHCI _ = pure ()

pauseAppGHCI :: (?context :: Context) => AppGHCIState -> IO ()
pauseAppGHCI RunningAppGHCI { process } = sendGhciCommand process "ClassyPrelude.uninterruptibleCancel app"
pauseAppGHCI _ = pure ()

checkDatabaseIsOutdated :: IO Bool
checkDatabaseIsOutdated = do
    diff <- MigrationGenerator.diffAppDatabase
    pure (not (isEmpty diff))

updateDatabaseIsOutdated state = ((do
            let databaseNeedsMigrationRef = state |> get #databaseNeedsMigration
            databaseNeedsMigration <- checkDatabaseIsOutdated
            writeIORef databaseNeedsMigrationRef databaseNeedsMigration
        ) `catch` (\(exception :: SomeException) -> do
            Log.error (tshow exception)
            dispatch (ReceiveAppOutput { line = ErrorOutput (cs $ tshow exception) })
        ))


instance FrontController RootApplication where
    controllers = [
            mountFrontController WebApplication
        ]

instance Worker RootApplication where
    workers _ = []
