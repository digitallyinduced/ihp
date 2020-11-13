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
import System.Info
import Data.String.Conversions (cs)
import qualified IHP.FrameworkConfig as Config
import IHP.Environment
import qualified IHP.LibDir as LibDir

main :: IO ()
main = do
    actionVar <- newEmptyMVar
    appStateRef <- newIORef emptyAppState
    portConfig <- findAvailablePortConfig
    LibDir.ensureSymlink

    -- Start the dev server in Debug mode by setting the env var DEBUG=1
    -- Like: $ DEBUG=1 ./start
    isDebugMode <- maybe False (\value -> value == "1") <$> Env.lookupEnv "DEBUG"
    let ?context = Context { actionVar, portConfig, appStateRef, isDebugMode }

    threadId <- myThreadId
    let catchHandler = do
            state <- readIORef appStateRef
            stop state
            throwTo threadId ExitSuccess
    installHandler sigINT (Catch catchHandler) Nothing

    start
    forever do
        appState <- readIORef appStateRef
        when isDebugMode (putStrLn $ " ===> " <> (tshow appState))
        action <- takeMVar actionVar
        when isDebugMode (putStrLn $ tshow action)
        nextAppState <- handleAction appState action
        writeIORef appStateRef nextAppState


handleAction :: (?context :: Context) => AppState -> Action -> IO AppState
handleAction state (UpdatePostgresState postgresState) = pure state { postgresState }
handleAction state (UpdateAppGHCIState appGHCIState) = pure state { appGHCIState }
handleAction state (UpdateToolServerState toolServerState) = pure state { toolServerState }
handleAction state@(AppState { statusServerState = StatusServerNotStarted }) (UpdateStatusServerState statusServerState) = pure state { statusServerState }
handleAction state@(AppState { statusServerState = StatusServerStarted { } }) (UpdateStatusServerState StatusServerNotStarted) = pure state { statusServerState = StatusServerNotStarted }
handleAction state@(AppState { statusServerState = StatusServerPaused { } }) (UpdateStatusServerState statusServerState) = pure state { statusServerState = StatusServerNotStarted }
handleAction state@(AppState { liveReloadNotificationServerState = LiveReloadNotificationServerNotStarted }) (UpdateLiveReloadNotificationServerState liveReloadNotificationServerState) = pure state { liveReloadNotificationServerState }
handleAction state@(AppState { liveReloadNotificationServerState = LiveReloadNotificationServerStarted {} }) (UpdateLiveReloadNotificationServerState liveReloadNotificationServerState) = 
    case liveReloadNotificationServerState of
        LiveReloadNotificationServerNotStarted -> pure state { liveReloadNotificationServerState }
        otherwise -> error "Cannot start live reload notification server twice"
handleAction state (UpdateFileWatcherState fileWatcherState) = pure state { fileWatcherState }
handleAction state@(AppState { statusServerState }) ReceiveAppOutput { line } = do
    notifyBrowserOnApplicationOutput statusServerState line
    pure state
handleAction state@(AppState { appGHCIState, statusServerState, postgresState }) (AppModulesLoaded { success = True }) = do
    case appGHCIState of
        AppGHCILoading { .. } -> do
            case postgresState of
                PostgresStarted {} -> do
                    let appGHCIState' = AppGHCIModulesLoaded { .. }

                    stopStatusServer statusServerState
                    startLoadedApp appGHCIState

                    let statusServerState' = case statusServerState of
                            StatusServerStarted { .. } -> StatusServerPaused { .. }
                            _ -> statusServerState
                    
                    pure state { appGHCIState = appGHCIState', statusServerState = statusServerState' }
                _ -> do
                    putStrLn "Cannot start app as postgres is not ready yet"
                    pure state
        RunningAppGHCI { } -> pure state -- Do nothing as app is already in running state
        AppGHCINotStarted -> error "Unreachable AppGHCINotStarted"
        AppGHCIModulesLoaded { } -> do
            -- You can trigger this case by running: $ while true; do touch test.hs; done;
            when (get #isDebugMode ?context) (putStrLn "AppGHCIModulesLoaded triggered multiple times. This happens when multiple file change events are detected. Skipping app start as the app is already starting from a previous file change event")
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

handleAction state@(AppState { liveReloadNotificationServerState, appGHCIState, statusServerState }) HaskellFileChanged = do
    case appGHCIState of
        AppGHCIModulesLoaded { .. } -> sendGhciCommand process ":r"
        RunningAppGHCI { .. } -> do
            sendGhciCommand process "ClassyPrelude.uninterruptibleCancel app"
            sendGhciCommand process ":r"
        AppGHCILoading { .. } -> sendGhciCommand process ":r"

    clearStatusServer statusServerState

    let appGHCIState' = 
            case appGHCIState of
                AppGHCILoading { .. } -> AppGHCILoading { .. }
                AppGHCIModulesLoaded { .. } -> AppGHCILoading { .. }
                RunningAppGHCI { .. } -> AppGHCILoading { .. }
    pure state { appGHCIState = appGHCIState' }

handleAction state SchemaChanged = do
    async do
        SchemaCompiler.compile `catch` (\(exception :: SomeException) -> do putStrLn (tshow exception); dispatch (ReceiveAppOutput { line = ErrorOutput (cs $ tshow exception) }))
    pure state

handleAction state@(AppState { appGHCIState }) PauseApp =
    case appGHCIState of
        RunningAppGHCI { .. } -> do
            pauseAppGHCI appGHCIState
            pure state { appGHCIState = AppGHCIModulesLoaded { .. } }
        otherwise -> do putStrLn ("Could not pause app as it's not in running state" <> tshow otherwise); pure state



start :: (?context :: Context) => IO ()
start = do
    async startToolServer
    async startStatusServer
    async startLiveReloadNotificationServer
    async startAppGHCI
    async startPostgres
    async startFilewatcher
    pure ()

stop :: (?context :: Context) => AppState -> IO ()
stop AppState { .. } = do
    when (get #isDebugMode ?context) (putStrLn "Stop called")
    stopAppGHCI appGHCIState
    stopPostgres postgresState
    stopStatusServer statusServerState
    stopLiveReloadNotification liveReloadNotificationServerState
    stopFileWatcher fileWatcherState
    stopToolServer toolServerState

startFilewatcher :: (?context :: Context) => IO ()
startFilewatcher = do
        thread <- async $ FS.withManager $ \manager -> do
            FS.watchTree manager "." shouldActOnFileChange handleFileChange
            forever (threadDelay maxBound) `finally` FS.stopManager manager
        dispatch (UpdateFileWatcherState (FileWatcherStarted { thread }))
    where
        handleFileChange event = do
            let filePath = getEventFilePath event
            if isHaskellFile filePath
                then dispatch HaskellFileChanged
                else if "Application/Schema.sql" `isSuffixOf` filePath
                    then dispatch SchemaChanged
                    else if isAssetFile filePath
                        then dispatch AssetChanged
                        else mempty

        shouldActOnFileChange :: FS.ActionPredicate
        shouldActOnFileChange event =
            let path = getEventFilePath event
            in isHaskellFile path || isAssetFile path || isSQLFile path

        isHaskellFile = isSuffixOf ".hs"
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
    let args = ["-threaded", "-fomit-interface-pragmas", "-j", "-O0", "+RTS", "-A512m", "-n4m", "-H512m", "-G3", "-qg"]
    createManagedProcess (Process.proc "ghci" args)
            { Process.std_in = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.CreatePipe
            }

startAppGHCI :: (?context :: Context) => IO ()
startAppGHCI = do
    let isDebugMode = ?context |> get #isDebugMode
    -- The app is using the `PORT` env variable for it's web server
    let appPort :: Int = ?context
            |> get #portConfig
            |> get #appPort
            |> fromIntegral
    Env.setEnv "PORT" (show appPort)

    process <- startGHCI

    let ManagedProcess { outputHandle, errorHandle } = process

    libDirectory <- LibDir.findLibDirectory

    let loadAppCommands = 
            [ ":script " <> cs libDirectory <> "/applicationGhciConfig"
            , ":set prompt \"\"" -- Disable the prompt as this caused output such as '[38;5;208mIHP>[m Ser[v3e8r; 5s;t2a0r8tmedI' instead of 'Server started'
            , "import qualified ClassyPrelude"
            , ":l Main.hs"
            ]

    async $ forever $ ByteString.hGetLine outputHandle >>= \line -> do
                unless isDebugMode (ByteString.putStrLn line)
                if "Server started" `isInfixOf` line
                    then dispatch AppStarted
                    else if "Failed," `isInfixOf` line
                            then do
                                dispatch AppModulesLoaded { success = False }
                            else if "modules loaded." `isInfixOf` line
                                then do
                                    dispatch AppModulesLoaded { success = True }
                                else dispatch ReceiveAppOutput { line = StandardOutput line }

    async $ forever $ ByteString.hGetLine errorHandle >>= \line -> do
        unless isDebugMode (ByteString.putStrLn line)
        if "cannot find object file for module" `isInfixOf` line
            then do
                forEach loadAppCommands (sendGhciCommand process)
                dispatch ReceiveAppOutput { line = ErrorOutput "Linking Issue: Reloading Main" }
            else dispatch ReceiveAppOutput { line = ErrorOutput line }


    -- Compile Schema before loading the app
    SchemaCompiler.compile `catch` (\(e :: SomeException) -> putStrLn (tshow e))

    forEach loadAppCommands (sendGhciCommand process)

    dispatch (UpdateAppGHCIState (AppGHCILoading { .. }))


startLoadedApp :: (?context :: Context) => AppGHCIState -> IO ()
startLoadedApp (AppGHCIModulesLoaded { .. }) = do
    let commands =
            [ "ClassyPrelude.uninterruptibleCancel app"
            , "app <- ClassyPrelude.async main"
            ]
    forEach commands (sendGhciCommand process)
startLoadedApp (RunningAppGHCI { .. }) = error "Cannot start app as it's already in running statstate"
startLoadedApp (AppGHCILoading { .. }) = sendGhciCommand process "app <- ClassyPrelude.async main"
startLoadedApp _ = when (get #isDebugMode ?context) (putStrLn "startLoadedApp: App not running")


stopAppGHCI :: AppGHCIState -> IO ()
stopAppGHCI RunningAppGHCI { process } = cleanupManagedProcess process
stopAppGHCI AppGHCIModulesLoaded { process } = cleanupManagedProcess process
stopAppGHCI _ = pure ()

pauseAppGHCI :: (?context :: Context) => AppGHCIState -> IO ()
pauseAppGHCI RunningAppGHCI { process } = sendGhciCommand process "ClassyPrelude.uninterruptibleCancel app"
pauseAppGHCI _ = pure ()
