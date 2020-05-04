module Main (main) where

import ClassyPrelude
import qualified System.Process as Process
import TurboHaskell.HaskellSupport
import qualified Data.ByteString.Char8 as ByteString
import Control.Concurrent (threadDelay, myThreadId)
import System.Exit
import System.Posix.Signals
import qualified System.FSNotify as FS

import TurboHaskell.IDE.Types
import TurboHaskell.IDE.Postgres
import TurboHaskell.IDE.StatusServer
import TurboHaskell.IDE.LiveReloadNotificationServer
import TurboHaskell.IDE.PortConfig
import TurboHaskell.IDE.ToolServer
import qualified System.Environment as Env

main :: IO ()
main = do
    actionVar <- newEmptyMVar
    appStateRef <- newIORef emptyAppState
    portConfig <- findAvailablePortConfig
    putStrLn $ tshow $ portConfig
    let ?context = Context { actionVar, portConfig, appStateRef }

    threadId <- myThreadId
    let catchHandler = do
            state <- readIORef appStateRef
            stop state
            throwTo threadId ExitSuccess
    installHandler sigINT (Catch catchHandler) Nothing

    start
    forever do
        appState <- readIORef appStateRef
        putStrLn $ " ===> " <> (tshow appState)
        action <- takeMVar actionVar
        putStrLn $ tshow action
        nextAppState <- handleAction appState action
        writeIORef appStateRef nextAppState


handleAction :: (?context :: Context) => AppState -> Action -> IO AppState
handleAction state (UpdatePostgresState postgresState) = pure state { postgresState }
handleAction state (UpdateAppGHCIState appGHCIState) = pure state { appGHCIState }
handleAction state (UpdateToolServerState toolServerState) = pure state { toolServerState }
handleAction state@(AppState { codeGenerationState = CodeGenerationFailed { standardOutput = cgStdOut, errorOutput = cgErrOut } }) (UpdateStatusServerState statusServerState@(StatusServerStarted { standardOutput, errorOutput })) = do
    readIORef cgStdOut >>= writeIORef standardOutput
    readIORef cgErrOut >>= writeIORef errorOutput
    pure state { statusServerState }
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
handleAction state@(AppState { statusServerState }) ReceiveCodeGenerationOutput { line } = do
    notifyBrowserOnApplicationOutput statusServerState line
    pure state
handleAction state@(AppState { appGHCIState, statusServerState, postgresState }) (AppModulesLoaded { success = True }) = do
    case appGHCIState of
        AppGHCILoading { .. } -> do
            case postgresState of
                PostgresStarted {} -> do
                    let appGHCIState' = AppGHCIModulesLoaded { .. }
                    startLoadedApp appGHCIState'
                    pure state { appGHCIState = appGHCIState' }
                _ -> do
                    putStrLn "Cannot start app as postgres is not ready yet"
                    pure state
        RunningAppGHCI { } -> pure state -- Do nothing as app is already in running state
        AppGHCINotStarted -> error "Unreachable"
        AppGHCIModulesLoaded { } -> do
            startLoadedApp appGHCIState
            pure state
handleAction state@(AppState { appGHCIState, statusServerState, postgresState, liveReloadNotificationServerState }) (AppModulesLoaded { success = False }) = do
    statusServerState' <- case statusServerState of
        s@(StatusServerPaused { .. }) -> do
            async $ continueStatusServer s False
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
    stopStatusServer statusServerState
    let state' = case statusServerState of
            StatusServerStarted { .. } -> state { statusServerState = StatusServerPaused { .. } }
            _ -> state
    case appGHCIState of
        AppGHCIModulesLoaded { .. } -> pure state' { appGHCIState = RunningAppGHCI { .. } }
        RunningAppGHCI { } -> do
            notifyHaskellChange liveReloadNotificationServerState
            pure state'
        otherwise -> pure state'
    
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

handleAction state@(AppState { codeGenerationState }) SchemaChanged = do
    case codeGenerationState of
        CodeGenerationReady { .. } -> do
            writeIORef standardOutput ""
            writeIORef errorOutput ""
            runCodeGeneration process
            pure state { codeGenerationState = CodeGenerationRunning { .. } }
        CodeGenerationFailed { .. } -> do
            writeIORef standardOutput ""
            writeIORef errorOutput ""
            runCodeGeneration process
            pure state { codeGenerationState = CodeGenerationRunning { .. } }
        otherwise -> do
            putStrLn "CodeGeneration skipped as it's still busy"
            pure state

handleAction state@(AppState { statusServerState, liveReloadNotificationServerState }) (UpdateCodeGenerationState (CodeGenerationRunning {})) = do
    statusServerState' <- case statusServerState of
        s@(StatusServerPaused { .. }) -> do
            async $ continueStatusServer s True
            pure StatusServerStarted { .. }
        s -> pure s
    notifyHaskellChange liveReloadNotificationServerState
    pure state { statusServerState = statusServerState' }

handleAction state@(AppState { liveReloadNotificationServerState, appGHCIState, codeGenerationState = CodeGenerationRunning {}, statusServerState }) (UpdateCodeGenerationState codeGenerationState@(CodeGenerationFailed {})) = do
    state' <- handleAction state PauseApp
    case statusServerState of
        StatusServerNotStarted -> do
            _ <- async do
                threadDelay 100000
                startStatusServer
            pure ()
        o -> putStrLn $ "Not starting status server as already running" <> tshow o
    stopLiveReloadNotification liveReloadNotificationServerState
    pure state' { codeGenerationState, liveReloadNotificationServerState = LiveReloadNotificationServerNotStarted }

handleAction state@(AppState { liveReloadNotificationServerState, appGHCIState, codeGenerationState = CodeGenerationRunning {}, statusServerState }) (UpdateCodeGenerationState codeGenerationState@(CodeGenerationReady {})) = do
    stopStatusServer statusServerState
    let AppGHCIModulesLoaded { .. } = appGHCIState
    startLoadedApp appGHCIState
    case liveReloadNotificationServerState of
        LiveReloadNotificationServerNotStarted -> do
            async startLiveReloadNotificationServer
            pure ()
        otherwise -> putStrLn "LiveReloadNotificationServer already started"
    pure state { appGHCIState, codeGenerationState, statusServerState = StatusServerNotStarted }

handleAction state@(AppState { appGHCIState, codeGenerationState, statusServerState }) (UpdateCodeGenerationState cgState) = do
    pure state { codeGenerationState = cgState }
    

handleAction state@(AppState { appGHCIState }) PauseApp =
    case appGHCIState of
        RunningAppGHCI { .. } -> do
            pauseAppGHCI appGHCIState
            pure state { appGHCIState = AppGHCIModulesLoaded { .. } }
        otherwise -> do putStrLn ("Could not pause app as it's not in running state" <> tshow otherwise); pure state



start :: (?context :: Context) => IO ()
start = do
    async startStatusServer
    async startLiveReloadNotificationServer
    async startAppGHCI
    async startPostgres
    async startFilewatcher
    async startCodeGenerationGHCI
    async startToolServer

    pure ()

stop :: AppState -> IO ()
stop AppState { .. } = do
    putStrLn "Stop called"
    stopAppGHCI appGHCIState
    stopPostgres postgresState
    stopStatusServer statusServerState
    stopLiveReloadNotification liveReloadNotificationServerState
    stopFileWatcher fileWatcherState
    stopCodeGenerationGHCI codeGenerationState
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
                then if "Application/Schema.hs" `isSuffixOf` filePath
                    then dispatch SchemaChanged
                    else do
                        dispatch HaskellFileChanged
                else if isAssetFile filePath
                    then dispatch AssetChanged
                    else mempty

        shouldActOnFileChange :: FS.ActionPredicate
        shouldActOnFileChange event =
            let path = getEventFilePath event
            in isHaskellFile path || isAssetFile path

        isHaskellFile = isSuffixOf ".hs"
        isAssetFile = isSuffixOf ".css"

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
    let args = ["-threaded", "-fexternal-interpreter", "-fomit-interface-pragmas", "-j", "-O0", "+RTS", "-A512m", "-n4m", "-H512m", "-G3", "-qg"]
    createManagedProcess (Process.proc "ghci" args)
            { Process.std_in = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.CreatePipe
            }

startAppGHCI :: (?context :: Context) => IO ()
startAppGHCI = do
    -- The app is using the `PORT` env variable for it's web server
    let appPort :: Int = ?context
            |> get #portConfig
            |> get #appPort
            |> fromIntegral
    Env.setEnv "PORT" (show appPort)

    isFirstStart <- newIORef True
    needsErrorRecovery <- newIORef False
    process <- startGHCI

    let ManagedProcess { outputHandle, errorHandle } = process

    async $ forever $ ByteString.hGetLine outputHandle >>= \line -> do
                if "Server started" `isInfixOf` line
                    then dispatch AppStarted
                    else if "modules loaded." `isInfixOf` line
                        then do
                            writeIORef needsErrorRecovery False
                            dispatch AppModulesLoaded { success = True }
                        else if "Failed," `isInfixOf` line
                            then do
                                writeIORef needsErrorRecovery True
                                dispatch AppModulesLoaded { success = False }
                            else dispatch ReceiveAppOutput { line = StandardOutput line }

    async $ forever $ ByteString.hGetLine errorHandle >>= \line -> do
        if "cannot find object file for module" `isInfixOf` line
            then do
                sendGhciCommand process ":script TurboHaskell/TurboHaskell/IDE/loadAppModules"
                dispatch ReceiveAppOutput { line = ErrorOutput "Linking Issue: Reloading Main" }
            else dispatch ReceiveAppOutput { line = ErrorOutput line }

    sendGhciCommand process ":script TurboHaskell/TurboHaskell/IDE/loadAppModules"

    dispatch (UpdateAppGHCIState (AppGHCILoading { .. }))


startLoadedApp :: AppGHCIState -> IO ()
startLoadedApp (AppGHCIModulesLoaded { .. }) = do
    recover <- readIORef needsErrorRecovery
    firstStart <- readIORef isFirstStart
    let theScript =
            if recover
                then "startDevServerGhciScriptAfterError"
                else if firstStart
                    then "startDevServerGhciScript"
                    else "startDevServerGhciScriptRec"
    sendGhciCommand process (":script TurboHaskell/TurboHaskell/IDE/" <> theScript)
    when firstStart (writeIORef isFirstStart False)
startLoadedApp (RunningAppGHCI { .. }) = sendGhciCommand process (":script TurboHaskell/TurboHaskell/IDE/startDevServerGhciScriptRec")
startLoadedApp _ = putStrLn "startLoadedApp: App not running"


stopAppGHCI :: AppGHCIState -> IO ()
stopAppGHCI RunningAppGHCI { process } = cleanupManagedProcess process
stopAppGHCI AppGHCIModulesLoaded { process } = cleanupManagedProcess process
stopAppGHCI _ = pure ()

pauseAppGHCI :: AppGHCIState -> IO ()
pauseAppGHCI RunningAppGHCI { process } = sendGhciCommand process ":script TurboHaskell/TurboHaskell/IDE/pauseDevServer"
pauseAppGHCI _ = pure ()

startCodeGenerationGHCI :: (?context :: Context) => IO ()
startCodeGenerationGHCI = do
    standardOutput <- newIORef ""
    errorOutput <- newIORef ""
    process <- startGHCI

    let ManagedProcess { outputHandle, errorHandle } = process
    async $ forever $ ByteString.hGetLine outputHandle >>= \line -> do
                modifyIORef standardOutput (\o -> o <> "\n" <> line)
                dispatch ReceiveCodeGenerationOutput { line = StandardOutput line }
                when ("Schema Compiled" `isSuffixOf` line) (dispatch (UpdateCodeGenerationState (CodeGenerationReady { .. })))
    async $ forever $ ByteString.hGetLine errorHandle >>= \line -> do
                modifyIORef errorOutput (\o -> o <> "\n" <> line)
                dispatch ReceiveCodeGenerationOutput { line = ErrorOutput line }
                unless ("Warning" `isInfixOf` line) (dispatch (UpdateCodeGenerationState (CodeGenerationFailed { .. })))

    dispatch (UpdateCodeGenerationState (CodeGenerationReady { .. }))

stopCodeGenerationGHCI :: CodeGenerationState -> IO ()
stopCodeGenerationGHCI CodeGenerationReady { .. } = cleanupManagedProcess process
stopCodeGenerationGHCI CodeGenerationFailed { .. } = cleanupManagedProcess process
stopCodeGenerationGHCI CodeGenerationRunning { .. } = cleanupManagedProcess process
stopCodeGenerationGHCI _ = pure ()

runCodeGeneration :: ManagedProcess -> IO ()
runCodeGeneration process = do
    sendGhciCommand process ":script TurboHaskell/TurboHaskell/IDE/compileModels"
