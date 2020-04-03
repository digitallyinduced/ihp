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

main :: IO ()
main = do
    actionVar <- newEmptyMVar
    appStateRef <- newIORef emptyAppState
    let ?context = Context { actionVar }
    


    threadId <- myThreadId
    let catchHandler = do
            state <- readIORef appStateRef
            stop state
            throwTo threadId ExitSuccess
    installHandler sigINT (Catch catchHandler) Nothing

    start
    forever do
        appState <- readIORef appStateRef
        action <- takeMVar actionVar
        putStrLn $ tshow action
        nextAppState <- handleAction appState action
        writeIORef appStateRef nextAppState


handleAction :: (?context :: Context) => AppState -> Action -> IO AppState
handleAction state (UpdatePostgresState postgresState) = pure state { postgresState }
handleAction state (UpdateAppGHCIState appGHCIState) = pure state { appGHCIState }
handleAction state (UpdateStatusServerState statusServerState) = pure state { statusServerState }
handleAction state (UpdateLiveReloadNotificationServerState liveReloadNotificationServerState) = pure state { liveReloadNotificationServerState }
handleAction state (UpdateFileWatcherState fileWatcherState) = pure state { fileWatcherState }
handleAction state@(AppState { statusServerState }) ReceiveAppOutput { line } = do
    notifyBrowserOnApplicationOutput statusServerState line
    pure state
handleAction state@(AppState { statusServerState }) ReceiveCodeGenerationOutput { line } = pure state
handleAction state@(AppState { appGHCIState, statusServerState, postgresState }) AppModulesLoaded = do
    stopStatusServer statusServerState
    case postgresState of
        PostgresStarted {} -> do
            let AppGHCILoading { .. } = appGHCIState
            let appGHCIState' = AppGHCIModulesLoaded { .. }
            startLoadedApp appGHCIState'
            pure state { appGHCIState = appGHCIState' }
        _ -> do
            putStrLn "Cannot start app as postgres is not ready yet"
            pure state
    pure state
handleAction state@(AppState { statusServerState }) AppStarted = do
    stopStatusServer statusServerState
    pure state
handleAction state@(AppState { liveReloadNotificationServerState }) AssetChanged = do
    notifyAssetChange liveReloadNotificationServerState
    pure state

handleAction state@(AppState { liveReloadNotificationServerState }) HaskellFileChanged = do
    notifyHaskellChange liveReloadNotificationServerState
    pure state

handleAction state@(AppState { codeGenerationState }) SchemaChanged = do
    case codeGenerationState of
        CodeGenerationReady { .. } -> do
            runCodeGeneration process
            pure state { codeGenerationState = CodeGenerationRunning { .. } }
        CodeGenerationFailed { .. } -> do
            runCodeGeneration process
            pure state { codeGenerationState = CodeGenerationRunning { .. } }
        otherwise -> do
            putStrLn "CodeGeneration skiped as it's still busy"
            pure state

handleAction state@(AppState { statusServerState, liveReloadNotificationServerState }) (UpdateCodeGenerationState (CodeGenerationRunning {})) = do
    case statusServerState of
        StatusServerNotStarted -> do _ <- async startStatusServer; pure ()
        _ -> pure ()
    notifyHaskellChange liveReloadNotificationServerState
    pure state
handleAction state (UpdateCodeGenerationState codeGenerationState) = pure state { codeGenerationState }

handleAction state@(AppState { appGHCIState }) PauseApp =
    case appGHCIState of
        RunningAppGHCI { .. } -> do
            pauseAppGHCI appGHCIState
            pure state { appGHCIState = AppGHCIModulesLoaded { .. } }
        _ -> pure state



start :: (?context :: Context) => IO ()
start = do
    async startStatusServer
    async startLiveReloadNotificationServer
    --(codeGenerationGHCI, codeGenerationHandleFileChange) <- startCodeGenerationGHCI (applicationOnStandardOutput, applicationOnErrorOutput, startStatusServer, do putStrLn "on fin"; stopStatusServer)
    async startAppGHCI
    async startPostgres
    async startFilewatcher
    async startCodeGenerationGHCI

    --pure AppState { .. }

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

    --readIORef statusServer >>= uninterruptibleCancel

startFilewatcher :: (?context :: Context) => IO (Async ())
startFilewatcher =
    async $ FS.withManager $ \manager -> do
        FS.watchTree manager "." shouldActOnFileChange handleFileChange
        forever (threadDelay maxBound) `finally` FS.stopManager manager

    where
        handleFileChange event = do
            let filePath = getEventFilePath event
            if isHaskellFile filePath
                then if "Application/Schema.hs" `isSuffixOf` filePath
                    then dispatch SchemaChanged
                    else do
                        dispatch HaskellFileChanged
                        --appHandleFileChange event
                        --startStatusServer
                        --refreshBrowser
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
    isAppRunning <- newIORef False
    process <- startGHCI

    let ManagedProcess { outputHandle, errorHandle } = process

    async $ forever $ ByteString.hGetLine outputHandle >>= \line -> do
                if "Server started" `isSuffixOf` line
                    then dispatch AppStarted
                    else if "Ok," `isPrefixOf` line
                        then dispatch AppModulesLoaded
                        else dispatch ReceiveAppOutput { line = StandardOutput line }
    async $ forever $ ByteString.hGetLine errorHandle >>= \line -> dispatch ReceiveAppOutput { line = ErrorOutput line }

    let handleFileChange event = do
            isAppRunning' <- readIORef isAppRunning
            sendGhciCommand process (":script TurboHaskell/" <> (if isAppRunning' then "startDevServerGhciScriptRec" else "startDevServerGhciScriptAfterError"))
            writeIORef isAppRunning False

    sendGhciCommand process ":script TurboHaskell/loadAppModules"

    dispatch (UpdateAppGHCIState (AppGHCILoading { .. }))
    pure ()

startLoadedApp :: AppGHCIState -> IO ()
startLoadedApp (AppGHCIModulesLoaded { .. }) = sendGhciCommand process ":script TurboHaskell/startDevServerGhciScript"
startLoadedApp _ = putStrLn "startLoadedApp: App not running"


stopAppGHCI :: AppGHCIState -> IO ()
stopAppGHCI RunningAppGHCI { process } = cleanupManagedProcess process
stopAppGHCI AppGHCIModulesLoaded { process } = cleanupManagedProcess process
stopAppGHCI _ = pure ()

pauseAppGHCI :: AppGHCIState -> IO ()
pauseAppGHCI RunningAppGHCI { process } = sendGhciCommand process ":script TurboHaskell/pauseDevServer"
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
runCodeGeneration process = sendGhciCommand process ":script TurboHaskell/compileModels"