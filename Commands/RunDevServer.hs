{-# LANGUAGE NamedFieldPuns #-}

module Main where
import ClassyPrelude hiding (threadDelay)
import qualified System.Process as Process
import System.Exit
import System.Posix.Signals
import qualified Control.Exception as Exception
import qualified GHC.IO.Handle as Handle
import System.Process.Internals
import Data.String.Conversions (cs)
import Control.Concurrent (threadDelay, myThreadId)
import qualified System.FSNotify as FS
import System.Directory (getCurrentDirectory, withCurrentDirectory)
import qualified System.Directory as Directory
import Data.Default (def)
import Data.Maybe (fromJust)
import qualified Control.Concurrent.Lock as Lock
import qualified TurboHaskell.DevelopmentSupport.LiveReloadNotificationServer as LiveReloadNotificationServer
import qualified Data.ByteString.Char8 as ByteString

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import TurboHaskell.HtmlSupport.QQ (hsx)
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Network.HTTP.Types.Header as HTTP
import qualified Text.Blaze.Html5 as Html5
import GHC.Records
import Data.String.Interpolate (i)

data DevServerState = DevServerState
    { postgresProcess :: !(IORef ManagedProcess)
    , serverProcess :: !(IORef ManagedProcess)
    , modelCompilerProcess :: !(IORef ManagedProcess)
    , rebuildServerLock :: !Lock.Lock
    , liveReloadNotificationServerProcess :: !(IORef ManagedProcess)
    }

data ManagedProcess = ManagedProcess
    { inputHandle :: !Handle
    , outputHandle :: !Handle
    , errorHandle :: !Handle
    , processHandle :: !ProcessHandle
    , errorLog :: Maybe (IORef ByteString)
    }

data GhciState = Ok | Failed | Pending deriving (Eq, Show)

initDevServerState = do
    postgresProcess <- startPostgres >>= newIORef
    serverProcess <- startPlainGhci >>= initServer >>= newIORef
    modelCompilerProcess <- startCompileGhci >>= newIORef
    rebuildServerLock <- Lock.new
    liveReloadNotificationServerProcess <- startLiveReloadNotificationServer >>= newIORef
    return DevServerState {
            postgresProcess = postgresProcess,
            serverProcess = serverProcess,
            modelCompilerProcess = modelCompilerProcess,
            rebuildServerLock = rebuildServerLock,
            liveReloadNotificationServerProcess = liveReloadNotificationServerProcess
        }

renderErrorView :: ByteString -> Bool -> Html5.Html
renderErrorView code isCompiling = [hsx|
        <html lang="en">
          <head>
            <meta charset="utf-8"/>
            <title>Compilation Error</title>
            <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous"/>
          </head>
          <body style="background-color: #002b36; color: #839496">
            <div class="m-2">{inner}</div>
          </body>
        </html>
    |]
        where
            inner = if isCompiling
                then [hsx|<h1>Is compiling</h1><pre style="color: #839496">{code}</pre>|]
                else [hsx|<h1>Error while compiling</h1><pre style="color: #839496">{code}</pre>|]

initErrorWatcher :: IORef ManagedProcess -> IO () -> IO () -> IO ()
initErrorWatcher serverProcess willStartErrorServer didStopErrorServer = do
    errorServerRef <- newIORef Nothing
    isCompiling <- newIORef True
    lock <- Lock.new
    let stopServer = do
            server <- readIORef errorServerRef
            case server of
                Just server -> do
                    cancel server
                    didStopErrorServer
                    clearErrorLog serverProcess
                    writeIORef errorServerRef Nothing
                Nothing -> return ()
    let getLastErrors (ManagedProcess { errorLog }) = readIORef (fromJust errorLog)

    let startErrorServer = do
            server <- readIORef errorServerRef
            when (isNothing server) do
                    let errorApp req respond = do
                            isCompiling' <- readIORef isCompiling
                            errorLog <- readIORef serverProcess
                                    >>= return . fromJust . getField @"errorLog"
                                    >>= readIORef
                            respond $ Wai.responseBuilder HTTP.status200 [(HTTP.hContentType, "text/html")] (Blaze.renderHtmlBuilder $ renderErrorView errorLog isCompiling')
                    let port = 8000
                    willStartErrorServer
                    errorServer <- async $ Warp.run port errorApp
                    writeIORef errorServerRef (Just errorServer)

    let onGhciStateChange state = Lock.with lock $ case state of
            Ok -> do
                writeIORef isCompiling False
                stopServer
            Failed -> do
                pingDevServer
                writeIORef isCompiling False
                startErrorServer
                
            Pending -> do
                writeIORef isCompiling True
                startErrorServer
    watchGhciProcessState serverProcess onGhciStateChange


registerExitHandler handler = do
    threadId <- myThreadId
    let catchHandler = do
            catchAny handler (const $ Exception.throwTo threadId ExitSuccess)
            Exception.throwTo threadId ExitSuccess
    installHandler keyboardSignal (Catch catchHandler) Nothing

main :: IO ()
main = do
    state <- initDevServerState
    initServerProcessWatcher state
    initModelCompilerProcessWatcher state
    rebuildModels state
    registerExitHandler (cleanup state)

    currentDir <- getCurrentDirectory

    FS.withManager $ \manager -> do
            FS.watchTree manager "." shouldActOnFileChange (watch state)
            forever (threadDelay maxBound) `finally` (do FS.stopManager manager; cleanup state)

initServerProcessWatcher DevServerState { serverProcess } = do
    initErrorWatcher serverProcess (return ()) (return ())

initModelCompilerProcessWatcher :: DevServerState -> IO ()
initModelCompilerProcessWatcher DevServerState { serverProcess, modelCompilerProcess } = do
    initErrorWatcher modelCompilerProcess pauseRunningApp (continueRunningApp serverProcess)

shouldActOnFileChange :: FS.ActionPredicate
shouldActOnFileChange event =
    let path = getEventFilePath event
    in isHaskellFile path || isAssetFile path

isHaskellFile = isSuffixOf ".hs"
isAssetFile = isSuffixOf ".css"

getEventFilePath :: FS.Event -> FilePath
getEventFilePath event =
    case event of
        FS.Added filePath _ _ -> filePath
        FS.Modified filePath _ _ -> filePath
        FS.Removed filePath _ _ -> filePath
        FS.Unknown filePath _ _ -> filePath


cleanup :: DevServerState -> IO ()
cleanup state = do
    putStrLn "cleanup"
    let DevServerState { serverProcess, postgresProcess, modelCompilerProcess, liveReloadNotificationServerProcess } = state
    let processes = [serverProcess, postgresProcess, modelCompilerProcess]
    let stopProcess process = do ManagedProcess { processHandle } <- readIORef serverProcess; Process.terminateProcess processHandle
    stopServer
    forM_ processes stopProcess
    _ <- Process.system "lsof -i :8002|awk '{print $2}'|tail -n1|xargs kill -9"
    return ()

startPlainGhci :: IO ManagedProcess
startPlainGhci = do
    let process = (Process.proc "ghci" ["-threaded", "-fexternal-interpreter", "-fomit-interface-pragmas", "-j", "-O0", "+RTS", "-A512m", "-n4m", "-H512m", "-G3", "-qg"]) { Process.std_in = Process.CreatePipe, Process.std_out = Process.CreatePipe, Process.std_err = Process.CreatePipe }
    (Just inputHandle, Just outputHandle, Just errorHandle, processHandle) <- Process.createProcess process
    errorLog <- Just <$> newIORef ""
    return ManagedProcess { .. }

startCompileGhci :: IO ManagedProcess
startCompileGhci = do
    let process = (Process.proc "ghci" ["-threaded", "-w", "-j2", "-fobject-code", "-fomit-interface-pragmas", "+RTS", "-A128m"]) { Process.std_in = Process.CreatePipe, Process.std_out = Process.CreatePipe, Process.std_err = Process.CreatePipe }
    (Just inputHandle, Just outputHandle, Just errorHandle, processHandle) <- Process.createProcess process
    errorLog <- Just <$> newIORef ""
    return ManagedProcess { .. }

startLiveReloadNotificationServer :: IO ManagedProcess
startLiveReloadNotificationServer = do
    let process = (Process.proc "RunLiveReloadNotificationServer" []) { Process.std_in = Process.CreatePipe, Process.std_out = Process.CreatePipe, Process.std_err = Process.CreatePipe }
    (Just inputHandle, Just outputHandle, Just errorHandle, processHandle) <- Process.createProcess process
    let errorLog = Nothing
    return ManagedProcess { .. }


initServer :: ManagedProcess -> IO ManagedProcess
initServer ghci = do
    sendGhciCommand ghci (":script TurboHaskell/startDevServerGhciScript")
    return ghci

watch :: DevServerState -> FS.Event -> IO ()
watch state@(DevServerState {serverProcess, rebuildServerLock}) event =
    let
        filePath = getEventFilePath event
    in
        if isSuffixOf "Application/Schema.hs" filePath 
            then rebuildModels state
            else
                if isAssetFile filePath
                    then triggerAssetReload 
                    else rebuild serverProcess rebuildServerLock


rebuildModels (DevServerState {modelCompilerProcess}) = do
    ghci <- readIORef modelCompilerProcess
    sendGhciCommand ghci ":!clear"
    sendGhciCommand ghci ":script TurboHaskell/compileModels"

triggerAssetReload :: IO ()
triggerAssetReload = do
    _ <- Process.system "(lsof -i :8002|awk '{print $2}'|tail -n1|xargs kill -SIGUSR1) || true"
    return ()

rebuild :: IORef ManagedProcess -> Lock.Lock -> IO ()
rebuild serverProcess rebuildServerLock = do
    _ <- Lock.tryWith rebuildServerLock $ do
        clearErrorLog serverProcess
        pauseRunningApp
        continueRunningApp serverProcess
    return ()

pauseRunningApp :: IO ()
pauseRunningApp = do
    _ <- Process.system "lsof -i :8000|grep ghc-iserv | awk '{print $2}'|head -n1|xargs kill -SIGINT"
    return ()

continueRunningApp :: IORef ManagedProcess -> IO ()
continueRunningApp serverProcess = do
    ghci <- readIORef serverProcess
    sendGhciCommand ghci ":script TurboHaskell/startDevServerGhciScriptRec"

readGhciState :: GhciState -> ByteString -> Maybe GhciState
readGhciState _ line | "Ok," `isPrefixOf` line = Just Ok
readGhciState _ line | "Failed," `isPrefixOf` line = Just Failed
readGhciState prevState _ | prevState == Failed || prevState == Pending = Just Pending
readGhciState _ _ = Nothing

watchGhciProcessState :: IORef ManagedProcess -> (GhciState -> IO ()) -> IO ()
watchGhciProcessState ghciRef onStateChange = do
    ghci <- readIORef ghciRef
    stateRef <- newIORef Pending
    let ManagedProcess { outputHandle, errorHandle, errorLog } = ghci
    async $ forever $ do
            line <- ByteString.hGetLine outputHandle
            ByteString.putStrLn line
            prevState <- readIORef stateRef
            case readGhciState prevState line of
                Just state -> do
                    writeIORef stateRef state
                    onStateChange state
                    case errorLog of
                        Just errorLog -> do
                            modifyIORef errorLog (\log -> log <> "\n" <> line)
                            _ <- async pingDevServer
                            return ()
                        Nothing -> return ()
                Nothing -> return ()
    async $ forever $ do
            line <- ByteString.hGetLine errorHandle
            ByteString.putStrLn line
            case errorLog of
                Just errorLog -> do
                    modifyIORef errorLog (\log -> log <> "\n" <> line)
                    _ <- async pingDevServer
                    return ()
                Nothing -> return ()
    return ()


sendGhciCommand :: ManagedProcess -> String -> IO ()
sendGhciCommand ghciProcess command = do
    let input = inputHandle ghciProcess
    -- putStrLn $ "Sending to ghci: " <> cs command
    Handle.hPutStr input (command <> "\n")
    Handle.hFlush input

stopServer :: IO ()
stopServer = do
    putStrLn "stopServer called"
    _ <- Process.system "(lsof -i :8000|grep ghc-iserv | awk '{print $2}'|head -n1|xargs kill -9) || true"
    return ()

startPostgres :: IO ManagedProcess
startPostgres = do
    currentDir <- getCurrentDirectory
    let process = (Process.proc "postgres" ["-D", "build/db/state", "-k", currentDir <> "/build/db"]) { Process.std_in = Process.CreatePipe, Process.std_out = Process.CreatePipe, Process.std_err = Process.CreatePipe }
    (Just inputHandle, Just outputHandle, Just errorHandle, processHandle) <- Process.createProcess process
    let errorLog = Nothing
    return ManagedProcess { .. }

getPid ph = withProcessHandle ph go
    where
        go ph_ = case ph_ of
            OpenHandle x   -> return $ Just x
            ClosedHandle _ -> return Nothing

clearErrorLog :: IORef ManagedProcess -> IO ()
clearErrorLog serverProcess = do
    ManagedProcess { errorLog } <- readIORef serverProcess
    writeIORef (fromJust errorLog) ""

pingDevServer :: IO ()
pingDevServer = do
    _ <- Process.system "(lsof -i :8002|awk '{print $2}'|tail -n1|xargs kill -SIGINT) || true"
    return ()

