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
    state <- start

    threadId <- myThreadId

    let catchHandler = do
            stop state
            throwTo threadId ExitSuccess
    installHandler sigINT (Catch catchHandler) Nothing

    forever (threadDelay maxBound)

start :: IO State
start = do
    (statusServer, applicationOnStandardOutput, applicationOnErrorOutput, startStatusServer, stopStatusServer)  <- startStatusServer
    (liveReloadNotificationServer, handleAssetChange, refreshBrowser) <- startLiveReloadNotificationServer
    (codeGenerationGHCI, codeGenerationHandleFileChange) <- startCodeGenerationGHCI (applicationOnStandardOutput, applicationOnErrorOutput)
    (applicationGHCI, appHandleFileChange) <- startAppGHCI (applicationOnStandardOutput, applicationOnErrorOutput, stopStatusServer >> refreshBrowser)
    postgres <- startPostgres
    
    fileWatcher <- startFilewatcher $ \event -> do
        let filePath = getEventFilePath event
        if isHaskellFile filePath
            then if filePath `isSuffixOf` "Application/Schema.hs"
                then codeGenerationHandleFileChange event
                else do
                    appHandleFileChange event
                    startStatusServer
                    refreshBrowser
            else if isAssetFile filePath
                then handleAssetChange
                else mempty

    pure State { .. }

stop :: State -> IO ()
stop State { .. } = do
    cleanupManagedProcess postgres
    readIORef statusServer >>= uninterruptibleCancel
    cleanupManagedProcess applicationGHCI
    uninterruptibleCancel fileWatcher
    uninterruptibleCancel liveReloadNotificationServer

startFilewatcher :: FileEventHandler -> IO (Async ())
startFilewatcher handleFileChange =
    async $ FS.withManager $ \manager -> do
        FS.watchTree manager "." shouldActOnFileChange handleFileChange
        forever (threadDelay maxBound) `finally` FS.stopManager manager

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

startGHCI :: (ByteString -> IO (), ByteString -> IO ()) -> IO ManagedProcess
startGHCI (applicationOnStandardOutput, applicationOnErrorOutput) = do
    let args = ["-threaded", "-fexternal-interpreter", "-fomit-interface-pragmas", "-j", "-O0", "+RTS", "-A512m", "-n4m", "-H512m", "-G3", "-qg"]
    let process = (Process.proc "ghci" args)
            { Process.std_in = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.CreatePipe
            , Process.delegate_ctlc = True
            }
    managedProcess <- createManagedProcess process

    let ManagedProcess { outputHandle, errorHandle } = managedProcess

    async $ forever $ do
            line <- ByteString.hGetLine outputHandle
            -- ByteString.putStrLn line
            applicationOnStandardOutput line
    async $ forever $ do
            line <- ByteString.hGetLine errorHandle
            -- ByteString.putStrLn line
            applicationOnErrorOutput line

    pure managedProcess

startAppGHCI :: (ByteString -> IO (), ByteString -> IO (), IO ()) -> IO (ManagedProcess, FileEventHandler)
startAppGHCI (applicationOnStandardOutput', applicationOnErrorOutput, applicationOnStart) = do
    isAppRunning <- newIORef False
    let applicationOnStandardOutput line =
            if "Server started" `isSuffixOf` line
                then do
                    writeIORef isAppRunning True
                    applicationOnStart
                else applicationOnStandardOutput' line

    process <- startGHCI (applicationOnStandardOutput, applicationOnErrorOutput)

    let handleFileChange event = do
            isAppRunning' <- readIORef isAppRunning
            sendGhciCommand process (":script TurboHaskell/" <> (if isAppRunning' then "startDevServerGhciScriptRec" else "startDevServerGhciScriptAfterError"))
            writeIORef isAppRunning False

    sendGhciCommand process ":script TurboHaskell/startDevServerGhciScript"
    pure (process, handleFileChange)

startCodeGenerationGHCI :: (ByteString -> IO (), ByteString -> IO ()) -> IO (ManagedProcess, FileEventHandler)
startCodeGenerationGHCI (applicationOnStandardOutput, applicationOnErrorOutput) = do
    process <- startGHCI (applicationOnStandardOutput, applicationOnErrorOutput)

    let compileModelsCommand = ":script TurboHaskell/compileModels"
    let handleFileChange event = sendGhciCommand process compileModelsCommand

    sendGhciCommand process compileModelsCommand

    pure (process, handleFileChange)