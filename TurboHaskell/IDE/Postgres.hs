module TurboHaskell.IDE.Postgres (startPostgres) where

import TurboHaskell.IDE.Types
import ClassyPrelude
import qualified System.Process as Process
import qualified System.Directory as Directory
import qualified Data.ByteString.Char8 as ByteString

startPostgres :: IO ManagedProcess
startPostgres = do
    currentDir <- Directory.getCurrentDirectory
    ensureNoOtherPostgresIsRunning 
    let args = ["-D", "build/db/state", "-k", currentDir <> "/build/db"]
    let params = (Process.proc "postgres" args)
                { Process.std_in = Process.CreatePipe
                , Process.std_out = Process.CreatePipe
                , Process.std_err = Process.CreatePipe
                }
    process <- createManagedProcess params


    let ManagedProcess { outputHandle, errorHandle } = process
    standardOutput <- redirectHandleToVariable outputHandle
    errorOutput <- redirectHandleToVariable errorHandle

    pure process
    
redirectHandleToVariable :: Handle -> IO (IORef ByteString)
redirectHandleToVariable handle = do
    ref <- newIORef ""
    async $ forever $ do
        line <- ByteString.hGetLine handle
        modifyIORef ref (\log -> log <> "\n" <> line)
    pure ref

ensureNoOtherPostgresIsRunning :: IO ()
ensureNoOtherPostgresIsRunning = do
    pidFileExists <- Directory.doesPathExist "build/db/state/postmaster.pid"
    when pidFileExists do
        Process.callProcess "pg_ctl" ["stop", "-D", "build/db/state"]
        