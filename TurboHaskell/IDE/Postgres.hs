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
    shouldInit <- needsDatabaseInit
    when shouldInit initDatabase
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


needsDatabaseInit :: IO Bool
needsDatabaseInit = not <$> Directory.doesDirectoryExist "build/db"

initDatabase :: IO ()
initDatabase = do
    currentDir <- Directory.getCurrentDirectory
    Directory.createDirectoryIfMissing True "build/db"
    Directory.withCurrentDirectory "build/db" do
        Process.callProcess "initdb" ["state"]

        process <- createManagedProcess (Process.proc "postgres" ["-D", "state", "-k", currentDir <> "/build/db"])
                    { Process.std_in = Process.CreatePipe
                    , Process.std_out = Process.CreatePipe
                    , Process.std_err = Process.CreatePipe
                    }

        waitUntilReady process do
            Process.callProcess "createdb" ["app", "-h", currentDir <> "/build/db"]
            let importSql file = Process.callCommand ("psql -h '" <> currentDir <> "/build/db' -d app < ../../" <> file)
            importSql "Application/Schema.sql"
            importSql "Application/Fixtures.sql"

            let ManagedProcess { processHandle } = process
            Process.terminateProcess processHandle
            _ <- Process.waitForProcess processHandle
            pure ()

waitUntilReady process callback = do
    let ManagedProcess { errorHandle } = process
    line <- ByteString.hGetLine errorHandle
    if "database system is ready to accept connections" `isInfixOf` line
        then callback
        else waitUntilReady process callback
