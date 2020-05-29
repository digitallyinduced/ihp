module IHP.IDE.Postgres (startPostgres, stopPostgres) where

import IHP.IDE.Types
import ClassyPrelude
import qualified System.Process as Process
import qualified System.Directory as Directory
import qualified Data.ByteString.Char8 as ByteString

startPostgres :: (?context :: Context) => IO ManagedProcess
startPostgres = do
    currentDir <- Directory.getCurrentDirectory
    ensureNoOtherPostgresIsRunning 
    shouldInit <- needsDatabaseInit
    when shouldInit initDatabase
    let args = ["-D", "build/db/state", "-k", currentDir <> "/build/db", "-c", "listen_addresses="]
    let params = (Process.proc "postgres" args)
                { Process.std_in = Process.CreatePipe
                , Process.std_out = Process.CreatePipe
                , Process.std_err = Process.CreatePipe
                }
    process <- createManagedProcess params


    let ManagedProcess { outputHandle, errorHandle } = process
    standardOutput <- redirectHandleToVariable outputHandle
    errorOutput <- redirectHandleToVariable errorHandle

    dispatch (UpdatePostgresState (PostgresStarted { .. }))

    pure process

stopPostgres :: PostgresState -> IO ()
stopPostgres PostgresStarted { .. } = cleanupManagedProcess process
stopPostgres _ = pure ()

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

    Process.callProcess "initdb" [
                "build/db/state"
                , "--no-locale" -- Avoid issues with impure host system locale in dev mode
                , "--encoding"
                , "UTF8"
            ]

    process <- createManagedProcess (Process.proc "postgres" ["-D", "build/db/state", "-k", currentDir <> "/build/db", "-c", "listen_addresses="])
                { Process.std_in = Process.CreatePipe
                , Process.std_out = Process.CreatePipe
                , Process.std_err = Process.CreatePipe
                }

    waitUntilReady process do
        Process.callProcess "createdb" ["app", "-h", currentDir <> "/build/db"]
        let importSql file = Process.callCommand ("psql -h '" <> currentDir <> "/build/db' -d app < " <> file)
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
