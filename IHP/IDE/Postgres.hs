module IHP.IDE.Postgres (startPostgres, stopPostgres) where

import IHP.IDE.Types
import IHP.Prelude
import qualified System.Process as Process
import qualified System.Directory as Directory
import qualified Data.ByteString.Char8 as ByteString
import GHC.IO.Handle

import qualified IHP.Log as Log
import qualified IHP.LibDir as LibDir

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

redirectHandleToVariable :: (?context :: Context) => Handle -> IO (IORef ByteString)
redirectHandleToVariable handle = do
    let isDebugMode = ?context |> get #isDebugMode

    ref <- newIORef ""
    async $ forever $ do
        line <- ByteString.hGetLine handle

        -- Always log fatal errors to the output:
        -- 2021-09-04 12:18:08.888 CEST [55794] FATAL:  database files are incompatible with server
        --
        -- If we're in debug mode, log all output
        if "FATAL" `ByteString.isInfixOf` line
            then if "database files are incompatible with server" `ByteString.isInfixOf` line
                then Log.error ("The current database state has been created with a different postgres server. Likely you just upgraded the IHP version. Delete your local dev database with 'rm -rf build/db'. You can use 'make dumpdb' to save your database state to Fixtures.sql, otherwise all changes in your local db will be lost. After that run './start' again." :: Text)
                else Log.error line
            else when isDebugMode (Log.debug line)
        modifyIORef ref (\log -> log <> "\n" <> line)
    pure ref

ensureNoOtherPostgresIsRunning :: IO ()
ensureNoOtherPostgresIsRunning = do
    pidFileExists <- Directory.doesPathExist "build/db/state/postmaster.pid"
    let stopFailedHandler (exception :: SomeException) = do
            -- pg_ctl: could not send stop signal (PID: 123456765432): No such process
            if ("No such process" `isInfixOf` (tshow exception))
                then Directory.removeFile "build/db/state/postmaster.pid"
                else putStrLn "Found postgres lockfile at 'build/db/state/postmaster.pid'. Could not bring the other postgres instance to halt. Please stop the running postgres manually and then restart this dev server"
    when pidFileExists do
        (Process.callProcess "pg_ctl" ["stop", "-D", "build/db/state"]) `catch` stopFailedHandler

needsDatabaseInit :: IO Bool
needsDatabaseInit = not <$> Directory.doesDirectoryExist "build/db/state"

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

        ihpLib <- LibDir.findLibDirectory
        let importSql file = Process.callCommand ("psql -h '" <> currentDir <> "/build/db' -d app < " <> file)
        importSql (cs ihpLib <> "/IHPSchema.sql")
        importSql "Application/Schema.sql"
        importSql "Application/Fixtures.sql"

        let ManagedProcess { processHandle } = process
        Process.terminateProcess processHandle
        _ <- Process.waitForProcess processHandle
        pure ()

waitUntilReady process callback = do
    let ManagedProcess { errorHandle } = process
    line <- ByteString.hGetLine errorHandle
    if "database system is ready to accept connections" `ByteString.isInfixOf` line
        then callback
        else waitUntilReady process callback
