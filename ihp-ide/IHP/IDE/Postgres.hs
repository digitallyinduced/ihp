module IHP.IDE.Postgres (withPostgres, withBuiltinOrDevenvPostgres) where

import IHP.IDE.Types
import IHP.Prelude
import qualified System.Process as Process
import qualified System.Directory as Directory
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Builder as ByteString
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import GHC.IO.Handle
import qualified Control.Exception.Safe as Exception

import qualified IHP.Log as Log
import qualified IHP.EnvVar as EnvVar
import Paths_ihp_ide (getDataFileName)

withPostgres :: (?context :: Context) => (MVar () -> IORef ByteString.Builder -> IORef ByteString.Builder -> IO a) -> IO a
withPostgres callback = do
    currentDir <- Directory.getCurrentDirectory
    ensureNoOtherPostgresIsRunning
    shouldInit <- needsDatabaseInit
    when shouldInit initDatabase

    Process.withCreateProcess (postgresProcessParams currentDir) \(Just inputHandle) (Just outputHandle) (Just errorHandle) processHandle -> do
        let main = do
                standardOutput <- newIORef mempty
                errorOutput <- newIORef mempty
                databaseIsReady <- newEmptyMVar

                redirectHandleToVariable standardOutput outputHandle handleOutdatedDatabase
                redirectHandleToVariable errorOutput errorHandle (handleOutdatedDatabase >> handleDatabaseReady databaseIsReady)

                callback databaseIsReady standardOutput errorOutput

        Exception.finally main (softStopPostgres processHandle)

softStopPostgres :: Process.ProcessHandle -> IO ()
softStopPostgres processHandle = do
    let interruptAndWait = Process.interruptProcessGroupOf processHandle >> Process.waitForProcess processHandle
    let waitAndKill = threadDelay 1000000 >> pure ()
    race_
        interruptAndWait
        waitAndKill

postgresProcessParams :: (?context :: Context) => FilePath -> Process.CreateProcess
postgresProcessParams workingDirectory =
    let
        args = ["-D", "build/db/state", "-k", workingDirectory <> "/build/db", "-c", "listen_addresses="]
    in (procDirenvAware "postgres" args)
        { Process.std_in = Process.CreatePipe
        , Process.std_out = Process.CreatePipe
        , Process.std_err = Process.CreatePipe
        , Process.create_group = True
        }

handleDatabaseReady :: MVar () -> ByteString -> IO ()
handleDatabaseReady onReady line = when ("database system is ready to accept connections" `ByteString.isInfixOf` line) (putMVar onReady ())

handleOutdatedDatabase :: (?context :: Context) => ByteString -> IO ()
handleOutdatedDatabase line =
        -- Always log fatal errors to the output:
        -- 2021-09-04 12:18:08.888 CEST [55794] FATAL:  database files are incompatible with server
        --
        -- If we're in debug mode, log all output
        if "FATAL" `ByteString.isInfixOf` line
            then if "database files are incompatible with server" `ByteString.isInfixOf` line
                then Log.error ("The current database state has been created with a different postgres server. Likely you just upgraded the IHP version. Delete your local dev database with 'rm -rf build/db'. You can use 'make dumpdb' to save your database state to Fixtures.sql, otherwise all changes in your local db will be lost. After that run 'devenv up' again." :: Text)
                else Log.error line
            else when ?context.isDebugMode (Log.debug line)

redirectHandleToVariable :: IORef ByteString.Builder -> Handle -> (ByteString -> IO ()) -> IO (Async ())
redirectHandleToVariable !ref !handle !onLine = do
    async $ forever $ do
        line <- ByteString.hGetLine handle
        onLine line
        modifyIORef ref (\log -> log <> "\n" <> ByteString.byteString line)

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

    let params = (Process.proc "postgres" ["-D", "build/db/state", "-k", currentDir <> "/build/db", "-c", "listen_addresses="])
                { Process.std_in = Process.CreatePipe
                , Process.std_out = Process.CreatePipe
                , Process.std_err = Process.CreatePipe
                }

    Process.withCreateProcess params \(Just inputHandle) (Just outputHandle) (Just errorHandle) processHandle -> do
        waitUntilReady errorHandle do
            Process.callProcess "createdb" ["app", "-h", currentDir <> "/build/db"]

            let importSql file = Process.callCommand ("psql -h '" <> currentDir <> "/build/db' -d app < " <> file)
            ihpSchemaSql <- getDataFileName "IHPSchema.sql"
            importSql ihpSchemaSql
            importSql "Application/Schema.sql"
            importSql "Application/Fixtures.sql"

            Process.terminateProcess processHandle
            _ <- Process.waitForProcess processHandle
            pure ()

waitUntilReady handle callback = do
    line <- ByteString.hGetLine handle
    if "database system is ready to accept connections" `ByteString.isInfixOf` line
        then callback
        else waitUntilReady handle callback

waitPostgres :: (?context :: Context) => IO ()
waitPostgres = do
    let isDebugMode = ?context.isDebugMode
    threadDelay 1000000
    (_, stdout, _) <- Process.readProcessWithExitCode "pg_ctl" ["status"] ""
    if "server is running" `isInfixOf` (cs stdout)
    then pure ()
    else do
        when isDebugMode (Log.debug ("Waiting for postgres to start" :: Text))
        waitPostgres


withBuiltinOrDevenvPostgres :: (?context :: Context) => (MVar () -> IORef ByteString.Builder -> IORef ByteString.Builder -> IO a) -> IO a
withBuiltinOrDevenvPostgres callback = do
    useDevenv <- EnvVar.envOrDefault "IHP_DEVENV" False
    if useDevenv
    then do
        waitPostgres

        -- For devenv postgres we don't have access to the postgres logs
        standardOutput <- newIORef mempty
        errorOutput <- newIORef mempty
        databaseIsReady <- newMVar ()

        callback databaseIsReady standardOutput errorOutput
    else do
        withPostgres callback