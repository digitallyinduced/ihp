{-# LANGUAGE NamedFieldPuns #-}

module IHP.TypedSql.CompileTimeDatabase
    ( AutoDatabase (..)
    , autoDatabaseEnabled
    , dependentSchemaFiles
    , ensureAutoDatabase
    , reapAbandonedAutoDatabasesIn
    ) where

import qualified Control.Exception       as Exception
import           Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import           Control.Monad           (filterM)
import           Data.Bits               (xor)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as BSC
import qualified Data.Char               as Char
import           Data.Functor            ((<&>))
import qualified Data.List               as List
import qualified Data.String.Conversions as CS
import           IHP.Prelude
import           Numeric                 (showHex)
import           System.Directory        (createDirectoryIfMissing, doesFileExist,
                                          findExecutable, getCurrentDirectory,
                                          getModificationTime,
                                          getTemporaryDirectory, listDirectory,
                                          removePathForcibly)
import           System.Environment      (lookupEnv)
import           System.Exit             (ExitCode (ExitFailure, ExitSuccess))
import           System.FilePath         (takeDirectory)
import           System.IO               (appendFile)
import           System.IO.Temp          (createTempDirectory)
import           System.Process          (createProcess, proc,
                                          readProcessWithExitCode)
import           Text.Read               (readMaybe)
import qualified System.IO.Unsafe        as Unsafe
import qualified Prelude

data AutoDatabase = AutoDatabase
    { adbUrl        :: !BS.ByteString
    , adbRoot       :: !FilePath
    , adbPgData     :: !FilePath
    , adbSchemaHash :: !String
    }

data SchemaInputs = SchemaInputs
    { siAppSchema :: !FilePath
    , siIhpSchema :: !(Maybe FilePath)
    , siHash      :: !String
    }

autoDatabaseState :: MVar (Maybe AutoDatabase)
autoDatabaseState = Unsafe.unsafePerformIO (newMVar Nothing)
{-# NOINLINE autoDatabaseState #-}

autoDatabaseEnabled :: IO Bool
autoDatabaseEnabled =
    lookupEnv "IHP_TYPED_SQL_AUTO_DB" <&> \case
        Just value -> map Char.toLower value `elem` ["1", "true", "yes", "on"]
        Nothing -> False

ensureAutoDatabase :: IO AutoDatabase
ensureAutoDatabase = do
    schemaInputs <- discoverSchemaInputs
    modifyMVar autoDatabaseState \case
        Just cached | adbSchemaHash cached == siHash schemaInputs -> do
            ready <- databaseIsReady cached
            if ready
                then pure (Just cached, cached)
                else do
                    stopAutoDatabase cached
                    fresh <- startAutoDatabase schemaInputs
                    pure (Just fresh, fresh)
        Just cached -> do
            stopAutoDatabase cached
            fresh <- startAutoDatabase schemaInputs
            pure (Just fresh, fresh)
        Nothing -> do
            fresh <- startAutoDatabase schemaInputs
            pure (Just fresh, fresh)

dependentSchemaFiles :: IO [FilePath]
dependentSchemaFiles = do
    appSchema <- findAppSchema
    ihpSchema <- findIhpSchema
    pure (catMaybes [appSchema, ihpSchema])

discoverSchemaInputs :: IO SchemaInputs
discoverSchemaInputs = do
    appSchema <- findAppSchema >>= \case
        Just path -> pure path
        Nothing -> do
            cwd <- getCurrentDirectory
            fail
                ( "typedSql: automatic compile-time database is enabled via "
                    <> "IHP_TYPED_SQL_AUTO_DB, but Application/Schema.sql could "
                    <> "not be found from " <> cwd <> ". Set "
                    <> "IHP_TYPED_SQL_SCHEMA=/path/to/Application/Schema.sql "
                    <> "or run GHC from the IHP project root."
                )
    ihpSchema <- findIhpSchema
    hash <- schemaHash (catMaybes [ihpSchema, Just appSchema])
    pure SchemaInputs
        { siAppSchema = appSchema
        , siIhpSchema = ihpSchema
        , siHash = hash
        }

findAppSchema :: IO (Maybe FilePath)
findAppSchema =
    lookupEnv "IHP_TYPED_SQL_SCHEMA" >>= \case
        Just path | not (null path) -> existing path
        _ -> do
            cwd <- getCurrentDirectory
            findUpwards cwd ("Application" </> "Schema.sql")

findIhpSchema :: IO (Maybe FilePath)
findIhpSchema =
    lookupEnv "IHP_TYPED_SQL_IHP_SCHEMA" >>= \case
        Just path | not (null path) -> existing path
        _ ->
            firstExistingEnvPath
                [ ("IHP_LIB", "IHPSchema.sql")
                , ("IHP", "IHPSchema.sql")
                ] >>= \case
                    Just path -> pure (Just path)
                    Nothing -> do
                        cwd <- getCurrentDirectory
                        findUpwards cwd ("ihp-schema-compiler" </> "data" </> "IHPSchema.sql")

existing :: FilePath -> IO (Maybe FilePath)
existing path = do
    exists <- doesFileExist path
    pure (if exists then Just path else Nothing)

firstExistingEnvPath :: [(String, FilePath)] -> IO (Maybe FilePath)
firstExistingEnvPath [] = pure Nothing
firstExistingEnvPath ((envName, relativePath):rest) =
    lookupEnv envName >>= \case
        Just base | not (null base) -> do
            let path = base </> relativePath
            existing path >>= \case
                Just found -> pure (Just found)
                Nothing -> firstExistingEnvPath rest
        _ -> firstExistingEnvPath rest

findUpwards :: FilePath -> FilePath -> IO (Maybe FilePath)
findUpwards directory relativePath = do
    let candidate = directory </> relativePath
    doesFileExist candidate >>= \case
        True -> pure (Just candidate)
        False ->
            let parent = takeDirectory directory
            in if parent == directory
                then pure Nothing
                else findUpwards parent relativePath

schemaHash :: [FilePath] -> IO String
schemaHash paths = do
    parts <- forM paths \path -> do
        contents <- BS.readFile path
        pure (BSC.pack path <> "\n" <> contents)
    pure (showHex (fnv1a64 (BS.concat parts)) "")

fnv1a64 :: BS.ByteString -> Word64
fnv1a64 =
    BS.foldl' step 14695981039346656037
  where
    step hash byte = (hash `xor` fromIntegral byte) * 1099511628211

startAutoDatabase :: SchemaInputs -> IO AutoDatabase
startAutoDatabase schemaInputs = do
    ensureRequiredExecutables
    reapAbandonedAutoDatabases
    tempDirectory <- getTemporaryDirectory
    root <- createTempDirectory tempDirectory "ihp-typed-sql-"
    let pgData = root </> "pgdata"
    let pgHost = root </> "socket"
    let logFile = root </> "postgresql.log"
    let databaseUrl = "postgresql:///app?host=" <> pgHost
    let autoDatabase = AutoDatabase
            { adbUrl = CS.cs databaseUrl
            , adbRoot = root
            , adbPgData = pgData
            , adbSchemaHash = siHash schemaInputs
            }

    (do
        createDirectoryIfMissing True pgHost
        -- Started before initdb, not after the cluster is up: if this process is
        -- killed (Ctrl-C, a cancelled parallel build, the OOM killer) there is no
        -- exception to unwind, so the monitor is the only thing that can clean up.
        startCleanupMonitor root pgData
        runChecked "initdb" ["-D", pgData, "--no-locale", "--encoding=UTF8"]
        appendFile (pgData </> "postgresql.conf")
            ( "unix_socket_directories = '" <> pgHost <> "'\n"
                <> "listen_addresses = ''\n"
            )
        runChecked "pg_ctl" ["-D", pgData, "-l", logFile, "-w", "start"]
        runChecked "createdb" ["-h", pgHost, "app"]
        forM_ (catMaybes [siIhpSchema schemaInputs, Just (siAppSchema schemaInputs)]) \schemaFile ->
            runChecked "psql" ["-v", "ON_ERROR_STOP=1", "-h", pgHost, "app", "-f", schemaFile]
        pure autoDatabase
        ) `Exception.onException` cleanupAutoDatabase autoDatabase

stopAutoDatabase :: AutoDatabase -> IO ()
stopAutoDatabase autoDatabase =
    cleanupAutoDatabase autoDatabase `Exception.catch` \(_ :: Exception.IOException) ->
        pure ()

cleanupAutoDatabase :: AutoDatabase -> IO ()
cleanupAutoDatabase AutoDatabase { adbRoot, adbPgData } = do
    _ <- readProcessWithExitCode "pg_ctl" ["-D", adbPgData, "-m", "fast", "-w", "stop"] ""
        `Exception.catch` \(_ :: Exception.IOException) -> pure (ExitSuccess, "", "")
    removePathForcibly adbRoot

databaseIsReady :: AutoDatabase -> IO Bool
databaseIsReady AutoDatabase { adbRoot } = do
    let pgHost = adbRoot </> "socket"
    (exitCode, _, _) <- readProcessWithExitCode "pg_isready" ["-h", pgHost, "-d", "app"] ""
        `Exception.catch` \(_ :: Exception.IOException) -> pure (ExitFailure 1, "", "")
    pure (exitCode == ExitSuccess)

ensureRequiredExecutables :: IO ()
ensureRequiredExecutables = do
    let commands = ["initdb", "pg_ctl", "createdb", "psql", "pg_isready", "sh"]
    missing <- filterM (fmap isNothing . findExecutable) commands
    unless (null missing) do
        fail
            ( "typedSql: automatic compile-time database is enabled via "
                <> "IHP_TYPED_SQL_AUTO_DB, but these PostgreSQL tools are not "
                <> "available on PATH: " <> List.intercalate ", " missing <> ". "
                <> "Enter the IHP nix/devenv shell or start the development "
                <> "database with devenv up."
            )

runChecked :: String -> [String] -> IO ()
runChecked command args = do
    (exitCode, stdOut, stdErr) <- readProcessWithExitCode command args ""
    case exitCode of
        ExitSuccess -> pure ()
        ExitFailure code ->
            fail
                ( "typedSql: automatic compile-time database command failed: "
                    <> Prelude.unwords (command : args)
                    <> "\nexit code: " <> Prelude.show code
                    <> "\nstdout:\n" <> stdOut
                    <> "\nstderr:\n" <> stdErr
                )

startCleanupMonitor :: FilePath -> FilePath -> IO ()
startCleanupMonitor root pgData = do
    let script = List.unlines
            [ "parent=$PPID"
            , "root=$1"
            , "pgdata=$2"
            -- Record the owning compiler process so a later run can tell an
            -- abandoned database apart from one that is still in use. See
            -- 'reapAbandonedAutoDatabases'.
            , "echo \"$parent\" > \"$root/" <> ownerPidFileName <> "\""
            , "("
            , "  while kill -0 \"$parent\" 2>/dev/null; do sleep 1; done"
            , "  pg_ctl -D \"$pgdata\" -m fast -w stop >/dev/null 2>&1 || true"
            , "  rm -rf \"$root\""
            , ") >/dev/null 2>&1 &"
            ]
    _ <- createProcess (proc "sh" ["-c", script, "ihp-typed-sql-monitor", root, pgData])
    pure ()

ownerPidFileName :: String
ownerPidFileName = "owner.pid"

-- | Removes auto-database directories left behind by earlier runs.
--
-- 'startCleanupMonitor' handles the normal case, but it cannot help when the
-- compiler process dies without unwinding — Ctrl-C, a parallel build being
-- cancelled, the OOM killer, a reboot. Those runs leave the temp directory, and
-- sometimes a live postgres, behind forever.
--
-- That accumulates: enough clusters exhaust the system's SysV shared memory, at
-- which point every later typedSql compile fails during initdb with
-- @could not create shared memory segment: No space left on device@, which
-- points at the kernel rather than at the real cause.
reapAbandonedAutoDatabases :: IO ()
reapAbandonedAutoDatabases =
    getTemporaryDirectory >>= reapAbandonedAutoDatabasesIn

-- | 'reapAbandonedAutoDatabases' against an explicit directory. Exposed for tests.
reapAbandonedAutoDatabasesIn :: FilePath -> IO ()
reapAbandonedAutoDatabasesIn tempDirectory = ignoringIOExceptions do
    entries <- listDirectory tempDirectory
    let candidates = filter ("ihp-typed-sql-" `List.isPrefixOf`) entries
    forM_ candidates \entry -> ignoringIOExceptions do
        let root = tempDirectory </> entry
        abandoned <- isAbandonedAutoDatabase root
        when abandoned do
            _ <- readProcessWithExitCode "pg_ctl"
                    ["-D", root </> "pgdata", "-m", "immediate", "-w", "stop"] ""
                `Exception.catch` \(_ :: Exception.IOException) -> pure (ExitSuccess, "", "")
            removePathForcibly root

-- | A directory is abandoned when the compiler process that created it is gone.
--
-- If there is no owner file the directory was created but never got as far as
-- starting the monitor, so fall back to age. The grace period only has to
-- exceed the time between 'createTempDirectory' and 'startCleanupMonitor', but
-- is generous so that a concurrently starting database is never reaped.
isAbandonedAutoDatabase :: FilePath -> IO Bool
isAbandonedAutoDatabase root = do
    let ownerFile = root </> ownerPidFileName
    hasOwner <- doesFileExist ownerFile
    if hasOwner
        then do
            contents <- Prelude.readFile ownerFile
            case readMaybe (List.dropWhileEnd Char.isSpace contents) of
                Nothing -> pure True
                Just pid -> not <$> processIsAlive (pid :: Int)
        else do
            modifiedAt <- getModificationTime root
            now <- getCurrentTime
            pure (diffUTCTime now modifiedAt > abandonedGracePeriod)

abandonedGracePeriod :: NominalDiffTime
abandonedGracePeriod = 60 * 60 -- one hour

-- | @ps -p@ rather than @kill -0@: @kill -0@ fails with EPERM for a process
-- owned by another user, which is indistinguishable from "no such process" by
-- exit code alone and would make us treat a live database as abandoned.
processIsAlive :: Int -> IO Bool
processIsAlive pid = do
    (exitCode, _, _) <- readProcessWithExitCode "ps" ["-p", Prelude.show pid] ""
    pure (exitCode == ExitSuccess)

ignoringIOExceptions :: IO () -> IO ()
ignoringIOExceptions action =
    action `Exception.catch` \(_ :: Exception.IOException) -> pure ()
