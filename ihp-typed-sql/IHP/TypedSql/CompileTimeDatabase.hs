{-# LANGUAGE NamedFieldPuns #-}

module IHP.TypedSql.CompileTimeDatabase
    ( AutoDatabase (..)
    , autoDatabaseEnabled
    , dependentSchemaFiles
    , withAutoDatabase
    ) where

import           Control.Concurrent       (MVar, forkIO, modifyMVar, modifyMVar_,
                                            newEmptyMVar, newMVar, putMVar,
                                            readMVar, threadDelay, withMVar)
import qualified Control.Exception        as Exception
import           Data.Bits                (xor)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BSC
import qualified Data.Char                as Char
import           Data.Functor             ((<&>))
import qualified Data.List                as List
import qualified Data.String.Conversions  as CS
import           IHP.Prelude
import           Numeric                  (showHex)
import           System.Directory         (canonicalizePath, createDirectory,
                                            createDirectoryIfMissing,
                                            doesDirectoryExist, doesFileExist,
                                            findExecutable, getCurrentDirectory,
                                            listDirectory, makeAbsolute,
                                            removePathForcibly)
import           System.Environment       (lookupEnv)
import           System.Exit              (ExitCode (ExitFailure, ExitSuccess))
import           System.FilePath          (takeDirectory)
import           System.IO                (Handle, appendFile, hClose)
import           System.IO.Temp           (createTempDirectory)
import           System.Posix.Files       (fileOwner, getSymbolicLinkStatus,
                                            isDirectory, setFileMode)
import           System.Posix.Signals     (nullSignal, signalProcess)
import           System.Posix.User        (getEffectiveUserID)
import           System.Process           (CreateProcess (..), StdStream (CreatePipe, NoStream),
                                            createProcess, getCurrentPid, proc,
                                            readProcessWithExitCode,
                                            terminateProcess, waitForProcess)
import           System.Timeout           (timeout)
import           Text.Read                (readMaybe)
import qualified System.IO.Unsafe         as Unsafe
import qualified Prelude

data AutoDatabase = AutoDatabase
    { adbUrl        :: !BS.ByteString
    , adbRoot       :: !FilePath
    , adbPgData     :: !FilePath
    , adbPgHost     :: !FilePath
    , adbSchemaHash :: !String
    }

data ManagedAutoDatabase = ManagedAutoDatabase
    { madDatabase             :: !AutoDatabase
    , madToolchainFingerprint :: !String
    , madWatchdogInput        :: !Handle
    , madWatchdogDone         :: !(MVar ())
    }

data AutoDatabaseState = AutoDatabaseState
    { adsDatabase   :: !(Maybe ManagedAutoDatabase)
    , adsGeneration :: !Word64
    }

data SchemaInputs = SchemaInputs
    { siAppSchema :: !FilePath
    , siIhpSchema :: !(Maybe FilePath)
    , siHash      :: !String
    }

data PostgreSqlTools = PostgreSqlTools
    { initdbPath    :: !FilePath
    , pgCtlPath     :: !FilePath
    , createdbPath  :: !FilePath
    , dropdbPath    :: !FilePath
    , psqlPath      :: !FilePath
    , pgIsReadyPath :: !FilePath
    , shellPath     :: !FilePath
    }

autoDatabaseState :: MVar AutoDatabaseState
autoDatabaseState = Unsafe.unsafePerformIO (newMVar AutoDatabaseState
    { adsDatabase = Nothing
    , adsGeneration = 0
    })
{-# NOINLINE autoDatabaseState #-}

autoDatabaseOperationLock :: MVar ()
autoDatabaseOperationLock = Unsafe.unsafePerformIO (newMVar ())
{-# NOINLINE autoDatabaseOperationLock #-}

autoDatabaseEnabled :: IO Bool
autoDatabaseEnabled =
    lookupEnv "IHP_TYPED_SQL_AUTO_DB" <&> \case
        Just value -> map Char.toLower value `elem` ["1", "true", "yes", "on"]
        Nothing -> False

-- | Run a metadata operation against a database private to this GHC process.
-- PostgreSQL is stopped after a short idle period, while an EOF watchdog removes
-- the complete cluster when the compiler exits, including after an abrupt exit.
withAutoDatabase :: (AutoDatabase -> IO a) -> IO a
withAutoDatabase action =
    withMVar autoDatabaseOperationLock \_ -> do
        schemaInputs <- discoverSchemaInputs
        tools <- findPostgreSqlTools
        (managed, generation) <- modifyMVar autoDatabaseState \state -> do
            database <- ensureManagedDatabase tools schemaInputs (adsDatabase state)
            let generation = adsGeneration state + 1
            pure
                ( AutoDatabaseState
                    { adsDatabase = Just database
                    , adsGeneration = generation
                    }
                , (database, generation)
                )
        action (madDatabase managed)
            `Exception.finally` scheduleIdleStop tools generation

scheduleIdleStop :: PostgreSqlTools -> Word64 -> IO ()
scheduleIdleStop tools generation = do
    idleSeconds <- autoDatabaseIdleSeconds
    _ <- forkIO do
        threadDelay (idleSeconds * 1000000)
        modifyMVar_ autoDatabaseState \state -> do
            when (adsGeneration state == generation) do
                forM_ (adsDatabase state) (stopServer tools . madDatabase)
            pure state
    pure ()

autoDatabaseIdleSeconds :: IO Int
autoDatabaseIdleSeconds = do
    configured <- lookupEnv "IHP_TYPED_SQL_IDLE_SECONDS"
    pure $ case configured >>= readMaybe of
        Just seconds | seconds > 0 -> seconds
        _ -> 3

ensureManagedDatabase
    :: PostgreSqlTools
    -> SchemaInputs
    -> Maybe ManagedAutoDatabase
    -> IO ManagedAutoDatabase
ensureManagedDatabase tools schemaInputs current = do
    fingerprint <- toolchainFingerprint tools
    case current of
        Just managed -> do
            let database = madDatabase managed
            pgVersionExists <- doesFileExist (adbPgData database </> "PG_VERSION")
            if pgVersionExists && madToolchainFingerprint managed == fingerprint
                then do
                    database' <- (do
                        ensureServer tools database
                        ensureCurrentSchema tools schemaInputs database
                        ) `Exception.onException` stopServer tools database
                    pure managed { madDatabase = database' }
                else do
                    destroyManagedDatabase tools managed
                    startManagedDatabase tools fingerprint schemaInputs
        Nothing -> startManagedDatabase tools fingerprint schemaInputs

startManagedDatabase
    :: PostgreSqlTools
    -> String
    -> SchemaInputs
    -> IO ManagedAutoDatabase
startManagedDatabase tools fingerprint schemaInputs = do
    stateRoot <- autoDatabaseStateRoot schemaInputs
    createDirectoryIfMissing True stateRoot
    stateRoot' <- canonicalizePath stateRoot
    ensurePrivateDirectory stateRoot'
    let processesRoot = stateRoot' </> "processes"
    createDirectoryIfMissing True processesRoot
    ensurePrivateDirectory processesRoot
    pruneStaleProcessDirectories tools processesRoot

    processId <- getCurrentPid
    root <- createTempDirectory processesRoot ("ghc-" <> Prelude.show processId <> "-")
    ensurePrivateDirectory root
    Prelude.writeFile (root </> "owner.pid") (Prelude.show processId)
    pgHost <- socketDirectory root
    let pgData = root </> "pgdata"
        database = AutoDatabase
            { adbUrl = CS.cs ("postgresql:///app?host=" <> pgHost)
            , adbRoot = root
            , adbPgData = pgData
            , adbPgHost = pgHost
            , adbSchemaHash = ""
            }

    (watchdogInput, watchdogDone) <- startWatchdog tools database
    let managed = ManagedAutoDatabase
            { madDatabase = database
            , madToolchainFingerprint = fingerprint
            , madWatchdogInput = watchdogInput
            , madWatchdogDone = watchdogDone
            }
    (do
        runChecked (initdbPath tools)
            [ "-D", pgData
            , "--no-locale"
            , "--encoding=UTF8"
            , "--no-sync"
            , "--wal-segsize=1"
            ]
        appendFile (pgData </> "postgresql.conf") (postgresqlConfiguration pgHost)
        ensureServer tools database
        database' <- ensureCurrentSchema tools schemaInputs database
        pure managed { madDatabase = database' }
        ) `Exception.onException` destroyManagedDatabase tools managed

postgresqlConfiguration :: FilePath -> String
postgresqlConfiguration pgHost =
    let escapedSocketPath = concatMap (\character -> if character == '\'' then "''" else [character]) pgHost
    in List.unlines
        [ "unix_socket_directories = '" <> escapedSocketPath <> "'"
        , "listen_addresses = ''"
        , "shared_buffers = 4MB"
        , "max_connections = 10"
        , "autovacuum = off"
        , "fsync = off"
        , "synchronous_commit = off"
        , "full_page_writes = off"
        , "wal_level = minimal"
        , "max_wal_senders = 0"
        , "max_worker_processes = 0"
        , "max_parallel_workers = 0"
        , "jit = off"
        , "min_wal_size = 4MB"
        , "max_wal_size = 32MB"
        ]

startWatchdog :: PostgreSqlTools -> AutoDatabase -> IO (Handle, MVar ())
startWatchdog PostgreSqlTools { pgCtlPath, shellPath } AutoDatabase { adbRoot, adbPgData, adbPgHost } = do
    let process = (proc shellPath
            [ "-c", watchdogScript, "ihp-typed-sql-watchdog"
            , pgCtlPath, adbRoot, adbPgData, adbPgHost
            ])
            { std_in = CreatePipe
            , std_out = NoStream
            , std_err = NoStream
            , close_fds = True
            , new_session = True
            }
    (maybeInput, _, _, processHandle) <- createProcess process
    case maybeInput of
        Nothing -> do
            terminateProcess processHandle
            fail "typedSql: failed to create the compile-time database watchdog pipe"
        Just watchdogInput -> do
            watchdogDone <- newEmptyMVar
            _ <- forkIO do
                _ <- waitForProcess processHandle
                    `Exception.finally` putMVar watchdogDone ()
                pure ()
            pure (watchdogInput, watchdogDone)

watchdogScript :: String
watchdogScript = List.unlines
    [ "pgctl=$1"
    , "root=$2"
    , "pgdata=$3"
    , "pghost=$4"
    , "IFS= read -r _ || true"
    , "\"$pgctl\" -D \"$pgdata\" -m fast -w stop >/dev/null 2>&1 || true"
    , "rm -rf \"$root\""
    , "rm -rf \"$pghost\""
    ]

destroyManagedDatabase :: PostgreSqlTools -> ManagedAutoDatabase -> IO ()
destroyManagedDatabase tools ManagedAutoDatabase { madDatabase, madWatchdogInput, madWatchdogDone } = do
    ignoreIOException (hClose madWatchdogInput)
    finished <- timeout 10000000 (readMVar madWatchdogDone)
    when (isNothing finished) do
        stopServer tools madDatabase
        ignoreIOException (removePathForcibly (adbRoot madDatabase))
        ignoreIOException (removePathForcibly (adbPgHost madDatabase))

pruneStaleProcessDirectories :: PostgreSqlTools -> FilePath -> IO ()
pruneStaleProcessDirectories tools processesRoot = do
    entries <- listDirectory processesRoot
    forM_ entries \entry -> do
        let root = processesRoot </> entry
        isProcessDirectory <- doesDirectoryExist root
        when isProcessDirectory do
            owner <- readFileIfExists (root </> "owner.pid")
            forM_ owner \ownerPid -> do
                alive <- processIsAlive ownerPid
                unless alive do
                    pgHost <- socketDirectoryPath root
                    let database = AutoDatabase
                            { adbUrl = ""
                            , adbRoot = root
                            , adbPgData = root </> "pgdata"
                            , adbPgHost = pgHost
                            , adbSchemaHash = ""
                            }
                    stopServer tools database
                    ignoreIOException (removePathForcibly root)
                    ignoreIOException (removePathForcibly pgHost)

ensureServer :: PostgreSqlTools -> AutoDatabase -> IO ()
ensureServer tools database@AutoDatabase { adbPgData, adbRoot } = do
    ready <- databaseIsReady tools database
    unless ready do
        stopServer tools database
        Prelude.writeFile (adbRoot </> "postgresql.log") ""
        runChecked (pgCtlPath tools)
            ["-D", adbPgData, "-l", adbRoot </> "postgresql.log", "-w", "start"]

stopServer :: PostgreSqlTools -> AutoDatabase -> IO ()
stopServer PostgreSqlTools { pgCtlPath } AutoDatabase { adbPgData } = do
    pgDataExists <- doesDirectoryExist adbPgData
    when pgDataExists do
        _ <- readProcessWithExitCode
            pgCtlPath ["-D", adbPgData, "-m", "fast", "-w", "stop"] ""
            `Exception.catch` \(_ :: IOException) -> pure (ExitFailure 1, "", "")
        pure ()

databaseIsReady :: PostgreSqlTools -> AutoDatabase -> IO Bool
databaseIsReady PostgreSqlTools { pgIsReadyPath } AutoDatabase { adbPgHost } = do
    (exitCode, _, _) <- readProcessWithExitCode pgIsReadyPath ["-h", adbPgHost, "-d", "postgres"] ""
        `Exception.catch` \(_ :: IOException) -> pure (ExitFailure 1, "", "")
    pure (exitCode == ExitSuccess)

ensureCurrentSchema :: PostgreSqlTools -> SchemaInputs -> AutoDatabase -> IO AutoDatabase
ensureCurrentSchema tools schemaInputs database@AutoDatabase { adbRoot, adbPgHost, adbSchemaHash }
    | adbSchemaHash == siHash schemaInputs = pure database
    | otherwise = do
        runChecked (dropdbPath tools) ["--if-exists", "-h", adbPgHost, "app"]
        runChecked (createdbPath tools) ["-h", adbPgHost, "app"]
        forM_ (catMaybes [siIhpSchema schemaInputs, Just (siAppSchema schemaInputs)]) \schemaFile ->
            runChecked (psqlPath tools)
                ["-v", "ON_ERROR_STOP=1", "-h", adbPgHost, "app", "-f", schemaFile]
        Prelude.writeFile (adbRoot </> "schema.hash") (siHash schemaInputs)
        pure database { adbSchemaHash = siHash schemaInputs }

dependentSchemaFiles :: IO [FilePath]
dependentSchemaFiles = do
    appSchema <- findAppSchema
    ihpSchema <- findIhpSchema
    pure (catMaybes [appSchema, ihpSchema])

discoverSchemaInputs :: IO SchemaInputs
discoverSchemaInputs = do
    appSchema <- findAppSchema >>= \case
        Just path -> canonicalizePath path
        Nothing -> do
            cwd <- getCurrentDirectory
            fail
                ( "typedSql: automatic compile-time database is enabled via "
                    <> "IHP_TYPED_SQL_AUTO_DB, but Application/Schema.sql could "
                    <> "not be found from " <> cwd <> ". Set "
                    <> "IHP_TYPED_SQL_SCHEMA=/path/to/Application/Schema.sql "
                    <> "or run GHC from the IHP project root."
                )
    ihpSchema <- findIhpSchema >>= Prelude.traverse canonicalizePath
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

autoDatabaseStateRoot :: SchemaInputs -> IO FilePath
autoDatabaseStateRoot SchemaInputs { siAppSchema } = do
    lookupEnv "IHP_TYPED_SQL_STATE_DIR" >>= \case
        Just path | not (null path) -> makeAbsolute path
        _ -> lookupEnv "DEVENV_STATE" >>= \case
            Just path | not (null path) -> makeAbsolute (path </> "ihp-typed-sql")
            _ -> lookupEnv "DEVENV_ROOT" >>= \case
                Just path | not (null path) ->
                    makeAbsolute (path </> ".devenv" </> "state" </> "ihp-typed-sql")
                _ -> do
                    let projectRoot = takeDirectory (takeDirectory siAppSchema)
                    pure (projectRoot </> ".devenv" </> "state" </> "ihp-typed-sql")

socketDirectory :: FilePath -> IO FilePath
socketDirectory root = do
    path <- socketDirectoryPath root
    let socketBase = takeDirectory path
    ensurePrivateDirectory socketBase
    ensurePrivateDirectory path
    pure path

socketDirectoryPath :: FilePath -> IO FilePath
socketDirectoryPath root = do
    userId <- getEffectiveUserID
    let socketBase = "/tmp" </> ("ihp-typed-sql-" <> Prelude.show userId)
    pure (socketBase </> showHex (fnv1a64 (BSC.pack root)) "")

ensurePrivateDirectory :: FilePath -> IO ()
ensurePrivateDirectory path = do
    createDirectory path `Exception.catch` \exception ->
        unless (isAlreadyExistsError exception) (Exception.throwIO (exception :: IOException))
    status <- getSymbolicLinkStatus path
    userId <- getEffectiveUserID
    unless (isDirectory status && fileOwner status == userId) do
        fail ("typedSql: refusing to use an unsafe state directory: " <> path)
    setFileMode path 0o700

findPostgreSqlTools :: IO PostgreSqlTools
findPostgreSqlTools = do
    let commandNames = ["initdb", "pg_ctl", "createdb", "dropdb", "psql", "pg_isready", "sh"]
    commands <- forM commandNames \command -> do
        executable <- findExecutable command >>= Prelude.traverse canonicalizePath
        pure (command, executable)
    let missing = [command | (command, Nothing) <- commands]
    unless (null missing) do
        fail
            ( "typedSql: automatic compile-time database is enabled via "
                <> "IHP_TYPED_SQL_AUTO_DB, but these PostgreSQL tools are not "
                <> "available on PATH: " <> List.intercalate ", " missing <> ". "
                <> "Enter the IHP nix/devenv shell or start the development "
                <> "database with devenv up."
            )
    let command name = fromMaybe
            (Prelude.error ("typedSql: missing checked executable " <> name))
            (join (Prelude.lookup name commands))
    pure PostgreSqlTools
        { initdbPath = command "initdb"
        , pgCtlPath = command "pg_ctl"
        , createdbPath = command "createdb"
        , dropdbPath = command "dropdb"
        , psqlPath = command "psql"
        , pgIsReadyPath = command "pg_isready"
        , shellPath = command "sh"
        }

toolchainFingerprint :: PostgreSqlTools -> IO String
toolchainFingerprint PostgreSqlTools { initdbPath, pgCtlPath } = do
    (exitCode, stdOut, stdErr) <- readProcessWithExitCode initdbPath ["--version"] ""
    case exitCode of
        ExitSuccess -> pure (List.unlines [initdbPath, pgCtlPath, stdOut])
        ExitFailure code ->
            fail
                ( "typedSql: could not identify the PostgreSQL toolchain"
                    <> "\nexit code: " <> Prelude.show code
                    <> "\nstderr:\n" <> stdErr
                )

processIsAlive :: String -> IO Bool
processIsAlive processId =
    case readMaybe processId :: Maybe Int of
        Just pid | pid > 1 ->
            (signalProcess nullSignal (fromIntegral pid) >> pure True)
                `Exception.catch` \(_ :: IOException) -> pure False
        _ -> pure False

readFileIfExists :: FilePath -> IO (Maybe String)
readFileIfExists path = do
    exists <- doesFileExist path
    if exists
        then Just <$> Prelude.readFile path
        else pure Nothing

ignoreIOException :: IO a -> IO ()
ignoreIOException action =
    (action >> pure ()) `Exception.catch` \(_ :: IOException) -> pure ()

runChecked :: FilePath -> [String] -> IO ()
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
