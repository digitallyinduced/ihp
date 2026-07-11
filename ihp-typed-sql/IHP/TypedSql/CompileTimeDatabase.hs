{-# LANGUAGE NamedFieldPuns #-}

module IHP.TypedSql.CompileTimeDatabase
    ( AutoDatabase (..)
    , autoDatabaseEnabled
    , dependentSchemaFiles
    , withAutoDatabase
    ) where

import           Control.Concurrent       (forkIO, threadDelay)
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
                                            getModificationTime, listDirectory,
                                            makeAbsolute, removeFile,
                                            removePathForcibly, renameFile)
import           System.Environment       (lookupEnv)
import           System.Exit              (ExitCode (ExitFailure, ExitSuccess))
import           System.FilePath          (takeDirectory)
import           System.IO                (appendFile)
import           System.IO.Temp           (createTempDirectory)
import           System.Posix.Files       (fileOwner, getSymbolicLinkStatus,
                                            isDirectory, setFileMode)
import           System.Posix.Signals     (nullSignal, signalProcess)
import           System.Posix.User        (getEffectiveUserID)
import           System.Process           (CreateProcess (..), StdStream (NoStream),
                                            createProcess, getCurrentPid, getPid,
                                            proc, readProcessWithExitCode,
                                            waitForProcess)
import           Text.Read                (readMaybe)
import qualified Prelude

data AutoDatabase = AutoDatabase
    { adbUrl        :: !BS.ByteString
    , adbRoot       :: !FilePath
    , adbPgData     :: !FilePath
    , adbPgHost     :: !FilePath
    , adbSchemaHash :: !String
    }

data AutoDatabaseLease = AutoDatabaseLease
    { adlDatabase  :: !AutoDatabase
    , adlLeasePath :: !FilePath
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
    , psPath        :: !(Maybe FilePath)
    }

autoDatabaseEnabled :: IO Bool
autoDatabaseEnabled =
    lookupEnv "IHP_TYPED_SQL_AUTO_DB" <&> \case
        Just value -> map Char.toLower value `elem` ["1", "true", "yes", "on"]
        Nothing -> False

-- | Run a metadata operation against the worktree-local compile-time database.
-- The lease covers only the operation, allowing the database to stop while a
-- long-running GHCi or HLS process remains idle.
withAutoDatabase :: (AutoDatabase -> IO a) -> IO a
withAutoDatabase action = do
    schemaInputs <- discoverSchemaInputs
    Exception.bracket
        (acquireAutoDatabase schemaInputs)
        releaseAutoDatabase
        (action . adlDatabase)

acquireAutoDatabase :: SchemaInputs -> IO AutoDatabaseLease
acquireAutoDatabase schemaInputs = do
    tools <- findPostgreSqlTools
    configuredRoot <- autoDatabaseRoot schemaInputs
    createDirectoryIfMissing True configuredRoot
    root <- canonicalizePath configuredRoot
    ensurePrivateDirectory root
    createDirectoryIfMissing True (leasesDirectory root)
    ensurePrivateDirectory (leasesDirectory root)
    pgHost <- socketDirectory root
    let pgData = root </> "pgdata"
        autoDatabase = AutoDatabase
            { adbUrl = CS.cs ("postgresql:///app?host=" <> pgHost)
            , adbRoot = root
            , adbPgData = pgData
            , adbPgHost = pgHost
            , adbSchemaHash = siHash schemaInputs
            }

    withStateLock root do
        ensurePrivateDirectory pgHost
        ensureCompatibleCluster tools autoDatabase
        ensureSupervisor tools autoDatabase
        ensureServer tools autoDatabase
        ensureCurrentSchema tools schemaInputs autoDatabase
        leasePath <- createLease root
        pure AutoDatabaseLease { adlDatabase = autoDatabase, adlLeasePath = leasePath }

releaseAutoDatabase :: AutoDatabaseLease -> IO ()
releaseAutoDatabase AutoDatabaseLease { adlLeasePath } = do
    ignoreIOException (Prelude.writeFile (adlLeasePath </> "released") "")
    ignoreIOException (removePathForcibly adlLeasePath)

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

autoDatabaseRoot :: SchemaInputs -> IO FilePath
autoDatabaseRoot SchemaInputs { siAppSchema } = do
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
    userId <- getEffectiveUserID
    let socketBase = "/tmp" </> ("ihp-typed-sql-" <> Prelude.show userId)
        socketPath = socketBase </> showHex (fnv1a64 (BSC.pack root)) ""
    ensurePrivateDirectory socketBase
    ensurePrivateDirectory socketPath
    pure socketPath

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
    psPath <- findExecutable "ps" >>= Prelude.traverse canonicalizePath
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
        , psPath
        }

ensureCompatibleCluster :: PostgreSqlTools -> AutoDatabase -> IO ()
ensureCompatibleCluster tools autoDatabase@AutoDatabase { adbRoot, adbPgData, adbPgHost } = do
    fingerprint <- toolchainFingerprint tools
    pgDataExists <- doesDirectoryExist adbPgData
    pgVersionExists <- doesFileExist (adbPgData </> "PG_VERSION")
    storedFingerprint <- readFileIfExists (adbRoot </> "toolchain")
    let reusable = pgDataExists && pgVersionExists && storedFingerprint == Just fingerprint

    unless reusable do
        waitForNoLeases adbRoot
        when pgDataExists do
            stopped <- stopServer tools autoDatabase
            unless stopped do
                fail
                    ( "typedSql: cannot replace an incompatible compile-time database "
                        <> "because PostgreSQL did not stop: " <> adbPgData
                    )
            removePathForcibly adbPgData
        removeFileIfExists (adbRoot </> "schema.hash")
        removeFileIfExists (adbRoot </> "toolchain")
        runChecked (initdbPath tools) ["-D", adbPgData, "--no-locale", "--encoding=UTF8"]
        appendFile (adbPgData </> "postgresql.conf") "\ninclude_if_exists = 'ihp-typed-sql.conf'\n"
        writeFileAtomic (adbRoot </> "toolchain") fingerprint

    let escapedSocketPath = concatMap (\character -> if character == '\'' then "''" else [character]) adbPgHost
    Prelude.writeFile (adbPgData </> "ihp-typed-sql.conf")
        ( "unix_socket_directories = '" <> escapedSocketPath <> "'\n"
            <> "listen_addresses = ''\n"
        )

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

ensureServer :: PostgreSqlTools -> AutoDatabase -> IO ()
ensureServer tools autoDatabase@AutoDatabase { adbPgData, adbPgHost, adbRoot } = do
    ready <- databaseIsReady tools autoDatabase
    unless ready do
        (statusCode, _, _) <- readProcessWithExitCode (pgCtlPath tools) ["-D", adbPgData, "status"] ""
            `Exception.catch` \(_ :: IOException) -> pure (ExitFailure 1, "", "")
        when (statusCode == ExitSuccess) do
            waitForNoLeases adbRoot
            stopped <- stopServer tools autoDatabase
            unless stopped do
                fail
                    ( "typedSql: PostgreSQL is running but not reachable at its expected socket: "
                        <> adbPgHost
                    )
        Prelude.writeFile (adbRoot </> "postgresql.log") ""
        runChecked (pgCtlPath tools)
            ["-D", adbPgData, "-l", adbRoot </> "postgresql.log", "-w", "start"]
    rememberPostmasterPid autoDatabase

rememberPostmasterPid :: AutoDatabase -> IO ()
rememberPostmasterPid AutoDatabase { adbPgData, adbPgHost } = do
    postmasterPid <- readFileIfExists (adbPgData </> "postmaster.pid")
    case postmasterPid >>= listToMaybe . Prelude.lines of
        Just processId -> writeFileAtomic (adbPgHost </> "postmaster.pid") processId
        Nothing -> fail ("typedSql: PostgreSQL did not write postmaster.pid in " <> adbPgData)

stopServer :: PostgreSqlTools -> AutoDatabase -> IO Bool
stopServer PostgreSqlTools { pgCtlPath } AutoDatabase { adbPgData } = do
    _ <- readProcessWithExitCode
        pgCtlPath ["-D", adbPgData, "-m", "fast", "-w", "stop"] ""
        `Exception.catch` \(_ :: IOException) -> pure (ExitFailure 1, "", "")
    not <$> postmasterIsAlive adbPgData

postmasterIsAlive :: FilePath -> IO Bool
postmasterIsAlive pgData = do
    postmasterPid <- readFileIfExists (pgData </> "postmaster.pid")
    case postmasterPid >>= listToMaybe . Prelude.lines of
        Just processId -> processIsAlive processId
        Nothing -> pure False

databaseIsReady :: PostgreSqlTools -> AutoDatabase -> IO Bool
databaseIsReady PostgreSqlTools { pgIsReadyPath } AutoDatabase { adbPgHost } = do
    (exitCode, _, _) <- readProcessWithExitCode pgIsReadyPath ["-h", adbPgHost, "-d", "postgres"] ""
        `Exception.catch` \(_ :: IOException) -> pure (ExitFailure 1, "", "")
    pure (exitCode == ExitSuccess)

ensureCurrentSchema :: PostgreSqlTools -> SchemaInputs -> AutoDatabase -> IO ()
ensureCurrentSchema tools schemaInputs AutoDatabase { adbRoot, adbPgHost, adbSchemaHash } = do
    storedHash <- readFileIfExists (adbRoot </> "schema.hash")
    unless (storedHash == Just adbSchemaHash) do
        waitForNoLeases adbRoot
        removeFileIfExists (adbRoot </> "schema.hash")
        (do
            runChecked (dropdbPath tools) ["--if-exists", "-h", adbPgHost, "app"]
            runChecked (createdbPath tools) ["-h", adbPgHost, "app"]
            forM_ (catMaybes [siIhpSchema schemaInputs, Just (siAppSchema schemaInputs)]) \schemaFile ->
                runChecked (psqlPath tools)
                    ["-v", "ON_ERROR_STOP=1", "-h", adbPgHost, "app", "-f", schemaFile]
            writeFileAtomic (adbRoot </> "schema.hash") adbSchemaHash
            ) `Exception.onException` removeFileIfExists (adbRoot </> "schema.hash")

createLease :: FilePath -> IO FilePath
createLease root = do
    processId <- getCurrentPid
    leasePath <- createTempDirectory (leasesDirectory root) ("lease-" <> Prelude.show processId <> "-")
    ensurePrivateDirectory leasePath
    Prelude.writeFile (leasePath </> "owner.pid") (Prelude.show processId)
        `Exception.onException` removePathForcibly leasePath
    pure leasePath

leasesDirectory :: FilePath -> FilePath
leasesDirectory root = root </> "leases"

waitForNoLeases :: FilePath -> IO ()
waitForNoLeases root = loop (0 :: Int)
  where
    loop attempts = do
        liveLeases <- pruneAndCountLiveLeases root
        if liveLeases == 0
            then pure ()
            else if attempts >= 6000
                then fail "typedSql: timed out waiting for another compile-time database operation"
                else threadDelay 50000 >> loop (attempts + 1)

pruneAndCountLiveLeases :: FilePath -> IO Int
pruneAndCountLiveLeases root = do
    entries <- listDirectory (leasesDirectory root)
        `Exception.catch` \(_ :: IOException) -> pure []
    fmap sum $ forM entries \entry -> do
        let leasePath = leasesDirectory root </> entry
        isLeaseDirectory <- doesDirectoryExist leasePath
        if not isLeaseDirectory
            then pure 0
            else do
                released <- doesFileExist (leasePath </> "released")
                owner <- readFileIfExists (leasePath </> "owner.pid")
                alive <- maybe (pure False) processIsAlive owner
                if released || not alive
                    then ignoreIOException (removePathForcibly leasePath) >> pure 0
                    else pure 1

withStateLock :: FilePath -> IO a -> IO a
withStateLock root action =
    Exception.bracket (acquireStateLock root) (releaseStateLock root) \_ -> action

acquireStateLock :: FilePath -> IO String
acquireStateLock root = do
    processId <- Prelude.show <$> getCurrentPid
    loop processId (0 :: Int) (0 :: Int)
  where
    lockPath = root </> "lock"
    ownerPath = lockPath </> "owner.pid"

    loop processId attempts missingOwnerAttempts
        | attempts >= 6000 = fail "typedSql: timed out waiting for the compile-time database lock"
        | otherwise = do
            result <- Exception.try (createDirectory lockPath)
            case result of
                Right () -> do
                    Prelude.writeFile ownerPath processId
                        `Exception.onException` ignoreIOException (removePathForcibly lockPath)
                    pure processId
                Left (exception :: IOException)
                    | isAlreadyExistsError exception -> do
                        owner <- readFileIfExists ownerPath
                        case owner of
                            Just ownerPid -> do
                                alive <- processIsAlive ownerPid
                                unless alive (ignoreIOException (removePathForcibly lockPath))
                                threadDelay 50000
                                loop processId (attempts + 1) 0
                            Nothing -> do
                                when (missingOwnerAttempts >= 40) do
                                    ignoreIOException (removePathForcibly lockPath)
                                threadDelay 50000
                                loop processId (attempts + 1) (missingOwnerAttempts + 1)
                    | otherwise -> Exception.throwIO exception

releaseStateLock :: FilePath -> String -> IO ()
releaseStateLock root processId = do
    let lockPath = root </> "lock"
    owner <- readFileIfExists (lockPath </> "owner.pid")
    when (owner == Just processId) do
        ignoreIOException (removePathForcibly lockPath)

ensureSupervisor :: PostgreSqlTools -> AutoDatabase -> IO ()
ensureSupervisor tools autoDatabase@AutoDatabase { adbRoot, adbPgData, adbPgHost } = do
    healthy <- supervisorIsHealthy autoDatabase
    unless healthy do
        idleSeconds <- supervisorIdleSeconds
        Prelude.writeFile (adbRoot </> "supervisor.log") ""
        let script = supervisorScript
            process = (proc (shellPath tools)
                [ "-c", script, "ihp-typed-sql-supervisor"
                , adbRoot, adbPgData, adbPgHost, pgCtlPath tools
                , fromMaybe "" (psPath tools)
                , Prelude.show idleSeconds
                ])
                { std_in = NoStream
                , std_out = NoStream
                , std_err = NoStream
                , close_fds = True
                , new_session = True
                }
        (_, _, _, processHandle) <- createProcess process
        processId <- getPid processHandle
        forM_ processId \pid -> writeFileAtomic (adbRoot </> "supervisor.pid") (Prelude.show pid)
        Prelude.writeFile (adbPgHost </> "supervisor.heartbeat") ""
        _ <- forkIO do
            _ <- waitForProcess processHandle
            pure ()
        pure ()

supervisorIsHealthy :: AutoDatabase -> IO Bool
supervisorIsHealthy AutoDatabase { adbRoot, adbPgHost } = do
    let heartbeat = adbPgHost </> "supervisor.heartbeat"
    exists <- doesFileExist heartbeat
    supervisorPid <- readFileIfExists (adbRoot </> "supervisor.pid")
    supervisorAlive <- maybe (pure False) processIsAlive supervisorPid
    if not exists || not supervisorAlive
        then pure False
        else do
            modified <- getModificationTime heartbeat
            now <- getCurrentTime
            pure (diffUTCTime now modified < 5)

supervisorIdleSeconds :: IO Int
supervisorIdleSeconds = do
    configured <- lookupEnv "IHP_TYPED_SQL_IDLE_SECONDS"
    pure $ case configured >>= readMaybe of
        Just seconds | seconds > 0 -> seconds
        _ -> 15

supervisorScript :: String
supervisorScript = List.unlines
    [ "root=$1"
    , "pgdata=$2"
    , "pghost=$3"
    , "pgctl=$4"
    , "ps=$5"
    , "idle_seconds=$6"
    , "heartbeat=$pghost/supervisor.heartbeat"
    , "remembered_postmaster_pid=$pghost/postmaster.pid"
    , "log=$root/supervisor.log"
    , "postmaster_pid="
    , "empty_seconds=0"
    , "missing_lock_owner_seconds=0"
    , ""
    , "refresh_postmaster_pid() {"
    , "  postmaster_pid="
    , "  if [ -r \"$pgdata/postmaster.pid\" ]; then"
    , "    IFS= read -r postmaster_pid < \"$pgdata/postmaster.pid\" || true"
    , "    if [ -n \"$postmaster_pid\" ]; then"
    , "      printf '%s' \"$postmaster_pid\" > \"$remembered_postmaster_pid\" 2>/dev/null || true"
    , "    fi"
    , "  elif [ -r \"$remembered_postmaster_pid\" ]; then"
    , "    IFS= read -r postmaster_pid < \"$remembered_postmaster_pid\" || true"
    , "  fi"
    , "}"
    , ""
    , "postmaster_belongs_to_cluster() {"
    , "  [ -n \"$postmaster_pid\" ] || return 1"
    , "  [ -n \"$ps\" ] || return 1"
    , "  postmaster_command=$(\"$ps\" -p \"$postmaster_pid\" -o command= 2>/dev/null) || return 1"
    , "  case \"$postmaster_command\" in"
    , "    *\"$pgdata\"*) return 0 ;;"
    , "    *) return 1 ;;"
    , "  esac"
    , "}"
    , ""
    , "prune_leases() {"
    , "  live_leases=0"
    , "  for lease in \"$root\"/leases/*; do"
    , "    [ -d \"$lease\" ] || continue"
    , "    if [ -e \"$lease/released\" ]; then"
    , "      rm -rf \"$lease\""
    , "      continue"
    , "    fi"
    , "    owner="
    , "    if [ -r \"$lease/owner.pid\" ]; then"
    , "      IFS= read -r owner < \"$lease/owner.pid\" || true"
    , "    fi"
    , "    if [ -n \"$owner\" ] && kill -0 \"$owner\" 2>/dev/null; then"
    , "      live_leases=1"
    , "    else"
    , "      rm -rf \"$lease\""
    , "    fi"
    , "  done"
    , "}"
    , ""
    , "acquire_lock() {"
    , "  while ! mkdir \"$root/lock\" 2>/dev/null; do"
    , "    [ -d \"$root\" ] || return 1"
    , "    lock_owner="
    , "    if [ -r \"$root/lock/owner.pid\" ]; then"
    , "      IFS= read -r lock_owner < \"$root/lock/owner.pid\" || true"
    , "    fi"
    , "    if [ -n \"$lock_owner\" ]; then"
    , "      missing_lock_owner_seconds=0"
    , "      if ! kill -0 \"$lock_owner\" 2>/dev/null; then"
    , "        printf 'removing stale lock owned by %s\\n' \"$lock_owner\" >> \"$log\" 2>/dev/null || true"
    , "        rm -rf \"$root/lock\""
    , "      fi"
    , "    else"
    , "      missing_lock_owner_seconds=$((missing_lock_owner_seconds + 1))"
    , "      if [ \"$missing_lock_owner_seconds\" -ge 3 ]; then"
    , "        printf 'removing lock without owner pid\\n' >> \"$log\" 2>/dev/null || true"
    , "        rm -rf \"$root/lock\""
    , "        missing_lock_owner_seconds=0"
    , "      fi"
    , "    fi"
    , "    : > \"$heartbeat\" 2>/dev/null || true"
    , "    sleep 1"
    , "  done"
    , "  printf '%s' \"$$\" > \"$root/lock/owner.pid\""
    , "}"
    , ""
    , "while [ -d \"$root\" ]; do"
    , "  : > \"$heartbeat\" 2>/dev/null || true"
    , "  refresh_postmaster_pid"
    , "  prune_leases"
    , "  if [ \"$live_leases\" -eq 1 ]; then"
    , "    empty_seconds=0"
    , "  else"
    , "    empty_seconds=$((empty_seconds + 1))"
    , "  fi"
    , ""
    , "  if [ \"$empty_seconds\" -ge \"$idle_seconds\" ]; then"
    , "    if acquire_lock; then"
    , "      prune_leases"
    , "      if [ \"$live_leases\" -eq 0 ]; then"
    , "        refresh_postmaster_pid"
    , "        \"$pgctl\" -D \"$pgdata\" -m fast -w stop >> \"$log\" 2>&1 || true"
    , "        refresh_postmaster_pid"
    , "        if postmaster_belongs_to_cluster && kill -0 \"$postmaster_pid\" 2>/dev/null; then"
    , "          printf 'PostgreSQL is still running after pg_ctl stop\\n' >> \"$log\" 2>/dev/null || true"
    , "          rm -rf \"$root/lock\""
    , "          empty_seconds=0"
    , "        else"
    , "          rm -rf \"$root/lock\""
    , "          rm -f \"$root/supervisor.pid\""
    , "          rm -rf \"$pghost\""
    , "          exit 0"
    , "        fi"
    , "      fi"
    , "      rm -rf \"$root/lock\""
    , "      empty_seconds=0"
    , "    else"
    , "      break"
    , "    fi"
    , "  fi"
    , "  sleep 1"
    , "done"
    , ""
    , "refresh_postmaster_pid"
    , "if postmaster_belongs_to_cluster && kill -0 \"$postmaster_pid\" 2>/dev/null; then"
    , "  kill -INT \"$postmaster_pid\" 2>/dev/null || true"
    , "fi"
    , "rm -f \"$root/supervisor.pid\" 2>/dev/null || true"
    , "rm -rf \"$pghost\" 2>/dev/null || true"
    ]

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
        then (Just . BSC.unpack <$> BS.readFile path)
            `Exception.catch` \(_ :: IOException) -> pure Nothing
        else pure Nothing

writeFileAtomic :: FilePath -> String -> IO ()
writeFileAtomic path contents = do
    processId <- getCurrentPid
    let temporaryPath = path <> ".tmp-" <> Prelude.show processId
    ignoreIOException (removeFile temporaryPath)
    Prelude.writeFile temporaryPath contents
    renameFile temporaryPath path

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = ignoreIOException (removeFile path)

ignoreIOException :: IO () -> IO ()
ignoreIOException action = action `Exception.catch` \(_ :: IOException) -> pure ()

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
