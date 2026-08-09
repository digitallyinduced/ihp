{-# LANGUAGE NamedFieldPuns #-}

module IHP.TypedSql.CompileTimeDatabase
    ( AutoDatabase (..)
    , autoDatabaseEnabled
    , dependentSchemaFiles
    , withAutoDatabase
    ) where

import           Control.Concurrent       (MVar, forkFinally, forkIO, isEmptyMVar,
                                            modifyMVar, modifyMVar_, newEmptyMVar,
                                            newMVar, putMVar, readMVar, takeMVar,
                                            tryTakeMVar, withMVar)
import qualified Control.Exception        as Exception
import           Control.Monad            (guard, void)
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
                                            makeAbsolute,
                                            removeFile, removePathForcibly)
import           System.Environment       (lookupEnv)
import           System.Exit              (ExitCode (ExitFailure, ExitSuccess))
import           System.FilePath          (takeDirectory, takeFileName)
import           System.IO                (Handle, IOMode (WriteMode), appendFile,
                                            hClose, withFile)
import           System.IO.Temp           (createTempDirectory)
import           System.Posix.Files       (fileOwner, getSymbolicLinkStatus,
                                            isDirectory, setFileMode)
import           System.Posix.IO          (FdOption (CloseOnExec), closeFd,
                                            fdToHandle, handleToFd, setFdOption)
import           System.Posix.Signals     (nullSignal, signalProcess)
import           System.Posix.User        (getEffectiveUserID)
import           System.Process           (CreateProcess (..), ProcessHandle,
                                            StdStream (CreatePipe, NoStream,
                                                       UseHandle),
                                            createProcess, getCurrentPid, proc,
                                            interruptProcessGroupOf,
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
    , madWatchdogProcess      :: !ProcessHandle
    , madWatchdogDone         :: !(MVar ExitCode)
    }

data AutoDatabaseState = AutoDatabaseState
    { adsDatabase   :: !(Maybe ManagedAutoDatabase)
    , adsGeneration :: !Word64
    , adsTools      :: !(Maybe ResolvedPostgreSqlTools)
    }

data SchemaInputs = SchemaInputs
    { siAppSchema :: !FilePath
    , siIhpSchema :: !(Maybe FilePath)
    , siHash      :: !String
    }

data PostgreSqlTools = PostgreSqlTools
    { initdbPath    :: !FilePath
    , pgCtlPath     :: !FilePath
    , dropdbPath    :: !FilePath
    , psqlPath      :: !FilePath
    , pgIsReadyPath :: !FilePath
    , shellPath     :: !FilePath
    , rmPath        :: !FilePath
    , sleepPath     :: !FilePath
    , psPath        :: !FilePath
    }

data ResolvedPostgreSqlTools = ResolvedPostgreSqlTools
    { rptTools               :: !PostgreSqlTools
    , rptFingerprint         :: !String
    , rptPathEnvironment     :: !(Maybe String)
    }

data StopPolicy = FastOnly | ForceIfNeeded

data StopServerResult
    = ServerStopped
    | ServerNotRunning
    | ServerIdentityUnverified !String
    | ServerIdentityMismatch !Int !String
    | ServerStopFailed !String

data PostmasterPidState
    = PostmasterPidMissing
    | PostmasterPidInvalid !String
    | PostmasterPidFound !Int

data PostgreSqlProcessIdentity = PostgreSqlProcessIdentity
    { ppiPid       :: !Int
    , ppiPgData    :: !FilePath
    , ppiPort      :: !String
    , ppiSocketDir :: !FilePath
    }

data IdleStopRequest = IdleStopRequest
    { isrTools      :: !PostgreSqlTools
    , isrGeneration :: !Word64
    , isrDelay      :: !Int
    }

privatePostgreSqlPort :: String
privatePostgreSqlPort = "5432"

autoDatabaseState :: MVar AutoDatabaseState
autoDatabaseState = Unsafe.unsafePerformIO (newMVar AutoDatabaseState
    { adsDatabase = Nothing
    , adsGeneration = 0
    , adsTools = Nothing
    })
{-# NOINLINE autoDatabaseState #-}

autoDatabaseOperationLock :: MVar ()
autoDatabaseOperationLock = Unsafe.unsafePerformIO (newMVar ())
{-# NOINLINE autoDatabaseOperationLock #-}

idleStopRequests :: MVar IdleStopRequest
idleStopRequests = Unsafe.unsafePerformIO newEmptyMVar
{-# NOINLINE idleStopRequests #-}

idleStopRequestQueueLock :: MVar ()
idleStopRequestQueueLock = Unsafe.unsafePerformIO (newMVar ())
{-# NOINLINE idleStopRequestQueueLock #-}

idleStopWorkerStarted :: MVar Bool
idleStopWorkerStarted = Unsafe.unsafePerformIO (newMVar False)
{-# NOINLINE idleStopWorkerStarted #-}

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
        pathEnvironment <- lookupEnv "PATH"
        (managed, generation, tools) <- modifyMVar autoDatabaseState \state -> do
            resolvedTools <- resolvePostgreSqlTools pathEnvironment (adsTools state)
            let tools = rptTools resolvedTools
            database <- ensureManagedDatabase
                tools
                (rptFingerprint resolvedTools)
                schemaInputs
                (adsDatabase state)
            let generation = adsGeneration state + 1
            pure
                ( AutoDatabaseState
                    { adsDatabase = Just database
                    , adsGeneration = generation
                    , adsTools = Just resolvedTools
                    }
                , (database, generation, tools)
                )
        action (madDatabase managed)
            `Exception.finally` scheduleIdleStop tools generation

scheduleIdleStop :: PostgreSqlTools -> Word64 -> IO ()
scheduleIdleStop tools generation = do
    idleSeconds <- autoDatabaseIdleSeconds
    queueLatestIdleStopRequest IdleStopRequest
        { isrTools = tools
        , isrGeneration = generation
        , isrDelay = idleSeconds * 1000000
        }
    ensureIdleStopWorker

queueLatestIdleStopRequest :: IdleStopRequest -> IO ()
queueLatestIdleStopRequest request =
    withMVar idleStopRequestQueueLock \_ -> do
        current <- tryTakeMVar idleStopRequests
        case current of
            Just currentRequest
                | isrGeneration currentRequest > isrGeneration request ->
                    putMVar idleStopRequests currentRequest
            _ -> putMVar idleStopRequests request

ensureIdleStopWorker :: IO ()
ensureIdleStopWorker =
    modifyMVar_ idleStopWorkerStarted \started ->
        if started
            then pure True
            else do
                _ <- forkFinally idleStopWorker (const idleStopWorkerExited)
                pure True

idleStopWorkerExited :: IO ()
idleStopWorkerExited = do
    modifyMVar_ idleStopWorkerStarted (const (pure False))
    pendingRequest <- not <$> isEmptyMVar idleStopRequests
    when pendingRequest ensureIdleStopWorker

idleStopWorker :: IO ()
idleStopWorker = forever do
    request <- takeMVar idleStopRequests
    waitUntilIdleSafely request

waitUntilIdleSafely :: IdleStopRequest -> IO ()
waitUntilIdleSafely request =
    waitUntilIdle request `Exception.catch` handleIdleStopException request

handleIdleStopException :: IdleStopRequest -> Exception.SomeException -> IO ()
handleIdleStopException request exception =
    case Exception.fromException exception of
        Just asyncException -> Exception.throwIO (asyncException :: Exception.AsyncException)
        Nothing -> do
            withMVar autoDatabaseState \state ->
                when (adsGeneration state == isrGeneration request) do
                    forM_ (adsDatabase state) \managed ->
                        appendLifecycleLog (madDatabase managed)
                            ("idle stop failed with an exception: " <> Exception.displayException exception)
            queueLatestIdleStopRequest request

waitUntilIdle :: IdleStopRequest -> IO ()
waitUntilIdle request =
    timeout (isrDelay request) (takeMVar idleStopRequests) >>= \case
        Just newerRequest -> waitUntilIdleSafely newerRequest
        Nothing ->
            modifyMVar autoDatabaseState (stopIfCurrent request) >>= \shouldRetry ->
                when shouldRetry (queueLatestIdleStopRequest request)

stopIfCurrent :: IdleStopRequest -> AutoDatabaseState -> IO (AutoDatabaseState, Bool)
stopIfCurrent request state
    | adsGeneration state /= isrGeneration request = pure (state, False)
    | otherwise = do
        shouldRetry <- case adsDatabase state of
            Nothing -> pure False
            Just managed -> do
                result <- stopServer (isrTools request) FastOnly (madDatabase managed)
                logStopFailure (madDatabase managed) "idle stop" result
                pure $ case result of
                    ServerStopFailed _ -> True
                    _ -> False
        pure (state, shouldRetry)

autoDatabaseIdleSeconds :: IO Int
autoDatabaseIdleSeconds = do
    configured <- lookupEnv "IHP_TYPED_SQL_IDLE_SECONDS"
    pure $ case configured >>= readMaybe of
        Just seconds | seconds > 0 -> seconds
        _ -> 3

ensureManagedDatabase
    :: PostgreSqlTools
    -> String
    -> SchemaInputs
    -> Maybe ManagedAutoDatabase
    -> IO ManagedAutoDatabase
ensureManagedDatabase tools fingerprint schemaInputs current = do
    case current of
        Just managed -> do
            let database = madDatabase managed
            pgVersionExists <- doesFileExist (adbPgData database </> "PG_VERSION")
            if pgVersionExists
                && madToolchainFingerprint managed == fingerprint
                && adbSchemaHash database == siHash schemaInputs
                then do
                    ensureServer tools database
                        `Exception.onException` void (stopServer tools FastOnly database)
                    pure managed
                else do
                    destroyed <- destroyManagedDatabase tools managed
                    unless destroyed do
                        fail
                            ( "typedSql: could not safely remove the previous "
                                <> "compile-time database; see "
                                <> (adbRoot database </> "watchdog.log")
                            )
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
            { adbUrl = CS.cs ("postgresql:///app?host=" <> pgHost <> "&port=" <> privatePostgreSqlPort)
            , adbRoot = root
            , adbPgData = pgData
            , adbPgHost = pgHost
            , adbSchemaHash = ""
            }

    (watchdogInput, watchdogProcess, watchdogDone) <-
        startWatchdog tools database `Exception.onException` do
            ignoreIOException (removePathForcibly root)
            ignoreIOException (removePathForcibly pgHost)
    let managed = ManagedAutoDatabase
            { madDatabase = database
            , madToolchainFingerprint = fingerprint
            , madWatchdogInput = watchdogInput
            , madWatchdogProcess = watchdogProcess
            , madWatchdogDone = watchdogDone
            }
    (do
        runCheckedTracked tools database (initdbPath tools)
            [ "-D", pgData
            , "--no-locale"
            , "--encoding=UTF8"
            , "--no-sync"
            , "--wal-segsize=1"
            ]
        appendFile (pgData </> "postgresql.conf") (postgresqlConfiguration pgHost)
        ensureServer tools database
        database' <- initializeDatabase tools schemaInputs database
        pure managed { madDatabase = database' }
        ) `Exception.onException` void (destroyManagedDatabase tools managed)

postgresqlConfiguration :: FilePath -> String
postgresqlConfiguration pgHost =
    let escapedSocketPath = concatMap (\character -> if character == '\'' then "''" else [character]) pgHost
    in List.unlines
        [ "unix_socket_directories = '" <> escapedSocketPath <> "'"
        , "listen_addresses = ''"
        , "port = " <> privatePostgreSqlPort
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

startWatchdog :: PostgreSqlTools -> AutoDatabase -> IO (Handle, ProcessHandle, MVar ExitCode)
startWatchdog PostgreSqlTools { pgCtlPath, psPath, shellPath, rmPath, sleepPath } AutoDatabase { adbRoot, adbPgData, adbPgHost } = do
    let process = (proc shellPath
            [ "-c", watchdogScript, "ihp-typed-sql-watchdog"
            , pgCtlPath, psPath, rmPath, sleepPath, adbRoot, adbPgData, adbPgHost
            , privatePostgreSqlPort
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
            watchdogInput' <- makeCloseOnExec watchdogInput
                `Exception.onException` terminateProcess processHandle
            watchdogDone <- newEmptyMVar
            _ <- forkIO do
                exitCode <- waitForProcess processHandle
                    `Exception.catch` \(_ :: IOException) -> pure (ExitFailure 1)
                putMVar watchdogDone exitCode
            pure (watchdogInput', processHandle, watchdogDone)

makeCloseOnExec :: Handle -> IO Handle
makeCloseOnExec handle = do
    fileDescriptor <- handleToFd handle
    (setFdOption fileDescriptor CloseOnExec True >> fdToHandle fileDescriptor)
        `Exception.onException` closeFd fileDescriptor

watchdogScript :: String
watchdogScript = List.unlines
    [ "pgctl=$1"
    , "ps_command=$2"
    , "rm_command=$3"
    , "sleep_command=$4"
    , "root=$5"
    , "pgdata=$6"
    , "pghost=$7"
    , "pgport=$8"
    , "log=$root/watchdog.log"
    , "socket_lock=$pghost/.s.PGSQL.$pgport.lock"
    , "active_pid_file=$root/active-command.pid"
    , "active_pending_file=$root/active-command.pending"
    , "exec >>\"$log\" 2>&1"
    , "identity_file_matches() {"
    , "  identity_file=$1"
    , "  expected_pid=$2"
    , "  [ -f \"$identity_file\" ] || return 1"
    , "  {"
    , "    IFS= read -r identity_pid || return 1"
    , "    IFS= read -r identity_pgdata || return 1"
    , "    IFS= read -r _ || return 1"
    , "    IFS= read -r identity_port || return 1"
    , "    IFS= read -r identity_pghost || return 1"
    , "  } < \"$identity_file\""
    , "  [ \"$identity_pid\" = \"$expected_pid\" ] || return 1"
    , "  [ \"$identity_pgdata\" = \"$pgdata\" ] || return 1"
    , "  [ \"$identity_port\" = \"$pgport\" ] || return 1"
    , "  [ \"$identity_pghost\" = \"$pghost\" ] || return 1"
    , "}"
    , "process_command_matches() {"
    , "  checked_pid=$1"
    , "  command_line=$(\"$ps_command\" -p \"$checked_pid\" -o command= 2>/dev/null) || return 1"
    , "  case \"$command_line\" in"
    , "    *\"/postgres -D $pgdata\"|*\"/postgres -D $pgdata \"*|\"postgres -D $pgdata\"|\"postgres -D $pgdata \"*) return 0 ;;"
    , "    *) return 1 ;;"
    , "  esac"
    , "}"
    , "process_matches() {"
    , "  identity_file_matches \"$pgdata/postmaster.pid\" \"$pid\" || return 1"
    , "  identity_file_matches \"$socket_lock\" \"$pid\" || return 1"
    , "  process_command_matches \"$pid\""
    , "}"
    , "socket_reports_expected_server() {"
    , "  socket_pid="
    , "  [ -f \"$socket_lock\" ] && IFS= read -r socket_pid < \"$socket_lock\" || return 1"
    , "  case \"$socket_pid\" in ''|*[!0-9]*|0*|1) return 1 ;; esac"
    , "  identity_file_matches \"$socket_lock\" \"$socket_pid\" || return 1"
    , "  kill -0 \"$socket_pid\" 2>/dev/null || return 1"
    , "  process_command_matches \"$socket_pid\""
    , "}"
    , "active_group_alive() {"
    , "  kill -0 -- \"-$active_pid\" 2>/dev/null"
    , "}"
    , "wait_for_active_group_exit() {"
    , "  maximum_attempts=$1"
    , "  attempts=0"
    , "  while active_group_alive; do"
    , "    [ \"$attempts\" -ge \"$maximum_attempts\" ] && return 1"
    , "    \"$sleep_command\" 0.1"
    , "    attempts=$((attempts + 1))"
    , "  done"
    , "}"
    , "stop_active_command() {"
    , "  attempts=0"
    , "  while [ -f \"$active_pending_file\" ] && [ ! -s \"$active_pid_file\" ]; do"
    , "    [ \"$attempts\" -ge 50 ] && break"
    , "    \"$sleep_command\" 0.1"
    , "    attempts=$((attempts + 1))"
    , "  done"
    , "  if [ ! -s \"$active_pid_file\" ]; then"
    , "    if [ -f \"$active_pending_file\" ]; then"
    , "      printf '%s\\n' 'active command never started; continuing cleanup'"
    , "      \"$rm_command\" -f \"$active_pending_file\""
    , "    fi"
    , "    return 0"
    , "  fi"
    , "  IFS= read -r active_pid < \"$active_pid_file\" || active_pid="
    , "  case \"$active_pid\" in ''|*[!0-9]*|0*|1) printf 'invalid active command pid <%s>; preserving cluster\\n' \"$active_pid\"; return 1 ;; esac"
    , "  if active_group_alive; then"
    , "    kill -TERM -- \"-$active_pid\" 2>/dev/null || true"
    , "    if ! wait_for_active_group_exit 20; then"
    , "      kill -KILL -- \"-$active_pid\" 2>/dev/null || true"
    , "      wait_for_active_group_exit 50 || { printf '%s\\n' 'active command group did not exit; preserving cluster'; return 1; }"
    , "    fi"
    , "  fi"
    , "  \"$rm_command\" -f \"$active_pid_file\" \"$active_pending_file\""
    , "}"
    , "wait_for_postmaster_exit() {"
    , "  attempts=0"
    , "  while kill -0 \"$pid\" 2>/dev/null; do"
    , "    [ \"$attempts\" -ge 50 ] && return 1"
    , "    \"$sleep_command\" 0.1"
    , "    attempts=$((attempts + 1))"
    , "  done"
    , "  return 0"
    , "}"
    , "IFS= read -r _ || true"
    , "printf '%s\\n' 'watchdog cleanup started'"
    , "stop_active_command || exit 1"
    , "if [ -f \"$pgdata/postmaster.pid\" ]; then"
    , "  IFS= read -r pid < \"$pgdata/postmaster.pid\" || pid="
    , "  pid_kind=postmaster"
    , "  case \"$pid\" in -*) pid=${pid#-}; pid_kind=single-user ;; esac"
    , "  case \"$pid\" in ''|*[!0-9]*|0*|1) printf 'invalid postmaster.pid <%s>; preserving cluster\\n' \"$pid\"; exit 1 ;; esac"
    , "  if [ \"$pid_kind\" = single-user ]; then"
    , "    if kill -0 \"$pid\" 2>/dev/null && ! wait_for_postmaster_exit; then"
    , "      printf '%s\\n' 'single-user PostgreSQL process is still running; preserving cluster'"
    , "      exit 1"
    , "    fi"
    , "  elif process_matches; then"
    , "    if ! \"$pgctl\" -D \"$pgdata\" -m fast -t 5 -w stop; then"
    , "      process_matches && \"$pgctl\" -D \"$pgdata\" -m immediate -t 5 -w stop || true"
    , "    fi"
    , "    if kill -0 \"$pid\" 2>/dev/null && ! wait_for_postmaster_exit; then"
    , "      printf '%s\\n' 'PostgreSQL did not stop; preserving cluster'"
    , "      exit 1"
    , "    fi"
    , "  elif socket_reports_expected_server; then"
    , "    printf '%s\\n' 'postmaster.pid does not match the private socket lock; preserving cluster'"
    , "    exit 1"
    , "  elif kill -0 \"$pid\" 2>/dev/null; then"
    , "    if ! wait_for_postmaster_exit; then"
    , "      printf '%s\\n' 'postmaster.pid belongs to another process; preserving cluster'"
    , "      exit 1"
    , "    fi"
    , "  fi"
    , "elif socket_reports_expected_server; then"
    , "  printf '%s\\n' 'private PostgreSQL is running but postmaster.pid is missing; preserving cluster'"
    , "  exit 1"
    , "fi"
    , "\"$rm_command\" -rf \"$root\" \"$pghost\""
    ]

destroyManagedDatabase :: PostgreSqlTools -> ManagedAutoDatabase -> IO Bool
destroyManagedDatabase tools ManagedAutoDatabase { madDatabase, madWatchdogInput, madWatchdogProcess, madWatchdogDone } = do
    ignoreIOException (hClose madWatchdogInput)
    finished <- timeout 30000000 (readMVar madWatchdogDone)
    case finished of
        Just ExitSuccess -> pure ()
        Just (ExitFailure code) ->
            appendLifecycleLog madDatabase
                ("watchdog exited with code " <> Prelude.show code)
        Nothing -> do
            appendLifecycleLog madDatabase "watchdog timed out"
            ignoreIOException (interruptProcessGroupOf madWatchdogProcess)
            ignoreIOException (terminateProcess madWatchdogProcess)
            _ <- timeout 2000000 (readMVar madWatchdogDone)
            pure ()
    rootExists <- doesDirectoryExist (adbRoot madDatabase)
    if rootExists
        then destroyDatabaseRoot tools madDatabase
        else do
            ignoreIOException (removePathForcibly (adbPgHost madDatabase))
            not <$> doesDirectoryExist (adbPgHost madDatabase)

destroyDatabaseRoot :: PostgreSqlTools -> AutoDatabase -> IO Bool
destroyDatabaseRoot tools database = do
    result <- stopServer tools ForceIfNeeded database
    logStopFailure database "cluster cleanup" result
    if serverHasStopped result
        then do
            ignoreIOException (removePathForcibly (adbRoot database))
            ignoreIOException (removePathForcibly (adbPgHost database))
            rootExists <- doesDirectoryExist (adbRoot database)
            hostExists <- doesDirectoryExist (adbPgHost database)
            pure (not rootExists && not hostExists)
        else pure False

pruneStaleProcessDirectories :: PostgreSqlTools -> FilePath -> IO ()
pruneStaleProcessDirectories tools processesRoot = do
    entries <- listDirectory processesRoot
    forM_ (filter ("ghc-" `List.isPrefixOf`) entries) \entry ->
        pruneProcessDirectory entry
            `Exception.catch` \(_ :: IOException) -> pure ()
  where
    pruneProcessDirectory entry = do
        let root = processesRoot </> entry
        rootStatus <- getSymbolicLinkStatus root
        userId <- getEffectiveUserID
        when (isDirectory rootStatus && fileOwner rootStatus == userId) do
            owner <- readFileIfExists (root </> "owner.pid")
            shouldPrune <- case owner of
                Just ownerPid -> not <$> ownerProcessIsAlive tools root ownerPid
                Nothing -> processDirectoryIsStale root
            when shouldPrune (destroyProcessDirectory tools root)

processDirectoryIsStale :: FilePath -> IO Bool
processDirectoryIsStale root = do
    staleSeconds <- lookupEnv "IHP_TYPED_SQL_STALE_SECONDS" <&> \configured ->
        fromMaybe 3600 do
            seconds <- configured >>= readMaybe
            guard (seconds > 0)
            pure seconds
    modifiedAt <- getModificationTime root
    currentTime <- getCurrentTime
    pure (diffUTCTime currentTime modifiedAt > fromIntegral staleSeconds)

destroyProcessDirectory :: PostgreSqlTools -> FilePath -> IO ()
destroyProcessDirectory tools root = do
    pgHost <- socketDirectoryPath root
    let database = AutoDatabase
            { adbUrl = ""
            , adbRoot = root
            , adbPgData = root </> "pgdata"
            , adbPgHost = pgHost
            , adbSchemaHash = ""
            }
    _ <- destroyDatabaseRoot tools database
    pure ()

ensureServer :: PostgreSqlTools -> AutoDatabase -> IO ()
ensureServer tools database@AutoDatabase { adbPgData, adbRoot } = do
    ready <- databaseIsReady tools database
    readyAndVerified <- if ready
        then readPostmasterPid database >>= \case
            PostmasterPidFound processId -> processMatchesDatabase tools database processId
            _ -> pure False
        else pure False
    unless readyAndVerified do
        stopped <- stopServer tools ForceIfNeeded database
        unless (serverHasStopped stopped) do
            logStopFailure database "server restart" stopped
            fail
                ( "typedSql: refusing to start PostgreSQL while the previous "
                    <> "postmaster identity cannot be verified; see "
                    <> (adbRoot </> "watchdog.log")
                )
        Prelude.writeFile (adbRoot </> "postgresql.log") ""
        runCheckedTracked tools database (pgCtlPath tools)
            ["-D", adbPgData, "-l", adbRoot </> "postgresql.log", "-w", "start"]

stopServer :: PostgreSqlTools -> StopPolicy -> AutoDatabase -> IO StopServerResult
stopServer tools@PostgreSqlTools { pgCtlPath } policy database@AutoDatabase { adbPgData } = do
    pgDataExists <- doesDirectoryExist adbPgData
    if not pgDataExists
        then socketReportsExpectedServer tools database >>= \serverRunning ->
            pure $ if serverRunning
                then ServerIdentityUnverified "PGDATA is missing while its private server is still running"
                else ServerNotRunning
        else readPostmasterPid database >>= \case
            PostmasterPidMissing ->
                socketReportsExpectedServer tools database >>= \serverRunning ->
                    pure $ if serverRunning
                        then ServerIdentityUnverified "postmaster.pid is missing while its private server is still running"
                        else ServerNotRunning
            PostmasterPidInvalid contents ->
                pure (ServerIdentityUnverified ("invalid postmaster.pid: " <> contents))
            PostmasterPidFound processId -> do
                alive <- processIsAlive processId
                if not alive
                    then socketReportsExpectedServer tools database >>= \serverRunning ->
                        if serverRunning
                            then pure (ServerIdentityMismatch processId
                                "the private socket is live but postmaster.pid names a dead process")
                            else clearStalePostmasterPid database >> pure ServerNotRunning
                    else do
                        matches <- processMatchesDatabase tools database processId
                        if not matches
                            then pure (ServerIdentityMismatch processId
                                "the private PID/socket identity files or process command do not match")
                            else do
                                fastResult <- runStopCommand pgCtlPath adbPgData "fast"
                                case fastResult of
                                    (ExitSuccess, _, _) -> pure ServerStopped
                                    fastFailure -> do
                                        stillMatches <- processMatchesDatabase tools database processId
                                        if not stillMatches
                                            then stopResultAfterFailedCommand database processId fastFailure
                                            else case policy of
                                                FastOnly -> pure (ServerStopFailed (formatCommandFailure fastFailure))
                                                ForceIfNeeded -> do
                                                    immediateResult <- runStopCommand pgCtlPath adbPgData "immediate"
                                                    case immediateResult of
                                                        (ExitSuccess, _, _) -> pure ServerStopped
                                                        immediateFailure ->
                                                            stopResultAfterFailedCommand database processId immediateFailure

runStopCommand :: FilePath -> FilePath -> String -> IO (ExitCode, String, String)
runStopCommand pgCtl pgData mode =
    readProcessWithExitCode
        pgCtl ["-D", pgData, "-m", mode, "-t", "5", "-w", "stop"] ""
        `Exception.catch` \exception ->
            pure (ExitFailure 1, "", displayException (exception :: IOException))

stopResultAfterFailedCommand
    :: AutoDatabase
    -> Int
    -> (ExitCode, String, String)
    -> IO StopServerResult
stopResultAfterFailedCommand database processId commandFailure = do
    alive <- processIsAlive processId
    if alive
        then pure (ServerStopFailed (formatCommandFailure commandFailure))
        else clearStalePostmasterPid database >> pure ServerStopped

formatCommandFailure :: (ExitCode, String, String) -> String
formatCommandFailure (exitCode, stdOut, stdErr) =
    "exit code " <> Prelude.show exitCode
        <> "; stdout: " <> stdOut
        <> "; stderr: " <> stdErr

serverHasStopped :: StopServerResult -> Bool
serverHasStopped ServerStopped = True
serverHasStopped ServerNotRunning = True
serverHasStopped _ = False

logStopFailure :: AutoDatabase -> String -> StopServerResult -> IO ()
logStopFailure _ _ ServerStopped = pure ()
logStopFailure _ _ ServerNotRunning = pure ()
logStopFailure database context (ServerIdentityUnverified message) =
    appendLifecycleLog database
        (context <> ": refusing to stop PostgreSQL because its identity cannot be verified: " <> message)
logStopFailure database context (ServerIdentityMismatch processId command) =
    appendLifecycleLog database
        ( context <> ": refusing to signal PID " <> Prelude.show processId
            <> " because its identity cannot be verified: " <> command
        )
logStopFailure database context (ServerStopFailed message) =
    appendLifecycleLog database (context <> ": PostgreSQL stop failed: " <> message)

appendLifecycleLog :: AutoDatabase -> String -> IO ()
appendLifecycleLog AutoDatabase { adbRoot } message =
    ignoreIOException (appendFile (adbRoot </> "watchdog.log") (message <> "\n"))

readPostmasterPid :: AutoDatabase -> IO PostmasterPidState
readPostmasterPid AutoDatabase { adbPgData } =
    readPidFile (adbPgData </> "postmaster.pid")

clearStalePostmasterPid :: AutoDatabase -> IO ()
clearStalePostmasterPid AutoDatabase { adbPgData } =
    ignoreIOException (removeFile (adbPgData </> "postmaster.pid"))

processMatchesDatabase :: PostgreSqlTools -> AutoDatabase -> Int -> IO Bool
processMatchesDatabase tools database processId = do
    postmasterIdentity <- readPostgreSqlProcessIdentity
        (adbPgData database </> "postmaster.pid")
    socketIdentity <- readPostgreSqlProcessIdentity (socketLockPath database)
    commandMatches <- processCommandMatchesDatabase tools database processId
    pure
        ( maybe False (identityMatchesDatabase database processId) postmasterIdentity
            && maybe False (identityMatchesDatabase database processId) socketIdentity
            && commandMatches
        )

socketReportsExpectedServer :: PostgreSqlTools -> AutoDatabase -> IO Bool
socketReportsExpectedServer tools database =
    readPostgreSqlProcessIdentity (socketLockPath database) >>= \case
        Just identity
            | identityMatchesDatabase database (ppiPid identity) identity -> do
                alive <- processIsAlive (ppiPid identity)
                commandMatches <- processCommandMatchesDatabase tools database (ppiPid identity)
                pure (alive && commandMatches)
        _ -> pure False

socketLockPath :: AutoDatabase -> FilePath
socketLockPath AutoDatabase { adbPgHost } =
    adbPgHost </> (".s.PGSQL." <> privatePostgreSqlPort <> ".lock")

readPostgreSqlProcessIdentity :: FilePath -> IO (Maybe PostgreSqlProcessIdentity)
readPostgreSqlProcessIdentity path =
    readFileIfExists path <&> \case
        Nothing -> Nothing
        Just contents -> parseIdentity (Prelude.lines contents)
  where
    parseIdentity (pidLine:pgDataLine:_:portLine:socketLine:_) = do
        processId <- readMaybe pidLine
        guard (processId > 1)
        pure PostgreSqlProcessIdentity
            { ppiPid = processId
            , ppiPgData = pgDataLine
            , ppiPort = portLine
            , ppiSocketDir = socketLine
            }
    parseIdentity _ = Nothing

identityMatchesDatabase :: AutoDatabase -> Int -> PostgreSqlProcessIdentity -> Bool
identityMatchesDatabase AutoDatabase { adbPgData, adbPgHost } processId PostgreSqlProcessIdentity { ppiPid, ppiPgData, ppiPort, ppiSocketDir } =
    ppiPid == processId
        && ppiPgData == adbPgData
        && ppiPort == privatePostgreSqlPort
        && ppiSocketDir == adbPgHost

processCommandMatchesDatabase :: PostgreSqlTools -> AutoDatabase -> Int -> IO Bool
processCommandMatchesDatabase PostgreSqlTools { psPath } AutoDatabase { adbPgData } processId = do
    (exitCode, stdOut, _) <- readProcessWithExitCode psPath
        ["-p", Prelude.show processId, "-o", "command="] ""
        `Exception.catch` \(_ :: IOException) -> pure (ExitFailure 1, "", "")
    let expectedCommand = "/postgres -D " <> adbPgData
        expectedBareCommand = "postgres -D " <> adbPgData
        commandLine = List.dropWhileEnd Char.isSpace (dropWhile Char.isSpace stdOut)
        commandMatches expected =
            expected `List.isSuffixOf` commandLine
                || (expected <> " ") `List.isInfixOf` commandLine
    pure
        ( exitCode == ExitSuccess
            && (commandMatches expectedCommand
                || expectedBareCommand == commandLine
                || (expectedBareCommand <> " ") `List.isPrefixOf` commandLine)
        )

ownerProcessIsAlive :: PostgreSqlTools -> FilePath -> String -> IO Bool
ownerProcessIsAlive PostgreSqlTools { psPath } _ ownerPid =
    case readMaybe ownerPid :: Maybe Int of
        Nothing -> pure False
        Just processId -> do
            (exitCode, stdOut, _) <- readProcessWithExitCode psPath
                ["-p", Prelude.show processId, "-o", "pid="] ""
                `Exception.catch` \(_ :: IOException) -> pure (ExitFailure 1, "", "")
            pure (exitCode == ExitSuccess && not (null (dropWhile Char.isSpace stdOut)))

databaseIsReady :: PostgreSqlTools -> AutoDatabase -> IO Bool
databaseIsReady PostgreSqlTools { pgIsReadyPath } AutoDatabase { adbPgHost } = do
    (exitCode, _, _) <- readProcessWithExitCode pgIsReadyPath
        ["-h", adbPgHost, "-p", privatePostgreSqlPort, "-t", "2"] ""
        `Exception.catch` \(_ :: IOException) -> pure (ExitFailure 1, "", "")
    pure (exitCode == ExitSuccess)

initializeDatabase :: PostgreSqlTools -> SchemaInputs -> AutoDatabase -> IO AutoDatabase
initializeDatabase tools schemaInputs database@AutoDatabase { adbRoot, adbPgHost } = do
    runCheckedTracked tools database (psqlPath tools)
        [ "-v", "ON_ERROR_STOP=1", "-h", adbPgHost, "-p", privatePostgreSqlPort
        , "postgres", "-c"
        , "ALTER DATABASE template0 IS_TEMPLATE false; ALTER DATABASE template1 IS_TEMPLATE false;"
        ]
    runCheckedTracked tools database (dropdbPath tools)
        ["-h", adbPgHost, "-p", privatePostgreSqlPort, "--maintenance-db=postgres", "template0"]
    runCheckedTracked tools database (psqlPath tools)
        [ "-v", "ON_ERROR_STOP=1", "-h", adbPgHost, "-p", privatePostgreSqlPort
        , "postgres", "-c"
        , "ALTER DATABASE template1 RENAME TO app;"
        ]
    runCheckedTracked tools database (dropdbPath tools)
        ["-h", adbPgHost, "-p", privatePostgreSqlPort, "--maintenance-db=app", "postgres"]
    forM_ (catMaybes [siIhpSchema schemaInputs, Just (siAppSchema schemaInputs)]) \schemaFile ->
        runCheckedTracked tools database (psqlPath tools)
            [ "-v", "ON_ERROR_STOP=1", "-h", adbPgHost, "-p", privatePostgreSqlPort
            , "app", "-f", schemaFile
            ]
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

resolvePostgreSqlTools
    :: Maybe String
    -> Maybe ResolvedPostgreSqlTools
    -> IO ResolvedPostgreSqlTools
resolvePostgreSqlTools pathEnvironment cached =
    case cached of
        Just resolved | rptPathEnvironment resolved == pathEnvironment -> pure resolved
        _ -> do
            tools <- findPostgreSqlTools
            fingerprint <- toolchainFingerprint tools
            pure ResolvedPostgreSqlTools
                { rptTools = tools
                , rptFingerprint = fingerprint
                , rptPathEnvironment = pathEnvironment
                }

findPostgreSqlTools :: IO PostgreSqlTools
findPostgreSqlTools = do
    let commandNames = ["initdb", "pg_ctl", "dropdb", "psql", "pg_isready", "sh", "rm", "sleep", "ps"]
    commands <- forM commandNames \command -> do
        executable <- findExecutable command
        pure (command, executable)
    let missing = [command | (command, Nothing) <- commands]
    unless (null missing) do
        fail
            ( "typedSql: automatic compile-time database is enabled via "
                <> "IHP_TYPED_SQL_AUTO_DB, but these required tools are not "
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
        , dropdbPath = command "dropdb"
        , psqlPath = command "psql"
        , pgIsReadyPath = command "pg_isready"
        , shellPath = command "sh"
        , rmPath = command "rm"
        , sleepPath = command "sleep"
        , psPath = command "ps"
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

processIsAlive :: Int -> IO Bool
processIsAlive processId =
    if processId > 1
        then (signalProcess nullSignal (fromIntegral processId) >> pure True)
            `Exception.catch` \(_ :: IOException) -> pure False
        else pure False

readFileIfExists :: FilePath -> IO (Maybe String)
readFileIfExists path = do
    exists <- doesFileExist path
    if exists
        then Just <$> Prelude.readFile path
        else pure Nothing

readPidFile :: FilePath -> IO PostmasterPidState
readPidFile path =
    readFileIfExists path <&> \case
        Nothing -> PostmasterPidMissing
        Just contents ->
            let firstLine = fromMaybe "" (listToMaybe (Prelude.lines contents))
            in case readMaybe firstLine of
                Just processId | abs processId > 1 -> PostmasterPidFound (abs processId)
                _ -> PostmasterPidInvalid firstLine

ignoreIOException :: IO a -> IO ()
ignoreIOException action =
    (action >> pure ()) `Exception.catch` \(_ :: IOException) -> pure ()

runCheckedTracked :: PostgreSqlTools -> AutoDatabase -> FilePath -> [String] -> IO ()
runCheckedTracked PostgreSqlTools { shellPath } database command args = do
    let activePath = adbRoot database </> "active-command.pid"
        pendingPath = adbRoot database </> "active-command.pending"
        stdOutPath = adbRoot database </> "active-command.stdout"
        stdErrPath = adbRoot database </> "active-command.stderr"
        launcher = List.unlines
            [ "set -e"
            , "active_file=$1"
            , "shift"
            , "printf '%s\\n' \"$$\" > \"$active_file\""
            , "exec \"$@\""
            ]
        process = (proc shellPath
            (["-c", launcher, "ihp-typed-sql-command", activePath, command] <> args))
            { std_in = NoStream
            , close_fds = True
            , create_group = True
            }
    ignoreIOException (removeFile activePath)
    Prelude.writeFile pendingPath (takeFileName command <> "\n")
    exitCode <- withFile stdOutPath WriteMode \stdOutHandle ->
        withFile stdErrPath WriteMode \stdErrHandle -> do
            (_, _, _, processHandle) <- createProcess process
                { std_out = UseHandle stdOutHandle
                , std_err = UseHandle stdErrHandle
                }
            waitForProcess processHandle
    ignoreIOException (removeFile activePath)
    ignoreIOException (removeFile pendingPath)
    stdOut <- fromMaybe "" <$> readFileIfExists stdOutPath
    stdErr <- fromMaybe "" <$> readFileIfExists stdErrPath
    ignoreIOException (removeFile stdOutPath)
    ignoreIOException (removeFile stdErrPath)
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
