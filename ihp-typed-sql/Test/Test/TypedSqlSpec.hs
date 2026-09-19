{-# LANGUAGE CPP #-}
module Test.TypedSqlSpec where

import           Control.Concurrent                 (threadDelay)
import qualified Control.Exception                 as Exception
import           Control.Exception                  (IOException)
import           Control.Monad                      (forM, forM_, unless, when)
import           Data.Maybe                         (catMaybes, fromMaybe, isJust,
                                                     isNothing, listToMaybe)
import           Data.String.Conversions            (cs)
import qualified Data.List                         as List
import qualified Data.Set                          as Set
import qualified Data.Text                         as Text
import qualified Data.Text.IO                      as Text
import qualified Hasql.Decoders                    as HasqlDecoders
import qualified Hasql.Encoders                    as HasqlEncoders
import qualified Hasql.Pool                        as HasqlPool
import qualified Hasql.Pool.Config                 as HasqlPoolConfig
import qualified Hasql.Connection.Settings         as HasqlSettings
import qualified Hasql.Session                     as HasqlSession
import qualified Hasql.Statement                   as HasqlStatement
#ifdef LIBPQ_BACKEND
import qualified Pqi.Ffi as Pqi
#else
import qualified Pqi.Native as Pqi
#endif
import           IHP.TypedSql.ParamHints           (parseSql, extractJoinNullableTables,
                                                    extractNonNullableComputedColumnsFromAst,
                                                    detectStarSelects,
                                                    detectInsertWithoutColumns)
import           System.Directory                  (createDirectoryIfMissing,
                                                    doesDirectoryExist,
                                                    doesFileExist,
                                                    findExecutable,
                                                    getCurrentDirectory,
                                                    getHomeDirectory,
                                                    listDirectory,
                                                    removePathForcibly,
                                                    renameFile)
import           System.Environment                (getEnvironment, getExecutablePath,
                                                     lookupEnv)
import           System.Exit                       (ExitCode (..))
import           System.FilePath                   (searchPathSeparator,
                                                    takeDirectory, (</>))
import           System.IO                         (Handle, hClose, hFlush)
import           System.IO.Temp.OsPath              (withSystemTempDirectory)
import           System.OsPath                     (encodeUtf, decodeUtf)
import           System.Posix.Files                (setFileMode, setFileTimes)
import           System.Posix.Signals              (nullSignal, signalProcess,
                                                    sigCONT, sigKILL, sigSTOP)
import           System.Process                    (CreateProcess (..), ProcessHandle,
                                                    StdStream (CreatePipe, Inherit, NoStream),
                                                    createProcess,
                                                    getProcessExitCode,
                                                    getPid,
                                                    interruptProcessGroupOf, proc,
                                                    readCreateProcessWithExitCode,
                                                    readProcessWithExitCode,
                                                    terminateProcess, waitForProcess)
import           System.Timeout                    (timeout)
import           Test.Hspec
import           Text.Read                         (readMaybe)
import           Prelude
import           Data.Text                         (Text)

-- | 'IHP.Prelude.tshow' replacement: render with 'show' and convert to 'Text'.
tshow :: Show a => a -> Text
tshow = cs . show

-- | The Hasql connection adapter, matching the @libpq-backend@ flag used by
-- the library (see @ihp-typed-sql.cabal@). hasql 2.x threads the adapter
-- explicitly through 'Hasql.Pool.acquire'. (No type annotation: the 'Adapter'
-- type lives in the @pqi@ package, which the test suite does not depend on
-- directly; the use site fixes the type.)
pqiAdapter = Pqi.adapter

-- | The @Pqi@ adapter import for generated ghci test modules, matching the
-- @libpq-backend@ flag (ghci resolves it from the test package database).
#ifdef LIBPQ_BACKEND
pqiAdapterImport :: Text
pqiAdapterImport = "import qualified Pqi.Ffi as Pqi"
#else
pqiAdapterImport :: Text
pqiAdapterImport = "import qualified Pqi.Native as Pqi"
#endif

tests :: Spec
tests = do
    describe "TypedSql macro compile-time checks" do
        it "compiles valid typedSql queries with inferred types" do
            requirePostgresTestHook
            withTestPool \pool -> do
                setupSchema pool
                ghciOutput <- ghciLoadModule compilePassModule
                assertGhciSuccess ghciOutput

        compileFailTest "fails when a scalar parameter has the wrong type"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "[typedSql| SELECT name FROM typed_sql_test_items WHERE views = ${(\"not an int\" :: Text)} LIMIT 1 |]")
            []

        compileFailTest "fails when a foreign-key parameter has the wrong type"
            (mkTestModuleWithPK ["typed_sql_test_items", "typed_sql_test_authors"] "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "[typedSql| SELECT name FROM typed_sql_test_items WHERE author_id = ${(\"not-an-id\" :: Text)} LIMIT 1 |]")
            []

        compileFailTest "fails when an IN parameter has the wrong element type"
            (mkTestModuleWithPK ["typed_sql_test_items", "typed_sql_test_authors"] "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "let authorIds = [\"one\" :: Text, \"two\" :: Text]\n      in [typedSql| SELECT name FROM typed_sql_test_items WHERE author_id IN (${authorIds}) LIMIT 1 |]")
            []

        compileFailTest "fails when a placeholder expression is invalid Haskell"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "[typedSql| SELECT name FROM typed_sql_test_items WHERE views = ${(} LIMIT 1 |]")
            ["failed to parse expression"]

        compileFailTest "fails when SQL parameter count does not match ${...} placeholders"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "[typedSql| SELECT name FROM typed_sql_test_items WHERE views = $1 LIMIT 1 |]")
            ["placeholder count mismatch"]

        compileFailTest "fails when selecting a single composite value without expansion"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "[typedSql| SELECT ROW(name, views)::typed_sql_test_pair FROM typed_sql_test_items LIMIT 1 |]")
            ["composite columns must be expanded"]

        compileFailTest "fails when using SELECT * (bare asterisk)"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "[typedSql| SELECT * FROM typed_sql_test_items LIMIT 1 |]")
            ["is not allowed"]

        compileFailTest "fails when using SELECT table.*"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "[typedSql| SELECT typed_sql_test_items.* FROM typed_sql_test_items LIMIT 1 |]")
            ["is not allowed"]

        compileFailTest "fails when INSERT VALUES has no explicit column list"
            (mkTestModule "TypedQuery 'ManyRows 'ReturnsRows Text"
                "[typedSql| INSERT INTO typed_sql_test_items VALUES ('00000000-0000-0000-0000-000000000099'::uuid, '00000000-0000-0000-0000-000000000001'::uuid, 'X', 1, 1.0, ARRAY['x']::text[]) RETURNING name |]")
            ["explicit column list"]

        compileFailTest "fails when SQL references an unknown column"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "[typedSql| SELECT no_such_column FROM typed_sql_test_items LIMIT 1 |]")
            ["does not exist"]

        compileFailTest "fails when primary-key result type is annotated as UUID instead of Id"
            (mkTestModuleWithPK ["typed_sql_test_items"] "TypedQuery 'AtMostOneRow 'ReturnsRows UUID"
                "[typedSql| SELECT id FROM typed_sql_test_items LIMIT 1 |]")
            []

        compileFailTest "fails when nullable column result is annotated as non-Maybe"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Double"
                "[typedSql| SELECT score FROM typed_sql_test_items LIMIT 1 |]")
            []

        compileFailTest "fails when multi-column result is annotated as a tuple"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows (Text, Text)"
                "[typedSql| SELECT i.name, a.name FROM typed_sql_test_items i LEFT JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]")
            []

        compileFailTest "fails when multi-column result is annotated as a scalar"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "[typedSql| SELECT name, views FROM typed_sql_test_items LIMIT 1 |]")
            []

        compileFailTest "fails when boolean expression result is annotated as Int"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Int"
                "[typedSql| SELECT author_id IS NULL FROM typed_sql_test_items LIMIT 1 |]")
            []

        compileFailTest "fails when boolean expression result is annotated as non-Maybe Bool"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Bool"
                "[typedSql| SELECT author_id IS NULL FROM typed_sql_test_items LIMIT 1 |]")
            []

        compileFailTest "fails when COUNT(*) result is annotated as Maybe Int64"
            (mkTestModule "TypedQuery 'ExactlyOneRow 'ReturnsRows (Maybe Int64)"
                "[typedSql| SELECT COUNT(*) FROM typed_sql_test_items |]")
            []

        compileFailTest "fails when COALESCE multi-column result is annotated as a tuple"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows (Maybe Text, Text)"
                "[typedSql| SELECT COALESCE(i.name, '(no-item)'), a.name FROM typed_sql_test_items i RIGHT JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]")
            []

        compileFailTest "fails when literal expression result is annotated as Maybe Int"
            (mkTestModule "TypedQuery 'ExactlyOneRow 'ReturnsRows (Maybe Int)"
                "[typedSql| SELECT 1 |]")
            []

        compileFailTest "fails when arithmetic expression result is annotated as non-Maybe Int"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Int"
                "[typedSql| SELECT views + 1 FROM typed_sql_test_items LIMIT 1 |]")
            []

        compileFailTest "fails when CASE expression result is annotated as non-Maybe Text"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "[typedSql| SELECT CASE WHEN views > 5 THEN name ELSE 'low' END FROM typed_sql_test_items LIMIT 1 |]")
            []

        compileFailTest "fails when EXISTS expression result is annotated as Maybe Bool"
            (mkTestModule "TypedQuery 'ExactlyOneRow 'ReturnsRows (Maybe Bool)"
                "[typedSql| SELECT EXISTS(SELECT 1 FROM typed_sql_test_items WHERE views > 7) |]")
            []

        compileFailTest "fails when NULL literal result is annotated as non-Maybe Text"
            (mkTestModule "TypedQuery 'ExactlyOneRow 'ReturnsRows Text"
                "[typedSql| SELECT NULL::text |]")
            []

        compileFailTest "fails when CTE result is annotated as Maybe Text"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows (Maybe Text)"
                "[typedSql| WITH item_names AS (SELECT name FROM typed_sql_test_items WHERE views > 6) SELECT name FROM item_names LIMIT 1 |]")
            []

        compileFailTest "fails when subquery result is annotated as Maybe Text"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows (Maybe Text)"
                "[typedSql| SELECT name FROM (SELECT name FROM typed_sql_test_items WHERE views < 6) sub LIMIT 1 |]")
            []

        compileFailTest "fails when UNION result is annotated as non-Maybe Text"
            (mkTestModule "TypedQuery 'ManyRows 'ReturnsRows Text"
                "[typedSql| SELECT name FROM typed_sql_test_items WHERE views > 6 UNION ALL SELECT name FROM typed_sql_test_items WHERE views < 6 |]")
            []

        compileFailTest "fails when window function result is annotated as Maybe Int64"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows (Maybe Int64)"
                "[typedSql| SELECT row_number() OVER (ORDER BY name) FROM typed_sql_test_items LIMIT 1 |]")
            []

        compileFailTest "fails when grouped COUNT(*) result is annotated as a tuple"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows (Text, Maybe Int64)"
                "[typedSql| SELECT name, COUNT(*) FROM typed_sql_test_items GROUP BY name ORDER BY name LIMIT 1 |]")
            []

        compileFailTest "fails when array literal result is annotated as non-Maybe [Text]"
            (mkTestModule "TypedQuery 'ExactlyOneRow 'ReturnsRows [Text]"
                "[typedSql| SELECT ARRAY['x','y']::text[] |]")
            []

        compileFailTest "fails when NULLIF expression result is annotated as non-Maybe Text"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "[typedSql| SELECT NULLIF(name, 'First') FROM typed_sql_test_items LIMIT 1 |]")
            []

        compileFailTest "explains polymorphic-argument inference failure with placeholder context"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "let chunk = (\"x\" :: Text) in [typedSql| SELECT CONCAT(name, ${chunk}) FROM typed_sql_test_items LIMIT 1 |]")
            ["could not determine the type of `${chunk}`", "polymorphic-argument context", "::text"]

        compileFailTest "fails when sqlExecTyped is used for a row-returning query"
            sqlExecTypedSelectCompileFailModule
            ["sqlExecTyped cannot run SQL statements that return rows"]

        -- AUTO_DB tests are disabled (xit): they boot private unix-socket
        -- clusters, while the suite always runs against TCP via DATABASE_URL.
        xit "rebuilds one compact private cluster across schema changes" do
            requireAutoDatabaseTools
            withAutoDatabaseFixture
                "CREATE TABLE typed_sql_schema_before (id UUID PRIMARY KEY, name TEXT NOT NULL);\n"
                \tempDir schemaPath stateDir envOverrides -> do
                realInitdb <- findExecutable "initdb" >>= \case
                    Just path -> pure path
                    Nothing -> expectationFailure "initdb disappeared from PATH" >> pure "initdb"
                basePath <- fromMaybe "" <$> lookupEnv "PATH"
                let wrapperDirectory = tempDir </> "initdb-version-wrapper"
                    wrapperPath = wrapperDirectory </> "initdb"
                    versionMarker = tempDir </> "initdb-version-calls"
                    wrappedPath = wrapperDirectory <> [searchPathSeparator] <> basePath
                    testEnvironment =
                        setEnvironmentOverride "PATH" wrappedPath
                            (setEnvironmentOverride "IHP_TEST_REAL_INITDB" realInitdb
                                (setEnvironmentOverride "IHP_TEST_INITDB_VERSION_MARKER" versionMarker envOverrides))
                    modulePath = tempDir </> "ReusableTypedSqlCase.hs"
                    firstModule = mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                        "[typedSql| SELECT name FROM typed_sql_schema_before LIMIT 1 |]"
                    secondModule = mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                        "[typedSql| SELECT name FROM typed_sql_schema_after LIMIT 1 |]"
                createDirectoryIfMissing True wrapperDirectory
                Text.writeFile wrapperPath (Text.unlines
                    [ "#!/bin/sh"
                    , "if [ \"$1\" = --version ]; then"
                    , "  printf 'called\\n' >> \"$IHP_TEST_INITDB_VERSION_MARKER\""
                    , "fi"
                    , "exec \"$IHP_TEST_REAL_INITDB\" \"$@\""
                    ])
                setFileMode wrapperPath 0o700
                (inputHandle, processHandle) <- startGhciLoadProcess modulePath firstModule testEnvironment
                let cleanup = stopGhciProcess inputHandle processHandle
                flip Exception.finally cleanup do
                    waitForCondition 1200 ((== 1) . length <$> readyAutoDatabaseProcessRoots stateDir)
                        `shouldReturn` True
                    [processRoot] <- readyAutoDatabaseProcessRoots stateDir
                    let postmasterPath = processRoot </> "pgdata" </> "postmaster.pid"
                        schemaHashPath = processRoot </> "schema.hash"
                    firstHash <- Prelude.readFile schemaHashPath
                    configuration <- Prelude.readFile (processRoot </> "pgdata" </> "postgresql.conf")
                    configuration `shouldContain` "shared_buffers = 4MB"
                    configuration `shouldContain` "max_worker_processes = 0"
                    databaseDirectories processRoot `shouldReturn` 1

                    waitForCondition 300 (not <$> doesFileExist postmasterPath) `shouldReturn` True
                    getProcessExitCode processHandle `shouldReturn` Nothing

                    Text.writeFile schemaPath
                        "CREATE TABLE typed_sql_schema_after (id UUID PRIMARY KEY, name TEXT NOT NULL);\n"
                    Text.writeFile modulePath secondModule
                    Text.hPutStr inputHandle (":load " <> tshow modulePath <> "\n")
                    hFlush inputHandle

                    waitForCondition 1200 (do
                        roots <- readyAutoDatabaseProcessRoots stateDir
                        pure $ case roots of
                            [newRoot] -> newRoot /= processRoot
                            _ -> False
                        ) `shouldReturn` True
                    [newProcessRoot] <- readyAutoDatabaseProcessRoots stateDir
                    doesDirectoryExist processRoot `shouldReturn` False
                    secondHash <- Prelude.readFile (newProcessRoot </> "schema.hash")
                    secondHash `shouldNotBe` firstHash
                    databaseDirectories newProcessRoot `shouldReturn` 1
                    versionCalls <- Prelude.lines <$> Prelude.readFile versionMarker
                    length versionCalls `shouldBe` 1

                    Text.hPutStr inputHandle ":quit\n"
                    hFlush inputHandle
                    exited <- timeout 10000000 (waitForProcess processHandle)
                    exited `shouldSatisfy` isJust
                    ignoreProcessException (hClose inputHandle)
                    removed <- waitForCondition 400 (null <$> autoDatabaseProcessRoots stateDir)
                    unless removed do
                        roots <- autoDatabaseProcessRoots stateDir
                        watchdogLogs <- forM roots \root -> do
                            logContents <- readTestFileIfExists (root </> "watchdog.log")
                            pure (root <> ":\n" <> fromMaybe "<missing>" logContents)
                        expectationFailure
                            ( "private cluster was not removed after GHCi exited"
                                <> "\nwatchdog logs:\n"
                                <> Prelude.unlines watchdogLogs
                            )

        xit "uses isolated clusters for concurrent GHC processes" do
            requireAutoDatabaseTools
            withAutoDatabaseFixture
                "CREATE TABLE typed_sql_concurrent (id UUID PRIMARY KEY, name TEXT NOT NULL);\n"
                \tempDir _schemaPath stateDir envOverrides -> do
                let firstModulePath = tempDir </> "ConcurrentTypedSqlCase1.hs"
                    secondModulePath = tempDir </> "ConcurrentTypedSqlCase2.hs"
                    testModule = mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                        "[typedSql| SELECT name FROM typed_sql_concurrent LIMIT 1 |]"
                (firstInput, firstProcess) <- startGhciLoadProcess firstModulePath testModule envOverrides
                (secondInput, secondProcess) <- startGhciLoadProcess secondModulePath testModule envOverrides
                let cleanup = do
                        stopGhciProcess firstInput firstProcess
                        stopGhciProcess secondInput secondProcess
                flip Exception.finally cleanup do
                    waitForCondition 1200 ((== 2) . length <$> readyAutoDatabaseProcessRoots stateDir)
                        `shouldReturn` True
                    roots <- readyAutoDatabaseProcessRoots stateDir
                    ownerPids <- Prelude.traverse (Prelude.readFile . (</> "owner.pid")) roots
                    Set.size (Set.fromList ownerPids) `shouldBe` 2
                    getProcessExitCode firstProcess `shouldReturn` Nothing
                    getProcessExitCode secondProcess `shouldReturn` Nothing

        xit "removes the private cluster when only the compiler dies during schema loading" do
            requireAutoDatabaseTools
            withAutoDatabaseFixture
                "CREATE TABLE typed_sql_interrupted_placeholder (id UUID PRIMARY KEY);\n"
                \tempDir schemaPath stateDir envOverrides -> do
                let modulePath = tempDir </> "InterruptedTypedSqlCase.hs"
                    schemaLoadMarker = tempDir </> "schema-load-started"
                    testModule = mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                        "[typedSql| SELECT name FROM typed_sql_interrupted LIMIT 1 |]"
                    markerSqlPath = Text.replace "'" "''" (Text.pack schemaLoadMarker)
                Text.writeFile schemaPath
                    ( "CREATE TABLE typed_sql_interrupted (id UUID PRIMARY KEY, name TEXT NOT NULL);\n"
                        <> "COPY (SELECT 'started') TO '" <> markerSqlPath <> "';\n"
                        <> "SELECT pg_sleep(30);\n"
                    )
                (inputHandle, processHandle) <- startGhciLoadProcess modulePath testModule envOverrides
                let cleanup = stopGhciProcess inputHandle processHandle
                flip Exception.finally cleanup do
                    waitForCondition 1200 ((== 1) . length <$> autoDatabaseProcessRoots stateDir)
                        `shouldReturn` True
                    [processRoot] <- autoDatabaseProcessRoots stateDir
                    let postmasterPath = processRoot </> "pgdata" </> "postmaster.pid"
                    waitForCondition 1200 (doesFileExist postmasterPath) `shouldReturn` True
                    waitForCondition 1200 (doesFileExist schemaLoadMarker) `shouldReturn` True
                    postmasterPidContents <- Prelude.readFile postmasterPath
                    postmasterPid <- case listToMaybe (Prelude.lines postmasterPidContents) of
                        Just processId -> pure processId
                        Nothing -> expectationFailure "postmaster.pid did not contain a process id" >> pure ""
                    ownerPid <- Prelude.readFile (processRoot </> "owner.pid")

                    getPid processHandle >>= \case
                        Just compilerPid ->
                            ignoreProcessException (signalProcess sigKILL compilerPid)
                        Nothing -> expectationFailure "GHCi process exited before it could be killed"
                    ignoreProcessException (hClose inputHandle)
                    exited <- timeout 5000000 (waitForProcess processHandle)
                    when (isNothing exited) do
                        ignoreProcessException (terminateProcess processHandle)
                        _ <- timeout 5000000 (waitForProcess processHandle)
                        pure ()

                    waitForCondition 300 (not <$> processIsAlive ownerPid) `shouldReturn` True
                    waitForCondition 300 (not <$> processIsAlive postmasterPid) `shouldReturn` True
                    removed <- waitForCondition 400 (not <$> doesDirectoryExist processRoot)
                    unless removed do
                        watchdogLog <- readTestFileIfExists (processRoot </> "watchdog.log")
                        expectationFailure
                            ( "private cluster was not removed after compiler SIGKILL"
                                <> "\nwatchdog.log:\n" <> fromMaybe "<missing>" watchdogLog
                            )

        xit "removes the private cluster when only the compiler dies during initdb" do
            requireAutoDatabaseTools
            withAutoDatabaseFixture
                "CREATE TABLE typed_sql_initdb_interrupted (id UUID PRIMARY KEY, name TEXT NOT NULL);\n"
                \tempDir _schemaPath stateDir envOverrides -> do
                realInitdb <- findExecutable "initdb" >>= \case
                    Just path -> pure path
                    Nothing -> expectationFailure "initdb disappeared from PATH" >> pure "initdb"
                basePath <- fromMaybe "" <$> lookupEnv "PATH"
                let wrapperDirectory = tempDir </> "initdb-wrapper"
                    wrapperPath = wrapperDirectory </> "initdb"
                    markerPath = tempDir </> "initdb-single-user-stopped"
                    modulePath = tempDir </> "InitdbInterruptedTypedSqlCase.hs"
                    testModule = mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                        "[typedSql| SELECT name FROM typed_sql_initdb_interrupted LIMIT 1 |]"
                    wrappedPath = wrapperDirectory <> [searchPathSeparator] <> basePath
                    testEnvironment =
                        setEnvironmentOverride "PATH" wrappedPath
                            (setEnvironmentOverride "IHP_TEST_REAL_INITDB" realInitdb
                                (setEnvironmentOverride "IHP_TEST_INITDB_MARKER" markerPath envOverrides))
                createDirectoryIfMissing True wrapperDirectory
                Text.writeFile wrapperPath (Text.unlines
                    [ "#!/bin/sh"
                    , "real_initdb=$IHP_TEST_REAL_INITDB"
                    , "marker=$IHP_TEST_INITDB_MARKER"
                    , "pgdata="
                    , "previous="
                    , "for argument in \"$@\"; do"
                    , "  if [ \"$previous\" = -D ]; then pgdata=$argument; break; fi"
                    , "  previous=$argument"
                    , "done"
                    , "\"$real_initdb\" \"$@\" &"
                    , "initdb_pid=$!"
                    , "if [ -n \"$pgdata\" ]; then"
                    , "  while kill -0 \"$initdb_pid\" 2>/dev/null; do"
                    , "    backend_pid="
                    , "    [ -f \"$pgdata/postmaster.pid\" ] && IFS= read -r backend_pid < \"$pgdata/postmaster.pid\" || true"
                    , "    case \"$backend_pid\" in"
                    , "      -[0-9]*)"
                    , "        backend_pid=${backend_pid#-}"
                    , "        if kill -STOP \"$backend_pid\" 2>/dev/null; then"
                    , "          echo \"$$ $initdb_pid $backend_pid\" > \"$marker\""
                    , "          break"
                    , "        fi"
                    , "        ;;"
                    , "    esac"
                    , "    sleep 0.01"
                    , "  done"
                    , "fi"
                    , "wait \"$initdb_pid\""
                    ])
                setFileMode wrapperPath 0o700

                (inputHandle, processHandle) <- startGhciLoadProcess modulePath testModule testEnvironment
                let cleanup = stopGhciProcess inputHandle processHandle
                flip Exception.finally cleanup do
                    waitForCondition 1200 (doesFileExist markerPath) `shouldReturn` True
                    markerContents <- Prelude.readFile markerPath
                    (wrapperPid, initdbPid, backendPid) <- case Prelude.words markerContents of
                        [wrapper, initdb, backend] -> pure (wrapper, initdb, backend)
                        _ -> expectationFailure ("invalid initdb marker: " <> markerContents) >> pure ("", "", "")
                    [processRoot] <- autoDatabaseProcessRoots stateDir

                    getPid processHandle >>= \case
                        Just compilerPid -> ignoreProcessException (signalProcess sigKILL compilerPid)
                        Nothing -> expectationFailure "GHCi process exited before it could be killed"
                    case readMaybe backendPid :: Maybe Int of
                        Just processId -> ignoreProcessException (signalProcess sigCONT (fromIntegral processId))
                        Nothing -> expectationFailure "initdb marker did not contain a backend PID"
                    ignoreProcessException (hClose inputHandle)
                    _ <- timeout 5000000 (waitForProcess processHandle)

                    waitForCondition 400 (not <$> processIsAlive wrapperPid) `shouldReturn` True
                    waitForCondition 400 (not <$> processIsAlive initdbPid) `shouldReturn` True
                    waitForCondition 400 (not <$> processIsAlive backendPid) `shouldReturn` True
                    waitForCondition 400 (not <$> doesDirectoryExist processRoot) `shouldReturn` True

        xit "retries idle shutdown after a worker exception" do
            requireAutoDatabaseTools
            withAutoDatabaseFixture
                "CREATE TABLE typed_sql_idle_retry (id UUID PRIMARY KEY, name TEXT NOT NULL);\n"
                \tempDir _schemaPath stateDir envOverrides -> do
                let modulePath = tempDir </> "IdleRetryTypedSqlCase.hs"
                    testModule = mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                        "[typedSql| SELECT name FROM typed_sql_idle_retry LIMIT 1 |]"
                (inputHandle, processHandle) <- startGhciLoadProcess modulePath testModule envOverrides
                let cleanup = stopGhciProcess inputHandle processHandle
                flip Exception.finally cleanup do
                    waitForCondition 1200 ((== 1) . length <$> readyAutoDatabaseProcessRoots stateDir)
                        `shouldReturn` True
                    [processRoot] <- readyAutoDatabaseProcessRoots stateDir
                    let postmasterPath = processRoot </> "pgdata" </> "postmaster.pid"
                        watchdogLogPath = processRoot </> "watchdog.log"
                        restorePermissions = do
                            exists <- doesFileExist postmasterPath
                            when exists (setFileMode postmasterPath 0o600)
                    setFileMode postmasterPath 0o000
                    flip Exception.finally restorePermissions do
                        waitForCondition 300 (do
                            logContents <- readTestFileIfExists watchdogLogPath
                            pure (maybe False (List.isInfixOf "idle stop failed with an exception") logContents)
                            ) `shouldReturn` True
                        restorePermissions
                        waitForCondition 400 (not <$> doesFileExist postmasterPath) `shouldReturn` True
                        getProcessExitCode processHandle `shouldReturn` Nothing

        xit "does not query an unresponsive postmaster to verify its identity" do
            requireAutoDatabaseTools
            withAutoDatabaseFixture
                "CREATE TABLE typed_sql_unresponsive (id UUID PRIMARY KEY, name TEXT NOT NULL);\n"
                \tempDir _schemaPath stateDir envOverrides -> do
                let modulePath = tempDir </> "UnresponsiveTypedSqlCase.hs"
                    testModule = mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                        "[typedSql| SELECT name FROM typed_sql_unresponsive LIMIT 1 |]"
                (inputHandle, processHandle) <- startGhciLoadProcess modulePath testModule envOverrides
                let cleanup = stopGhciProcess inputHandle processHandle
                flip Exception.finally cleanup do
                    waitForCondition 1200 ((== 1) . length <$> readyAutoDatabaseProcessRoots stateDir)
                        `shouldReturn` True
                    [processRoot] <- readyAutoDatabaseProcessRoots stateDir
                    let postmasterPath = processRoot </> "pgdata" </> "postmaster.pid"
                        watchdogLogPath = processRoot </> "watchdog.log"
                    postmasterPid <- Prelude.readFile postmasterPath >>= \contents ->
                        case listToMaybe (Prelude.lines contents) >>= readMaybe of
                            Just processId -> pure processId
                            Nothing -> expectationFailure "postmaster.pid did not contain a process id" >> pure 0
                    signalProcess sigSTOP postmasterPid
                    flip Exception.finally
                        (ignoreProcessException (signalProcess sigCONT postmasterPid)) do
                        waitForCondition 300 (do
                            logContents <- readTestFileIfExists watchdogLogPath
                            pure (maybe False (List.isInfixOf "idle stop: PostgreSQL stop failed") logContents)
                            ) `shouldReturn` True
                    waitForCondition 400 (not <$> doesFileExist postmasterPath) `shouldReturn` True
                    getProcessExitCode processHandle `shouldReturn` Nothing

        xit "does not signal a postmaster PID that disagrees with the private socket lock" do
            requireAutoDatabaseTools
            withAutoDatabaseFixture
                "CREATE TABLE typed_sql_pid_binding (id UUID PRIMARY KEY, name TEXT NOT NULL);\n"
                \tempDir _schemaPath stateDir envOverrides -> do
                let modulePath = tempDir </> "PidBindingTypedSqlCase.hs"
                    compilationFinishedMarker = tempDir </> "pid-binding-load-finished"
                    testModule = mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                        "[typedSql| SELECT name FROM typed_sql_pid_binding LIMIT 1 |]"
                    longIdleEnvironment =
                        setEnvironmentOverride "IHP_TYPED_SQL_IDLE_SECONDS" "30" envOverrides
                (inputHandle, processHandle) <- startGhciLoadProcess modulePath testModule longIdleEnvironment
                let processCleanup = stopGhciProcess inputHandle processHandle
                flip Exception.finally processCleanup do
                    Text.hPutStr inputHandle
                        (":! touch " <> Text.pack compilationFinishedMarker <> "\n")
                    hFlush inputHandle
                    waitForCondition 1200 ((== 1) . length <$> readyAutoDatabaseProcessRoots stateDir)
                        `shouldReturn` True
                    waitForCondition 1200 (doesFileExist compilationFinishedMarker)
                        `shouldReturn` True
                    [processRoot] <- readyAutoDatabaseProcessRoots stateDir
                    let postmasterPath = processRoot </> "pgdata" </> "postmaster.pid"
                        replacementPath = postmasterPath <> ".test-replacement"
                        replacePostmasterPid contents = do
                            Prelude.writeFile replacementPath contents
                            renameFile replacementPath postmasterPath
                    originalPostmasterPid <- Prelude.readFile postmasterPath
                    actualPostmasterPid <- case listToMaybe (Prelude.lines originalPostmasterPid) of
                        Just processId -> pure processId
                        Nothing -> expectationFailure "postmaster.pid did not contain a process id" >> pure ""
                    socketDirectory <- case drop 4 (Prelude.lines originalPostmasterPid) of
                        path : _ -> pure path
                        _ -> expectationFailure "postmaster.pid did not contain a socket directory" >> pure ""
                    (_, _, _, unrelatedProcess) <- createProcess (proc "sleep" ["60"])
                        { std_in = NoStream
                        , std_out = NoStream
                        , std_err = NoStream
                        }
                    unrelatedPid <- getPid unrelatedProcess >>= \case
                        Just processId -> pure processId
                        Nothing -> expectationFailure "sleep process exited unexpectedly" >> pure 0
                    let corruptedPostmasterPid = Prelude.unlines
                            (Prelude.show unrelatedPid : drop 1 (Prelude.lines originalPostmasterPid))
                        identityCleanup = do
                            ignoreProcessException (replacePostmasterPid originalPostmasterPid)
                            maybePgCtl <- findExecutable "pg_ctl"
                            forM_ maybePgCtl \pgCtl -> do
                                _ <- readProcessWithExitCode pgCtl
                                    ["-D", processRoot </> "pgdata", "-m", "immediate", "-t", "5", "-w", "stop"] ""
                                    `Exception.catch` \(_ :: IOException) -> pure (ExitFailure 1, "", "")
                                pure ()
                            ignoreProcessException (removePathForcibly processRoot)
                            when ("ihp-typed-sql-" `List.isInfixOf` socketDirectory) do
                                ignoreProcessException (removePathForcibly socketDirectory)
                            ignoreProcessException (terminateProcess unrelatedProcess)
                            _ <- timeout 5000000 (waitForProcess unrelatedProcess)
                            pure ()
                    flip Exception.finally identityCleanup do
                        replacePostmasterPid corruptedPostmasterPid
                        getPid processHandle >>= \case
                            Just compilerPid -> ignoreProcessException (signalProcess sigKILL compilerPid)
                            Nothing -> expectationFailure "GHCi process exited before it could be killed"
                        ignoreProcessException (hClose inputHandle)
                        _ <- timeout 5000000 (waitForProcess processHandle)

                        waitForCondition 300 (do
                            logContents <- readTestFileIfExists (processRoot </> "watchdog.log")
                            pure (maybe False (List.isInfixOf "does not match the private socket lock") logContents)
                            ) `shouldReturn` True
                        getProcessExitCode unrelatedProcess `shouldReturn` Nothing
                        processIsAlive actualPostmasterPid `shouldReturn` True
                        doesDirectoryExist processRoot `shouldReturn` True

        xit "preserves unverified stale clusters without signaling their PID" do
            requireAutoDatabaseTools
            withAutoDatabaseFixture
                "CREATE TABLE typed_sql_stale_pid (id UUID PRIMARY KEY, name TEXT NOT NULL);\n"
                \tempDir _schemaPath stateDir envOverrides -> do
                let staleRoot = stateDir </> "processes" </> "ghc-stale-process"
                    stalePgData = staleRoot </> "pgdata"
                    malformedRoot = stateDir </> "processes" </> "ghc-malformed-process"
                    malformedPgData = malformedRoot </> "pgdata"
                    modulePath = tempDir </> "StalePidTypedSqlCase.hs"
                    testModule = mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                        "[typedSql| SELECT name FROM typed_sql_stale_pid LIMIT 1 |]"
                createDirectoryIfMissing True stalePgData
                Prelude.writeFile (staleRoot </> "owner.pid") "99999999"
                Prelude.writeFile (stalePgData </> "PG_VERSION") "17\n"
                (_, _, _, unrelatedProcess) <- createProcess (proc "sleep" ["60"])
                    { std_in = NoStream
                    , std_out = NoStream
                    , std_err = NoStream
                    }
                unrelatedPid <- getPid unrelatedProcess >>= \case
                    Just processId -> pure processId
                    Nothing -> expectationFailure "sleep process exited unexpectedly" >> pure 0
                Prelude.writeFile (stalePgData </> "postmaster.pid")
                    (Prelude.show unrelatedPid <> "\n")
                createDirectoryIfMissing True malformedPgData
                Prelude.writeFile (malformedRoot </> "owner.pid") "99999999"
                Prelude.writeFile (malformedPgData </> "PG_VERSION") "17\n"
                Prelude.writeFile (malformedPgData </> "postmaster.pid") "not-a-pid\n"

                (inputHandle, processHandle) <- startGhciLoadProcess modulePath testModule envOverrides
                let cleanup = do
                        stopGhciProcess inputHandle processHandle
                        ignoreProcessException (terminateProcess unrelatedProcess)
                        _ <- timeout 5000000 (waitForProcess unrelatedProcess)
                        ignoreProcessException (removePathForcibly staleRoot)
                        ignoreProcessException (removePathForcibly malformedRoot)
                flip Exception.finally cleanup do
                    waitForCondition 1200 ((== 1) . length <$> readyAutoDatabaseProcessRoots stateDir)
                        `shouldReturn` True
                    getProcessExitCode unrelatedProcess `shouldReturn` Nothing
                    doesDirectoryExist staleRoot `shouldReturn` True
                    doesDirectoryExist malformedRoot `shouldReturn` True

        xit "reaps an old ownerless process directory" do
            requireAutoDatabaseTools
            withAutoDatabaseFixture
                "CREATE TABLE typed_sql_ownerless (id UUID PRIMARY KEY, name TEXT NOT NULL);\n"
                \tempDir _schemaPath stateDir envOverrides -> do
                let ownerlessRoot = stateDir </> "processes" </> "ghc-ownerless-process"
                    modulePath = tempDir </> "OwnerlessTypedSqlCase.hs"
                    testModule = mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                        "[typedSql| SELECT name FROM typed_sql_ownerless LIMIT 1 |]"
                    testEnvironment =
                        setEnvironmentOverride "IHP_TYPED_SQL_STALE_SECONDS" "1" envOverrides
                createDirectoryIfMissing True (ownerlessRoot </> "pgdata")
                setFileTimes ownerlessRoot 1 1

                (inputHandle, processHandle) <- startGhciLoadProcess modulePath testModule testEnvironment
                let cleanup = stopGhciProcess inputHandle processHandle
                flip Exception.finally cleanup do
                    waitForCondition 1200 ((== 1) . length <$> readyAutoDatabaseProcessRoots stateDir)
                        `shouldReturn` True
                    doesDirectoryExist ownerlessRoot `shouldReturn` False

    describe "TypedSql macro compile-time success" do
        compilePassTest "primary key inferred as Id'"
            (mkTestModuleWithPK ["typed_sql_test_items"] "TypedQuery 'AtMostOneRow 'ReturnsRows (Id' \"typed_sql_test_items\")"
                "[typedSql| SELECT id FROM typed_sql_test_items LIMIT 1 |]")

        compilePassTest "nullable column inferred as Maybe"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows (Maybe Double)"
                "[typedSql| SELECT score FROM typed_sql_test_items LIMIT 1 |]")

        compilePassTest "LEFT JOIN right side inferred as Maybe"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows (SqlRow '[ '(\"name\", Text), '(\"name_1\", Maybe Text) ])"
                "[typedSql| SELECT i.name, a.name FROM typed_sql_test_items i LEFT JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]")

        compilePassTest "RIGHT JOIN left side inferred as Maybe"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows (SqlRow '[ '(\"name\", Maybe Text), '(\"name_1\", Text) ])"
                "[typedSql| SELECT i.name, a.name FROM typed_sql_test_items i RIGHT JOIN typed_sql_test_authors a ON a.id = i.author_id ORDER BY a.name LIMIT 1 |]")

        compilePassTest "multi-column ad-hoc returns SqlRow"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows (SqlRow '[ '(\"name\", Text), '(\"views\", Int) ])"
                "[typedSql| SELECT name, views FROM typed_sql_test_items LIMIT 1 |]")

        -- Regression for https://github.com/digitallyinduced/ihp/issues/2767:
        -- RowTuple / TupleGet support up to 16 columns (previously 10).
        compilePassTest "supports more than 10 selected columns"
            (mkTestModule
                "TypedQuery 'ExactlyOneRow 'ReturnsRows (SqlRow '[ '(\"c1\", Int), '(\"c2\", Int), '(\"c3\", Int), '(\"c4\", Int), '(\"c5\", Int), '(\"c6\", Int), '(\"c7\", Int), '(\"c8\", Int), '(\"c9\", Int), '(\"c10\", Int), '(\"c11\", Int) ])"
                "[typedSql| SELECT 1 AS c1, 2 AS c2, 3 AS c3, 4 AS c4, 5 AS c5, 6 AS c6, 7 AS c7, 8 AS c8, 9 AS c9, 10 AS c10, 11 AS c11 |]")

        compilePassTest "supports 16 selected columns"
            (mkTestModule
                "TypedQuery 'ExactlyOneRow 'ReturnsRows (SqlRow '[ '(\"c1\", Int), '(\"c2\", Int), '(\"c3\", Int), '(\"c4\", Int), '(\"c5\", Int), '(\"c6\", Int), '(\"c7\", Int), '(\"c8\", Int), '(\"c9\", Int), '(\"c10\", Int), '(\"c11\", Int), '(\"c12\", Int), '(\"c13\", Int), '(\"c14\", Int), '(\"c15\", Int), '(\"c16\", Int) ])"
                "[typedSql| SELECT 1 AS c1, 2 AS c2, 3 AS c3, 4 AS c4, 5 AS c5, 6 AS c6, 7 AS c7, 8 AS c8, 9 AS c9, 10 AS c10, 11 AS c11, 12 AS c12, 13 AS c13, 14 AS c14, 15 AS c15, 16 AS c16 |]")

        compilePassTest "boolean expression inferred as Maybe Bool"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows (Maybe Bool)"
                "[typedSql| SELECT author_id IS NULL FROM typed_sql_test_items LIMIT 1 |]")

        compilePassTest "COUNT(*) inferred as Int64"
            (mkTestModule "TypedQuery 'ExactlyOneRow 'ReturnsRows Int64"
                "[typedSql| SELECT COUNT(*) FROM typed_sql_test_items |]")

        compilePassTest "COALESCE with non-null fallback inferred as non-Maybe"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows (SqlRow '[ '(\"coalesce\", Text), '(\"name\", Text) ])"
                "[typedSql| SELECT COALESCE(i.name, '(no-item)'), a.name FROM typed_sql_test_items i RIGHT JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]")

        compilePassTest "literal expression inferred as Int"
            (mkTestModule "TypedQuery 'ExactlyOneRow 'ReturnsRows Int"
                "[typedSql| SELECT 1 |]")

        compilePassTest "arithmetic expression inferred as Maybe Int"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows (Maybe Int)"
                "[typedSql| SELECT views + 1 FROM typed_sql_test_items LIMIT 1 |]")

        compilePassTest "CASE expression inferred as Maybe Text"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows (Maybe Text)"
                "[typedSql| SELECT CASE WHEN views > 5 THEN name ELSE 'low' END FROM typed_sql_test_items LIMIT 1 |]")

        compilePassTest "EXISTS expression inferred as Bool"
            (mkTestModule "TypedQuery 'ExactlyOneRow 'ReturnsRows Bool"
                "[typedSql| SELECT EXISTS(SELECT 1 FROM typed_sql_test_items WHERE views > 7) |]")

        compilePassTest "NULL literal inferred as Maybe Text"
            (mkTestModule "TypedQuery 'ExactlyOneRow 'ReturnsRows (Maybe Text)"
                "[typedSql| SELECT NULL::text |]")

        compilePassTest "CTE preserves column type"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "[typedSql| WITH item_names AS (SELECT name FROM typed_sql_test_items WHERE views > 6) SELECT name FROM item_names LIMIT 1 |]")

        compilePassTest "subquery preserves column type"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "[typedSql| SELECT name FROM (SELECT name FROM typed_sql_test_items WHERE views < 6) sub LIMIT 1 |]")

        compilePassTest "UNION inferred as Maybe"
            (mkTestModule "TypedQuery 'ManyRows 'ReturnsRows (Maybe Text)"
                "[typedSql| SELECT name FROM typed_sql_test_items WHERE views > 6 UNION ALL SELECT name FROM typed_sql_test_items WHERE views < 6 |]")

        compilePassTest "window function inferred as Int64"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Int64"
                "[typedSql| SELECT row_number() OVER (ORDER BY name) FROM typed_sql_test_items LIMIT 1 |]")

        compilePassTest "grouped COUNT(*) returns SqlRow"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows (SqlRow '[ '(\"name\", Text), '(\"count\", Int64) ])"
                "[typedSql| SELECT name, COUNT(*) FROM typed_sql_test_items GROUP BY name ORDER BY name LIMIT 1 |]")

        compilePassTest "array literal inferred as Maybe [Text]"
            (mkTestModule "TypedQuery 'ExactlyOneRow 'ReturnsRows (Maybe [Text])"
                "[typedSql| SELECT ARRAY['x','y']::text[] |]")

        compilePassTest "NULLIF inferred as Maybe Text"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows (Maybe Text)"
                "[typedSql| SELECT NULLIF(name, 'First') FROM typed_sql_test_items LIMIT 1 |]")

        compilePassTest "scalar parameter accepts correct type"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "[typedSql| SELECT name FROM typed_sql_test_items WHERE views = ${(5 :: Int)} LIMIT 1 |]")

        compilePassTest "foreign-key parameter accepts Id' type"
            (mkTestModuleWithPK ["typed_sql_test_items", "typed_sql_test_authors"] "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "let authorId = (Id (uuid \"00000000-0000-0000-0000-000000000001\") :: Id' \"typed_sql_test_authors\")\n      in [typedSql| SELECT name FROM typed_sql_test_items WHERE author_id = ${authorId} LIMIT 1 |]")

        compilePassTest "foreign-key parameter accepts raw primary key list"
            (mkTestModuleWithPK ["typed_sql_test_items", "typed_sql_test_authors"] "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "let authorIds = [(\"00000000-0000-0000-0000-000000000001\" :: UUID)]\n      in [typedSql| SELECT name FROM typed_sql_test_items WHERE author_id IN (${authorIds}) LIMIT 1 |]")

        compilePassTest "foreign-key parameter accepts Maybe raw primary key"
            (mkTestModuleWithPK ["typed_sql_test_items", "typed_sql_test_authors"] "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "let authorId = Just (\"00000000-0000-0000-0000-000000000001\" :: UUID)\n      in [typedSql| SELECT name FROM typed_sql_test_items WHERE author_id IS NOT DISTINCT FROM ${authorId} LIMIT 1 |]")

        compilePassTest "foreign-key parameter accepts maybe raw primary key list"
            (mkTestModuleWithPK ["typed_sql_test_items", "typed_sql_test_authors"] "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "let authorIds = [Just (\"00000000-0000-0000-0000-000000000001\" :: UUID)]\n      in [typedSql| SELECT name FROM typed_sql_test_items WHERE author_id = ANY(${authorIds}) LIMIT 1 |]")

        compilePassTest "Maybe parameter accepts unannotated Nothing"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "[typedSql| SELECT name FROM typed_sql_test_items WHERE score IS NOT DISTINCT FROM ${Nothing} LIMIT 1 |]")

        compilePassTest "list parameter accepts unannotated empty list"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "[typedSql| SELECT name FROM typed_sql_test_items WHERE name IN (${[]}) LIMIT 1 |]")

        compilePassTest "list-of-Maybe parameter accepts unannotated Nothing list"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                "[typedSql| SELECT name FROM typed_sql_test_items WHERE score = ANY(${[Nothing]}) LIMIT 1 |]")

        compilePassTest "INNER JOIN columns are non-Maybe"
            (mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows (SqlRow '[ '(\"name\", Text), '(\"name_1\", Text) ])"
                "[typedSql| SELECT i.name, a.name FROM typed_sql_test_items i INNER JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]")

        compilePassTest "COUNT through subquery alias inferred as Int64"
            (mkTestModule "TypedQuery 'ExactlyOneRow 'ReturnsRows Int64"
                "[typedSql| SELECT p.c FROM (SELECT count(*) AS c FROM typed_sql_test_items) AS p |]")

        compilePassTest "SUM through subquery alias remains Maybe"
            (mkTestModule "TypedQuery 'ExactlyOneRow 'ReturnsRows (Maybe Int64)"
                "[typedSql| SELECT p.s FROM (SELECT sum(views) AS s FROM typed_sql_test_items) AS p |]")

        compilePassTest "COUNT through CTE inferred as Int64"
            (mkTestModule "TypedQuery 'ExactlyOneRow 'ReturnsRows Int64"
                "[typedSql| WITH item_counts AS (SELECT count(*) AS c FROM typed_sql_test_items) SELECT c FROM item_counts |]")

        compilePassTest "jsonb_build_object inferred as non-Maybe JSON"
            (mkTestModuleWithAeson "TypedQuery 'ExactlyOneRow 'ReturnsRows Aeson.Value"
                "[typedSql| SELECT jsonb_build_object('name', NULL::text) |]")

        compilePassTest "json_build_array inferred as non-Maybe JSON"
            (mkTestModuleWithAeson "TypedQuery 'ExactlyOneRow 'ReturnsRows Aeson.Value"
                "[typedSql| SELECT json_build_array(NULL::text) |]")

    describe "TypedSql macro runtime execution" do
        -- These run against the standalone @IHP.TypedSql.Hasql@ runner with a
        -- caller-managed pool. The @IHP.TypedSql@ request-scoped runners
        -- (@sqlQueryTyped@, @paginatedTypedSql@, ...) are covered by
        -- @Test.TypedSqlSpec@ in the @ihp@ package instead, so this package
        -- does not depend on @ihp@.
        runtimeTest "executes typedSql queries with a caller-managed Hasql pool" runtimeExplicitHasqlModule
        runtimeTest "enum, Maybe enum, and [Maybe enum] parameters via ${...}" runtimeEnumModule

    describe "TypedSql SQL parser (pure, no postgres)" do
        it "parseSql succeeds on simple SELECT" do
            parseSql "SELECT 1" `shouldSatisfy` isJust

        it "parseSql handles leading/trailing whitespace from quasiquoter" do
            parseSql " SELECT 1 " `shouldSatisfy` isJust

        it "parseSql accepts unspaced ANY operators" do
            parseSql "SELECT 1 WHERE 1=ANY(ARRAY[1])" `shouldSatisfy` isJust

        it "parseSql accepts the JSONB key-existence operator" do
            parseSql "SELECT '{}'::jsonb ? 'key'" `shouldSatisfy` isJust

        it "extractJoinNullableTables detects LEFT JOIN nullable table" do
            let sql = " SELECT i.name, a.name FROM items i LEFT JOIN authors a ON a.id = i.aid LIMIT 1 "
            extractJoinNullableTables sql `shouldBe` Set.fromList ["authors"]

        it "extractJoinNullableTables detects RIGHT JOIN nullable table" do
            let sql = " SELECT i.name, a.name FROM items i RIGHT JOIN authors a ON a.id = i.aid LIMIT 1 "
            extractJoinNullableTables sql `shouldBe` Set.fromList ["items"]

        it "extractJoinNullableTables detects FULL JOIN nullable tables" do
            let sql = "SELECT i.name, a.name FROM items i FULL JOIN authors a ON a.id = i.aid"
            extractJoinNullableTables sql `shouldBe` Set.fromList ["items", "authors"]

        it "extractJoinNullableTables returns empty for INNER JOIN" do
            let sql = " SELECT i.name, a.name FROM items i INNER JOIN authors a ON a.id = i.aid "
            extractJoinNullableTables sql `shouldBe` Set.empty

        it "extractJoinNullableTables returns empty for plain FROM" do
            let sql = " SELECT name FROM items LIMIT 1 "
            extractJoinNullableTables sql `shouldBe` Set.empty

        it "extractNonNullableComputedColumns detects count(*)" do
            let Just ast = parseSql "SELECT count(*) FROM items"
            extractNonNullableComputedColumnsFromAst ast `shouldBe` Set.fromList [0]

        it "extractNonNullableComputedColumns detects count(expr)" do
            let Just ast = parseSql "SELECT count(id) FROM items"
            extractNonNullableComputedColumnsFromAst ast `shouldBe` Set.fromList [0]

        it "extractNonNullableComputedColumns does not mark sum as non-nullable" do
            let Just ast = parseSql "SELECT sum(x) FROM items"
            extractNonNullableComputedColumnsFromAst ast `shouldBe` Set.empty

        it "extractNonNullableComputedColumns handles mixed columns" do
            let Just ast = parseSql "SELECT name, count(*) FROM items GROUP BY name"
            extractNonNullableComputedColumnsFromAst ast `shouldBe` Set.fromList [1]

        it "extractNonNullableComputedColumns detects count through subquery alias" do
            let Just ast = parseSql "SELECT p.c FROM (SELECT count(*) AS c FROM items) AS p"
            extractNonNullableComputedColumnsFromAst ast `shouldBe` Set.fromList [0]

        it "extractNonNullableComputedColumns detects count through CTE" do
            let Just ast = parseSql "WITH x AS (SELECT count(*) AS c FROM items) SELECT c FROM x"
            extractNonNullableComputedColumnsFromAst ast `shouldBe` Set.fromList [0]

        it "extractNonNullableComputedColumns does not mark regular columns" do
            let Just ast = parseSql "SELECT name FROM items"
            extractNonNullableComputedColumnsFromAst ast `shouldBe` Set.empty

        it "extractNonNullableComputedColumns detects EXISTS" do
            let Just ast = parseSql "SELECT EXISTS(SELECT 1 FROM items)"
            extractNonNullableComputedColumnsFromAst ast `shouldBe` Set.fromList [0]

        it "extractNonNullableComputedColumns detects row_number()" do
            let Just ast = parseSql "SELECT row_number() OVER (ORDER BY name) FROM items"
            extractNonNullableComputedColumnsFromAst ast `shouldBe` Set.fromList [0]

        it "extractNonNullableComputedColumns detects rank()" do
            let Just ast = parseSql "SELECT rank() OVER (ORDER BY name) FROM items"
            extractNonNullableComputedColumnsFromAst ast `shouldBe` Set.fromList [0]

        it "extractNonNullableComputedColumns detects dense_rank()" do
            let Just ast = parseSql "SELECT dense_rank() OVER (ORDER BY name) FROM items"
            extractNonNullableComputedColumnsFromAst ast `shouldBe` Set.fromList [0]

        it "extractNonNullableComputedColumns detects non-NULL literals" do
            let Just ast = parseSql "SELECT 1, 'hello', TRUE"
            extractNonNullableComputedColumnsFromAst ast `shouldBe` Set.fromList [0, 1, 2]

        it "extractNonNullableComputedColumns does not mark NULL literal" do
            let Just ast = parseSql "SELECT NULL"
            extractNonNullableComputedColumnsFromAst ast `shouldBe` Set.empty

        it "extractNonNullableComputedColumns detects COALESCE with non-null arg" do
            let Just ast = parseSql "SELECT COALESCE(name, 'default') FROM items"
            extractNonNullableComputedColumnsFromAst ast `shouldBe` Set.fromList [0]

        it "extractNonNullableComputedColumns does not mark COALESCE with all nullable args" do
            let Just ast = parseSql "SELECT COALESCE(a, b) FROM items"
            extractNonNullableComputedColumnsFromAst ast `shouldBe` Set.empty

        it "extractNonNullableComputedColumns detects typecast of non-null" do
            let Just ast = parseSql "SELECT 1::bigint"
            extractNonNullableComputedColumnsFromAst ast `shouldBe` Set.fromList [0]

        it "extractNonNullableComputedColumns does not mark NULL::text" do
            let Just ast = parseSql "SELECT NULL::text"
            extractNonNullableComputedColumnsFromAst ast `shouldBe` Set.empty

        it "extractNonNullableComputedColumns detects JSON build constructors" do
            let Just ast = parseSql "SELECT jsonb_build_object('x', NULL::text), json_build_array(NULL::text)"
            extractNonNullableComputedColumnsFromAst ast `shouldBe` Set.fromList [0, 1]

        it "detectStarSelects detects bare SELECT *" do
            let Just ast = parseSql "SELECT * FROM items"
            detectStarSelects ast `shouldBe` ["*"]

        it "detectStarSelects detects SELECT table.*" do
            let Just ast = parseSql "SELECT items.* FROM items"
            detectStarSelects ast `shouldBe` ["items.*"]

        it "detectStarSelects detects SELECT alias.*" do
            let Just ast = parseSql "SELECT i.* FROM items i"
            detectStarSelects ast `shouldBe` ["i.*"]

        it "detectStarSelects does not flag COUNT(*)" do
            let Just ast = parseSql "SELECT COUNT(*) FROM items"
            detectStarSelects ast `shouldBe` []

        it "detectStarSelects does not flag explicit columns" do
            let Just ast = parseSql "SELECT id, name FROM items"
            detectStarSelects ast `shouldBe` []

        it "detectStarSelects does not flag composite expansion" do
            let Just ast = parseSql "SELECT (ROW(name, views)::my_type).* FROM items"
            detectStarSelects ast `shouldBe` []

        it "detectStarSelects detects star in parenthesized SELECT" do
            let Just ast = parseSql "(SELECT * FROM items)"
            detectStarSelects ast `shouldBe` ["*"]

        it "detectInsertWithoutColumns detects INSERT VALUES without column list" do
            let Just ast = parseSql "INSERT INTO items VALUES (1, 'name')"
            detectInsertWithoutColumns ast `shouldBe` ["INSERT INTO items"]

        it "detectInsertWithoutColumns detects INSERT SELECT without column list" do
            let Just ast = parseSql "INSERT INTO items SELECT 1, 'name'"
            detectInsertWithoutColumns ast `shouldBe` ["INSERT INTO items"]

        it "detectInsertWithoutColumns does not flag INSERT with column list" do
            let Just ast = parseSql "INSERT INTO items (id, name) VALUES (1, 'name')"
            detectInsertWithoutColumns ast `shouldBe` []

        it "detectInsertWithoutColumns does not flag INSERT DEFAULT VALUES" do
            let Just ast = parseSql "INSERT INTO items DEFAULT VALUES"
            detectInsertWithoutColumns ast `shouldBe` []

        it "detectInsertWithoutColumns does not flag SELECT" do
            let Just ast = parseSql "SELECT * FROM items"
            detectInsertWithoutColumns ast `shouldBe` []

-- Test helpers ---------------------------------------------------------------

requirePostgresTestHook :: IO ()
requirePostgresTestHook = do
    maybePgHost <- lookupEnv "PGHOST"
    when (isNothing maybePgHost) do
        pendingWith "requires postgresqlTestHook / withTestPostgres (PGHOST is not set)"

-- | Run an action with a Hasql pool connected to @DATABASE_URL@.
-- Replaces the former @IHP.ModelSupport@ 'ModelContext' helper so the test
-- suite does not depend on the @ihp@ package.
withTestPool :: (HasqlPool.Pool -> IO a) -> IO a
withTestPool action = do
    databaseUrl <- cs . fromMaybe "" <$> lookupEnv "DATABASE_URL"
    let poolConfig = HasqlPoolConfig.settings
            [ HasqlPoolConfig.size 2
            , HasqlPoolConfig.staticConnectionSettings (HasqlSettings.connectionString databaseUrl)
            ]
    Exception.bracket (HasqlPool.acquire pqiAdapter poolConfig) HasqlPool.release action

-- | Run a SQL statement without a result (for DDL in 'setupSchema').
execDiscard :: HasqlPool.Pool -> Text -> IO ()
execDiscard pool sql = do
    result <- HasqlPool.use pool (HasqlSession.statement () statement)
    case result of
        Left usageError -> fail ("setupSchema failed for " <> show sql <> ": " <> show usageError)
        Right () -> pure ()
  where
    statement = HasqlStatement.preparable sql HasqlEncoders.noParams HasqlDecoders.noResult

setupSchema :: HasqlPool.Pool -> IO ()
setupSchema pool = do
    -- DDL statements have no rows-affected count, so they run as no-result statements
    execDiscard pool "DROP TABLE IF EXISTS typed_sql_test_enum_items"
    execDiscard pool "DROP TABLE IF EXISTS typed_sql_test_extras"
    execDiscard pool "DROP TABLE IF EXISTS typed_sql_test_items"
    execDiscard pool "DROP TABLE IF EXISTS typed_sql_test_authors"
    execDiscard pool "DROP TYPE IF EXISTS typed_sql_test_pair"
    execDiscard pool "DROP TYPE IF EXISTS typed_sql_test_mood"

    execDiscard pool "CREATE TYPE typed_sql_test_pair AS (name TEXT, views INT)"

    execDiscard pool
        "CREATE TABLE typed_sql_test_authors (id UUID PRIMARY KEY, name TEXT NOT NULL)"

    execDiscard pool
        "CREATE TABLE typed_sql_test_items (id UUID PRIMARY KEY, author_id UUID REFERENCES typed_sql_test_authors(id), name TEXT NOT NULL, views INT NOT NULL, score DOUBLE PRECISION, tags TEXT[] NOT NULL DEFAULT '{}')"

    execDiscard pool
        "INSERT INTO typed_sql_test_authors (id, name) VALUES ('00000000-0000-0000-0000-000000000001'::uuid, 'Alice')"

    execDiscard pool
        "INSERT INTO typed_sql_test_authors (id, name) VALUES ('00000000-0000-0000-0000-000000000002'::uuid, 'Bob')"

    execDiscard pool
        "INSERT INTO typed_sql_test_items (id, author_id, name, views, score, tags) VALUES ('10000000-0000-0000-0000-000000000001'::uuid, '00000000-0000-0000-0000-000000000001'::uuid, 'First', 5, 1.5, ARRAY['red', 'blue'])"

    execDiscard pool
        "INSERT INTO typed_sql_test_items (id, author_id, name, views, score, tags) VALUES ('10000000-0000-0000-0000-000000000002'::uuid, '00000000-0000-0000-0000-000000000001'::uuid, 'Second', 8, NULL, ARRAY['green'])"

    execDiscard pool
        "CREATE TABLE typed_sql_test_extras (id UUID PRIMARY KEY, small_count SMALLINT NOT NULL DEFAULT 0, big_count BIGINT NOT NULL DEFAULT 0, amount NUMERIC, payload BYTEA, metadata JSONB, created_at TIMESTAMPTZ NOT NULL DEFAULT '2025-06-15 12:00:00+00', due_date DATE, active BOOLEAN NOT NULL DEFAULT TRUE)"

    execDiscard pool
        "INSERT INTO typed_sql_test_extras (id, small_count, big_count, amount, payload, metadata, created_at, due_date, active) VALUES ('20000000-0000-0000-0000-000000000001'::uuid, 7, 1000000000, 99.95, '\\xDEADBEEF', '{\"key\": \"value\"}', '2025-06-15 12:00:00+00', '2025-06-15', true)"

    -- Enum type + table for exercising DefaultParamEncoder enum interpolation in typedSql
    execDiscard pool "CREATE TYPE typed_sql_test_mood AS ENUM ('happy', 'sad', 'neutral')"

    execDiscard pool
        "CREATE TABLE typed_sql_test_enum_items (id UUID PRIMARY KEY, name TEXT NOT NULL, mood typed_sql_test_mood NOT NULL, opt_mood typed_sql_test_mood)"

    execDiscard pool
        "INSERT INTO typed_sql_test_enum_items (id, name, mood, opt_mood) VALUES ('30000000-0000-0000-0000-000000000001'::uuid, 'HappyItem', 'happy', 'happy')"

    execDiscard pool
        "INSERT INTO typed_sql_test_enum_items (id, name, mood, opt_mood) VALUES ('30000000-0000-0000-0000-000000000002'::uuid, 'SadItem', 'sad', NULL)"

    pure ()

-- GHCi infrastructure --------------------------------------------------------

requireAutoDatabaseTools :: IO ()
requireAutoDatabaseTools = do
    available <- Prelude.traverse findExecutable ["initdb", "ps"]
    when (any isNothing available) do
        pendingWith "requires PostgreSQL tools on PATH"

withAutoDatabaseFixture
    :: Text
    -> (FilePath -> FilePath -> FilePath -> [(String, String)] -> IO a)
    -> IO a
withAutoDatabaseFixture schema action = do
    template <- encodeUtf "typed-sql-auto-db"
    withSystemTempDirectory template \tempOsDir -> do
        tempDir <- decodeUtf tempOsDir
        let applicationDir = tempDir </> "Application"
            schemaPath = applicationDir </> "Schema.sql"
            stateDir = tempDir <> "-typed-sql-state"
            missingSocket = tempDir </> "missing-socket"
            envOverrides =
                [ ("DATABASE_URL", "postgresql:///app?host=" <> missingSocket)
                , ("IHP_TYPED_SQL_AUTO_DB", "1")
                , ("IHP_TYPED_SQL_SCHEMA", schemaPath)
                , ("IHP_TYPED_SQL_STATE_DIR", stateDir)
                , ("IHP_TYPED_SQL_IDLE_SECONDS", "3")
                ]
        createDirectoryIfMissing True applicationDir
        Text.writeFile schemaPath schema
        action tempDir schemaPath stateDir envOverrides
            `Exception.finally` cleanupAutoDatabaseFixture stateDir

cleanupAutoDatabaseFixture :: FilePath -> IO ()
cleanupAutoDatabaseFixture stateDir = do
    _ <- waitForCondition 300 (null <$> autoDatabaseProcessRoots stateDir)
    processRoots <- autoDatabaseProcessRoots stateDir
    forM_ processRoots \processRoot -> do
        maybePgCtl <- findExecutable "pg_ctl"
        forM_ maybePgCtl \pgCtl -> do
            _ <- readProcessWithExitCode pgCtl
                ["-D", processRoot </> "pgdata", "-m", "fast", "-w", "stop"] ""
                `Exception.catch` \(_ :: IOException) -> pure (ExitFailure 1, "", "")
            pure ()
    ignoreProcessException (removePathForcibly stateDir)

autoDatabaseProcessRoots :: FilePath -> IO [FilePath]
autoDatabaseProcessRoots stateDir = do
    let processesRoot = stateDir </> "processes"
    exists <- doesDirectoryExist processesRoot
    if not exists
        then pure []
        else do
            entries <- listDirectory processesRoot
            catMaybes <$> forM entries \entry -> do
                let processRoot = processesRoot </> entry
                isProcessRoot <- doesDirectoryExist processRoot
                pure (if isProcessRoot then Just processRoot else Nothing)

readyAutoDatabaseProcessRoots :: FilePath -> IO [FilePath]
readyAutoDatabaseProcessRoots stateDir = do
    roots <- autoDatabaseProcessRoots stateDir
    catMaybes <$> forM roots \root -> do
        initialized <- doesFileExist (root </> "pgdata" </> "PG_VERSION")
        schemaLoaded <- doesFileExist (root </> "schema.hash")
        pure (if initialized && schemaLoaded then Just root else Nothing)

databaseDirectories :: FilePath -> IO Int
databaseDirectories processRoot = do
    let baseDirectory = processRoot </> "pgdata" </> "base"
    entries <- listDirectory baseDirectory
    length . catMaybes <$> forM entries \entry -> do
        let path = baseDirectory </> entry
        isDatabaseDirectory <- doesDirectoryExist path
        pure (if isDatabaseDirectory then Just path else Nothing)

readTestFileIfExists :: FilePath -> IO (Maybe String)
readTestFileIfExists path = do
    exists <- doesFileExist path
    if exists
        then (Just <$> Prelude.readFile path)
            `Exception.catch` \(_ :: IOException) -> pure Nothing
        else pure Nothing

startGhciLoadProcess
    :: FilePath
    -> Text
    -> [(String, String)]
    -> IO (Handle, ProcessHandle)
startGhciLoadProcess modulePath source envOverrides = do
    packageRoot <- findIhpPackageRoot
    env <- ghciEnvironment envOverrides
    extraPackageArgs <- ghciExtraPackageArgs packageRoot
    Text.writeFile modulePath source

    let commands = ghciDefaultExtensionCommands
            <> [ ":set -fno-code"
               , ":l " <> tshow modulePath
               ]
        -- Standalone cabal-only: ignore any dot-ghci (e.g. the monorepo
        -- @.ghci@) and load the quoter sources straight from this package.
        ghciArgs = ["-ignore-dot-ghci", "-v0", "-i" <> packageRoot] <> extraPackageArgs
        process = (proc "ghci" ghciArgs)
            { cwd = Just packageRoot
            , env = Just env
            , std_in = CreatePipe
            , std_out = Inherit
            , std_err = Inherit
            , close_fds = True
            , create_group = True
            }

    (maybeInputHandle, _, _, processHandle) <- createProcess process
    case maybeInputHandle of
        Nothing -> do
            ignoreProcessException (terminateProcess processHandle)
            fail "TypedSqlSpec: ghci stdin pipe was not created"
        Just inputHandle -> do
            Text.hPutStr inputHandle (Text.unlines commands)
            hFlush inputHandle
            pure (inputHandle, processHandle)

stopGhciProcess :: Handle -> ProcessHandle -> IO ()
stopGhciProcess inputHandle processHandle = do
    ignoreProcessException (hClose inputHandle)
    exited <- timeout 5000000 (waitForProcess processHandle)
    when (isNothing exited) do
        ignoreProcessException (interruptProcessGroupOf processHandle)
        interrupted <- timeout 5000000 (waitForProcess processHandle)
        when (isNothing interrupted) do
            ignoreProcessException (terminateProcess processHandle)
            _ <- timeout 5000000 (waitForProcess processHandle)
            pure ()

ignoreProcessException :: IO a -> IO ()
ignoreProcessException action =
    (action >> pure ()) `Exception.catch` \(_ :: IOException) -> pure ()

waitForCondition :: Int -> IO Bool -> IO Bool
waitForCondition attempts condition
    | attempts <= 0 = pure False
    | otherwise = do
        satisfied <- condition
        if satisfied
            then pure True
            else threadDelay 50000 >> waitForCondition (attempts - 1) condition

processIsAlive :: String -> IO Bool
processIsAlive processId =
    case readMaybe processId :: Maybe Int of
        Just pid | abs pid > 1 ->
            (signalProcess nullSignal (fromIntegral (abs pid)) >> pure True)
                `Exception.catch` \(_ :: IOException) -> pure False
        _ -> pure False

ghciLoadModule :: Text -> IO Text
ghciLoadModule source =
    ghciLoadModuleWithEnv source []

ghciLoadModuleWithEnv :: Text -> [(String, String)] -> IO Text
ghciLoadModuleWithEnv source envOverrides =
    ghciRunWithEnv source [":set -fno-code"] [] envOverrides

ghciRunModule :: Text -> IO Text
ghciRunModule source =
    ghciRunWithEnv source [] ["main"] []

ghciRun :: Text -> [Text] -> [Text] -> IO Text
ghciRun source preLoadCommands postLoadCommands =
    ghciRunWithEnv source preLoadCommands postLoadCommands []

ghciRunWithEnv :: Text -> [Text] -> [Text] -> [(String, String)] -> IO Text
ghciRunWithEnv source preLoadCommands postLoadCommands envOverrides = do
    template <- encodeUtf "typed-sql-ghci"
    withSystemTempDirectory template \tempOsDir -> do
        tempDir <- decodeUtf tempOsDir
        packageRoot <- findIhpPackageRoot
        env <- ghciEnvironment envOverrides
        extraPackageArgs <- ghciExtraPackageArgs packageRoot

        let modulePath = tempDir </> "TypedSqlCase.hs"
        Text.writeFile modulePath source

        let commands =
                ghciDefaultExtensionCommands
                    <> preLoadCommands
                    <> [":l " <> tshow modulePath]
                    <> postLoadCommands
                    <> [":quit"]

        -- Standalone cabal-only: ignore any dot-ghci (e.g. the monorepo
        -- @.ghci@) and load the quoter sources straight from this package.
        let ghciArgs = ["-ignore-dot-ghci", "-v0", "-i" <> packageRoot] <> extraPackageArgs

        let process = (proc "ghci" ghciArgs)
                { cwd = Just packageRoot
                , env = Just env
                }

        (_exitCode, stdOut, stdErr) <- readCreateProcessWithExitCode process (cs (Text.unlines commands))
        pure (cs stdOut <> cs stdErr)

ghciDefaultExtensionCommands :: [Text]
ghciDefaultExtensionCommands =
    map (":set " <>)
        ( [ "-XGHC2021"
          , "-XNoImplicitPrelude"
          , "-XImplicitParams"
          , "-XOverloadedStrings"
          , "-XDisambiguateRecordFields"
          , "-XDuplicateRecordFields"
          , "-XOverloadedLabels"
          , "-XDataKinds"
          , "-XQuasiQuotes"
          , "-XTypeFamilies"
          , "-XPackageImports"
          , "-XRecordWildCards"
          , "-XDefaultSignatures"
          , "-XFunctionalDependencies"
          , "-XPartialTypeSignatures"
          , "-XBlockArguments"
          , "-XLambdaCase"
          , "-XTemplateHaskell"
          , "-XOverloadedRecordDot"
          , "-XDeepSubsumption"
          , "-XExplicitNamespaces"
          ]
          <> backendDefines
        )
  where
    -- The quoter's compile-time backend selection is CPP-driven, and cabal's
    -- cpp-options do not propagate to the ghci subprocesses, so pass the same
    -- define the test suite itself was built with (see @ihp-typed-sql.cabal@).
#ifdef LIBPQ_BACKEND
    backendDefines = ["-DLIBPQ_BACKEND"]
#else
    backendDefines = []
#endif

findIhpPackageRoot :: IO FilePath
findIhpPackageRoot = do
    currentDirectory <- getCurrentDirectory

    let inPackageRoot = currentDirectory </> "IHP" </> "TypedSql" </> "Types.hs"
    inPackageExists <- doesFileExist inPackageRoot
    if inPackageExists
        then pure currentDirectory
        else do
            let fromRepoRoot = currentDirectory </> "ihp-typed-sql" </> "IHP" </> "TypedSql" </> "Types.hs"
            fromRepoExists <- doesFileExist fromRepoRoot
            if fromRepoExists
                then pure (currentDirectory </> "ihp-typed-sql")
                else fail "TypedSqlSpec: could not locate ihp-typed-sql package root"

ghciEnvironment :: [(String, String)] -> IO [(String, String)]
ghciEnvironment envOverrides = do
    baseEnvironment <- getEnvironment

    -- Prefer an existing DATABASE_URL from the environment
    existingDatabaseUrl <- lookupEnv "DATABASE_URL"

    let databaseUrl = case existingDatabaseUrl of
            Just url | not (null url) -> url
            _ ->
                let pgHost = fromMaybe "" (lookup "PGHOST" baseEnvironment)
                    pgDatabase = fromMaybe "" (lookup "PGDATABASE" baseEnvironment)
                    pgUser = fromMaybe "" (lookup "PGUSER" baseEnvironment)
                    pgPort = lookup "PGPORT" baseEnvironment
                    parts =
                        [ "host=" <> pgHost
                        , "dbname=" <> pgDatabase
                        , "user=" <> pgUser
                        ] <> case pgPort of
                            Just port | not (null port) -> ["port=" <> port]
                            _ -> []
                in Prelude.unwords parts

    let defaultOverrides :: [(String, String)]
        defaultOverrides =
            [ ("DATABASE_URL", databaseUrl)
            ]
    let overrideNames = map fst envOverrides
    let overrides = envOverrides <> filter (\(name, _) -> name `notElem` overrideNames) defaultOverrides

    pure (applyEnvironmentOverrides overrides baseEnvironment)

applyEnvironmentOverrides :: [(String, String)] -> [(String, String)] -> [(String, String)]
applyEnvironmentOverrides overrides base =
    overrides <> filter (\(name, _) -> name `notElem` map fst overrides) base

setEnvironmentOverride :: String -> String -> [(String, String)] -> [(String, String)]
setEnvironmentOverride name value environment =
    (name, value) : filter ((/= name) . fst) environment

-- | Extra @ghci@ flags exposing the exact package closure the test suite was
-- built against. This suite is cabal-only (no nix support): every @ghci@
-- subprocess gets its packages from these flags. Without them, every
-- @ghci@-based test fails with @Could not find module ...@ errors for
-- dependencies such as @hasql@, @vector@, or @string-conversions@.
--
-- The shared cabal store holds many versions of the same package, so passing
-- the whole store database would make imports ambiguous. Instead we pin the
-- exact units from the @cabal@ build plan: the inplace @.conf@ files in
-- @dist-newstyle/packagedb@ name the library's direct dependencies, and the
-- store @package.db@ @.conf@ files provide the transitive closure. Boot
-- packages (e.g. @base@) resolve through the global package database.
-- Returns no flags when the build databases cannot be located.
ghciExtraPackageArgs :: FilePath -> IO [String]
ghciExtraPackageArgs packageRoot = do
    info <- ghcInfo
    case info of
        Nothing -> pure []
        Just (ghcVersion, globalDb) -> do
            maybeDistDb <- findDistPackageDb packageRoot ghcVersion
            case maybeDistDb of
                Nothing -> pure []
                Just distDb -> do
                    roots <- inplaceRootDepends distDb
                    maybeStoreDb <- findStorePackageDb ghcVersion roots
                    case maybeStoreDb of
                        Nothing -> pure []
                        Just storeDb -> do
                            closure <- packageClosureUnitIds storeDb globalDb roots
                            -- Inplace units are skipped: their modules
                            -- are loaded from source through @-i@.
                            let exposed = filter (not . List.isSuffixOf "-inplace") closure
                            if null exposed
                                then pure []
                                else
                                    pure
                                        ( ["-package-db", storeDb, "-package-db", distDb, "-hide-all-packages"]
                                            ++ concatMap (\unitId -> ["-package-id", unitId]) exposed
                                        )

-- | The running @ghc@'s numeric version and global package database path.
ghcInfo :: IO (Maybe (String, FilePath))
ghcInfo = do
    maybeGhc <- findExecutable "ghc"
    case maybeGhc of
        Nothing -> pure Nothing
        Just ghc -> do
            versionResult <- runGhcQuery ghc "--numeric-version"
            globalDbResult <- runGhcQuery ghc "--print-global-package-db"
            case (versionResult, globalDbResult) of
                (Just versionLine, Just globalDbLine) ->
                    case listToMaybe (words versionLine) of
                        Just version -> pure (Just (version, takeWhile (/= '\n') globalDbLine))
                        Nothing -> pure Nothing
                _ -> pure Nothing
  where
    runGhcQuery ghc arg =
        (do
            (exitCode, out, _) <- readProcessWithExitCode ghc [arg] ""
            case exitCode of
                ExitSuccess -> pure (Just out)
                _ -> pure Nothing
         ) `Exception.catch` \(_ :: IOException) -> pure Nothing

-- | Locate the @cabal@ build's package database by walking up from the test
-- executable, the current directory, and the package root.
findDistPackageDb :: FilePath -> String -> IO (Maybe FilePath)
findDistPackageDb packageRoot ghcVersion = do
    exePath <- (getExecutablePath `Exception.catch` \(_ :: IOException) -> pure "")
    currentDir <- getCurrentDirectory
    let searchBases =
            ancestorDirsN 12 (takeDirectory exePath)
                ++ ancestorDirsN 12 currentDir
                ++ ancestorDirsN 12 packageRoot
        exactCandidates =
            [ base </> "dist-newstyle" </> "packagedb" </> ("ghc-" ++ ghcVersion)
            | base <- searchBases
            ]
    foundExact <- forM exactCandidates \candidate -> do
        exists <- doesDirectoryExist candidate
        pure (if exists then Just candidate else Nothing)
    case listToMaybe (catMaybes foundExact) of
        Just db -> pure (Just db)
        Nothing -> findPrefixedDistPackageDb searchBases
  where
    -- | Fall back to version-suffixed package db dirs (e.g.
    -- @ghc-9.12.4.20260713@ from a standalone build with a different cabal
    -- version). Prefers a db that actually holds an inplace @.conf@.
    findPrefixedDistPackageDb :: [FilePath] -> IO (Maybe FilePath)
    findPrefixedDistPackageDb bases = do
        let packagedbDirs = [base </> "dist-newstyle" </> "packagedb" | base <- bases]
        nested <- forM packagedbDirs \packagedbDir -> do
            entries <- (listDirectory packagedbDir `Exception.catch` \(_ :: IOException) -> pure [])
            let versioned = filter (List.isPrefixOf ("ghc-" ++ ghcVersion)) entries
            fmap catMaybes (forM versioned \entry -> do
                let db = packagedbDir </> entry
                exists <- doesDirectoryExist db
                if not exists
                    then pure Nothing
                    else do
                        confs <- (listDirectory db `Exception.catch` \(_ :: IOException) -> pure [])
                        let hasInplace = any (List.isSuffixOf "-inplace.conf") confs
                        pure (if hasInplace then Just db else Nothing))
        pure (listToMaybe (concat nested))

-- | Direct dependencies of the locally built inplace packages.
inplaceRootDepends :: FilePath -> IO [String]
inplaceRootDepends distDb = do
    entries <- (listDirectory distDb `Exception.catch` \(_ :: IOException) -> pure [])
    let inplaceConfs = filter (List.isSuffixOf "-inplace.conf") entries
    fmap concat (forM inplaceConfs \conf -> readConfDependsFile (distDb </> conf))

-- | Find the cabal store database holding the build's dependencies. The store
-- directory is matched by compiler version and disambiguated by checking
-- which candidate actually contains a @.conf@ file for a dependency unit.
findStorePackageDb :: String -> [String] -> IO (Maybe FilePath)
findStorePackageDb ghcVersion rootIds = do
    home <- (getHomeDirectory `Exception.catch` \(_ :: IOException) -> pure "")
    cabalDirOverride <- lookupEnv "CABAL_DIR"
    let storeBases = case cabalDirOverride of
            Just dir | not (null dir) -> [dir </> "store"]
            _ ->
                [ home </> ".local" </> "state" </> "cabal" </> "store"
                , home </> ".cabal" </> "store"
                ]
    nested <- forM storeBases \storeBase -> do
        entries <- (listDirectory storeBase `Exception.catch` \(_ :: IOException) -> pure [])
        let compilerDirs = filter (List.isPrefixOf ("ghc-" ++ ghcVersion ++ "-")) entries
        fmap catMaybes (forM compilerDirs \compilerDir -> do
            let db = storeBase </> compilerDir </> "package.db"
            exists <- doesDirectoryExist db
            pure (if exists then Just db else Nothing))
    pickStoreDb (concat nested) rootIds

-- | Pick the first store database containing a @.conf@ file for a root unit.
pickStoreDb :: [FilePath] -> [String] -> IO (Maybe FilePath)
pickStoreDb candidateDbs rootIds = do
    found <- forM candidateDbs \db -> do
        hits <- forM rootIds \unitId ->
            doesFileExist (db </> (unitId ++ ".conf"))
        pure (if or hits then Just db else Nothing)
    pure (listToMaybe (catMaybes found))

-- | Transitive dependency closure over @.conf@ files, including the roots.
-- Units without a @.conf@ file in either database (boot packages from the
-- global database) are kept as leaves.
packageClosureUnitIds :: FilePath -> FilePath -> [String] -> IO [String]
packageClosureUnitIds storeDb globalDb rootIds = go Set.empty rootIds
  where
    go visited [] = pure (Set.toList visited)
    go visited (unitId : queue)
        | unitId `Set.member` visited = go visited queue
        | otherwise = do
            directDeps <-
                if List.isSuffixOf "-inplace" unitId
                    then pure []
                    else readConfDepends storeDb globalDb unitId
            go (Set.insert unitId visited) (queue ++ directDeps)

-- | Dependencies of one unit, preferring the store database and falling back
-- to the global database for boot packages.
readConfDepends :: FilePath -> FilePath -> String -> IO [String]
readConfDepends storeDb globalDb unitId = do
    let storeConf = storeDb </> (unitId ++ ".conf")
    storeExists <- doesFileExist storeConf
    if storeExists
        then readConfDependsFile storeConf
        else readConfDependsFile (globalDb </> (unitId ++ ".conf"))

-- | Parse the @depends:@ field of a @ghc-pkg@ @.conf@ file into unit ids.
readConfDependsFile :: FilePath -> IO [String]
readConfDependsFile path = do
    content <- (Prelude.readFile path `Exception.catch` \(_ :: IOException) -> pure "")
    pure (parseDependsField content)

-- | Parse the @depends:@ field (including leading-whitespace continuations).
parseDependsField :: String -> [String]
parseDependsField content =
    case dropWhile (not . isDependsStart) (lines content) of
        [] -> []
        (first : rest) ->
            concatMap words (dropDependsPrefix first : takeWhile isContinuation rest)
  where
    isDependsStart line = "depends:" `List.isPrefixOf` line
    isContinuation line = " " `List.isPrefixOf` line || "\t" `List.isPrefixOf` line
    dropDependsPrefix line = drop (List.length ("depends:" :: String)) line

-- | A directory followed by its ancestors, bounded by depth.
ancestorDirsN :: Int -> FilePath -> [FilePath]
ancestorDirsN depth dir = take depth (iterate takeDirectory dir)

-- Assertion helpers ----------------------------------------------------------

assertGhciSuccess :: Text -> IO ()
assertGhciSuccess output =
    when (containsCompileError output) do
        expectationFailure ("expected ghci load/run to succeed, but got:\n" <> cs output)

assertGhciFailure :: Text -> [Text] -> IO ()
assertGhciFailure output expectedFragments = do
    when (not (containsCompileError output)) do
        expectationFailure ("expected ghci load to fail, but got:\n" <> cs output)

    forM_ expectedFragments \fragment ->
        when (not (Text.toLower fragment `Text.isInfixOf` Text.toLower output)) do
            expectationFailure
                ( "expected ghci output to contain fragment: "
                    <> cs fragment
                    <> "\nactual output:\n"
                    <> cs output
                )

containsCompileError :: Text -> Bool
containsCompileError output =
    let lower = Text.toLower output
    in " error:" `Text.isInfixOf` lower
        || "\nerror:" `Text.isInfixOf` lower

shouldContainText :: Text -> Text -> Expectation
shouldContainText haystack needle =
    when (not (needle `Text.isInfixOf` haystack)) do
        expectationFailure
            ( "expected text output to contain: "
                <> cs needle
                <> "\nactual output:\n"
                <> cs haystack
            )

-- Module generators ----------------------------------------------------------

-- | Spec helper: compile-pass test with shared boilerplate.
compilePassTest :: Text -> Text -> SpecWith ()
compilePassTest description moduleText =
    it (cs description) do
        requirePostgresTestHook
        withTestPool \pool -> do
            setupSchema pool
            ghciOutput <- ghciLoadModule moduleText
            assertGhciSuccess ghciOutput

-- | Spec helper: compile-fail test with shared boilerplate.
compileFailTest :: Text -> Text -> [Text] -> SpecWith ()
compileFailTest description moduleText expectedFragments =
    it (cs description) do
        requirePostgresTestHook
        withTestPool \pool -> do
            setupSchema pool
            ghciOutput <- ghciLoadModule moduleText
            assertGhciFailure ghciOutput expectedFragments

-- | Spec helper: runtime test with shared boilerplate.
runtimeTest :: Text -> Text -> SpecWith ()
runtimeTest description moduleText =
    it (cs description) do
        requirePostgresTestHook
        withTestPool \pool -> do
            setupSchema pool
            ghciOutput <- ghciRunModule moduleText
            assertGhciSuccess ghciOutput
            ghciOutput `shouldContainText` "RUNTIME_OK"

-- | Imports shared by the standalone quoter test modules below. They only use
-- the @ihp-typed-sql@ library itself (plus base libraries), never @ihp@.
standaloneTestModuleImports :: [Text]
standaloneTestModuleImports =
    [ "import Prelude"
    , "import Control.Monad (when)"
    , "import Data.Int (Int64)"
    , "import Data.String (IsString (..))"
    , "import Data.Text (Text)"
    , "import Data.UUID (UUID)"
    , "import qualified Data.UUID as UUID"
    , "import IHP.TypedSql.Id (Id' (..), PrimaryKey)"
    , "import IHP.TypedSql.Quoter (typedSql, typedSqlStar)"
    , "import IHP.TypedSql.Types (QueryCardinality (..), QueryExecResult (..), TypedQuery (..))"
    , "import IHP.TypedSql.RowType (SqlRow)"
    , ""
    ]

-- | Non-import boilerplate for the standalone quoter test modules: an
-- 'IsString UUID' orphan (which @ihp@ provides via 'IHP.HaskellSupport', but
-- the generated module never loads @ihp@, so there is no duplicate instance)
-- plus a helper to build @Id'@ values without @ihp@'s @IsString (Id' table)@.
standaloneTestModuleHelpers :: [Text]
standaloneTestModuleHelpers =
    [ "instance IsString UUID where"
    , "    fromString string = case UUID.fromString string of"
    , "        Just uuid -> uuid"
    , "        Nothing -> error (\"invalid UUID literal: \" <> string)"
    , ""
    , "uuid :: String -> UUID"
    , "uuid string = case UUID.fromString string of"
    , "    Just parsed -> parsed"
    , "    Nothing -> error (\"invalid UUID literal: \" <> string)"
    , ""
    ]

-- | Build a test module from a type signature and body expression.
-- Used for both compile-pass and compile-fail tests.
mkTestModule :: Text -> Text -> Text
mkTestModule typeSig body = Text.unlines
    ( [ "{-# LANGUAGE DataKinds #-}"
      , "{-# LANGUAGE NoImplicitPrelude #-}"
      , "{-# LANGUAGE NoFieldSelectors #-}"
      , "{-# LANGUAGE OverloadedStrings #-}"
      , "{-# LANGUAGE QuasiQuotes #-}"
      , "module TypedSqlCase where"
      , ""
      ]
      <> standaloneTestModuleImports
      <> standaloneTestModuleHelpers
      <>
      [ "query :: " <> typeSig
      , "query = " <> body
      ]
    )

mkTestModuleWithAeson :: Text -> Text -> Text
mkTestModuleWithAeson typeSig body = Text.unlines
    ( [ "{-# LANGUAGE DataKinds #-}"
      , "{-# LANGUAGE NoImplicitPrelude #-}"
      , "{-# LANGUAGE NoFieldSelectors #-}"
      , "{-# LANGUAGE OverloadedStrings #-}"
      , "{-# LANGUAGE QuasiQuotes #-}"
      , "module TypedSqlCase where"
      , ""
      ]
      <> standaloneTestModuleImports
      <>
      [ "import qualified Data.Aeson as Aeson"
      , ""
      ]
      <> standaloneTestModuleHelpers
      <>
      [ "query :: " <> typeSig
      , "query = " <> body
      ]
    )

-- | Build a test module that also needs PrimaryKey type instances.
mkTestModuleWithPK :: [Text] -> Text -> Text -> Text
mkTestModuleWithPK pkTables typeSig body = Text.unlines $
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE NoFieldSelectors #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "module TypedSqlCase where"
    , ""
    ]
    <> standaloneTestModuleImports
    <> map (\t -> "type instance PrimaryKey \"" <> t <> "\" = UUID") pkTables
    <> standaloneTestModuleHelpers
    <>
    [ ""
    , "query :: " <> typeSig
    , "query = " <> body
    ]

sqlExecTypedSelectCompileFailModule :: Text
sqlExecTypedSelectCompileFailModule = Text.unlines
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module TypedSqlExecCase where"
    , ""
    , "import Prelude"
    , "import qualified Hasql.Session as HasqlSession"
    , "import IHP.TypedSql.Hasql (SqlExecTypedResult, sqlExecTypedSession)"
    , "import IHP.TypedSql.Quoter (typedSql)"
    , "import IHP.TypedSql.Types (QueryExecResult (..))"
    , ""
    , "query :: HasqlSession.Session (SqlExecTypedResult 'ReturnsRows)"
    , "query = sqlExecTypedSession [typedSql| SELECT 1 |]"
    ]

-- Test modules ---------------------------------------------------------------

compilePassModule :: Text
compilePassModule = Text.unlines
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE NoFieldSelectors #-}"
    , "{-# LANGUAGE OverloadedRecordDot #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "{-# LANGUAGE TypeApplications #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "module TypedSqlCompilePass where"
    , ""
    , "import Prelude"
    , "import Data.Int (Int64)"
    , "import Data.String (IsString (..))"
    , "import Data.Text (Text)"
    , "import Data.UUID (UUID)"
    , "import qualified Data.UUID as UUID"
    , "import GHC.Records (HasField)"
    , "import IHP.TypedSql.Id (Id' (..), PrimaryKey)"
    , "import IHP.TypedSql.Quoter (typedSql, typedSqlStar)"
    , "import IHP.TypedSql.Row (TypedSqlRow (..))"
    , "import IHP.TypedSql.Types (QueryCardinality (..), QueryExecResult (..), TypedQuery (..))"
    , "import IHP.TypedSql.RowType (SqlRow)"
    , "import qualified Data.Aeson as Aeson"
    , "import qualified Hasql.Decoders as HasqlDecoders"
    , ""
    , "instance IsString UUID where"
    , "    fromString string = case UUID.fromString string of"
    , "        Just uuid -> uuid"
    , "        Nothing -> error (\"invalid UUID literal: \" <> string)"
    , ""
    , "uuid :: String -> UUID"
    , "uuid string = case UUID.fromString string of"
    , "    Just parsed -> parsed"
    , "    Nothing -> error (\"invalid UUID literal: \" <> string)"
    , ""
    , "type instance PrimaryKey \"typed_sql_test_items\" = UUID"
    , "type instance PrimaryKey \"typed_sql_test_authors\" = UUID"
    , ""
    , "data TypedSqlTestItem = TypedSqlTestItem"
    , "    { typedSqlTestItemId :: Id' \"typed_sql_test_items\""
    , "    , typedSqlTestItemAuthorId :: Maybe (Id' \"typed_sql_test_authors\")"
    , "    , typedSqlTestItemName :: Text"
    , "    , typedSqlTestItemViews :: Int"
    , "    , typedSqlTestItemScore :: Maybe Double"
    , "    , typedSqlTestItemTags :: [Text]"
    , "    } deriving (Eq, Show)"
    , ""
    , "instance TypedSqlRow TypedSqlTestItem where"
    , "    typedSqlRowDecoder ="
    , "        TypedSqlTestItem"
    , "            <$> (fmap Id (HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.uuid)))"
    , "            <*> (fmap (fmap Id) (HasqlDecoders.column (HasqlDecoders.nullable HasqlDecoders.uuid)))"
    , "            <*> HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.text)"
    , "            <*> (fmap fromIntegral (HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.int4)))"
    , "            <*> HasqlDecoders.column (HasqlDecoders.nullable HasqlDecoders.float8)"
    , "            <*> HasqlDecoders.column (HasqlDecoders.nonNullable (HasqlDecoders.listArray (HasqlDecoders.nonNullable HasqlDecoders.text)))"
    , ""
    , "qName :: TypedQuery 'AtMostOneRow 'ReturnsRows Text"
    , "qName = [typedSql| SELECT name FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qAllFields :: TypedQuery 'AtMostOneRow 'ReturnsRows TypedSqlTestItem"
    , "qAllFields = [typedSqlStar| SELECT typed_sql_test_items.* FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qAllFieldsAlias :: TypedQuery 'AtMostOneRow 'ReturnsRows TypedSqlTestItem"
    , "qAllFieldsAlias = [typedSqlStar| SELECT i.* FROM typed_sql_test_items i JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]"
    , ""
    , "qPrimaryKey :: TypedQuery 'AtMostOneRow 'ReturnsRows (Id' \"typed_sql_test_items\")"
    , "qPrimaryKey = [typedSql| SELECT id FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qForeignKey :: TypedQuery 'AtMostOneRow 'ReturnsRows (Maybe (Id' \"typed_sql_test_authors\"))"
    , "qForeignKey = [typedSql| SELECT author_id FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qNullable :: TypedQuery 'AtMostOneRow 'ReturnsRows (Maybe Double)"
    , "qNullable = [typedSql| SELECT score FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qArray :: TypedQuery 'AtMostOneRow 'ReturnsRows [Text]"
    , "qArray = [typedSql| SELECT tags FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qRecord :: TypedQuery 'AtMostOneRow 'ReturnsRows (SqlRow '[ '(\"id\", Id' \"typed_sql_test_items\"), '(\"name\", Text), '(\"views\", Int) ])"
    , "qRecord = [typedSql| SELECT id, name, views FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "-- Verify .field access works on the generated record type"
    , "qRecordAccess :: (HasField \"id\" row (Id' \"typed_sql_test_items\"), HasField \"name\" row Text, HasField \"views\" row Int) => row -> (Id' \"typed_sql_test_items\", Text, Int)"
    , "qRecordAccess row = (row.id, row.name, row.views)"
    , ""
    , "qEqParam :: TypedQuery 'AtMostOneRow 'ReturnsRows Text"
    , "qEqParam = [typedSql| SELECT name FROM typed_sql_test_items WHERE views = ${5 :: Int} LIMIT 1 |]"
    , ""
    , "qForeignKeyParamHint :: TypedQuery 'AtMostOneRow 'ReturnsRows Text"
    , "qForeignKeyParamHint ="
    , "    let authorId = (Id (uuid \"00000000-0000-0000-0000-000000000001\") :: Id' \"typed_sql_test_authors\")"
    , "    in [typedSql| SELECT name FROM typed_sql_test_items WHERE author_id = ${authorId} LIMIT 1 |]"
    , ""
    , "qInParamHint :: TypedQuery 'AtMostOneRow 'ReturnsRows Text"
    , "qInParamHint ="
    , "    let authorIds = [ (Id (uuid \"00000000-0000-0000-0000-000000000001\") :: Id' \"typed_sql_test_authors\") ]"
    , "    in [typedSql| SELECT name FROM typed_sql_test_items WHERE author_id IN (${authorIds}) LIMIT 1 |]"
    , ""
    , "qAnyParamHint :: TypedQuery 'AtMostOneRow 'ReturnsRows Text"
    , "qAnyParamHint ="
    , "    let itemIds ="
    , "            [ (Id (uuid \"10000000-0000-0000-0000-000000000001\") :: Id' \"typed_sql_test_items\")"
    , "            , (Id (uuid \"10000000-0000-0000-0000-000000000002\") :: Id' \"typed_sql_test_items\")"
    , "            ]"
    , "    in [typedSql| SELECT name FROM typed_sql_test_items WHERE id = ANY(${itemIds}) ORDER BY name LIMIT 1 |]"
    , ""
    , "qCompositeExpanded :: TypedQuery 'AtMostOneRow 'ReturnsRows (SqlRow '[ '(\"name\", Maybe Text), '(\"views\", Maybe Int) ])"
    , "qCompositeExpanded = [typedSql| SELECT (ROW(name, views)::typed_sql_test_pair).* FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qBoolExpr :: TypedQuery 'AtMostOneRow 'ReturnsRows (Maybe Bool)"
    , "qBoolExpr = [typedSql| SELECT author_id IS NULL FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qCountExpr :: TypedQuery 'ExactlyOneRow 'ReturnsRows Int64"
    , "qCountExpr = [typedSql| SELECT COUNT(*) FROM typed_sql_test_items |]"
    , ""
    , "qLiteralInt :: TypedQuery 'ExactlyOneRow 'ReturnsRows Int"
    , "qLiteralInt = [typedSql| SELECT 1 |]"
    , ""
    , "qArithmeticExpr :: TypedQuery 'AtMostOneRow 'ReturnsRows (Maybe Int)"
    , "qArithmeticExpr = [typedSql| SELECT views + 1 FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qCaseExpr :: TypedQuery 'AtMostOneRow 'ReturnsRows (Maybe Text)"
    , "qCaseExpr = [typedSql| SELECT CASE WHEN views > 5 THEN name ELSE 'low' END FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qExistsExpr :: TypedQuery 'ExactlyOneRow 'ReturnsRows Bool"
    , "qExistsExpr = [typedSql| SELECT EXISTS(SELECT 1 FROM typed_sql_test_items WHERE views > 7) |]"
    , ""
    , "qNullLiteral :: TypedQuery 'ExactlyOneRow 'ReturnsRows (Maybe Text)"
    , "qNullLiteral = [typedSql| SELECT NULL::text |]"
    , ""
    , "qCte :: TypedQuery 'AtMostOneRow 'ReturnsRows Text"
    , "qCte = [typedSql| WITH item_names AS (SELECT name FROM typed_sql_test_items WHERE views > 6) SELECT name FROM item_names LIMIT 1 |]"
    , ""
    , "qSubquery :: TypedQuery 'AtMostOneRow 'ReturnsRows Text"
    , "qSubquery = [typedSql| SELECT name FROM (SELECT name FROM typed_sql_test_items WHERE views < 6) sub LIMIT 1 |]"
    , ""
    , "qUnion :: TypedQuery 'ManyRows 'ReturnsRows (Maybe Text)"
    , "qUnion = [typedSql| SELECT name FROM typed_sql_test_items WHERE views > 6 UNION ALL SELECT name FROM typed_sql_test_items WHERE views < 6 |]"
    , ""
    , "qWindow :: TypedQuery 'AtMostOneRow 'ReturnsRows Int64"
    , "qWindow = [typedSql| SELECT row_number() OVER (ORDER BY name) FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qGroupedCount :: TypedQuery 'AtMostOneRow 'ReturnsRows (SqlRow '[ '(\"name\", Text), '(\"count\", Int64) ])"
    , "qGroupedCount = [typedSql| SELECT name, COUNT(*) FROM typed_sql_test_items GROUP BY name ORDER BY name LIMIT 1 |]"
    , ""
    , "qArrayLiteral :: TypedQuery 'ExactlyOneRow 'ReturnsRows (Maybe [Text])"
    , "qArrayLiteral = [typedSql| SELECT ARRAY['x','y']::text[] |]"
    , ""
    , "qNullIfExpr :: TypedQuery 'AtMostOneRow 'ReturnsRows (Maybe Text)"
    , "qNullIfExpr = [typedSql| SELECT NULLIF(name, 'First') FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qSchemaQualified :: TypedQuery 'AtMostOneRow 'ReturnsRows Text"
    , "qSchemaQualified = [typedSql| SELECT name FROM public.typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qQuotedIdentifiers :: TypedQuery 'AtMostOneRow 'ReturnsRows Text"
    , "qQuotedIdentifiers = [typedSql| SELECT \"name\" FROM \"typed_sql_test_items\" LIMIT 1 |]"
    , ""
    , "qInnerJoin :: TypedQuery 'AtMostOneRow 'ReturnsRows (SqlRow '[ '(\"name\", Text), '(\"name_1\", Text) ])"
    , "qInnerJoin = [typedSql| SELECT i.name, a.name FROM typed_sql_test_items i INNER JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]"
    , ""
    , "qLeftJoin :: TypedQuery 'AtMostOneRow 'ReturnsRows (SqlRow '[ '(\"name\", Text), '(\"name_1\", Maybe Text) ])"
    , "qLeftJoin = [typedSql| SELECT i.name, a.name FROM typed_sql_test_items i LEFT JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]"
    , ""
    , "qRightJoin :: TypedQuery 'AtMostOneRow 'ReturnsRows (SqlRow '[ '(\"name\", Maybe Text), '(\"name_1\", Text) ])"
    , "qRightJoin = [typedSql| SELECT i.name, a.name FROM typed_sql_test_items i RIGHT JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]"
    , ""
    , "qRightJoinCoalesced :: TypedQuery 'AtMostOneRow 'ReturnsRows (SqlRow '[ '(\"coalesce\", Text), '(\"name\", Text) ])"
    , "qRightJoinCoalesced = [typedSql| SELECT COALESCE(i.name, '(no-item)'), a.name FROM typed_sql_test_items i RIGHT JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]"
    , ""
    , "qJsonBuildObject :: TypedQuery 'ExactlyOneRow 'ReturnsRows Aeson.Value"
    , "qJsonBuildObject = [typedSql| SELECT jsonb_build_object('name', NULL::text) |]"
    , ""
    , "qJsonBuildArray :: TypedQuery 'ExactlyOneRow 'ReturnsRows Aeson.Value"
    , "qJsonBuildArray = [typedSql| SELECT json_build_array(NULL::text) |]"
    ]

runtimeExplicitHasqlModule :: Text
runtimeExplicitHasqlModule = Text.unlines
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE NoFieldSelectors #-}"
    , "{-# LANGUAGE OverloadedRecordDot #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "module Main where"
    , ""
    , "import qualified Control.Exception as Exception"
    , "import Prelude"
    , "import Control.Monad (when)"
    , "import Data.Int (Int64)"
    , "import Data.String (IsString (..))"
    , "import Data.String.Conversions (cs)"
    , "import Data.Text (Text)"
    , "import Data.UUID (UUID)"
    , "import qualified Data.UUID as UUID"
    , "import qualified Hasql.Connection.Settings as HasqlSettings"
    , "import qualified Hasql.Pool as HasqlPool"
    , "import qualified Hasql.Pool.Config as HasqlPoolConfig"
    , "import qualified Hasql.Session as HasqlSession"
    , pqiAdapterImport
    , "import IHP.TypedSql.Id (Id' (..), PrimaryKey)"
    , "import IHP.TypedSql.Quoter (typedSql)"
    , "import IHP.TypedSql.Types (QueryCardinality (..), QueryExecResult (..), TypedQuery (..))"
    , "import IHP.TypedSql.Hasql (sqlExecTypedSession, sqlExecTypedStatement, sqlExecTypedWithPool, sqlQueryTypedSession, sqlQueryTypedStatement, sqlQueryTypedWithPool)"
    , "import System.Environment (lookupEnv)"
    , ""
    , "instance IsString UUID where"
    , "    fromString string = case UUID.fromString string of"
    , "        Just uuid -> uuid"
    , "        Nothing -> error (\"invalid UUID literal: \" <> string)"
    , ""
    , "type instance PrimaryKey \"typed_sql_test_items\" = UUID"
    , "type instance PrimaryKey \"typed_sql_test_authors\" = UUID"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "    databaseUrl <- maybe \"postgresql:///postgres\" cs <$> lookupEnv \"DATABASE_URL\""
    , "    let poolConfig = HasqlPoolConfig.settings"
    , "            [ HasqlPoolConfig.size 2"
    , "            , HasqlPoolConfig.staticConnectionSettings (HasqlSettings.connectionString databaseUrl)"
    , "            ]"
    , "    Exception.bracket (HasqlPool.acquire Pqi.adapter poolConfig) HasqlPool.release \\pool -> do"
    , "        _ <- expectRight =<< sqlExecTypedWithPool pool [typedSql| DELETE FROM typed_sql_test_items |]"
    , ""
    , "        let itemId = (\"10000000-0000-0000-0000-000000000001\" :: UUID)"
    , "        let authorId = (\"00000000-0000-0000-0000-000000000001\" :: UUID)"
    , "        rowsInserted <- expectRight =<< sqlExecTypedWithPool pool [typedSql|"
    , "            INSERT INTO typed_sql_test_items (id, author_id, name, views, score, tags)"
    , "            VALUES (${itemId}, ${authorId}, ${(\"Explicit\" :: Text)}, ${7 :: Int}, ${(Nothing :: Maybe Double)}, ${([] :: [Text])})"
    , "        |]"
    , "        when (rowsInserted /= 1) do error \"unexpected insert count\""
    , ""
    , "        names <- expectRight =<< sqlQueryTypedWithPool pool [typedSql| SELECT name FROM typed_sql_test_items ORDER BY name |]"
    , "        when ((names :: [Text]) /= [\"Explicit\"]) do error \"unexpected explicit-pool query result\""
    , ""
    , "        namesViaSession <- expectRight =<< HasqlPool.use pool (sqlQueryTypedSession [typedSql| SELECT name FROM typed_sql_test_items ORDER BY name |])"
    , "        when ((namesViaSession :: [Text]) /= [\"Explicit\"]) do error \"unexpected session query result\""
    , ""
    , "        rowsUpdated <- expectRight =<< HasqlPool.use pool (sqlExecTypedSession [typedSql| UPDATE typed_sql_test_items SET views = ${8 :: Int} WHERE id = ${itemId} |])"
    , "        when (rowsUpdated /= 1) do error \"unexpected session update count\""
    , ""
    , "        views <- expectRight =<< HasqlPool.use pool (HasqlSession.statement () (sqlQueryTypedStatement [typedSql| SELECT views FROM typed_sql_test_items WHERE id = ${itemId} |]))"
    , "        when ((views :: Maybe Int) /= Just 8) do error \"unexpected statement query result\""
    , ""
    , "        rowsDeleted <- expectRight =<< HasqlPool.use pool (HasqlSession.statement () (sqlExecTypedStatement [typedSql| DELETE FROM typed_sql_test_items WHERE id = ${itemId} |]))"
    , "        when (rowsDeleted /= 1) do error \"unexpected statement delete count\""
    , ""
    , "        putStrLn \"RUNTIME_OK\""
    , ""
    , "expectRight :: Show error => Either error value -> IO value"
    , "expectRight result = case result of"
    , "    Left exception -> error (\"unexpected Hasql error: \" <> show exception)"
    , "    Right value -> pure value"
    ]

-- | Round-trips a generated-style enum through @${...}@ interpolation in a WHERE
-- clause. The local @TypedSqlTestMood@ type mirrors the @DefaultParamEncoder@ instances the
-- schema compiler emits for enums (scalar, @Maybe@, @[enum]@, and @[Maybe enum]@),
-- so binding @${enumVal}@, @${Just enumVal}@, @${Nothing}@, and @${[Just enumVal]}@
-- exercises exactly the generated instances.
runtimeEnumModule :: Text
runtimeEnumModule = Text.unlines
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE NoFieldSelectors #-}"
    , "{-# LANGUAGE OverloadedRecordDot #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module Main where"
    , ""
    , "import qualified Control.Exception as Exception"
    , "import Prelude"
    , "import Control.Monad (when)"
    , "import Data.String.Conversions (cs)"
    , "import Data.Text (Text)"
    , "import qualified Hasql.Connection.Settings as HasqlSettings"
    , "import qualified Hasql.Pool as HasqlPool"
    , "import qualified Hasql.Pool.Config as HasqlPoolConfig"
    , pqiAdapterImport
    , "import IHP.TypedSql.Hasql (sqlQueryTypedWithPool)"
    , "import IHP.TypedSql.Quoter (typedSql)"
    , "import qualified Hasql.Encoders"
    , "import qualified Hasql.Implicits.Encoders"
    , "import System.Environment (lookupEnv)"
    , ""
    , "-- Mirrors the data type + DefaultParamEncoder instances generated by the"
    , "-- schema compiler for `CREATE TYPE typed_sql_test_mood AS ENUM (...)`."
    , "-- The type name must match what the typedSql macro derives from the Postgres"
    , "-- enum type `typed_sql_test_mood` (tableNameToModelName), i.e. TypedSqlTestMood."
    , "data TypedSqlTestMood = Happy | Sad | Neutral deriving (Eq, Show)"
    , ""
    , "moodToText :: TypedSqlTestMood -> Text"
    , "moodToText Happy = \"happy\""
    , "moodToText Sad = \"sad\""
    , "moodToText Neutral = \"neutral\""
    , ""
    , "instance Hasql.Implicits.Encoders.DefaultParamEncoder TypedSqlTestMood where"
    , "    defaultParam = Hasql.Encoders.nonNullable (Hasql.Encoders.enum (Just \"public\") \"typed_sql_test_mood\" moodToText)"
    , "instance Hasql.Implicits.Encoders.DefaultParamEncoder (Maybe TypedSqlTestMood) where"
    , "    defaultParam = Hasql.Encoders.nullable (Hasql.Encoders.enum (Just \"public\") \"typed_sql_test_mood\" moodToText)"
    , "instance Hasql.Implicits.Encoders.DefaultParamEncoder [TypedSqlTestMood] where"
    , "    defaultParam = Hasql.Encoders.nonNullable $ Hasql.Encoders.foldableArray $ Hasql.Encoders.nonNullable (Hasql.Encoders.enum (Just \"public\") \"typed_sql_test_mood\" moodToText)"
    , "instance Hasql.Implicits.Encoders.DefaultParamEncoder [Maybe TypedSqlTestMood] where"
    , "    defaultParam = Hasql.Encoders.nonNullable $ Hasql.Encoders.foldableArray $ Hasql.Encoders.nullable (Hasql.Encoders.enum (Just \"public\") \"typed_sql_test_mood\" moodToText)"
    , ""
    , "assertTest :: Text -> Bool -> IO ()"
    , "assertTest name True  = putStrLn (cs (\"PASS: \" <> name))"
    , "assertTest name False = error (cs (\"FAIL: \" <> name))"
    , ""
    , "expectRight :: Show error => Either error value -> IO value"
    , "expectRight result = case result of"
    , "    Left exception -> error (\"unexpected Hasql error: \" <> show exception)"
    , "    Right value -> pure value"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "    databaseUrl <- maybe \"postgresql:///postgres\" cs <$> lookupEnv \"DATABASE_URL\""
    , "    let poolConfig = HasqlPoolConfig.settings"
    , "            [ HasqlPoolConfig.size 2"
    , "            , HasqlPoolConfig.staticConnectionSettings (HasqlSettings.connectionString databaseUrl)"
    , "            ]"
    , "    Exception.bracket (HasqlPool.acquire Pqi.adapter poolConfig) HasqlPool.release \\pool -> do"
    , "        -- ${enumVal}: a plain enum value binds against the non-null enum column"
    , "        scalarNames <- expectRight =<< sqlQueryTypedWithPool pool [typedSql| SELECT name FROM typed_sql_test_enum_items WHERE mood = ${Happy} ORDER BY name |]"
    , "        assertTest \"dollar-enumVal\" ((scalarNames :: [Text]) == [\"HappyItem\"])"
    , ""
    , "        -- ${Just enumVal}: a Maybe enum value binds against the nullable enum column"
    , "        justNames <- expectRight =<< sqlQueryTypedWithPool pool [typedSql| SELECT name FROM typed_sql_test_enum_items WHERE opt_mood = ${Just Happy} ORDER BY name |]"
    , "        assertTest \"dollar-Just-enumVal\" ((justNames :: [Text]) == [\"HappyItem\"])"
    , ""
    , "        -- ${Nothing}: a Maybe enum Nothing binds as SQL NULL"
    , "        nothingNames <- expectRight =<< sqlQueryTypedWithPool pool [typedSql| SELECT name FROM typed_sql_test_enum_items WHERE opt_mood IS NOT DISTINCT FROM ${Nothing} ORDER BY name |]"
    , "        assertTest \"dollar-Nothing-binds-as-NULL\" ((nothingNames :: [Text]) == [\"SadItem\"])"
    , ""
    , "        -- ${[enumVal]}: a list of enum values binds as an enum array for = ANY(...)"
    , "        listNames <- expectRight =<< sqlQueryTypedWithPool pool [typedSql| SELECT name FROM typed_sql_test_enum_items WHERE mood = ANY(${[Happy]}) ORDER BY name |]"
    , "        assertTest \"dollar-list-enumVal\" ((listNames :: [Text]) == [\"HappyItem\"])"
    , ""
    , "        -- ${[Just enumVal]}: a list of Maybe enum values binds as a nullable-element enum array"
    , "        anyNames <- expectRight =<< sqlQueryTypedWithPool pool [typedSql| SELECT name FROM typed_sql_test_enum_items WHERE mood = ANY(${[Just Happy, Just Sad]}) ORDER BY name |]"
    , "        assertTest \"dollar-list-Just-enumVal\" ((anyNames :: [Text]) == [\"HappyItem\", \"SadItem\"])"
    , ""
    , "        putStrLn \"RUNTIME_OK\""
    ]
