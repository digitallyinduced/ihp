module Test.TypedSqlSpec where

import           Control.Concurrent                 (threadDelay)
import qualified Control.Exception                 as Exception
import qualified Data.Set                          as Set
import qualified Data.Text                         as Text
import qualified Data.Text.IO                      as Text
import           IHP.ModelSupport                  (createModelContext,
                                                    releaseModelContext,
                                                    noopLogger,
                                                    unsafeSqlExecDiscardResult)
import           IHP.Prelude
import           IHP.TypedSql.ParamHints           (parseSql, extractJoinNullableTables,
                                                    extractNonNullableComputedColumnsFromAst,
                                                    detectStarSelects,
                                                    detectInsertWithoutColumns)
import           System.Directory                  (createDirectoryIfMissing,
                                                    doesDirectoryExist,
                                                    doesFileExist,
                                                    findExecutable,
                                                    getCurrentDirectory,
                                                    getModificationTime,
                                                    listDirectory,
                                                    removePathForcibly)
import           System.Environment                (getEnvironment, lookupEnv)
import           System.Exit                       (ExitCode (..))
import           System.FilePath                   (takeDirectory)
import           System.IO                         (Handle, hClose, hFlush)
import           System.IO.Temp.OsPath              (withSystemTempDirectory)
import           System.OsPath                     (encodeUtf, decodeUtf)
import           System.Posix.Signals              (nullSignal, signalProcess)
import           System.Process                    (CreateProcess (..), ProcessHandle,
                                                    StdStream (CreatePipe, NoStream),
                                                    createProcess,
                                                    getProcessExitCode,
                                                    interruptProcessGroupOf, proc,
                                                    readCreateProcessWithExitCode,
                                                    readProcessWithExitCode,
                                                    terminateProcess, waitForProcess)
import           System.Timeout                    (timeout)
import           Test.Hspec
import           Text.Read                         (readMaybe)
import qualified Prelude

tests :: Spec
tests = do
    describe "TypedSql macro compile-time checks" do
        it "compiles valid typedSql queries with inferred types" do
            requirePostgresTestHook
            withTestModelContext do
                setupSchema
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

        it "reuses one private cluster across schema changes in a GHC process" do
            requireAutoDatabaseTools
            withAutoDatabaseFixture
                "CREATE TABLE typed_sql_schema_before (id UUID PRIMARY KEY, name TEXT NOT NULL);\n"
                \tempDir schemaPath stateDir envOverrides -> do
                let modulePath = tempDir </> "ReusableTypedSqlCase.hs"
                    firstModule = mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                        "[typedSql| SELECT name FROM typed_sql_schema_before LIMIT 1 |]"
                    secondModule = mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                        "[typedSql| SELECT name FROM typed_sql_schema_after LIMIT 1 |]"
                (inputHandle, processHandle) <- startGhciLoadProcess modulePath firstModule envOverrides
                let cleanup = stopGhciProcess inputHandle processHandle
                flip Exception.finally cleanup do
                    waitForCondition 1200 ((== 1) . length <$> readyAutoDatabaseProcessRoots stateDir)
                        `shouldReturn` True
                    [processRoot] <- readyAutoDatabaseProcessRoots stateDir
                    let pgVersionPath = processRoot </> "pgdata" </> "PG_VERSION"
                        postmasterPath = processRoot </> "pgdata" </> "postmaster.pid"
                        schemaHashPath = processRoot </> "schema.hash"
                    firstInitializedAt <- getModificationTime pgVersionPath
                    firstHash <- Prelude.readFile schemaHashPath
                    configuration <- Prelude.readFile (processRoot </> "pgdata" </> "postgresql.conf")
                    configuration `shouldContain` "shared_buffers = 4MB"
                    configuration `shouldContain` "max_worker_processes = 0"

                    waitForCondition 300 (not <$> doesFileExist postmasterPath) `shouldReturn` True
                    getProcessExitCode processHandle `shouldReturn` Nothing

                    Text.writeFile schemaPath
                        "CREATE TABLE typed_sql_schema_after (id UUID PRIMARY KEY, name TEXT NOT NULL);\n"
                    Text.writeFile modulePath secondModule
                    Text.hPutStr inputHandle (":load " <> tshow modulePath <> "\n")
                    hFlush inputHandle

                    waitForCondition 1200
                        (do
                            currentHash <- Prelude.readFile schemaHashPath
                                `Exception.catch` \(_ :: IOException) -> pure firstHash
                            pure (currentHash /= firstHash)
                        ) `shouldReturn` True
                    getModificationTime pgVersionPath `shouldReturn` firstInitializedAt
                    readyAutoDatabaseProcessRoots stateDir `shouldReturn` [processRoot]

                    ignoreProcessException (hClose inputHandle)
                    _ <- timeout 5000000 (waitForProcess processHandle)
                    waitForCondition 300 (null <$> autoDatabaseProcessRoots stateDir) `shouldReturn` True

        it "uses isolated clusters for concurrent GHC processes" do
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

        it "removes the private cluster when the compiler dies during schema loading" do
            requireAutoDatabaseTools
            withAutoDatabaseFixture
                ( "CREATE TABLE typed_sql_interrupted (id UUID PRIMARY KEY, name TEXT NOT NULL);\n"
                    <> "SELECT pg_sleep(30);\n"
                )
                \tempDir _schemaPath stateDir envOverrides -> do
                let modulePath = tempDir </> "InterruptedTypedSqlCase.hs"
                    testModule = mkTestModule "TypedQuery 'AtMostOneRow 'ReturnsRows Text"
                        "[typedSql| SELECT name FROM typed_sql_interrupted LIMIT 1 |]"
                (inputHandle, processHandle) <- startGhciLoadProcess modulePath testModule envOverrides
                let cleanup = stopGhciProcess inputHandle processHandle
                flip Exception.finally cleanup do
                    waitForCondition 1200 ((== 1) . length <$> autoDatabaseProcessRoots stateDir)
                        `shouldReturn` True
                    [processRoot] <- autoDatabaseProcessRoots stateDir
                    let postmasterPath = processRoot </> "pgdata" </> "postmaster.pid"
                    waitForCondition 1200 (doesFileExist postmasterPath) `shouldReturn` True
                    postmasterPidContents <- Prelude.readFile postmasterPath
                    postmasterPid <- case listToMaybe (Prelude.lines postmasterPidContents) of
                        Just processId -> pure processId
                        Nothing -> expectationFailure "postmaster.pid did not contain a process id" >> pure ""

                    ignoreProcessException (interruptProcessGroupOf processHandle)
                    ignoreProcessException (hClose inputHandle)
                    exited <- timeout 5000000 (waitForProcess processHandle)
                    when (isNothing exited) do
                        ignoreProcessException (terminateProcess processHandle)
                        _ <- timeout 5000000 (waitForProcess processHandle)
                        pure ()

                    waitForCondition 300 (not <$> processIsAlive postmasterPid) `shouldReturn` True
                    waitForCondition 300 (not <$> doesDirectoryExist processRoot) `shouldReturn` True

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
                "let authorId = (\"00000000-0000-0000-0000-000000000001\" :: Id' \"typed_sql_test_authors\")\n      in [typedSql| SELECT name FROM typed_sql_test_items WHERE author_id = ${authorId} LIMIT 1 |]")

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
        runtimeTest "executes typedSql queries end-to-end via ghci" runtimeModule
        runtimeTest "UPDATE and DELETE with parameters" runtimeUpdateDeleteModule
        runtimeTest "empty results and edge cases" runtimeEdgeCasesModule
        runtimeTest "additional column types (smallint, bigint, numeric, bytea, bool, timestamptz, date, jsonb)" runtimeExtraTypesModule
        runtimeTest "paginatedTypedSql / paginatedTypedSqlWithOptions" runtimePaginationModule
        runtimeTest "enum, Maybe enum, and [Maybe enum] parameters via ${...}" runtimeEnumModule

    describe "TypedSql SQL parser (pure, no postgres)" do
        it "parseSql succeeds on simple SELECT" do
            parseSql "SELECT 1" `shouldSatisfy` isJust

        it "parseSql handles leading/trailing whitespace from quasiquoter" do
            parseSql " SELECT 1 " `shouldSatisfy` isJust

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

withTestModelContext :: ((?modelContext :: ModelContext) => IO a) -> IO a
withTestModelContext action = do
    let logger = noopLogger
    databaseUrl <- cs . fromMaybe "" <$> lookupEnv "DATABASE_URL"
    modelContext <- createModelContext databaseUrl logger
    let ?modelContext = modelContext
    action `Exception.finally` releaseModelContext modelContext

setupSchema :: (?modelContext :: ModelContext) => IO ()
setupSchema = do
    -- Use unsafeSqlExecDiscardResult for DDL (DROP/CREATE) since they have no rows-affected count
    unsafeSqlExecDiscardResult "DROP TABLE IF EXISTS typed_sql_test_enum_items" ()
    unsafeSqlExecDiscardResult "DROP TABLE IF EXISTS typed_sql_test_extras" ()
    unsafeSqlExecDiscardResult "DROP TABLE IF EXISTS typed_sql_test_items" ()
    unsafeSqlExecDiscardResult "DROP TABLE IF EXISTS typed_sql_test_authors" ()
    unsafeSqlExecDiscardResult "DROP TYPE IF EXISTS typed_sql_test_pair" ()
    unsafeSqlExecDiscardResult "DROP TYPE IF EXISTS typed_sql_test_mood" ()

    unsafeSqlExecDiscardResult "CREATE TYPE typed_sql_test_pair AS (name TEXT, views INT)" ()

    unsafeSqlExecDiscardResult
        "CREATE TABLE typed_sql_test_authors (id UUID PRIMARY KEY, name TEXT NOT NULL)"
        ()

    unsafeSqlExecDiscardResult
        "CREATE TABLE typed_sql_test_items (id UUID PRIMARY KEY, author_id UUID REFERENCES typed_sql_test_authors(id), name TEXT NOT NULL, views INT NOT NULL, score DOUBLE PRECISION, tags TEXT[] NOT NULL DEFAULT '{}')"
        ()

    unsafeSqlExecDiscardResult
        "INSERT INTO typed_sql_test_authors (id, name) VALUES ('00000000-0000-0000-0000-000000000001'::uuid, 'Alice')"
        ()

    unsafeSqlExecDiscardResult
        "INSERT INTO typed_sql_test_authors (id, name) VALUES ('00000000-0000-0000-0000-000000000002'::uuid, 'Bob')"
        ()

    unsafeSqlExecDiscardResult
        "INSERT INTO typed_sql_test_items (id, author_id, name, views, score, tags) VALUES ('10000000-0000-0000-0000-000000000001'::uuid, '00000000-0000-0000-0000-000000000001'::uuid, 'First', 5, 1.5, ARRAY['red', 'blue'])"
        ()

    unsafeSqlExecDiscardResult
        "INSERT INTO typed_sql_test_items (id, author_id, name, views, score, tags) VALUES ('10000000-0000-0000-0000-000000000002'::uuid, '00000000-0000-0000-0000-000000000001'::uuid, 'Second', 8, NULL, ARRAY['green'])"
        ()

    unsafeSqlExecDiscardResult
        "CREATE TABLE typed_sql_test_extras (id UUID PRIMARY KEY, small_count SMALLINT NOT NULL DEFAULT 0, big_count BIGINT NOT NULL DEFAULT 0, amount NUMERIC, payload BYTEA, metadata JSONB, created_at TIMESTAMPTZ NOT NULL DEFAULT '2025-06-15 12:00:00+00', due_date DATE, active BOOLEAN NOT NULL DEFAULT TRUE)"
        ()

    unsafeSqlExecDiscardResult
        "INSERT INTO typed_sql_test_extras (id, small_count, big_count, amount, payload, metadata, created_at, due_date, active) VALUES ('20000000-0000-0000-0000-000000000001'::uuid, 7, 1000000000, 99.95, '\\xDEADBEEF', '{\"key\": \"value\"}', '2025-06-15 12:00:00+00', '2025-06-15', true)"
        ()

    -- Enum type + table for exercising DefaultParamEncoder enum interpolation in typedSql
    unsafeSqlExecDiscardResult "CREATE TYPE typed_sql_test_mood AS ENUM ('happy', 'sad', 'neutral')" ()

    unsafeSqlExecDiscardResult
        "CREATE TABLE typed_sql_test_enum_items (id UUID PRIMARY KEY, name TEXT NOT NULL, mood typed_sql_test_mood NOT NULL, opt_mood typed_sql_test_mood)"
        ()

    unsafeSqlExecDiscardResult
        "INSERT INTO typed_sql_test_enum_items (id, name, mood, opt_mood) VALUES ('30000000-0000-0000-0000-000000000001'::uuid, 'HappyItem', 'happy', 'happy')"
        ()

    unsafeSqlExecDiscardResult
        "INSERT INTO typed_sql_test_enum_items (id, name, mood, opt_mood) VALUES ('30000000-0000-0000-0000-000000000002'::uuid, 'SadItem', 'sad', NULL)"
        ()

    pure ()

-- GHCi infrastructure --------------------------------------------------------

requireAutoDatabaseTools :: IO ()
requireAutoDatabaseTools = do
    maybeInitdb <- findExecutable "initdb"
    when (isNothing maybeInitdb) do
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

startGhciLoadProcess
    :: FilePath
    -> Text
    -> [(String, String)]
    -> IO (Handle, ProcessHandle)
startGhciLoadProcess modulePath source envOverrides = do
    packageRoot <- findIhpPackageRoot
    let repoRoot = takeDirectory packageRoot
    useRepoGhci <- doesFileExist (repoRoot </> ".ghci")
    env <- ghciEnvironment envOverrides
    Text.writeFile modulePath source

    let commands = ghciDefaultExtensionCommands
            <> [ ":set -fno-code"
               , ":l " <> tshow modulePath
               ]
        ghciArgs =
            if useRepoGhci
                then ["-v0"]
                else ["-ignore-dot-ghci", "-v0", "-i" <> packageRoot]
        process = (proc "ghci" ghciArgs)
            { cwd = Just (if useRepoGhci then repoRoot else packageRoot)
            , env = Just env
            , std_in = CreatePipe
            , std_out = NoStream
            , std_err = NoStream
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
        Just pid | pid > 1 ->
            (signalProcess nullSignal (fromIntegral pid) >> pure True)
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
        let repoRoot = takeDirectory packageRoot
        useRepoGhci <- doesFileExist (repoRoot </> ".ghci")
        env <- ghciEnvironment envOverrides

        let modulePath = tempDir </> "TypedSqlCase.hs"
        Text.writeFile modulePath source

        let commands =
                ghciDefaultExtensionCommands
                    <> preLoadCommands
                    <> [":l " <> tshow modulePath]
                    <> postLoadCommands
                    <> [":quit"]

        let ghciArgs =
                if useRepoGhci
                    then ["-v0"]
                    else ["-ignore-dot-ghci", "-v0", "-i" <> packageRoot]

        let process = (proc "ghci" ghciArgs)
                { cwd = Just (if useRepoGhci then repoRoot else packageRoot)
                , env = Just env
                }

        (_exitCode, stdOut, stdErr) <- readCreateProcessWithExitCode process (cs (Text.unlines commands))
        pure (cs stdOut <> cs stdErr)

ghciDefaultExtensionCommands :: [Text]
ghciDefaultExtensionCommands =
    map (":set " <>)
        [ "-XGHC2021"
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

findIhpPackageRoot :: IO FilePath
findIhpPackageRoot = do
    currentDirectory <- getCurrentDirectory

    let inPackageRoot = currentDirectory </> "IHP" </> "TypedSql.hs"
    inPackageExists <- doesFileExist inPackageRoot
    if inPackageExists
        then pure currentDirectory
        else do
            let fromRepoRoot = currentDirectory </> "ihp-typed-sql" </> "IHP" </> "TypedSql.hs"
            fromRepoExists <- doesFileExist fromRepoRoot
            if fromRepoExists
                then pure (currentDirectory </> "ihp-typed-sql")
                else fail "TypedSqlSpec: could not locate ihp-typed-sql package root"

ghciEnvironment :: [(String, String)] -> IO [(String, String)]
ghciEnvironment envOverrides = do
    baseEnvironment <- getEnvironment

    -- Prefer an existing DATABASE_URL (e.g. set by withTestPostgres in nix)
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
        withTestModelContext do
            setupSchema
            ghciOutput <- ghciLoadModule moduleText
            assertGhciSuccess ghciOutput

-- | Spec helper: compile-fail test with shared boilerplate.
compileFailTest :: Text -> Text -> [Text] -> SpecWith ()
compileFailTest description moduleText expectedFragments =
    it (cs description) do
        requirePostgresTestHook
        withTestModelContext do
            setupSchema
            ghciOutput <- ghciLoadModule moduleText
            assertGhciFailure ghciOutput expectedFragments

-- | Spec helper: runtime test with shared boilerplate.
runtimeTest :: Text -> Text -> SpecWith ()
runtimeTest description moduleText =
    it (cs description) do
        requirePostgresTestHook
        withTestModelContext do
            setupSchema
            ghciOutput <- ghciRunModule moduleText
            assertGhciSuccess ghciOutput
            ghciOutput `shouldContainText` "RUNTIME_OK"

-- | Build a test module from a type signature and body expression.
-- Used for both compile-pass and compile-fail tests.
mkTestModule :: Text -> Text -> Text
mkTestModule typeSig body = Text.unlines
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE NoFieldSelectors #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module TypedSqlCase where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.TypedSql (QueryCardinality (..), QueryExecResult (..), TypedQuery, typedSql)"
    , "import IHP.TypedSql.RowType (SqlRow)"
    , ""
    , "query :: " <> typeSig
    , "query = " <> body
    ]

mkTestModuleWithAeson :: Text -> Text -> Text
mkTestModuleWithAeson typeSig body = Text.unlines
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE NoFieldSelectors #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module TypedSqlCase where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.TypedSql (QueryCardinality (..), QueryExecResult (..), TypedQuery, typedSql)"
    , "import qualified Data.Aeson as Aeson"
    , ""
    , "query :: " <> typeSig
    , "query = " <> body
    ]

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
    , "import IHP.Prelude"
    , "import IHP.ModelSupport (Id'(..), PrimaryKey)"
    , "import IHP.TypedSql (QueryCardinality (..), QueryExecResult (..), TypedQuery, typedSql)"
    , "import IHP.TypedSql.RowType (SqlRow)"
    , ""
    ]
    <> map (\t -> "type instance PrimaryKey \"" <> t <> "\" = UUID") pkTables
    <>
    [ ""
    , "query :: " <> typeSig
    , "query = " <> body
    ]

sqlExecTypedSelectCompileFailModule :: Text
sqlExecTypedSelectCompileFailModule = Text.unlines
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE ImplicitParams #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module TypedSqlExecCase where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.ModelSupport (ModelContext)"
    , "import IHP.TypedSql (sqlExecTyped, typedSql)"
    , ""
    , "query :: (?modelContext :: ModelContext) => IO Int64"
    , "query = sqlExecTyped [typedSql| SELECT 1 |]"
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
    , "import IHP.Prelude"
    , "import GHC.Records (HasField)"
    , "import IHP.ModelSupport (Id'(..), PrimaryKey)"
    , "import IHP.Hasql.FromRow (FromRowHasql (..))"
    , "import IHP.TypedSql (QueryCardinality (..), QueryExecResult (..), TypedQuery, typedSql, typedSqlStar)"
    , "import IHP.TypedSql.RowType (SqlRow)"
    , "import qualified Data.Aeson as Aeson"
    , "import qualified Hasql.Decoders as HasqlDecoders"
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
    , "instance FromRowHasql TypedSqlTestItem where"
    , "    hasqlRowDecoder ="
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
    , "    let authorId = (\"00000000-0000-0000-0000-000000000001\" :: Id' \"typed_sql_test_authors\")"
    , "    in [typedSql| SELECT name FROM typed_sql_test_items WHERE author_id = ${authorId} LIMIT 1 |]"
    , ""
    , "qInParamHint :: TypedQuery 'AtMostOneRow 'ReturnsRows Text"
    , "qInParamHint ="
    , "    let authorIds = [ (\"00000000-0000-0000-0000-000000000001\" :: Id' \"typed_sql_test_authors\") ]"
    , "    in [typedSql| SELECT name FROM typed_sql_test_items WHERE author_id IN (${authorIds}) LIMIT 1 |]"
    , ""
    , "qAnyParamHint :: TypedQuery 'AtMostOneRow 'ReturnsRows Text"
    , "qAnyParamHint ="
    , "    let itemIds ="
    , "            [ (\"10000000-0000-0000-0000-000000000001\" :: Id' \"typed_sql_test_items\")"
    , "            , (\"10000000-0000-0000-0000-000000000002\" :: Id' \"typed_sql_test_items\")"
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

runtimeModule :: Text
runtimeModule = Text.unlines
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE ApplicativeDo #-}"
    , "{-# LANGUAGE ImplicitParams #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE NoFieldSelectors #-}"
    , "{-# LANGUAGE OverloadedRecordDot #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "module Main where"
    , ""
    , "import qualified Control.Exception as Exception"
    , "import IHP.Prelude"
    , "import IHP.ModelSupport (Id'(..), ModelContext, PrimaryKey, createModelContext, releaseModelContext, noopLogger)"
    , "import IHP.Hasql.FromRow (FromRowHasql (..))"
    , "import IHP.FetchPipelined (pipeline)"
    , "import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, sqlQueryTypedRows, sqlQueryTypedOneOrNothing, sqlQueryTypedSingle, sqlQueryTypedMaybeColumn, sqlQueryTypedPipelined, sqlQueryTypedMaybeColumnPipelined, typedSql, typedSqlStar)"
    , "import qualified Hasql.Decoders as HasqlDecoders"
    , "import System.Environment (lookupEnv)"
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
    , "instance FromRowHasql TypedSqlTestItem where"
    , "    hasqlRowDecoder ="
    , "        TypedSqlTestItem"
    , "            <$> (fmap Id (HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.uuid)))"
    , "            <*> (fmap (fmap Id) (HasqlDecoders.column (HasqlDecoders.nullable HasqlDecoders.uuid)))"
    , "            <*> HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.text)"
    , "            <*> (fmap fromIntegral (HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.int4)))"
    , "            <*> HasqlDecoders.column (HasqlDecoders.nullable HasqlDecoders.float8)"
    , "            <*> HasqlDecoders.column (HasqlDecoders.nonNullable (HasqlDecoders.listArray (HasqlDecoders.nonNullable HasqlDecoders.text)))"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "    let logger = noopLogger"
    , "    databaseUrl <- cs . fromMaybe \"\" <$> lookupEnv \"DATABASE_URL\""
    , "    modelContext <- createModelContext databaseUrl logger"
    , "    let ?modelContext = modelContext"
    , "    flip Exception.finally (releaseModelContext modelContext) do"
    , "        let authorId = (\"00000000-0000-0000-0000-000000000001\" :: UUID)"
    , "        let itemId1 = (\"10000000-0000-0000-0000-000000000001\" :: UUID)"
    , "        let itemId2 = (\"10000000-0000-0000-0000-000000000002\" :: UUID)"
    , ""
    , "        _ <- sqlExecTyped [typedSql| DELETE FROM typed_sql_test_items |]"
    , ""
    , "        _ <- sqlExecTyped [typedSql|"
    , "            INSERT INTO typed_sql_test_items (id, author_id, name, views, score, tags)"
    , "            VALUES (${itemId1}, ${authorId}, ${(\"First\" :: Text)}, ${5 :: Int}, ${(1.5 :: Double)}, ${([\"red\", \"blue\"] :: [Text])})"
    , "        |]"
    , ""
    , "        _ <- sqlExecTyped [typedSql|"
    , "            INSERT INTO typed_sql_test_items (id, author_id, name, views, score, tags)"
    , "            VALUES (${itemId2}, ${authorId}, ${(\"Second\" :: Text)}, ${8 :: Int}, ${(2.0 :: Double)}, ${([\"green\"] :: [Text])})"
    , "        |]"
    , ""
    , "        names <- sqlQueryTyped [typedSql|"
    , "            SELECT name FROM typed_sql_test_items"
    , "            WHERE views > ${3 :: Int}"
    , "            ORDER BY name"
    , "        |]"
    , ""
    , "        when ((names :: [Text]) /= [\"First\", \"Second\"]) do"
    , "            error (\"unexpected names from typedSql: \" <> show names)"
    , ""
    , "        namesViaTypedSql <- sqlQueryTyped [typedSql|"
    , "            SELECT name FROM typed_sql_test_items"
    , "            WHERE views >= ${5 :: Int}"
    , "            ORDER BY name"
    , "        |]"
    , ""
    , "        when ((namesViaTypedSql :: [Text]) /= [\"First\", \"Second\"]) do"
    , "            error (\"unexpected names from typedSql second query: \" <> show namesViaTypedSql)"
    , ""
    , "        namesViaRows <- sqlQueryTypedRows [typedSql|"
    , "            SELECT name FROM typed_sql_test_items"
    , "            ORDER BY name"
    , "        |]"
    , ""
    , "        when ((namesViaRows :: [Text]) /= [\"First\", \"Second\"]) do"
    , "            error (\"unexpected names from sqlQueryTypedRows: \" <> show namesViaRows)"
    , ""
    , "        maybeFirst <- sqlQueryTypedOneOrNothing [typedSql|"
    , "            SELECT name FROM typed_sql_test_items"
    , "            WHERE id = ${itemId1}"
    , "        |]"
    , ""
    , "        when ((maybeFirst :: Maybe Text) /= Just \"First\") do"
    , "            error (\"unexpected row from sqlQueryTypedOneOrNothing: \" <> show maybeFirst)"
    , ""
    , "        countViaSingle <- sqlQueryTypedSingle [typedSql| SELECT COUNT(*) FROM typed_sql_test_items |]"
    , ""
    , "        when ((countViaSingle :: Int64) /= 2) do"
    , "            error (\"unexpected count from sqlQueryTypedSingle: \" <> show countViaSingle)"
    , ""
    , "        maybeScore <- sqlQueryTypedMaybeColumn [typedSql|"
    , "            SELECT score FROM typed_sql_test_items"
    , "            WHERE id = ${itemId1}"
    , "        |]"
    , ""
    , "        when ((maybeScore :: Maybe Double) /= Just 1.5) do"
    , "            error (\"unexpected value from sqlQueryTypedMaybeColumn: \" <> show maybeScore)"
    , ""
    , "        missingScore <- sqlQueryTypedMaybeColumn [typedSql|"
    , "            SELECT score FROM typed_sql_test_items"
    , "            WHERE id = ${(\"10000000-0000-0000-0000-000000000099\" :: UUID)}"
    , "        |]"
    , ""
    , "        when ((missingScore :: Maybe Double) /= Nothing) do"
    , "            error (\"unexpected missing value from sqlQueryTypedMaybeColumn: \" <> show missingScore)"
    , ""
    , "        (pipelinedNames, pipelinedCount, pipelinedMissingScore) <- pipeline do"
    , "            pipelinedNames <- sqlQueryTypedPipelined [typedSql|"
    , "                SELECT name FROM typed_sql_test_items"
    , "                ORDER BY name"
    , "            |]"
    , "            pipelinedCount <- sqlQueryTypedPipelined [typedSql| SELECT COUNT(*) FROM typed_sql_test_items |]"
    , "            pipelinedMissingScore <- sqlQueryTypedMaybeColumnPipelined [typedSql|"
    , "                SELECT score FROM typed_sql_test_items"
    , "                WHERE id = ${(\"10000000-0000-0000-0000-000000000099\" :: UUID)}"
    , "            |]"
    , "            pure (pipelinedNames, pipelinedCount, pipelinedMissingScore)"
    , ""
    , "        when ((pipelinedNames :: [Text]) /= [\"First\", \"Second\"] || (pipelinedCount :: Int64) /= 2 || (pipelinedMissingScore :: Maybe Double) /= Nothing) do"
    , "            error (\"unexpected typedSql pipeline result: \" <> show (pipelinedNames, pipelinedCount, pipelinedMissingScore))"
    , ""
    , "        allItems <- sqlQueryTyped [typedSqlStar|"
    , "            SELECT typed_sql_test_items.*"
    , "            FROM typed_sql_test_items"
    , "            ORDER BY name"
    , "        |]"
    , ""
    , "        let expectedItems ="
    , "                [ TypedSqlTestItem (Id itemId1) (Just (Id authorId)) \"First\" 5 (Just 1.5) [\"red\", \"blue\"]"
    , "                , TypedSqlTestItem (Id itemId2) (Just (Id authorId)) \"Second\" 8 (Just 2.0) [\"green\"]"
    , "                ]"
    , "        when ((allItems :: [TypedSqlTestItem]) /= expectedItems) do"
    , "            error (\"unexpected rows from table.* query: \" <> show allItems)"
    , ""
    , "        boolExprRows <- sqlQueryTyped [typedSql|"
    , "            SELECT author_id IS NULL"
    , "            FROM typed_sql_test_items"
    , "            ORDER BY name"
    , "        |]"
    , ""
    , "        when ((boolExprRows :: [Maybe Bool]) /= [Just False, Just False]) do"
    , "            error (\"unexpected rows from bool expression query: \" <> show boolExprRows)"
    , ""
    , "        count <- sqlQueryTyped [typedSql| SELECT COUNT(*) FROM typed_sql_test_items |]"
    , ""
    , "        when ((count :: Int64) /= 2) do"
    , "            error (\"unexpected count query result: \" <> show count)"
    , ""
    , "        literal <- sqlQueryTyped [typedSql| SELECT 1 |]"
    , ""
    , "        when ((literal :: Int) /= 1) do"
    , "            error (\"unexpected literal query result: \" <> show literal)"
    , ""
    , "        arithmeticRows <- sqlQueryTyped [typedSql|"
    , "            SELECT views + 1 FROM typed_sql_test_items"
    , "            ORDER BY name"
    , "        |]"
    , ""
    , "        when ((arithmeticRows :: [Maybe Int]) /= [Just 6, Just 9]) do"
    , "            error (\"unexpected rows from arithmetic query: \" <> show arithmeticRows)"
    , ""
    , "        caseRows <- sqlQueryTyped [typedSql|"
    , "            SELECT CASE WHEN views > 5 THEN name ELSE 'low' END"
    , "            FROM typed_sql_test_items"
    , "            ORDER BY name"
    , "        |]"
    , ""
    , "        when ((caseRows :: [Maybe Text]) /= [Just \"low\", Just \"Second\"]) do"
    , "            error (\"unexpected rows from CASE query: \" <> show caseRows)"
    , ""
    , "        exists <- sqlQueryTyped [typedSql| SELECT EXISTS(SELECT 1 FROM typed_sql_test_items WHERE views > 7) |]"
    , ""
    , "        when ((exists :: Bool) /= True) do"
    , "            error (\"unexpected EXISTS query result: \" <> show exists)"
    , ""
    , "        nullLiteral <- sqlQueryTyped [typedSql| SELECT NULL::text |]"
    , ""
    , "        when ((nullLiteral :: Maybe Text) /= Nothing) do"
    , "            error (\"unexpected NULL literal query result: \" <> show nullLiteral)"
    , ""
    , "        cteRows <- sqlQueryTyped [typedSql|"
    , "            WITH item_names AS (SELECT name FROM typed_sql_test_items WHERE views > 6)"
    , "            SELECT name FROM item_names ORDER BY name"
    , "        |]"
    , ""
    , "        when ((cteRows :: [Text]) /= [\"Second\"]) do"
    , "            error (\"unexpected rows from CTE query: \" <> show cteRows)"
    , ""
    , "        subqueryRows <- sqlQueryTyped [typedSql|"
    , "            SELECT name FROM (SELECT name FROM typed_sql_test_items WHERE views < 6) sub"
    , "            ORDER BY name"
    , "        |]"
    , ""
    , "        when ((subqueryRows :: [Text]) /= [\"First\"]) do"
    , "            error (\"unexpected rows from subquery: \" <> show subqueryRows)"
    , ""
    , "        unionRows <- sqlQueryTyped [typedSql|"
    , "            SELECT name FROM typed_sql_test_items WHERE views > 6"
    , "            UNION ALL"
    , "            SELECT name FROM typed_sql_test_items WHERE views < 6"
    , "            ORDER BY name"
    , "        |]"
    , ""
    , "        when ((unionRows :: [Maybe Text]) /= [Just \"First\", Just \"Second\"]) do"
    , "            error (\"unexpected rows from UNION: \" <> show unionRows)"
    , ""
    , "        windowRows <- sqlQueryTyped [typedSql|"
    , "            SELECT row_number() OVER (ORDER BY name)"
    , "            FROM typed_sql_test_items"
    , "            ORDER BY name"
    , "        |]"
    , ""
    , "        when ((windowRows :: [Int64]) /= [1, 2]) do"
    , "            error (\"unexpected rows from window function: \" <> show windowRows)"
    , ""
    , "        groupedCountRows <- sqlQueryTyped [typedSql|"
    , "            SELECT name, COUNT(*)"
    , "            FROM typed_sql_test_items"
    , "            GROUP BY name"
    , "            ORDER BY name"
    , "        |]"
    , ""
    , "        let groupedCountValues = map (\\r -> (r.name, r.count)) groupedCountRows"
    , "        when (groupedCountValues /= [(\"First\", 1 :: Int64), (\"Second\", 1)]) do"
    , "            error (\"unexpected rows from grouped count: \" <> show groupedCountRows)"
    , ""
    , "        arrayLiteral <- sqlQueryTyped [typedSql| SELECT ARRAY['x','y']::text[] |]"
    , ""
    , "        when ((arrayLiteral :: Maybe [Text]) /= Just [\"x\", \"y\"]) do"
    , "            error (\"unexpected array literal result: \" <> show arrayLiteral)"
    , ""
    , "        nullIfRows <- sqlQueryTyped [typedSql|"
    , "            SELECT NULLIF(name, 'First')"
    , "            FROM typed_sql_test_items"
    , "            ORDER BY name"
    , "        |]"
    , ""
    , "        when ((nullIfRows :: [Maybe Text]) /= [Nothing, Just \"Second\"]) do"
    , "            error (\"unexpected rows from NULLIF: \" <> show nullIfRows)"
    , ""
    , "        innerJoinRows <- sqlQueryTyped [typedSql|"
    , "            SELECT i.name, a.name"
    , "            FROM typed_sql_test_items i"
    , "            INNER JOIN typed_sql_test_authors a ON a.id = i.author_id"
    , "            ORDER BY i.name"
    , "        |]"
    , ""
    , "        let innerJoinValues = map (\\r -> (r.name, r.name_1)) innerJoinRows"
    , "        when (innerJoinValues /= [(\"First\", \"Alice\"), (\"Second\", \"Alice\")]) do"
    , "            error (\"unexpected rows from inner join: \" <> show innerJoinRows)"
    , ""
    , "        leftJoinRows <- sqlQueryTyped [typedSql|"
    , "            SELECT i.name, a.name"
    , "            FROM typed_sql_test_items i"
    , "            LEFT JOIN typed_sql_test_authors a ON a.id = i.author_id"
    , "            ORDER BY i.name"
    , "        |]"
    , ""
    , "        let leftJoinValues = map (\\r -> (r.name, r.name_1)) leftJoinRows"
    , "        when (leftJoinValues /= [(\"First\", Just \"Alice\"), (\"Second\", Just \"Alice\")]) do"
    , "            error (\"unexpected rows from left join: \" <> show leftJoinRows)"
    , ""
    , "        rightJoinRows <- sqlQueryTyped [typedSql|"
    , "            SELECT i.name, a.name"
    , "            FROM typed_sql_test_items i"
    , "            RIGHT JOIN typed_sql_test_authors a ON a.id = i.author_id"
    , "            WHERE i.id IS NOT NULL"
    , "            ORDER BY a.name, i.name"
    , "        |]"
    , ""
    , "        let rightJoinValues = map (\\r -> (r.name, r.name_1)) rightJoinRows"
    , "        when (rightJoinValues /= [(Just \"First\", \"Alice\"), (Just \"Second\", \"Alice\")]) do"
    , "            error (\"unexpected rows from right join: \" <> show rightJoinRows)"
    , ""
    , "        rightJoinCoalescedRows <- sqlQueryTyped [typedSql|"
    , "            SELECT COALESCE(i.name, '(no-item)'), a.name"
    , "            FROM typed_sql_test_items i"
    , "            RIGHT JOIN typed_sql_test_authors a ON a.id = i.author_id"
    , "            ORDER BY a.name, i.name NULLS LAST"
    , "        |]"
    , ""
    , "        let rightJoinCoalescedValues = map (\\r -> (r.coalesce, r.name)) rightJoinCoalescedRows"
    , "        when (rightJoinCoalescedValues /= [(\"First\", \"Alice\"), (\"Second\", \"Alice\"), (\"(no-item)\", \"Bob\")]) do"
    , "            error (\"unexpected rows from right join with COALESCE: \" <> show rightJoinCoalescedRows)"
    , ""
    , "        putStrLn \"RUNTIME_OK\""
    ]

runtimeUpdateDeleteModule :: Text
runtimeUpdateDeleteModule = Text.unlines
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE ImplicitParams #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE NoFieldSelectors #-}"
    , "{-# LANGUAGE OverloadedRecordDot #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "module Main where"
    , ""
    , "import qualified Control.Exception as Exception"
    , "import IHP.Prelude"
    , "import IHP.ModelSupport (Id'(..), ModelContext, PrimaryKey, createModelContext, releaseModelContext, noopLogger, withTransaction)"
    , "import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)"
    , "import System.Environment (lookupEnv)"
    , ""
    , "type instance PrimaryKey \"typed_sql_test_items\" = UUID"
    , "type instance PrimaryKey \"typed_sql_test_authors\" = UUID"
    , ""
    , "assertTest :: Text -> Bool -> IO ()"
    , "assertTest name True  = putStrLn (\"PASS: \" <> name)"
    , "assertTest name False = error (\"FAIL: \" <> name)"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "    let logger = noopLogger"
    , "    databaseUrl <- cs . fromMaybe \"\" <$> lookupEnv \"DATABASE_URL\""
    , "    modelContext <- createModelContext databaseUrl logger"
    , "    let ?modelContext = modelContext"
    , "    flip Exception.finally (releaseModelContext modelContext) do"
    , "        let itemId1 = (\"10000000-0000-0000-0000-000000000001\" :: UUID)"
    , "        let itemId2 = (\"10000000-0000-0000-0000-000000000002\" :: UUID)"
    , "        let authorId = (\"00000000-0000-0000-0000-000000000001\" :: UUID)"
    , ""
    , "        -- Clean slate"
    , "        _ <- sqlExecTyped [typedSql| DELETE FROM typed_sql_test_items |]"
    , ""
    , "        setConstraintsResult <- withTransaction do"
    , "            sqlExecTyped [typedSql| SET CONSTRAINTS ALL DEFERRED |]"
    , "        assertTest \"SET CONSTRAINTS no-result returns unit\" (setConstraintsResult == ())"
    , ""
    , "        -- Insert two rows"
    , "        _ <- sqlExecTyped [typedSql|"
    , "            INSERT INTO typed_sql_test_items (id, author_id, name, views, score, tags)"
    , "            VALUES (${itemId1}, ${authorId}, ${(\"First\" :: Text)}, ${5 :: Int}, ${(1.5 :: Double)}, ${([\"red\", \"blue\"] :: [Text])})"
    , "        |]"
    , "        _ <- sqlExecTyped [typedSql|"
    , "            INSERT INTO typed_sql_test_items (id, author_id, name, views, score, tags)"
    , "            VALUES (${itemId2}, ${authorId}, ${(\"Second\" :: Text)}, ${8 :: Int}, ${(2.0 :: Double)}, ${([\"green\"] :: [Text])})"
    , "        |]"
    , ""
    , "        -- UPDATE single column"
    , "        rowsUpdated <- sqlExecTyped [typedSql|"
    , "            UPDATE typed_sql_test_items SET views = ${10 :: Int} WHERE id = ${itemId1}"
    , "        |]"
    , "        assertTest \"UPDATE single column rows affected\" (rowsUpdated == 1)"
    , ""
    , "        viewsAfter <- sqlQueryTyped [typedSql| SELECT views FROM typed_sql_test_items WHERE id = ${itemId1} |]"
    , "        assertTest \"UPDATE single column value\" ((viewsAfter :: Maybe Int) == Just 10)"
    , ""
    , "        -- UPDATE multiple columns"
    , "        rowsUpdated2 <- sqlExecTyped [typedSql|"
    , "            UPDATE typed_sql_test_items SET name = ${(\"Updated\" :: Text)}, views = ${99 :: Int} WHERE id = ${itemId2}"
    , "        |]"
    , "        assertTest \"UPDATE multiple columns rows affected\" (rowsUpdated2 == 1)"
    , ""
    , "        updated <- sqlQueryTyped [typedSql| SELECT name, views FROM typed_sql_test_items WHERE id = ${itemId2} |]"
    , "        let updatedValues = fmap (\\r -> (r.name, r.views)) updated"
    , "        assertTest \"UPDATE multiple columns values\" (updatedValues == Just ((\"Updated\" :: Text), 99 :: Int))"
    , ""
    , "        -- UPDATE with no matching rows"
    , "        noMatch <- sqlExecTyped [typedSql|"
    , "            UPDATE typed_sql_test_items SET views = ${0 :: Int} WHERE name = ${(\"NoSuchItem\" :: Text)}"
    , "        |]"
    , "        assertTest \"UPDATE no matching rows\" (noMatch == 0)"
    , ""
    , "        -- DELETE with WHERE"
    , "        rowsDeleted <- sqlExecTyped [typedSql|"
    , "            DELETE FROM typed_sql_test_items WHERE id = ${itemId1}"
    , "        |]"
    , "        assertTest \"DELETE WHERE rows affected\" (rowsDeleted == 1)"
    , ""
    , "        remaining <- sqlQueryTyped [typedSql| SELECT name FROM typed_sql_test_items ORDER BY name |]"
    , "        assertTest \"DELETE WHERE remaining rows\" ((remaining :: [Text]) == [\"Updated\"])"
    , ""
    , "        -- DELETE all remaining"
    , "        rowsDeletedAll <- sqlExecTyped [typedSql| DELETE FROM typed_sql_test_items |]"
    , "        assertTest \"DELETE all rows affected\" (rowsDeletedAll == 1)"
    , ""
    , "        putStrLn \"RUNTIME_OK\""
    ]

runtimeEdgeCasesModule :: Text
runtimeEdgeCasesModule = Text.unlines
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE ImplicitParams #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE NoFieldSelectors #-}"
    , "{-# LANGUAGE OverloadedRecordDot #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "module Main where"
    , ""
    , "import qualified Control.Exception as Exception"
    , "import IHP.Prelude"
    , "import IHP.ModelSupport (Id'(..), ModelContext, PrimaryKey, createModelContext, releaseModelContext, noopLogger)"
    , "import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)"
    , "import System.Environment (lookupEnv)"
    , ""
    , "type instance PrimaryKey \"typed_sql_test_items\" = UUID"
    , "type instance PrimaryKey \"typed_sql_test_authors\" = UUID"
    , ""
    , "assertTest :: Text -> Bool -> IO ()"
    , "assertTest name True  = putStrLn (\"PASS: \" <> name)"
    , "assertTest name False = error (\"FAIL: \" <> name)"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "    let logger = noopLogger"
    , "    databaseUrl <- cs . fromMaybe \"\" <$> lookupEnv \"DATABASE_URL\""
    , "    modelContext <- createModelContext databaseUrl logger"
    , "    let ?modelContext = modelContext"
    , "    flip Exception.finally (releaseModelContext modelContext) do"
    , "        let authorId = (\"00000000-0000-0000-0000-000000000001\" :: UUID)"
    , "        let itemId1 = (\"10000000-0000-0000-0000-000000000001\" :: UUID)"
    , "        let itemId2 = (\"10000000-0000-0000-0000-000000000002\" :: UUID)"
    , ""
    , "        -- Empty result set (delete all items first)"
    , "        _ <- sqlExecTyped [typedSql| DELETE FROM typed_sql_test_items |]"
    , ""
    , "        emptyRows <- sqlQueryTyped [typedSql| SELECT name FROM typed_sql_test_items ORDER BY name |]"
    , "        assertTest \"empty result set\" ((emptyRows :: [Text]) == [])"
    , ""
    , "        -- COUNT on empty table"
    , "        countEmpty <- sqlQueryTyped [typedSql| SELECT COUNT(*) FROM typed_sql_test_items |]"
    , "        assertTest \"COUNT on empty table\" ((countEmpty :: Int64) == 0)"
    , ""
    , "        -- Re-insert rows for further tests"
    , "        _ <- sqlExecTyped [typedSql|"
    , "            INSERT INTO typed_sql_test_items (id, author_id, name, views, score, tags)"
    , "            VALUES (${itemId1}, ${authorId}, ${(\"First\" :: Text)}, ${5 :: Int}, ${(1.5 :: Double)}, ${([\"red\", \"blue\"] :: [Text])})"
    , "        |]"
    , "        _ <- sqlExecTyped [typedSql|"
    , "            INSERT INTO typed_sql_test_items (id, author_id, name, views, score, tags)"
    , "            VALUES (${itemId2}, ${authorId}, ${(\"Second\" :: Text)}, ${8 :: Int}, ${(2.0 :: Double)}, ${([\"green\"] :: [Text])})"
    , "        |]"
    , ""
    , "        -- 5-column record select"
    , "        fiveColRows <- sqlQueryTyped [typedSql|"
    , "            SELECT name, views, score, author_id IS NULL AS is_orphan, tags"
    , "            FROM typed_sql_test_items"
    , "            WHERE id = ${itemId1}"
    , "        |]"
    , "        let fiveColValues = fmap (\\r -> (r.name, r.views, r.score, r.is_orphan, r.tags)) fiveColRows"
    , "        assertTest \"5-column record select\" (fiveColValues == Just (\"First\" :: Text, 5 :: Int, Just (1.5 :: Double), Just False, [\"red\", \"blue\"] :: [Text]))"
    , ""
    , "        -- Multi-param WHERE with AND"
    , "        andRows <- sqlQueryTyped [typedSql|"
    , "            SELECT name FROM typed_sql_test_items"
    , "            WHERE views > ${3 :: Int} AND views < ${7 :: Int}"
    , "            ORDER BY name"
    , "        |]"
    , "        assertTest \"multi-param WHERE AND\" ((andRows :: [Text]) == [\"First\"])"
    , ""
    , "        -- Multi-param WHERE with OR"
    , "        orRows <- sqlQueryTyped [typedSql|"
    , "            SELECT name FROM typed_sql_test_items"
    , "            WHERE name = ${(\"First\" :: Text)} OR name = ${(\"Second\" :: Text)}"
    , "            ORDER BY name"
    , "        |]"
    , "        assertTest \"multi-param WHERE OR\" ((orRows :: [Text]) == [\"First\", \"Second\"])"
    , ""
    , "        putStrLn \"RUNTIME_OK\""
    ]

runtimeExtraTypesModule :: Text
runtimeExtraTypesModule = Text.unlines
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE ImplicitParams #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE NoFieldSelectors #-}"
    , "{-# LANGUAGE OverloadedRecordDot #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "module Main where"
    , ""
    , "import qualified Control.Exception as Exception"
    , "import IHP.Prelude"
    , "import IHP.ModelSupport (ModelContext, PrimaryKey, createModelContext, releaseModelContext, noopLogger)"
    , "import IHP.TypedSql (sqlQueryTyped, typedSql)"
    , "import Data.Time (UTCTime, Day, parseTimeM, defaultTimeLocale)"
    , "import Data.Scientific (Scientific)"
    , "import qualified Data.Aeson as Aeson"
    , "import qualified Data.ByteString as BS"
    , "import System.Environment (lookupEnv)"
    , ""
    , "type instance PrimaryKey \"typed_sql_test_extras\" = UUID"
    , ""
    , "assertTest :: Text -> Bool -> IO ()"
    , "assertTest name True  = putStrLn (\"PASS: \" <> name)"
    , "assertTest name False = error (\"FAIL: \" <> name)"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "    let logger = noopLogger"
    , "    databaseUrl <- cs . fromMaybe \"\" <$> lookupEnv \"DATABASE_URL\""
    , "    modelContext <- createModelContext databaseUrl logger"
    , "    let ?modelContext = modelContext"
    , "    flip Exception.finally (releaseModelContext modelContext) do"
    , ""
    , "        -- smallint -> Int"
    , "        smallRows <- sqlQueryTyped [typedSql| SELECT small_count FROM typed_sql_test_extras LIMIT 1 |]"
    , "        assertTest \"smallint -> Int\" ((smallRows :: Maybe Int) == Just 7)"
    , ""
    , "        -- bigint -> Int64"
    , "        bigRows <- sqlQueryTyped [typedSql| SELECT big_count FROM typed_sql_test_extras LIMIT 1 |]"
    , "        assertTest \"bigint -> Int64\" ((bigRows :: Maybe Int64) == Just 1000000000)"
    , ""
    , "        -- numeric -> Scientific"
    , "        numericRows <- sqlQueryTyped [typedSql| SELECT amount FROM typed_sql_test_extras LIMIT 1 |]"
    , "        assertTest \"numeric -> Scientific\" ((numericRows :: Maybe (Maybe Scientific)) == Just (Just 99.95))"
    , ""
    , "        -- bytea -> ByteString"
    , "        byteaRows <- sqlQueryTyped [typedSql| SELECT payload FROM typed_sql_test_extras LIMIT 1 |]"
    , "        assertTest \"bytea -> ByteString\" ((byteaRows :: Maybe (Maybe BS.ByteString)) == Just (Just (BS.pack [0xDE, 0xAD, 0xBE, 0xEF])))"
    , ""
    , "        -- bool -> Bool"
    , "        boolRows <- sqlQueryTyped [typedSql| SELECT active FROM typed_sql_test_extras LIMIT 1 |]"
    , "        assertTest \"bool -> Bool\" ((boolRows :: Maybe Bool) == Just True)"
    , ""
    , "        -- timestamptz -> UTCTime"
    , "        tsRows <- sqlQueryTyped [typedSql| SELECT created_at FROM typed_sql_test_extras LIMIT 1 |]"
    , "        let Just expectedTime = parseTimeM True defaultTimeLocale \"%Y-%m-%d %H:%M:%S%Z\" \"2025-06-15 12:00:00UTC\" :: Maybe UTCTime"
    , "        assertTest \"timestamptz -> UTCTime\" ((tsRows :: Maybe UTCTime) == Just expectedTime)"
    , ""
    , "        -- date -> Day"
    , "        dateRows <- sqlQueryTyped [typedSql| SELECT due_date FROM typed_sql_test_extras LIMIT 1 |]"
    , "        let Just expectedDate = parseTimeM True defaultTimeLocale \"%Y-%m-%d\" \"2025-06-15\" :: Maybe Day"
    , "        assertTest \"date -> Day\" ((dateRows :: Maybe (Maybe Day)) == Just (Just expectedDate))"
    , ""
    , "        -- jsonb -> Aeson.Value"
    , "        jsonRows <- sqlQueryTyped [typedSql| SELECT metadata FROM typed_sql_test_extras LIMIT 1 |]"
    , "        let expectedJson = Aeson.object [(\"key\", Aeson.String \"value\")]"
    , "        assertTest \"jsonb -> Aeson.Value\" ((jsonRows :: Maybe (Maybe Aeson.Value)) == Just (Just expectedJson))"
    , ""
    , "        -- multi-type record"
    , "        multiTypeRows <- sqlQueryTyped [typedSql|"
    , "            SELECT small_count, big_count, active"
    , "            FROM typed_sql_test_extras LIMIT 1"
    , "        |]"
    , "        let multiTypeValues = fmap (\\r -> (r.small_count, r.big_count, r.active)) multiTypeRows"
    , "        assertTest \"multi-type record\" (multiTypeValues == Just (7 :: Int, 1000000000 :: Int64, True))"
    , ""
    , "        putStrLn \"RUNTIME_OK\""
    ]

-- | End-to-end test for 'paginatedTypedSql' \/ 'paginatedTypedSqlWithOptions'.
-- Mirrors the raw-SQL @paginatedSqlQueryWithOptions@ spec
-- (@ihp\/Test\/Test\/Pagination\/ControllerFunctionsSpec.hs@): seed 100 rows,
-- then assert the page slice and 'Pagination' fields for a couple of pages and a
-- @maxItems@ override.
runtimePaginationModule :: Text
runtimePaginationModule = Text.unlines
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE ImplicitParams #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE NoFieldSelectors #-}"
    , "{-# LANGUAGE OverloadedRecordDot #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "module Main where"
    , ""
    , "import qualified Control.Exception as Exception"
    , "import IHP.Prelude"
    , "import IHP.ModelSupport (ModelContext, createModelContext, releaseModelContext, noopLogger, unsafeSqlExecDiscardResult)"
    , "import IHP.TypedSql (QueryCardinality (..), QueryExecResult (..), TypedQuery, typedSql)"
    , "import IHP.TypedSql.Pagination (paginatedTypedSql, paginatedTypedSqlWithOptions)"
    , "import IHP.Pagination.ControllerFunctions (defaultPaginationOptions)"
    , "import IHP.Pagination.Types (Options (..), Pagination (..))"
    , "import System.Environment (lookupEnv)"
    , "import qualified Network.Wai as Wai"
    , "import qualified Data.Vault.Lazy as Vault"
    , "import Wai.Request.Params.Middleware (RequestBody (..), requestBodyVaultKey)"
    , ""
    , "assertTest :: Text -> Bool -> IO ()"
    , "assertTest name True  = putStrLn (\"PASS: \" <> name)"
    , "assertTest name False = error (\"FAIL: \" <> name)"
    , ""
    , "firstOf :: [a] -> Maybe a"
    , "firstOf (x : _) = Just x"
    , "firstOf []      = Nothing"
    , ""
    , "-- Build a Request carrying the given query params (page / maxItems)."
    , "contextWithParams :: [(ByteString, ByteString)] -> Wai.Request"
    , "contextWithParams params ="
    , "    let requestBody = FormBody { params, files = [], rawPayload = \"\" }"
    , "    in Wai.defaultRequest { Wai.vault = Vault.insert requestBodyVaultKey requestBody Vault.empty }"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "    let logger = noopLogger"
    , "    databaseUrl <- cs . fromMaybe \"\" <$> lookupEnv \"DATABASE_URL\""
    , "    modelContext <- createModelContext databaseUrl logger"
    , "    let ?modelContext = modelContext"
    , "    flip Exception.finally (releaseModelContext modelContext) do"
    , "        -- Seed 100 rows with zero-padded, sortable names (item-001 .. item-100)."
    , "        unsafeSqlExecDiscardResult \"DELETE FROM typed_sql_test_items\" ()"
    , "        unsafeSqlExecDiscardResult"
    , "            \"INSERT INTO typed_sql_test_items (id, author_id, name, views, score, tags) SELECT ('10000000-0000-0000-0000-' || lpad(g::text, 12, '0'))::uuid, '00000000-0000-0000-0000-000000000001'::uuid, 'item-' || lpad(g::text, 3, '0'), g, NULL, '{}'::text[] FROM generate_series(1, 100) g\""
    , "            ()"
    , ""
    , "        let pageQuery = [typedSql| SELECT name FROM typed_sql_test_items ORDER BY name |] :: TypedQuery 'ManyRows 'ReturnsRows Text"
    , ""
    , "        -- First page, default options (maxItems 50)."
    , "        do"
    , "            let ?request = contextWithParams []"
    , "            (results, pagination) <- paginatedTypedSql pageQuery"
    , "            assertTest \"default page 1 length\" (length results == 50)"
    , "            assertTest \"default page 1 first item\" (firstOf results == Just \"item-001\")"
    , "            assertTest \"default page 1 currentPage\" (pagination.currentPage == 1)"
    , "            assertTest \"default page 1 totalItems\" (pagination.totalItems == 100)"
    , "            assertTest \"default page 1 pageSize\" (pagination.pageSize == 50)"
    , ""
    , "        -- Second page should contain items 51-100."
    , "        do"
    , "            let ?request = contextWithParams [(\"page\", \"2\")]"
    , "            (results, pagination) <- paginatedTypedSql pageQuery"
    , "            assertTest \"default page 2 length\" (length results == 50)"
    , "            assertTest \"default page 2 first item\" (firstOf results == Just \"item-051\")"
    , "            assertTest \"default page 2 currentPage\" (pagination.currentPage == 2)"
    , ""
    , "        -- maxItems request param overrides the page size."
    , "        do"
    , "            let ?request = contextWithParams [(\"maxItems\", \"10\")]"
    , "            (results, pagination) <- paginatedTypedSqlWithOptions defaultPaginationOptions pageQuery"
    , "            assertTest \"maxItems=10 length\" (length results == 10)"
    , "            assertTest \"maxItems=10 pageSize\" (pagination.pageSize == 10)"
    , "            assertTest \"maxItems=10 totalItems\" (pagination.totalItems == 100)"
    , ""
    , "        -- page + maxItems together: page 3 with pageSize 10 starts at item-021."
    , "        do"
    , "            let ?request = contextWithParams [(\"page\", \"3\"), (\"maxItems\", \"10\")]"
    , "            (results, pagination) <- paginatedTypedSql pageQuery"
    , "            assertTest \"page 3 + maxItems 10 length\" (length results == 10)"
    , "            assertTest \"page 3 + maxItems 10 first item\" (firstOf results == Just \"item-021\")"
    , "            assertTest \"page 3 + maxItems 10 currentPage\" (pagination.currentPage == 3)"
    , "            assertTest \"page 3 + maxItems 10 pageSize\" (pagination.pageSize == 10)"
    , ""
    , "        -- Custom Options maxItems / windowSize are respected."
    , "        do"
    , "            let ?request = contextWithParams []"
    , "            let options = Options { maxItems = 25, windowSize = 3 }"
    , "            (results, pagination) <- paginatedTypedSqlWithOptions options pageQuery"
    , "            assertTest \"custom maxItems=25 length\" (length results == 25)"
    , "            assertTest \"custom maxItems=25 pageSize\" (pagination.pageSize == 25)"
    , "            assertTest \"custom maxItems=25 window\" (pagination.window == 3)"
    , ""
    , "        putStrLn \"RUNTIME_OK\""
    ]
-- | Round-trips a generated-style enum through @${...}@ interpolation in a WHERE
-- clause. The local @TypedSqlTestMood@ type mirrors the @DefaultParamEncoder@ instances the
-- schema compiler emits for enums (scalar, @Maybe@, @[enum]@, and @[Maybe enum]@),
-- so binding @${enumVal}@, @${Just enumVal}@, @${Nothing}@, and @${[Just enumVal]}@
-- exercises exactly the generated instances.
runtimeEnumModule :: Text
runtimeEnumModule = Text.unlines
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE ImplicitParams #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE NoFieldSelectors #-}"
    , "{-# LANGUAGE OverloadedRecordDot #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "module Main where"
    , ""
    , "import qualified Control.Exception as Exception"
    , "import IHP.Prelude"
    , "import IHP.ModelSupport (ModelContext, PrimaryKey, createModelContext, releaseModelContext, noopLogger)"
    , "import IHP.TypedSql (sqlQueryTyped, typedSql)"
    , "import qualified Hasql.Encoders"
    , "import qualified Hasql.Implicits.Encoders"
    , "import System.Environment (lookupEnv)"
    , ""
    , "type instance PrimaryKey \"typed_sql_test_enum_items\" = UUID"
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
    , "assertTest name True  = putStrLn (\"PASS: \" <> name)"
    , "assertTest name False = error (\"FAIL: \" <> name)"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "    let logger = noopLogger"
    , "    databaseUrl <- cs . fromMaybe \"\" <$> lookupEnv \"DATABASE_URL\""
    , "    modelContext <- createModelContext databaseUrl logger"
    , "    let ?modelContext = modelContext"
    , "    flip Exception.finally (releaseModelContext modelContext) do"
    , "        -- ${enumVal}: a plain enum value binds against the non-null enum column"
    , "        scalarNames <- sqlQueryTyped [typedSql| SELECT name FROM typed_sql_test_enum_items WHERE mood = ${Happy} ORDER BY name |]"
    , "        assertTest \"dollar-enumVal\" ((scalarNames :: [Text]) == [\"HappyItem\"])"
    , ""
    , "        -- ${Just enumVal}: a Maybe enum value binds against the nullable enum column"
    , "        justNames <- sqlQueryTyped [typedSql| SELECT name FROM typed_sql_test_enum_items WHERE opt_mood = ${Just Happy} ORDER BY name |]"
    , "        assertTest \"dollar-Just-enumVal\" ((justNames :: [Text]) == [\"HappyItem\"])"
    , ""
    , "        -- ${Nothing}: a Maybe enum Nothing binds as SQL NULL"
    , "        nothingNames <- sqlQueryTyped [typedSql| SELECT name FROM typed_sql_test_enum_items WHERE opt_mood IS NOT DISTINCT FROM ${Nothing} ORDER BY name |]"
    , "        assertTest \"dollar-Nothing-binds-as-NULL\" ((nothingNames :: [Text]) == [\"SadItem\"])"
    , ""
    , "        -- ${[enumVal]}: a list of enum values binds as an enum array for = ANY(...)"
    , "        listNames <- sqlQueryTyped [typedSql| SELECT name FROM typed_sql_test_enum_items WHERE mood = ANY(${[Happy]}) ORDER BY name |]"
    , "        assertTest \"dollar-list-enumVal\" ((listNames :: [Text]) == [\"HappyItem\"])"
    , ""
    , "        -- ${[Just enumVal]}: a list of Maybe enum values binds as a nullable-element enum array"
    , "        anyNames <- sqlQueryTyped [typedSql| SELECT name FROM typed_sql_test_enum_items WHERE mood = ANY(${[Just Happy, Just Sad]}) ORDER BY name |]"
    , "        assertTest \"dollar-list-Just-enumVal\" ((anyNames :: [Text]) == [\"HappyItem\", \"SadItem\"])"
    , ""
    , "        putStrLn \"RUNTIME_OK\""
    ]
