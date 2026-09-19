module Test.TypedSqlSpec where

import Test.Hspec
import IHP.Prelude
import qualified Prelude
import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import IHP.ModelSupport (createModelContext, releaseModelContext, noopLogger, unsafeSqlExecDiscardResult)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, getTemporaryDirectory, removeDirectoryRecursive)
import System.Environment (getEnvironment, lookupEnv)
import System.Posix.Process (getProcessID)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

-- | Request-scoped @IHP.TypedSql@ runner tests (@sqlQueryTyped@,
-- @sqlExecTyped@, pipelines, pagination).
--
-- These live here in the @ihp@ package rather than next to the quoter in
-- @ihp-typed-sql@, because they execute through @IHP.TypedSql@ and the
-- request-scoped 'ModelContext'. Keeping them here is what lets
-- @ihp-typed-sql@ (library and test suite) stay free of any dependency on
-- @ihp@, so the dependency points one way: @ihp@ depends on
-- @ihp-typed-sql@, never vice versa.
tests :: Spec
tests = do
    describe "TypedSql IHP runners" do
        runtimeTest "executes typedSql queries end-to-end via ghci" runtimeModule
        runtimeTest "UPDATE and DELETE with parameters" runtimeUpdateDeleteModule
        runtimeTest "empty results and edge cases" runtimeEdgeCasesModule
        runtimeTest "additional column types (smallint, bigint, numeric, bytea, bool, timestamptz, date, jsonb)" runtimeExtraTypesModule
        runtimeTest "paginatedTypedSql / paginatedTypedSqlWithOptions" runtimePaginationModule

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

-- Assertion helpers ----------------------------------------------------------

assertGhciSuccess :: Text -> IO ()
assertGhciSuccess output =
    when (containsCompileError output) do
        expectationFailure ("expected ghci load/run to succeed, but got:\n" <> cs output)

shouldContainText :: Text -> Text -> Expectation
shouldContainText haystack needle =
    when (not (needle `Text.isInfixOf` haystack)) do
        expectationFailure
            ( "expected text output to contain: "
                <> cs needle
                <> "\nactual output:\n"
                <> cs haystack
            )

containsCompileError :: Text -> Bool
containsCompileError output =
    let lower = Text.toLower output
    in " error:" `Text.isInfixOf` lower
        || "\nerror:" `Text.isInfixOf` lower

-- Minimal GHCi runner (no temporary-ospath dependency) -----------------------

withTempDirectory :: String -> (FilePath -> IO a) -> IO a
withTempDirectory template action = do
    systemTemp <- getTemporaryDirectory
    pid <- getProcessID
    let dir = systemTemp </> (template <> "-" <> Prelude.show pid)
    createDirectoryIfMissing True dir
    action dir `Exception.finally` removeDirectoryRecursive dir

ghciRunModule :: Text -> IO Text
ghciRunModule source =
    ghciRunWithEnv source [] ["main"] []

ghciRunWithEnv :: Text -> [Text] -> [Text] -> [(String, String)] -> IO Text
ghciRunWithEnv source preLoadCommands postLoadCommands envOverrides =
    withTempDirectory "typed-sql-ghci" \tempDir -> do
        ihpDir <- findIhpSourceDir
        env <- ghciEnvironment envOverrides

        let modulePath = tempDir </> "TypedSqlRunnerCase.hs"
        TextIO.writeFile modulePath source

        let commands =
                ghciDefaultExtensionCommands
                    <> preLoadCommands
                    <> [":l " <> tshow modulePath]
                    <> postLoadCommands
                    <> [":quit"]

        let process = (proc "ghci" ["-v0", "-ignore-dot-ghci", "-i" <> ihpDir])
                { cwd = Just ihpDir
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

findIhpSourceDir :: IO FilePath
findIhpSourceDir = do
    currentDirectory <- getCurrentDirectory
    let marker = currentDirectory </> "IHP" </> "Prelude.hs"
    exists <- doesFileExist marker
    if exists
        then pure currentDirectory
        else fail "Test.TypedSqlSpec: could not locate ihp package root"

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

-- Test modules (moved verbatim from ihp-typed-sql) ---------------------------


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
    , "import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, sqlQueryTypedRows, sqlQueryTypedOneOrNothing, sqlQueryTypedSingle, sqlQueryTypedMaybeColumn, sqlQueryTypedPipelined, sqlQueryTypedMaybeColumnPipelined, typedSql, typedSqlStar, TypedSqlRow (..))"
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
