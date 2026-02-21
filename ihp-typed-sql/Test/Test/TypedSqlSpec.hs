module Test.TypedSqlSpec where

import qualified Control.Exception                 as Exception
import qualified Data.Char                         as Char
import qualified Data.Set                          as Set
import qualified Data.Text                         as Text
import qualified Data.Text.IO                      as Text
import           IHP.Log.Types
import           IHP.ModelSupport                  (createModelContext,
                                                    releaseModelContext,
                                                    sqlExecDiscardResult)
import           IHP.Prelude
import           IHP.TypedSql.ParamHints           (parseSql, extractJoinNullableTables)
import           System.Directory                  (doesFileExist,
                                                    getCurrentDirectory)
import           System.Environment                (getEnvironment, lookupEnv)
import           System.FilePath                   (takeDirectory)
import           System.Process                    (CreateProcess (..), proc,
                                                    readCreateProcessWithExitCode)
import           System.IO.Temp                    (withSystemTempDirectory)
import           Test.Hspec
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
            (mkTestModule "TypedQuery Text"
                "[typedSql| SELECT name FROM typed_sql_test_items WHERE views = ${(\"not an int\" :: Text)} LIMIT 1 |]")
            []

        compileFailTest "fails when a foreign-key parameter has the wrong type"
            (mkTestModuleWithPK ["typed_sql_test_items", "typed_sql_test_authors"] "TypedQuery Text"
                "[typedSql| SELECT name FROM typed_sql_test_items WHERE author_id = ${(\"not-an-id\" :: Text)} LIMIT 1 |]")
            []

        compileFailTest "fails when an IN parameter has the wrong element type"
            (mkTestModuleWithPK ["typed_sql_test_items", "typed_sql_test_authors"] "TypedQuery Text"
                "let authorIds = [\"one\" :: Text, \"two\" :: Text]\n      in [typedSql| SELECT name FROM typed_sql_test_items WHERE author_id IN (${authorIds}) LIMIT 1 |]")
            []

        compileFailTest "fails when a placeholder expression is invalid Haskell"
            (mkTestModule "TypedQuery Text"
                "[typedSql| SELECT name FROM typed_sql_test_items WHERE views = ${(} LIMIT 1 |]")
            ["failed to parse expression"]

        compileFailTest "fails when SQL parameter count does not match ${...} placeholders"
            (mkTestModule "TypedQuery Text"
                "[typedSql| SELECT name FROM typed_sql_test_items WHERE views = $1 LIMIT 1 |]")
            ["placeholder count mismatch"]

        compileFailTest "fails when selecting a single composite value without expansion"
            (mkTestModule "TypedQuery Text"
                "[typedSql| SELECT ROW(name, views)::typed_sql_test_pair FROM typed_sql_test_items LIMIT 1 |]")
            ["composite columns must be expanded"]

        compileFailTest "fails when SQL references an unknown column"
            (mkTestModule "TypedQuery Text"
                "[typedSql| SELECT no_such_column FROM typed_sql_test_items LIMIT 1 |]")
            ["does not exist"]

        compileFailTest "fails when primary-key result type is annotated as UUID instead of Id"
            (mkTestModuleWithPK ["typed_sql_test_items"] "TypedQuery UUID"
                "[typedSql| SELECT id FROM typed_sql_test_items LIMIT 1 |]")
            []

        compileFailTest "fails when nullable column result is annotated as non-Maybe"
            (mkTestModule "TypedQuery Double"
                "[typedSql| SELECT score FROM typed_sql_test_items LIMIT 1 |]")
            []

        compileFailTest "fails when LEFT JOIN nullable side is not annotated as Maybe"
            (mkTestModule "TypedQuery (Text, Text)"
                "[typedSql| SELECT i.name, a.name FROM typed_sql_test_items i LEFT JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]")
            []

        compileFailTest "fails when RIGHT JOIN nullable side is not annotated as Maybe"
            (mkTestModule "TypedQuery (Text, Text)"
                "[typedSql| SELECT i.name, a.name FROM typed_sql_test_items i RIGHT JOIN typed_sql_test_authors a ON a.id = i.author_id ORDER BY a.name LIMIT 1 |]")
            []

        compileFailTest "fails when tuple arity does not match selected columns"
            (mkTestModule "TypedQuery Text"
                "[typedSql| SELECT name, views FROM typed_sql_test_items LIMIT 1 |]")
            []

        compileFailTest "fails when boolean expression result is annotated as Int"
            (mkTestModule "TypedQuery Int"
                "[typedSql| SELECT author_id IS NULL FROM typed_sql_test_items LIMIT 1 |]")
            []

        compileFailTest "fails when boolean expression result is annotated as non-Maybe Bool"
            (mkTestModule "TypedQuery Bool"
                "[typedSql| SELECT author_id IS NULL FROM typed_sql_test_items LIMIT 1 |]")
            []

        compileFailTest "fails when COUNT(*) result is annotated as non-Maybe Integer"
            (mkTestModule "TypedQuery Integer"
                "[typedSql| SELECT COUNT(*) FROM typed_sql_test_items |]")
            []

        compileFailTest "fails when COALESCE expression is annotated as non-Maybe Text"
            (mkTestModule "TypedQuery (Text, Text)"
                "[typedSql| SELECT COALESCE(i.name, '(no-item)'), a.name FROM typed_sql_test_items i RIGHT JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]")
            []

        compileFailTest "fails when literal expression result is annotated as non-Maybe Int"
            (mkTestModule "TypedQuery Int"
                "[typedSql| SELECT 1 |]")
            []

        compileFailTest "fails when arithmetic expression result is annotated as non-Maybe Int"
            (mkTestModule "TypedQuery Int"
                "[typedSql| SELECT views + 1 FROM typed_sql_test_items LIMIT 1 |]")
            []

        compileFailTest "fails when CASE expression result is annotated as non-Maybe Text"
            (mkTestModule "TypedQuery Text"
                "[typedSql| SELECT CASE WHEN views > 5 THEN name ELSE 'low' END FROM typed_sql_test_items LIMIT 1 |]")
            []

        compileFailTest "fails when EXISTS expression result is annotated as non-Maybe Bool"
            (mkTestModule "TypedQuery Bool"
                "[typedSql| SELECT EXISTS(SELECT 1 FROM typed_sql_test_items WHERE views > 7) |]")
            []

        compileFailTest "fails when NULL literal result is annotated as non-Maybe Text"
            (mkTestModule "TypedQuery Text"
                "[typedSql| SELECT NULL::text |]")
            []

        compileFailTest "fails when CTE result is annotated as Maybe Text"
            (mkTestModule "TypedQuery (Maybe Text)"
                "[typedSql| WITH item_names AS (SELECT name FROM typed_sql_test_items WHERE views > 6) SELECT name FROM item_names LIMIT 1 |]")
            []

        compileFailTest "fails when subquery result is annotated as Maybe Text"
            (mkTestModule "TypedQuery (Maybe Text)"
                "[typedSql| SELECT name FROM (SELECT name FROM typed_sql_test_items WHERE views < 6) sub LIMIT 1 |]")
            []

        compileFailTest "fails when UNION result is annotated as non-Maybe Text"
            (mkTestModule "TypedQuery Text"
                "[typedSql| SELECT name FROM typed_sql_test_items WHERE views > 6 UNION ALL SELECT name FROM typed_sql_test_items WHERE views < 6 |]")
            []

        compileFailTest "fails when window function result is annotated as non-Maybe Integer"
            (mkTestModule "TypedQuery Integer"
                "[typedSql| SELECT row_number() OVER (ORDER BY name) FROM typed_sql_test_items LIMIT 1 |]")
            []

        compileFailTest "fails when grouped COUNT(*) result is annotated as non-Maybe Integer"
            (mkTestModule "TypedQuery (Text, Integer)"
                "[typedSql| SELECT name, COUNT(*) FROM typed_sql_test_items GROUP BY name ORDER BY name LIMIT 1 |]")
            []

        compileFailTest "fails when array literal result is annotated as non-Maybe [Text]"
            (mkTestModule "TypedQuery [Text]"
                "[typedSql| SELECT ARRAY['x','y']::text[] |]")
            []

        compileFailTest "fails when NULLIF expression result is annotated as non-Maybe Text"
            (mkTestModule "TypedQuery Text"
                "[typedSql| SELECT NULLIF(name, 'First') FROM typed_sql_test_items LIMIT 1 |]")
            []

    describe "TypedSql macro compile-time success" do
        compilePassTest "primary key inferred as Id'"
            (mkTestModuleWithPK ["typed_sql_test_items"] "TypedQuery (Id' \"typed_sql_test_items\")"
                "[typedSql| SELECT id FROM typed_sql_test_items LIMIT 1 |]")

        compilePassTest "nullable column inferred as Maybe"
            (mkTestModule "TypedQuery (Maybe Double)"
                "[typedSql| SELECT score FROM typed_sql_test_items LIMIT 1 |]")

        compilePassTest "LEFT JOIN right side inferred as Maybe"
            (mkTestModule "TypedQuery (Text, Maybe Text)"
                "[typedSql| SELECT i.name, a.name FROM typed_sql_test_items i LEFT JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]")

        compilePassTest "RIGHT JOIN left side inferred as Maybe"
            (mkTestModule "TypedQuery (Maybe Text, Text)"
                "[typedSql| SELECT i.name, a.name FROM typed_sql_test_items i RIGHT JOIN typed_sql_test_authors a ON a.id = i.author_id ORDER BY a.name LIMIT 1 |]")

        compilePassTest "tuple arity matches selected columns"
            (mkTestModule "TypedQuery (Text, Int)"
                "[typedSql| SELECT name, views FROM typed_sql_test_items LIMIT 1 |]")

        compilePassTest "boolean expression inferred as Maybe Bool"
            (mkTestModule "TypedQuery (Maybe Bool)"
                "[typedSql| SELECT author_id IS NULL FROM typed_sql_test_items LIMIT 1 |]")

        compilePassTest "COUNT(*) inferred as Maybe Integer"
            (mkTestModule "TypedQuery (Maybe Integer)"
                "[typedSql| SELECT COUNT(*) FROM typed_sql_test_items |]")

        compilePassTest "COALESCE in RIGHT JOIN inferred as Maybe"
            (mkTestModule "TypedQuery (Maybe Text, Text)"
                "[typedSql| SELECT COALESCE(i.name, '(no-item)'), a.name FROM typed_sql_test_items i RIGHT JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]")

        compilePassTest "literal expression inferred as Maybe Int"
            (mkTestModule "TypedQuery (Maybe Int)"
                "[typedSql| SELECT 1 |]")

        compilePassTest "arithmetic expression inferred as Maybe Int"
            (mkTestModule "TypedQuery (Maybe Int)"
                "[typedSql| SELECT views + 1 FROM typed_sql_test_items LIMIT 1 |]")

        compilePassTest "CASE expression inferred as Maybe Text"
            (mkTestModule "TypedQuery (Maybe Text)"
                "[typedSql| SELECT CASE WHEN views > 5 THEN name ELSE 'low' END FROM typed_sql_test_items LIMIT 1 |]")

        compilePassTest "EXISTS expression inferred as Maybe Bool"
            (mkTestModule "TypedQuery (Maybe Bool)"
                "[typedSql| SELECT EXISTS(SELECT 1 FROM typed_sql_test_items WHERE views > 7) |]")

        compilePassTest "NULL literal inferred as Maybe Text"
            (mkTestModule "TypedQuery (Maybe Text)"
                "[typedSql| SELECT NULL::text |]")

        compilePassTest "CTE preserves column type"
            (mkTestModule "TypedQuery Text"
                "[typedSql| WITH item_names AS (SELECT name FROM typed_sql_test_items WHERE views > 6) SELECT name FROM item_names LIMIT 1 |]")

        compilePassTest "subquery preserves column type"
            (mkTestModule "TypedQuery Text"
                "[typedSql| SELECT name FROM (SELECT name FROM typed_sql_test_items WHERE views < 6) sub LIMIT 1 |]")

        compilePassTest "UNION inferred as Maybe"
            (mkTestModule "TypedQuery (Maybe Text)"
                "[typedSql| SELECT name FROM typed_sql_test_items WHERE views > 6 UNION ALL SELECT name FROM typed_sql_test_items WHERE views < 6 |]")

        compilePassTest "window function inferred as Maybe Integer"
            (mkTestModule "TypedQuery (Maybe Integer)"
                "[typedSql| SELECT row_number() OVER (ORDER BY name) FROM typed_sql_test_items LIMIT 1 |]")

        compilePassTest "grouped COUNT(*) inferred as (Text, Maybe Integer)"
            (mkTestModule "TypedQuery (Text, Maybe Integer)"
                "[typedSql| SELECT name, COUNT(*) FROM typed_sql_test_items GROUP BY name ORDER BY name LIMIT 1 |]")

        compilePassTest "array literal inferred as Maybe [Text]"
            (mkTestModule "TypedQuery (Maybe [Text])"
                "[typedSql| SELECT ARRAY['x','y']::text[] |]")

        compilePassTest "NULLIF inferred as Maybe Text"
            (mkTestModule "TypedQuery (Maybe Text)"
                "[typedSql| SELECT NULLIF(name, 'First') FROM typed_sql_test_items LIMIT 1 |]")

        compilePassTest "scalar parameter accepts correct type"
            (mkTestModule "TypedQuery Text"
                "[typedSql| SELECT name FROM typed_sql_test_items WHERE views = ${(5 :: Int)} LIMIT 1 |]")

        compilePassTest "foreign-key parameter accepts Id' type"
            (mkTestModuleWithPK ["typed_sql_test_items", "typed_sql_test_authors"] "TypedQuery Text"
                "let authorId = (\"00000000-0000-0000-0000-000000000001\" :: Id' \"typed_sql_test_authors\")\n      in [typedSql| SELECT name FROM typed_sql_test_items WHERE author_id = ${authorId} LIMIT 1 |]")

        compilePassTest "INNER JOIN columns are non-Maybe"
            (mkTestModule "TypedQuery (Text, Text)"
                "[typedSql| SELECT i.name, a.name FROM typed_sql_test_items i INNER JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]")

    describe "TypedSql macro runtime execution" do
        runtimeTest "executes typedSql queries end-to-end via ghci" runtimeModule
        runtimeTest "UPDATE and DELETE with parameters" runtimeUpdateDeleteModule
        runtimeTest "empty results and edge cases" runtimeEdgeCasesModule
        runtimeTest "additional column types (smallint, bigint, numeric, bytea, bool, timestamptz, date, jsonb)" runtimeExtraTypesModule

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

-- Test helpers ---------------------------------------------------------------

requirePostgresTestHook :: IO ()
requirePostgresTestHook = do
    maybePgHost <- lookupEnv "PGHOST"
    when (isNothing maybePgHost) do
        pendingWith "requires postgresqlTestHook / withTestPostgres (PGHOST is not set)"

withTestModelContext :: ((?modelContext :: ModelContext) => IO a) -> IO a
withTestModelContext action = do
    logger <- newLogger def { level = Warn }
    databaseUrl <- cs . fromMaybe "" <$> lookupEnv "DATABASE_URL"
    modelContext <- createModelContext databaseUrl logger
    let ?modelContext = modelContext
    action `Exception.finally` releaseModelContext modelContext

setupSchema :: (?modelContext :: ModelContext) => IO ()
setupSchema = do
    -- Use sqlExecDiscardResult for DDL (DROP/CREATE) since they have no rows-affected count
    sqlExecDiscardResult "DROP TABLE IF EXISTS typed_sql_test_extras" ()
    sqlExecDiscardResult "DROP TABLE IF EXISTS typed_sql_test_items" ()
    sqlExecDiscardResult "DROP TABLE IF EXISTS typed_sql_test_authors" ()
    sqlExecDiscardResult "DROP TYPE IF EXISTS typed_sql_test_pair" ()

    sqlExecDiscardResult "CREATE TYPE typed_sql_test_pair AS (name TEXT, views INT)" ()

    sqlExecDiscardResult
        "CREATE TABLE typed_sql_test_authors (id UUID PRIMARY KEY, name TEXT NOT NULL)"
        ()

    sqlExecDiscardResult
        "CREATE TABLE typed_sql_test_items (id UUID PRIMARY KEY, author_id UUID REFERENCES typed_sql_test_authors(id), name TEXT NOT NULL, views INT NOT NULL, score DOUBLE PRECISION, tags TEXT[] NOT NULL DEFAULT '{}')"
        ()

    sqlExecDiscardResult
        "INSERT INTO typed_sql_test_authors (id, name) VALUES ('00000000-0000-0000-0000-000000000001'::uuid, 'Alice')"
        ()

    sqlExecDiscardResult
        "INSERT INTO typed_sql_test_authors (id, name) VALUES ('00000000-0000-0000-0000-000000000002'::uuid, 'Bob')"
        ()

    sqlExecDiscardResult
        "INSERT INTO typed_sql_test_items (id, author_id, name, views, score, tags) VALUES ('10000000-0000-0000-0000-000000000001'::uuid, '00000000-0000-0000-0000-000000000001'::uuid, 'First', 5, 1.5, ARRAY['red', 'blue'])"
        ()

    sqlExecDiscardResult
        "INSERT INTO typed_sql_test_items (id, author_id, name, views, score, tags) VALUES ('10000000-0000-0000-0000-000000000002'::uuid, '00000000-0000-0000-0000-000000000001'::uuid, 'Second', 8, NULL, ARRAY['green'])"
        ()

    sqlExecDiscardResult
        "CREATE TABLE typed_sql_test_extras (id UUID PRIMARY KEY, small_count SMALLINT NOT NULL DEFAULT 0, big_count BIGINT NOT NULL DEFAULT 0, amount NUMERIC, payload BYTEA, metadata JSONB, created_at TIMESTAMPTZ NOT NULL DEFAULT '2025-06-15 12:00:00+00', due_date DATE, active BOOLEAN NOT NULL DEFAULT TRUE)"
        ()

    sqlExecDiscardResult
        "INSERT INTO typed_sql_test_extras (id, small_count, big_count, amount, payload, metadata, created_at, due_date, active) VALUES ('20000000-0000-0000-0000-000000000001'::uuid, 7, 1000000000, 99.95, '\\xDEADBEEF', '{\"key\": \"value\"}', '2025-06-15 12:00:00+00', '2025-06-15', true)"
        ()

    pure ()

-- GHCi infrastructure --------------------------------------------------------

ghciLoadModule :: Text -> IO Text
ghciLoadModule source =
    ghciRun source [":set -fno-code"] []

ghciRunModule :: Text -> IO Text
ghciRunModule source =
    ghciRun source [] ["main"]

ghciRun :: Text -> [Text] -> [Text] -> IO Text
ghciRun source preLoadCommands postLoadCommands =
    withSystemTempDirectory "typed-sql-ghci" \tempDir -> do
        packageRoot <- findIhpPackageRoot
        let repoRoot = takeDirectory packageRoot
        useRepoGhci <- doesFileExist (repoRoot </> ".ghci")
        env <- ghciEnvironment

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

ghciEnvironment :: IO [(String, String)]
ghciEnvironment = do
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

    let overrides :: [(String, String)]
        overrides =
            [ ("DATABASE_URL", databaseUrl)
            ]

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

-- | Generate Id newtype boilerplate for a test table.
-- E.g. for "typed_sql_test_items" generates TypedSqlTestItemId newtype.
mkTestIdNewtype :: Text -> [Text]
mkTestIdNewtype tableName =
    let idName = tableNameToIdName tableName
    in  [ "type instance PrimaryKey \"" <> tableName <> "\" = UUID"
        , "newtype " <> idName <> " = " <> idName <> " UUID deriving newtype (Eq, Ord, Show, Mapping.IsScalar)"
        , "type instance Id' \"" <> tableName <> "\" = " <> idName
        , "instance IdNewtype " <> idName <> " UUID where { toId = " <> idName <> "; fromId (" <> idName <> " x) = x }"
        , "instance IsString " <> idName <> " where"
        , "    fromString str = case readMay str of"
        , "        Just pk -> " <> idName <> " pk"
        , "        Nothing -> error \"Unable to convert to " <> idName <> "\""
        , "instance Hasql.Implicits.Encoders.DefaultParamEncoder " <> idName <> " where defaultParam = Hasql.Encoders.nonNullable Mapping.encoder"
        , "instance Hasql.Implicits.Encoders.DefaultParamEncoder [" <> idName <> "] where defaultParam = Hasql.Encoders.nonNullable $ Hasql.Encoders.foldableArray $ Hasql.Encoders.nonNullable Mapping.encoder"
        , ""
        ]

-- | Convert a table name like "typed_sql_test_items" to an Id type name like "TypedSqlTestItemId".
tableNameToIdName :: Text -> Text
tableNameToIdName tableName =
    let parts = Text.splitOn "_" tableName
        capitalize t = case Text.uncons t of
            Just (c, rest) -> Text.cons (Char.toUpper c) rest
            Nothing -> t
        -- Singularize: drop trailing 's' from last part
        singularize t = fromMaybe t (Text.stripSuffix "s" t)
        modelName = mconcat (map capitalize (initOrEmpty parts) <> [singularize (capitalize (lastOrEmpty parts))])
    in modelName <> "Id"
    where
        initOrEmpty [] = []
        initOrEmpty xs = init xs
        lastOrEmpty [] = ""
        lastOrEmpty xs = last xs

-- | Build a test module from a type signature and body expression.
-- Used for both compile-pass and compile-fail tests.
mkTestModule :: Text -> Text -> Text
mkTestModule typeSig body = Text.unlines
    [ "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module TypedSqlCase where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , ""
    , "query :: " <> typeSig
    , "query = " <> body
    ]

-- | Build a test module that also needs PrimaryKey type instances.
mkTestModuleWithPK :: [Text] -> Text -> Text -> Text
mkTestModuleWithPK pkTables typeSig body = Text.unlines $
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE DerivingStrategies #-}"
    , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "module TypedSqlCase where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.ModelSupport (Id', PrimaryKey, IdNewtype(..))"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , "import qualified Hasql.Implicits.Encoders"
    , "import qualified Hasql.Encoders"
    , "import qualified Hasql.Mapping.IsScalar as Mapping"
    , ""
    ]
    <> concatMap mkTestIdNewtype pkTables
    <>
    [ ""
    , "query :: " <> typeSig
    , "query = " <> body
    ]

-- Test modules ---------------------------------------------------------------

compilePassModule :: Text
compilePassModule = Text.unlines $
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE DerivingStrategies #-}"
    , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "{-# LANGUAGE TypeApplications #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "module TypedSqlCompilePass where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.ModelSupport (Id', PrimaryKey, IdNewtype(..))"
    , "import IHP.Hasql.FromRow (FromRowHasql (..))"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , "import qualified Hasql.Decoders as HasqlDecoders"
    , "import qualified Hasql.Implicits.Encoders"
    , "import qualified Hasql.Encoders"
    , "import qualified Hasql.Mapping.IsScalar as Mapping"
    , ""
    ]
    <> mkTestIdNewtype "typed_sql_test_items"
    <> mkTestIdNewtype "typed_sql_test_authors"
    <>
    [ ""
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
    , "qName :: TypedQuery Text"
    , "qName = [typedSql| SELECT name FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qAllFields :: TypedQuery TypedSqlTestItem"
    , "qAllFields = [typedSql| SELECT typed_sql_test_items.* FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qAllFieldsAlias :: TypedQuery TypedSqlTestItem"
    , "qAllFieldsAlias = [typedSql| SELECT i.* FROM typed_sql_test_items i JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]"
    , ""
    , "qPrimaryKey :: TypedQuery (Id' \"typed_sql_test_items\")"
    , "qPrimaryKey = [typedSql| SELECT id FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qForeignKey :: TypedQuery (Maybe (Id' \"typed_sql_test_authors\"))"
    , "qForeignKey = [typedSql| SELECT author_id FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qNullable :: TypedQuery (Maybe Double)"
    , "qNullable = [typedSql| SELECT score FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qArray :: TypedQuery [Text]"
    , "qArray = [typedSql| SELECT tags FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qTuple :: TypedQuery (Id' \"typed_sql_test_items\", Text, Int)"
    , "qTuple = [typedSql| SELECT id, name, views FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qEqParam :: TypedQuery Text"
    , "qEqParam = [typedSql| SELECT name FROM typed_sql_test_items WHERE views = ${5 :: Int} LIMIT 1 |]"
    , ""
    , "qForeignKeyParamHint :: TypedQuery Text"
    , "qForeignKeyParamHint ="
    , "    let authorId = (\"00000000-0000-0000-0000-000000000001\" :: Id' \"typed_sql_test_authors\")"
    , "    in [typedSql| SELECT name FROM typed_sql_test_items WHERE author_id = ${authorId} LIMIT 1 |]"
    , ""
    , "qInParamHint :: TypedQuery Text"
    , "qInParamHint ="
    , "    let authorIds = [ (\"00000000-0000-0000-0000-000000000001\" :: Id' \"typed_sql_test_authors\") ]"
    , "    in [typedSql| SELECT name FROM typed_sql_test_items WHERE author_id IN (${authorIds}) LIMIT 1 |]"
    , ""
    , "qAnyParamHint :: TypedQuery Text"
    , "qAnyParamHint ="
    , "    let itemIds ="
    , "            [ (\"10000000-0000-0000-0000-000000000001\" :: Id' \"typed_sql_test_items\")"
    , "            , (\"10000000-0000-0000-0000-000000000002\" :: Id' \"typed_sql_test_items\")"
    , "            ]"
    , "    in [typedSql| SELECT name FROM typed_sql_test_items WHERE id = ANY(${itemIds}) ORDER BY name LIMIT 1 |]"
    , ""
    , "qCompositeExpanded :: TypedQuery (Maybe Text, Maybe Int)"
    , "qCompositeExpanded = [typedSql| SELECT (ROW(name, views)::typed_sql_test_pair).* FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qBoolExpr :: TypedQuery (Maybe Bool)"
    , "qBoolExpr = [typedSql| SELECT author_id IS NULL FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qCountExpr :: TypedQuery (Maybe Integer)"
    , "qCountExpr = [typedSql| SELECT COUNT(*) FROM typed_sql_test_items |]"
    , ""
    , "qLiteralInt :: TypedQuery (Maybe Int)"
    , "qLiteralInt = [typedSql| SELECT 1 |]"
    , ""
    , "qArithmeticExpr :: TypedQuery (Maybe Int)"
    , "qArithmeticExpr = [typedSql| SELECT views + 1 FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qCaseExpr :: TypedQuery (Maybe Text)"
    , "qCaseExpr = [typedSql| SELECT CASE WHEN views > 5 THEN name ELSE 'low' END FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qExistsExpr :: TypedQuery (Maybe Bool)"
    , "qExistsExpr = [typedSql| SELECT EXISTS(SELECT 1 FROM typed_sql_test_items WHERE views > 7) |]"
    , ""
    , "qNullLiteral :: TypedQuery (Maybe Text)"
    , "qNullLiteral = [typedSql| SELECT NULL::text |]"
    , ""
    , "qCte :: TypedQuery Text"
    , "qCte = [typedSql| WITH item_names AS (SELECT name FROM typed_sql_test_items WHERE views > 6) SELECT name FROM item_names LIMIT 1 |]"
    , ""
    , "qSubquery :: TypedQuery Text"
    , "qSubquery = [typedSql| SELECT name FROM (SELECT name FROM typed_sql_test_items WHERE views < 6) sub LIMIT 1 |]"
    , ""
    , "qUnion :: TypedQuery (Maybe Text)"
    , "qUnion = [typedSql| SELECT name FROM typed_sql_test_items WHERE views > 6 UNION ALL SELECT name FROM typed_sql_test_items WHERE views < 6 |]"
    , ""
    , "qWindow :: TypedQuery (Maybe Integer)"
    , "qWindow = [typedSql| SELECT row_number() OVER (ORDER BY name) FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qGroupedCount :: TypedQuery (Text, Maybe Integer)"
    , "qGroupedCount = [typedSql| SELECT name, COUNT(*) FROM typed_sql_test_items GROUP BY name ORDER BY name LIMIT 1 |]"
    , ""
    , "qArrayLiteral :: TypedQuery (Maybe [Text])"
    , "qArrayLiteral = [typedSql| SELECT ARRAY['x','y']::text[] |]"
    , ""
    , "qNullIfExpr :: TypedQuery (Maybe Text)"
    , "qNullIfExpr = [typedSql| SELECT NULLIF(name, 'First') FROM typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qSchemaQualified :: TypedQuery Text"
    , "qSchemaQualified = [typedSql| SELECT name FROM public.typed_sql_test_items LIMIT 1 |]"
    , ""
    , "qQuotedIdentifiers :: TypedQuery Text"
    , "qQuotedIdentifiers = [typedSql| SELECT \"name\" FROM \"typed_sql_test_items\" LIMIT 1 |]"
    , ""
    , "qInnerJoin :: TypedQuery (Text, Text)"
    , "qInnerJoin = [typedSql| SELECT i.name, a.name FROM typed_sql_test_items i INNER JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]"
    , ""
    , "qLeftJoin :: TypedQuery (Text, Maybe Text)"
    , "qLeftJoin = [typedSql| SELECT i.name, a.name FROM typed_sql_test_items i LEFT JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]"
    , ""
    , "qRightJoin :: TypedQuery (Maybe Text, Text)"
    , "qRightJoin = [typedSql| SELECT i.name, a.name FROM typed_sql_test_items i RIGHT JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]"
    , ""
    , "qRightJoinCoalesced :: TypedQuery (Maybe Text, Text)"
    , "qRightJoinCoalesced = [typedSql| SELECT COALESCE(i.name, '(no-item)'), a.name FROM typed_sql_test_items i RIGHT JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]"
    ]

runtimeModule :: Text
runtimeModule = Text.unlines $
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE DerivingStrategies #-}"
    , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
    , "{-# LANGUAGE ImplicitParams #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "module Main where"
    , ""
    , "import qualified Control.Exception as Exception"
    , "import IHP.Prelude"
    , "import IHP.Log.Types"
    , "import IHP.ModelSupport (Id', ModelContext, PrimaryKey, IdNewtype(..), createModelContext, releaseModelContext)"
    , "import IHP.Hasql.FromRow (FromRowHasql (..))"
    , "import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)"
    , "import qualified Hasql.Decoders as HasqlDecoders"
    , "import qualified Hasql.Implicits.Encoders"
    , "import qualified Hasql.Encoders"
    , "import qualified Hasql.Mapping.IsScalar as Mapping"
    , "import System.Environment (lookupEnv)"
    , ""
    ]
    <> mkTestIdNewtype "typed_sql_test_items"
    <> mkTestIdNewtype "typed_sql_test_authors"
    <>
    [ ""
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
    , "    logger <- newLogger def { level = Warn }"
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
    , "        allItems <- sqlQueryTyped [typedSql|"
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
    , "        countRows <- sqlQueryTyped [typedSql| SELECT COUNT(*) FROM typed_sql_test_items |]"
    , ""
    , "        when ((countRows :: [Maybe Integer]) /= [Just 2]) do"
    , "            error (\"unexpected rows from count query: \" <> show countRows)"
    , ""
    , "        literalRows <- sqlQueryTyped [typedSql| SELECT 1 |]"
    , ""
    , "        when ((literalRows :: [Maybe Int]) /= [Just 1]) do"
    , "            error (\"unexpected rows from literal query: \" <> show literalRows)"
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
    , "        existsRows <- sqlQueryTyped [typedSql| SELECT EXISTS(SELECT 1 FROM typed_sql_test_items WHERE views > 7) |]"
    , ""
    , "        when ((existsRows :: [Maybe Bool]) /= [Just True]) do"
    , "            error (\"unexpected rows from EXISTS query: \" <> show existsRows)"
    , ""
    , "        nullLiteralRows <- sqlQueryTyped [typedSql| SELECT NULL::text |]"
    , ""
    , "        when ((nullLiteralRows :: [Maybe Text]) /= [Nothing]) do"
    , "            error (\"unexpected rows from NULL literal query: \" <> show nullLiteralRows)"
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
    , "        when ((windowRows :: [Maybe Integer]) /= [Just 1, Just 2]) do"
    , "            error (\"unexpected rows from window function: \" <> show windowRows)"
    , ""
    , "        groupedCountRows <- sqlQueryTyped [typedSql|"
    , "            SELECT name, COUNT(*)"
    , "            FROM typed_sql_test_items"
    , "            GROUP BY name"
    , "            ORDER BY name"
    , "        |]"
    , ""
    , "        when ((groupedCountRows :: [(Text, Maybe Integer)]) /= [(\"First\", Just 1), (\"Second\", Just 1)]) do"
    , "            error (\"unexpected rows from grouped count: \" <> show groupedCountRows)"
    , ""
    , "        arrayLiteralRows <- sqlQueryTyped [typedSql| SELECT ARRAY['x','y']::text[] |]"
    , ""
    , "        when ((arrayLiteralRows :: [Maybe [Text]]) /= [Just [\"x\", \"y\"]]) do"
    , "            error (\"unexpected rows from array literal: \" <> show arrayLiteralRows)"
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
    , "        when ((innerJoinRows :: [(Text, Text)]) /= [(\"First\", \"Alice\"), (\"Second\", \"Alice\")]) do"
    , "            error (\"unexpected rows from inner join: \" <> show innerJoinRows)"
    , ""
    , "        leftJoinRows <- sqlQueryTyped [typedSql|"
    , "            SELECT i.name, a.name"
    , "            FROM typed_sql_test_items i"
    , "            LEFT JOIN typed_sql_test_authors a ON a.id = i.author_id"
    , "            ORDER BY i.name"
    , "        |]"
    , ""
    , "        when ((leftJoinRows :: [(Text, Maybe Text)]) /= [(\"First\", Just \"Alice\"), (\"Second\", Just \"Alice\")]) do"
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
    , "        when ((rightJoinRows :: [(Maybe Text, Text)]) /= [(Just \"First\", \"Alice\"), (Just \"Second\", \"Alice\")]) do"
    , "            error (\"unexpected rows from right join: \" <> show rightJoinRows)"
    , ""
    , "        rightJoinCoalescedRows <- sqlQueryTyped [typedSql|"
    , "            SELECT COALESCE(i.name, '(no-item)'), a.name"
    , "            FROM typed_sql_test_items i"
    , "            RIGHT JOIN typed_sql_test_authors a ON a.id = i.author_id"
    , "            ORDER BY a.name, i.name NULLS LAST"
    , "        |]"
    , ""
    , "        when ((rightJoinCoalescedRows :: [(Maybe Text, Text)]) /= [(Just \"First\", \"Alice\"), (Just \"Second\", \"Alice\"), (Just \"(no-item)\", \"Bob\")]) do"
    , "            error (\"unexpected rows from right join with COALESCE: \" <> show rightJoinCoalescedRows)"
    , ""
    , "        putStrLn \"RUNTIME_OK\""
    ]

runtimeUpdateDeleteModule :: Text
runtimeUpdateDeleteModule = Text.unlines $
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE DerivingStrategies #-}"
    , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
    , "{-# LANGUAGE ImplicitParams #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "module Main where"
    , ""
    , "import qualified Control.Exception as Exception"
    , "import IHP.Prelude"
    , "import IHP.Log.Types"
    , "import IHP.ModelSupport (Id', ModelContext, PrimaryKey, IdNewtype(..), createModelContext, releaseModelContext)"
    , "import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)"
    , "import qualified Hasql.Implicits.Encoders"
    , "import qualified Hasql.Encoders"
    , "import qualified Hasql.Mapping.IsScalar as Mapping"
    , "import System.Environment (lookupEnv)"
    , ""
    ]
    <> mkTestIdNewtype "typed_sql_test_items"
    <> mkTestIdNewtype "typed_sql_test_authors"
    <>
    [ ""
    , "assertTest :: Text -> Bool -> IO ()"
    , "assertTest name True  = putStrLn (\"PASS: \" <> name)"
    , "assertTest name False = error (\"FAIL: \" <> name)"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "    logger <- newLogger def { level = Warn }"
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
    , "        assertTest \"UPDATE single column value\" ((viewsAfter :: [Int]) == [10])"
    , ""
    , "        -- UPDATE multiple columns"
    , "        rowsUpdated2 <- sqlExecTyped [typedSql|"
    , "            UPDATE typed_sql_test_items SET name = ${(\"Updated\" :: Text)}, views = ${99 :: Int} WHERE id = ${itemId2}"
    , "        |]"
    , "        assertTest \"UPDATE multiple columns rows affected\" (rowsUpdated2 == 1)"
    , ""
    , "        updated <- sqlQueryTyped [typedSql| SELECT name, views FROM typed_sql_test_items WHERE id = ${itemId2} |]"
    , "        assertTest \"UPDATE multiple columns values\" ((updated :: [(Text, Int)]) == [(\"Updated\", 99)])"
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
runtimeEdgeCasesModule = Text.unlines $
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE DerivingStrategies #-}"
    , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
    , "{-# LANGUAGE ImplicitParams #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "module Main where"
    , ""
    , "import qualified Control.Exception as Exception"
    , "import IHP.Prelude"
    , "import IHP.Log.Types"
    , "import IHP.ModelSupport (Id', ModelContext, PrimaryKey, IdNewtype(..), createModelContext, releaseModelContext)"
    , "import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)"
    , "import qualified Hasql.Implicits.Encoders"
    , "import qualified Hasql.Encoders"
    , "import qualified Hasql.Mapping.IsScalar as Mapping"
    , "import System.Environment (lookupEnv)"
    , ""
    ]
    <> mkTestIdNewtype "typed_sql_test_items"
    <> mkTestIdNewtype "typed_sql_test_authors"
    <>
    [ ""
    , "assertTest :: Text -> Bool -> IO ()"
    , "assertTest name True  = putStrLn (\"PASS: \" <> name)"
    , "assertTest name False = error (\"FAIL: \" <> name)"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "    logger <- newLogger def { level = Warn }"
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
    , "        assertTest \"COUNT on empty table\" ((countEmpty :: [Maybe Integer]) == [Just 0])"
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
    , "        -- 5-tuple select"
    , "        fiveTuple <- sqlQueryTyped [typedSql|"
    , "            SELECT name, views, score, author_id IS NULL, tags"
    , "            FROM typed_sql_test_items"
    , "            WHERE id = ${itemId1}"
    , "        |]"
    , "        assertTest \"5-tuple select\" ((fiveTuple :: [(Text, Int, Maybe Double, Maybe Bool, [Text])]) == [(\"First\", 5, Just 1.5, Just False, [\"red\", \"blue\"])])"
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
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "module Main where"
    , ""
    , "import qualified Control.Exception as Exception"
    , "import IHP.Prelude"
    , "import IHP.Log.Types"
    , "import IHP.ModelSupport (ModelContext, PrimaryKey, createModelContext, releaseModelContext)"
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
    , "    logger <- newLogger def { level = Warn }"
    , "    databaseUrl <- cs . fromMaybe \"\" <$> lookupEnv \"DATABASE_URL\""
    , "    modelContext <- createModelContext databaseUrl logger"
    , "    let ?modelContext = modelContext"
    , "    flip Exception.finally (releaseModelContext modelContext) do"
    , ""
    , "        -- smallint -> Int"
    , "        smallRows <- sqlQueryTyped [typedSql| SELECT small_count FROM typed_sql_test_extras LIMIT 1 |]"
    , "        assertTest \"smallint -> Int\" ((smallRows :: [Int]) == [7])"
    , ""
    , "        -- bigint -> Integer"
    , "        bigRows <- sqlQueryTyped [typedSql| SELECT big_count FROM typed_sql_test_extras LIMIT 1 |]"
    , "        assertTest \"bigint -> Integer\" ((bigRows :: [Integer]) == [1000000000])"
    , ""
    , "        -- numeric -> Scientific"
    , "        numericRows <- sqlQueryTyped [typedSql| SELECT amount FROM typed_sql_test_extras LIMIT 1 |]"
    , "        assertTest \"numeric -> Scientific\" ((numericRows :: [Maybe Scientific]) == [Just 99.95])"
    , ""
    , "        -- bytea -> ByteString"
    , "        byteaRows <- sqlQueryTyped [typedSql| SELECT payload FROM typed_sql_test_extras LIMIT 1 |]"
    , "        assertTest \"bytea -> ByteString\" ((byteaRows :: [Maybe BS.ByteString]) == [Just (BS.pack [0xDE, 0xAD, 0xBE, 0xEF])])"
    , ""
    , "        -- bool -> Bool"
    , "        boolRows <- sqlQueryTyped [typedSql| SELECT active FROM typed_sql_test_extras LIMIT 1 |]"
    , "        assertTest \"bool -> Bool\" ((boolRows :: [Bool]) == [True])"
    , ""
    , "        -- timestamptz -> UTCTime"
    , "        tsRows <- sqlQueryTyped [typedSql| SELECT created_at FROM typed_sql_test_extras LIMIT 1 |]"
    , "        let Just expectedTime = parseTimeM True defaultTimeLocale \"%Y-%m-%d %H:%M:%S%Z\" \"2025-06-15 12:00:00UTC\" :: Maybe UTCTime"
    , "        assertTest \"timestamptz -> UTCTime\" ((tsRows :: [UTCTime]) == [expectedTime])"
    , ""
    , "        -- date -> Day"
    , "        dateRows <- sqlQueryTyped [typedSql| SELECT due_date FROM typed_sql_test_extras LIMIT 1 |]"
    , "        let Just expectedDate = parseTimeM True defaultTimeLocale \"%Y-%m-%d\" \"2025-06-15\" :: Maybe Day"
    , "        assertTest \"date -> Day\" ((dateRows :: [Maybe Day]) == [Just expectedDate])"
    , ""
    , "        -- jsonb -> Aeson.Value"
    , "        jsonRows <- sqlQueryTyped [typedSql| SELECT metadata FROM typed_sql_test_extras LIMIT 1 |]"
    , "        let expectedJson = Aeson.object [(\"key\", Aeson.String \"value\")]"
    , "        assertTest \"jsonb -> Aeson.Value\" ((jsonRows :: [Maybe Aeson.Value]) == [Just expectedJson])"
    , ""
    , "        -- multi-type tuple"
    , "        tupleRows <- sqlQueryTyped [typedSql|"
    , "            SELECT small_count, big_count, active"
    , "            FROM typed_sql_test_extras LIMIT 1"
    , "        |]"
    , "        assertTest \"multi-type tuple\" ((tupleRows :: [(Int, Integer, Bool)]) == [(7, 1000000000, True)])"
    , ""
    , "        putStrLn \"RUNTIME_OK\""
    ]
