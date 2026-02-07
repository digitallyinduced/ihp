module Test.TypedSqlSpec where

import qualified Control.Exception                 as Exception
import qualified Data.Text                         as Text
import qualified Data.Text.IO                      as Text
import           IHP.Log.Types
import           IHP.ModelSupport                  (Id', ModelContext,
                                                    createModelContext,
                                                    releaseModelContext,
                                                    sqlExec)
import           IHP.Prelude
import           System.Directory                  (doesFileExist,
                                                    getCurrentDirectory)
import           System.Environment                (getEnvironment, lookupEnv)
import           System.FilePath                   (takeDirectory, (</>))
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

        it "fails when a scalar parameter has the wrong type" do
            requirePostgresTestHook
            withTestModelContext do
                setupSchema
                ghciOutput <- ghciLoadModule compileFailWrongScalarParameter
                assertGhciFailure ghciOutput []

        it "fails when a foreign-key parameter has the wrong type" do
            requirePostgresTestHook
            withTestModelContext do
                setupSchema
                ghciOutput <- ghciLoadModule compileFailWrongForeignKeyParameter
                assertGhciFailure ghciOutput []

        it "fails when an IN parameter has the wrong element type" do
            requirePostgresTestHook
            withTestModelContext do
                setupSchema
                ghciOutput <- ghciLoadModule compileFailWrongInParameter
                assertGhciFailure ghciOutput []

        it "fails when a placeholder expression is invalid Haskell" do
            requirePostgresTestHook
            withTestModelContext do
                setupSchema
                ghciOutput <- ghciLoadModule compileFailInvalidPlaceholderExpression
                assertGhciFailure ghciOutput ["failed to parse expression"]

        it "fails when SQL parameter count does not match ${...} placeholders" do
            requirePostgresTestHook
            withTestModelContext do
                setupSchema
                ghciOutput <- ghciLoadModule compileFailPlaceholderCountMismatch
                assertGhciFailure ghciOutput ["placeholder count mismatch"]

        it "fails when selecting a single composite value without expansion" do
            requirePostgresTestHook
            withTestModelContext do
                setupSchema
                ghciOutput <- ghciLoadModule compileFailSingleCompositeColumn
                assertGhciFailure ghciOutput ["composite columns must be expanded"]

        it "fails when SQL references an unknown column" do
            requirePostgresTestHook
            withTestModelContext do
                setupSchema
                ghciOutput <- ghciLoadModule compileFailUnknownColumn
                assertGhciFailure ghciOutput ["does not exist"]

        it "fails when primary-key result type is annotated as UUID instead of Id" do
            requirePostgresTestHook
            withTestModelContext do
                setupSchema
                ghciOutput <- ghciLoadModule compileFailPrimaryKeyResultAnnotation
                assertGhciFailure ghciOutput []

        it "fails when nullable column result is annotated as non-Maybe" do
            requirePostgresTestHook
            withTestModelContext do
                setupSchema
                ghciOutput <- ghciLoadModule compileFailNullableResultAnnotation
                assertGhciFailure ghciOutput []

        it "fails when LEFT JOIN result is annotated as Maybe" do
            requirePostgresTestHook
            withTestModelContext do
                setupSchema
                ghciOutput <- ghciLoadModule compileFailLeftJoinMaybeAnnotation
                assertGhciFailure ghciOutput []

        it "fails when RIGHT JOIN result is annotated as Maybe" do
            requirePostgresTestHook
            withTestModelContext do
                setupSchema
                ghciOutput <- ghciLoadModule compileFailRightJoinMaybeAnnotation
                assertGhciFailure ghciOutput []

        it "fails when tuple arity does not match selected columns" do
            requirePostgresTestHook
            withTestModelContext do
                setupSchema
                ghciOutput <- ghciLoadModule compileFailTupleArityMismatch
                assertGhciFailure ghciOutput []

        it "fails when boolean expression result is annotated as Int" do
            requirePostgresTestHook
            withTestModelContext do
                setupSchema
                ghciOutput <- ghciLoadModule compileFailBooleanResultAnnotation
                assertGhciFailure ghciOutput []

        it "fails when boolean expression result is annotated as non-Maybe Bool" do
            requirePostgresTestHook
            withTestModelContext do
                setupSchema
                ghciOutput <- ghciLoadModule compileFailBooleanNonMaybeAnnotation
                assertGhciFailure ghciOutput []

        it "fails when COUNT(*) result is annotated as non-Maybe Integer" do
            requirePostgresTestHook
            withTestModelContext do
                setupSchema
                ghciOutput <- ghciLoadModule compileFailCountNonMaybeAnnotation
                assertGhciFailure ghciOutput []

        it "fails when COALESCE expression is annotated as non-Maybe Text" do
            requirePostgresTestHook
            withTestModelContext do
                setupSchema
                ghciOutput <- ghciLoadModule compileFailCoalesceNonMaybeAnnotation
                assertGhciFailure ghciOutput []

        it "fails when literal expression result is annotated as non-Maybe Int" do
            requirePostgresTestHook
            withTestModelContext do
                setupSchema
                ghciOutput <- ghciLoadModule compileFailLiteralNonMaybeAnnotation
                assertGhciFailure ghciOutput []

    describe "TypedSql macro runtime execution" do
        it "executes typedSql queries end-to-end via ghci" do
            requirePostgresTestHook
            withTestModelContext do
                setupSchema
                ghciOutput <- ghciRunModule runtimeModule
                assertGhciSuccess ghciOutput
                ghciOutput `shouldContainText` "RUNTIME_OK"

requirePostgresTestHook :: IO ()
requirePostgresTestHook = do
    maybePgHost <- lookupEnv "PGHOST"
    when (isNothing maybePgHost) do
        pendingWith "requires postgresqlTestHook / withTestPostgres (PGHOST is not set)"

withTestModelContext :: ((?modelContext :: ModelContext) => IO a) -> IO a
withTestModelContext action = do
    logger <- newLogger def { level = Warn }
    modelContext <- createModelContext 10 1 "" logger
    let ?modelContext = modelContext
    action `Exception.finally` releaseModelContext modelContext

setupSchema :: (?modelContext :: ModelContext) => IO ()
setupSchema = do
    _ <- sqlExec "DROP TABLE IF EXISTS typed_sql_test_items" ()
    _ <- sqlExec "DROP TABLE IF EXISTS typed_sql_test_authors" ()
    _ <- sqlExec "DROP TYPE IF EXISTS typed_sql_test_pair" ()

    _ <- sqlExec "CREATE TYPE typed_sql_test_pair AS (name TEXT, views INT)" ()

    _ <- sqlExec
        "CREATE TABLE typed_sql_test_authors (id UUID PRIMARY KEY, name TEXT NOT NULL)"
        ()

    _ <- sqlExec
        "CREATE TABLE typed_sql_test_items (id UUID PRIMARY KEY, author_id UUID REFERENCES typed_sql_test_authors(id), name TEXT NOT NULL, views INT NOT NULL, score DOUBLE PRECISION, tags TEXT[] NOT NULL DEFAULT '{}')"
        ()

    _ <- sqlExec
        "INSERT INTO typed_sql_test_authors (id, name) VALUES (?, ?)"
        ( "00000000-0000-0000-0000-000000000001" :: UUID
        , "Alice" :: Text
        )

    _ <- sqlExec
        "INSERT INTO typed_sql_test_authors (id, name) VALUES (?, ?)"
        ( "00000000-0000-0000-0000-000000000002" :: UUID
        , "Bob" :: Text
        )

    _ <- sqlExec
        "INSERT INTO typed_sql_test_items (id, author_id, name, views, score, tags) VALUES (?, ?, ?, ?, ?, ?)"
        ( "10000000-0000-0000-0000-000000000001" :: UUID
        , Just ("00000000-0000-0000-0000-000000000001" :: UUID)
        , "First" :: Text
        , 5 :: Int
        , Just (1.5 :: Double)
        , ["red", "blue"] :: [Text]
        )

    _ <- sqlExec
        "INSERT INTO typed_sql_test_items (id, author_id, name, views, score, tags) VALUES (?, ?, ?, ?, ?, ?)"
        ( "10000000-0000-0000-0000-000000000002" :: UUID
        , Just ("00000000-0000-0000-0000-000000000001" :: UUID)
        , "Second" :: Text
        , 8 :: Int
        , Nothing :: Maybe Double
        , ["green"] :: [Text]
        )

    pure ()

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
            let fromRepoRoot = currentDirectory </> "ihp" </> "IHP" </> "TypedSql.hs"
            fromRepoExists <- doesFileExist fromRepoRoot
            if fromRepoExists
                then pure (currentDirectory </> "ihp")
                else fail "TypedSqlSpec: could not locate ihp package root"

ghciEnvironment :: IO [(String, String)]
ghciEnvironment = do
    baseEnvironment <- getEnvironment

    pgHost <- fromMaybe "" <$> lookupEnv "PGHOST"
    pgDatabase <- fromMaybe "" <$> lookupEnv "PGDATABASE"
    pgUser <- fromMaybe "" <$> lookupEnv "PGUSER"
    pgPort <- lookupEnv "PGPORT"

    let databaseUrlParts :: [String]
        databaseUrlParts =
            [ "host=" <> pgHost
            , "dbname=" <> pgDatabase
            , "user=" <> pgUser
            ] <> case pgPort of
                Just port | not (null port) -> ["port=" <> port]
                _ -> []

    let overrides :: [(String, String)]
        overrides =
            [ ("DATABASE_URL", Prelude.unwords databaseUrlParts)
            ]

    pure (applyEnvironmentOverrides overrides baseEnvironment)

applyEnvironmentOverrides :: [(String, String)] -> [(String, String)] -> [(String, String)]
applyEnvironmentOverrides overrides base =
    overrides <> filter (\(name, _) -> name `notElem` map fst overrides) base

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

compilePassModule :: Text
compilePassModule = Text.unlines
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "{-# LANGUAGE TypeApplications #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "module TypedSqlCompilePass where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.ModelSupport (Id', PrimaryKey)"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , ""
    , "type instance PrimaryKey \"typed_sql_test_items\" = UUID"
    , "type instance PrimaryKey \"typed_sql_test_authors\" = UUID"
    , ""
    , "qName :: TypedQuery Text"
    , "qName = [typedSql| SELECT name FROM typed_sql_test_items LIMIT 1 |]"
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
    , "qInnerJoin :: TypedQuery (Text, Text)"
    , "qInnerJoin = [typedSql| SELECT i.name, a.name FROM typed_sql_test_items i INNER JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]"
    , ""
    , "qLeftJoin :: TypedQuery (Text, Text)"
    , "qLeftJoin = [typedSql| SELECT i.name, a.name FROM typed_sql_test_items i LEFT JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]"
    , ""
    , "qRightJoin :: TypedQuery (Text, Text)"
    , "qRightJoin = [typedSql| SELECT i.name, a.name FROM typed_sql_test_items i RIGHT JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]"
    , ""
    , "qRightJoinCoalesced :: TypedQuery (Maybe Text, Text)"
    , "qRightJoinCoalesced = [typedSql| SELECT COALESCE(i.name, '(no-item)'), a.name FROM typed_sql_test_items i RIGHT JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]"
    ]

compileFailWrongScalarParameter :: Text
compileFailWrongScalarParameter = Text.unlines
    [ "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module TypedSqlCompileFailWrongScalarParameter where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , ""
    , "bad :: TypedQuery Text"
    , "bad = [typedSql| SELECT name FROM typed_sql_test_items WHERE views = ${(\"not an int\" :: Text)} LIMIT 1 |]"
    ]

compileFailWrongForeignKeyParameter :: Text
compileFailWrongForeignKeyParameter = Text.unlines
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "module TypedSqlCompileFailWrongForeignKeyParameter where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.ModelSupport (PrimaryKey)"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , ""
    , "type instance PrimaryKey \"typed_sql_test_items\" = UUID"
    , "type instance PrimaryKey \"typed_sql_test_authors\" = UUID"
    , ""
    , "bad :: TypedQuery Text"
    , "bad = [typedSql| SELECT name FROM typed_sql_test_items WHERE author_id = ${(\"not-an-id\" :: Text)} LIMIT 1 |]"
    ]

compileFailWrongInParameter :: Text
compileFailWrongInParameter = Text.unlines
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "module TypedSqlCompileFailWrongInParameter where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.ModelSupport (PrimaryKey)"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , ""
    , "type instance PrimaryKey \"typed_sql_test_items\" = UUID"
    , "type instance PrimaryKey \"typed_sql_test_authors\" = UUID"
    , ""
    , "bad :: TypedQuery Text"
    , "bad ="
    , "    let authorIds = [\"one\" :: Text, \"two\" :: Text]"
    , "    in [typedSql| SELECT name FROM typed_sql_test_items WHERE author_id IN (${authorIds}) LIMIT 1 |]"
    ]

compileFailInvalidPlaceholderExpression :: Text
compileFailInvalidPlaceholderExpression = Text.unlines
    [ "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module TypedSqlCompileFailInvalidPlaceholderExpression where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , ""
    , "bad :: TypedQuery Text"
    , "bad = [typedSql| SELECT name FROM typed_sql_test_items WHERE views = ${(} LIMIT 1 |]"
    ]

compileFailPlaceholderCountMismatch :: Text
compileFailPlaceholderCountMismatch = Text.unlines
    [ "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module TypedSqlCompileFailPlaceholderCountMismatch where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , ""
    , "bad :: TypedQuery Text"
    , "bad = [typedSql| SELECT name FROM typed_sql_test_items WHERE views = $1 LIMIT 1 |]"
    ]

compileFailSingleCompositeColumn :: Text
compileFailSingleCompositeColumn = Text.unlines
    [ "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module TypedSqlCompileFailSingleCompositeColumn where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , ""
    , "bad :: TypedQuery Text"
    , "bad = [typedSql| SELECT ROW(name, views)::typed_sql_test_pair FROM typed_sql_test_items LIMIT 1 |]"
    ]

compileFailUnknownColumn :: Text
compileFailUnknownColumn = Text.unlines
    [ "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module TypedSqlCompileFailUnknownColumn where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , ""
    , "bad :: TypedQuery Text"
    , "bad = [typedSql| SELECT no_such_column FROM typed_sql_test_items LIMIT 1 |]"
    ]

compileFailPrimaryKeyResultAnnotation :: Text
compileFailPrimaryKeyResultAnnotation = Text.unlines
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "module TypedSqlCompileFailPrimaryKeyResultAnnotation where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.ModelSupport (PrimaryKey)"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , ""
    , "type instance PrimaryKey \"typed_sql_test_items\" = UUID"
    , ""
    , "bad :: TypedQuery UUID"
    , "bad = [typedSql| SELECT id FROM typed_sql_test_items LIMIT 1 |]"
    ]

compileFailNullableResultAnnotation :: Text
compileFailNullableResultAnnotation = Text.unlines
    [ "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module TypedSqlCompileFailNullableResultAnnotation where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , ""
    , "bad :: TypedQuery Double"
    , "bad = [typedSql| SELECT score FROM typed_sql_test_items LIMIT 1 |]"
    ]

compileFailLeftJoinMaybeAnnotation :: Text
compileFailLeftJoinMaybeAnnotation = Text.unlines
    [ "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module TypedSqlCompileFailLeftJoinMaybeAnnotation where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , ""
    , "bad :: TypedQuery (Text, Maybe Text)"
    , "bad = [typedSql| SELECT i.name, a.name FROM typed_sql_test_items i LEFT JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]"
    ]

compileFailRightJoinMaybeAnnotation :: Text
compileFailRightJoinMaybeAnnotation = Text.unlines
    [ "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module TypedSqlCompileFailRightJoinMaybeAnnotation where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , ""
    , "bad :: TypedQuery (Maybe Text, Text)"
    , "bad = [typedSql| SELECT i.name, a.name FROM typed_sql_test_items i RIGHT JOIN typed_sql_test_authors a ON a.id = i.author_id ORDER BY a.name LIMIT 1 |]"
    ]

compileFailTupleArityMismatch :: Text
compileFailTupleArityMismatch = Text.unlines
    [ "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module TypedSqlCompileFailTupleArityMismatch where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , ""
    , "bad :: TypedQuery Text"
    , "bad = [typedSql| SELECT name, views FROM typed_sql_test_items LIMIT 1 |]"
    ]

compileFailBooleanResultAnnotation :: Text
compileFailBooleanResultAnnotation = Text.unlines
    [ "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module TypedSqlCompileFailBooleanResultAnnotation where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , ""
    , "bad :: TypedQuery Int"
    , "bad = [typedSql| SELECT author_id IS NULL FROM typed_sql_test_items LIMIT 1 |]"
    ]

compileFailBooleanNonMaybeAnnotation :: Text
compileFailBooleanNonMaybeAnnotation = Text.unlines
    [ "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module TypedSqlCompileFailBooleanNonMaybeAnnotation where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , ""
    , "bad :: TypedQuery Bool"
    , "bad = [typedSql| SELECT author_id IS NULL FROM typed_sql_test_items LIMIT 1 |]"
    ]

compileFailCountNonMaybeAnnotation :: Text
compileFailCountNonMaybeAnnotation = Text.unlines
    [ "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module TypedSqlCompileFailCountNonMaybeAnnotation where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , ""
    , "bad :: TypedQuery Integer"
    , "bad = [typedSql| SELECT COUNT(*) FROM typed_sql_test_items |]"
    ]

compileFailCoalesceNonMaybeAnnotation :: Text
compileFailCoalesceNonMaybeAnnotation = Text.unlines
    [ "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module TypedSqlCompileFailCoalesceNonMaybeAnnotation where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , ""
    , "bad :: TypedQuery (Text, Text)"
    , "bad = [typedSql| SELECT COALESCE(i.name, '(no-item)'), a.name FROM typed_sql_test_items i RIGHT JOIN typed_sql_test_authors a ON a.id = i.author_id LIMIT 1 |]"
    ]

compileFailLiteralNonMaybeAnnotation :: Text
compileFailLiteralNonMaybeAnnotation = Text.unlines
    [ "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module TypedSqlCompileFailLiteralNonMaybeAnnotation where"
    , ""
    , "import IHP.Prelude"
    , "import IHP.TypedSql (TypedQuery, typedSql)"
    , ""
    , "bad :: TypedQuery Int"
    , "bad = [typedSql| SELECT 1 |]"
    ]

runtimeModule :: Text
runtimeModule = Text.unlines
    [ "{-# LANGUAGE ImplicitParams #-}"
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
    , "module Main where"
    , ""
    , "import qualified Control.Exception as Exception"
    , "import IHP.Prelude"
    , "import IHP.Log.Types"
    , "import IHP.ModelSupport (ModelContext, createModelContext, releaseModelContext)"
    , "import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "    logger <- newLogger def { level = Warn }"
    , "    modelContext <- createModelContext 10 1 \"\" logger"
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
    , "        when ((leftJoinRows :: [(Text, Text)]) /= [(\"First\", \"Alice\"), (\"Second\", \"Alice\")]) do"
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
    , "        when ((rightJoinRows :: [(Text, Text)]) /= [(\"First\", \"Alice\"), (\"Second\", \"Alice\")]) do"
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
