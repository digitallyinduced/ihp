{-|
Module: IHP.IDE.CodeGen.MigrationGenerator
Description: Generates database migration sql files
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.IDE.CodeGen.MigrationGenerator where

import IHP.Prelude
import qualified System.Directory as Directory
import qualified Data.Text as Text
import qualified Data.Time.Clock.POSIX as POSIX
import qualified IHP.NameSupport as NameSupport
import qualified Data.Char as Char
import qualified System.Process as Process
import qualified IHP.IDE.SchemaDesigner.Parser as Parser
import IHP.IDE.SchemaDesigner.Types
import Text.Megaparsec
import IHP.IDE.SchemaDesigner.Compiler (compileSql)
import IHP.IDE.CodeGen.Types
import qualified IHP.FrameworkConfig as FrameworkConfig
import Paths_ihp_ide (getDataFileName)

buildPlan :: Text -> Maybe Text -> IO (Int, [GeneratorAction])
buildPlan description sqlStatements = buildPlan' True description sqlStatements

buildPlanWithoutIHPSchema :: Text -> Maybe Text -> IO (Int, [GeneratorAction])
buildPlanWithoutIHPSchema description sqlStatements = buildPlan' False description sqlStatements

buildPlan' :: Bool -> Text -> Maybe Text -> IO (Int, [GeneratorAction])
buildPlan' includeIHPSchema description sqlStatements = do
    revision <- round <$> POSIX.getPOSIXTime
    let slug = NameSupport.toSlug description
    let migrationFile = tshow revision <> (if isEmpty slug then "" else "-" <> slug) <> ".sql"

    migrationSql <- case sqlStatements of
        Just sql -> pure sql
        Nothing -> do
            databaseUrl <- cs <$> FrameworkConfig.defaultDatabaseUrl
            appDiff <- diffAppDatabase includeIHPSchema databaseUrl
            pure $ if isEmpty appDiff
                then "-- Write your SQL migration code in here\n"
                else compileSql appDiff
    pure (revision,
            [ EnsureDirectory { directory = "Application/Migration" }
            , CreateFile { filePath = "Application/Migration/" <> migrationFile, fileContent = migrationSql }
            ])

diffAppDatabase includeIHPSchema databaseUrl = do
    (Right schemaSql) <- Parser.parseSchemaSql
    (Right ihpSchemaSql) <- if includeIHPSchema
            then parseIHPSchema
            else pure (Right [])
    actualSchema <- getAppDBSchema databaseUrl

    let targetSchema = ihpSchemaSql <> schemaSql

    pure (diffSchemas targetSchema actualSchema)

parseIHPSchema :: IO (Either ByteString [Statement])
parseIHPSchema = do
    ihpSchemaSql <- findIHPSchemaSql
    Parser.parseSqlFile ihpSchemaSql

findIHPSchemaSql :: IO FilePath
findIHPSchemaSql = getDataFileName "IHPSchema.sql"

diffSchemas :: [Statement] -> [Statement] -> [Statement]
diffSchemas targetSchema' actualSchema' = (drop <> create)
            |> patchTable
            |> patchEnumType
            |> applyRenameTable
            |> removeImplicitDeletions actualSchema
            |> disableTransactionWhileAddingEnumValues
            |> applyReplaceFunction
    where
        create :: [Statement]
        create = targetSchema \\ actualSchema

        drop :: [Statement]
        drop = (actualSchema \\ targetSchema)
                |> mapMaybe toDropStatement

        targetSchema = removeNoise $ normalizeSchema targetSchema'
        actualSchema = removeNoise $ normalizeSchema actualSchema'

        -- | Replaces 'DROP TABLE x; CREATE TABLE x;' DDL sequences with a more efficient 'ALTER TABLE' sequence
        patchTable :: [Statement] -> [Statement]
        patchTable ((s@DropTable { tableName }):statements) =
                case createTable of
                    Just createTable -> (migrateTable createTable actualTable) <> patchTable (delete createTable statements)
                    Nothing -> s:(patchTable statements)
            where
                createTable :: Maybe Statement
                createTable = find isCreateTableStatement statements

                isCreateTableStatement :: Statement -> Bool
                isCreateTableStatement (StatementCreateTable { unsafeGetCreateTable = table }) | table.name == tableName = True
                isCreateTableStatement otherwise = False

                (Just actualTable) = actualSchema |> find \case
                        StatementCreateTable { unsafeGetCreateTable = table } -> table.name == tableName
                        otherwise                                                            -> False
        patchTable (s:rest) = s:(patchTable rest)
        patchTable [] = []

        -- | Replaces 'DROP TYPE x; CREATE TYPE x;' DDL sequences with a more efficient 'ALTER TYPE' sequence
        patchEnumType :: [Statement] -> [Statement]
        patchEnumType ((s@DropEnumType { name }):statements) =
                case createEnumType of
                    Just createEnumType -> (migrateEnum createEnumType actualEnumType) <> patchEnumType (delete createEnumType statements)
                    Nothing -> s:(patchEnumType statements)
            where
                createEnumType :: Maybe Statement
                createEnumType = find isCreateEnumTypeStatement statements

                isCreateEnumTypeStatement :: Statement -> Bool
                isCreateEnumTypeStatement CreateEnumType { name = n } = name == n
                isCreateEnumTypeStatement otherwise                   = False

                (Just actualEnumType) = actualSchema |> find \case
                        CreateEnumType { name = enum } -> enum == name
                        otherwise                      -> False
        patchEnumType (s:rest) = s:(patchEnumType rest)
        patchEnumType [] = []

        -- | Replaces 'DROP TABLE a; CREATE TABLE b;' DDL sequences with a more efficient 'ALTER TABLE a RENAME TO b' sequence if
        -- the tables have no differences except the name.
        applyRenameTable :: [Statement] -> [Statement]
        applyRenameTable ((s@DropTable { tableName }):statements) =
                case createTable of
                    Just createTable@(StatementCreateTable { unsafeGetCreateTable = createTable' }) ->
                        let
                            from = tableName
                            to = createTable'.name
                        in
                            (RenameTable { from, to }):(applyRenameTable (fixIdentifiers from to (delete createTable statements)))
                    Nothing -> s:(applyRenameTable statements)
            where
                createTable :: Maybe Statement
                createTable = find isCreateTableStatement statements

                isCreateTableStatement :: Statement -> Bool
                isCreateTableStatement (StatementCreateTable { unsafeGetCreateTable = table }) = (table.name /= actualTable'.name) && ((actualTable' :: CreateTable) { name = "" } == (table :: CreateTable) { name = "" })
                isCreateTableStatement otherwise = False

                (Just actualTable) = actualSchema |> find \case
                        StatementCreateTable { unsafeGetCreateTable = table } -> table.name == tableName
                        otherwise                                                            -> False

                actualTable' :: CreateTable
                actualTable' = case actualTable of
                    StatementCreateTable { unsafeGetCreateTable = table } -> table

                fixIdentifiers :: Text -> Text -> [Statement] -> [Statement]
                fixIdentifiers tableFrom tableTo statements = map fixIdentifier statements
                    where
                        fixIdentifier :: Statement -> Statement
                        fixIdentifier s@(DropConstraint { tableName }) | tableName == tableFrom = s { tableName = tableTo }
                        fixIdentifier s@(DropPolicy { tableName }) | tableName == tableFrom = s { tableName = tableTo }
                        fixIdentifier o = o
        applyRenameTable (s:rest) = s:(applyRenameTable rest)
        applyRenameTable [] = []

        toDropStatement :: Statement -> Maybe Statement
        toDropStatement StatementCreateTable { unsafeGetCreateTable = table } = Just DropTable { tableName = table.name }
        toDropStatement CreateEnumType { name } = Just DropEnumType { name }
        toDropStatement CreateIndex { indexName } = Just DropIndex { indexName }
        toDropStatement AddConstraint { tableName, constraint } = case constraint.name of
                Just constraintName -> Just DropConstraint { tableName, constraintName }
                Nothing -> Nothing
        toDropStatement CreatePolicy { tableName, name } = Just DropPolicy { tableName, policyName = name }
        toDropStatement CreateFunction { functionName } = Just DropFunction { functionName }
        toDropStatement CreateTrigger { name, tableName } = Just DropTrigger { name, tableName }
        toDropStatement CreateEventTrigger { name } = Just DropEventTrigger { name }
        toDropStatement otherwise = Nothing


        -- | Replaces 'DROP FUNCTION a; CREATE FUNCTION a ..;' DDL sequences with a more efficient 'CREATE OR REPLACE FUNCTION a' sequence if
        -- the function have no differences except the body.
        applyReplaceFunction :: [Statement] -> [Statement]
        applyReplaceFunction (DropFunction { functionName }:statements) =
            statements
                |> map \case
                    s@(CreateFunction { functionName = newFunctionName }) | newFunctionName == functionName -> (s { orReplace = True })
                    otherwise -> otherwise
                |> applyReplaceFunction
        applyReplaceFunction (s:rest) = s:(applyReplaceFunction rest)
        applyReplaceFunction [] = []

removeNoise = filter \case
        Comment {} -> False
        StatementCreateTable { unsafeGetCreateTable = CreateTable { name = "schema_migrations" } }      -> False
        AddConstraint { tableName = "schema_migrations" }                                               -> False
        CreateFunction { functionName } | "notify_" `Text.isPrefixOf` functionName                      -> False
        CreateTrigger { name } | any (`Text.isPrefixOf` name) ["did_update_", "did_delete_", "did_insert_", "ar_did_update_", "ar_did_delete_", "ar_did_insert_"] -> False
        StatementCreateTable { unsafeGetCreateTable = CreateTable { name = "large_pg_notifications" } } -> False
        CreateIndex { tableName = "large_pg_notifications" }                                            -> False
        _                                                                                               -> True

migrateTable :: Statement -> Statement -> [Statement]
migrateTable StatementCreateTable { unsafeGetCreateTable = targetTable } StatementCreateTable { unsafeGetCreateTable = actualTable } = migrateTable' targetTable actualTable
    where
        migrateTable' CreateTable { name = tableName, columns = targetColumns } CreateTable { columns = actualColumns } =
                (map dropColumn dropColumns <> map createColumn createColumns)
                    |> applyRenameColumn
                    |> applyMakeUnique
                    |> applySetDefault
                    |> applyToggleNull
            where

                createColumns :: [Column]
                createColumns = targetColumns \\ actualColumns

                dropColumns :: [Column]
                dropColumns = actualColumns \\ targetColumns

                createColumn :: Column -> Statement
                createColumn column = AddColumn { tableName, column }

                dropColumn :: Column -> Statement
                dropColumn column = DropColumn { tableName, columnName = column.name }

                applyRenameColumn (s@(DropColumn { columnName }):statements) = case matchingCreateColumn of
                        Just matchingCreateColumn -> RenameColumn { tableName, from = columnName, to = matchingCreateColumn.column.name } : (applyRenameColumn (filter ((/=) matchingCreateColumn) statements))
                        Nothing -> s:(applyRenameColumn statements)
                    where
                        matchingCreateColumn :: Maybe Statement
                        matchingCreateColumn = find isMatchingCreateColumn statements

                        isMatchingCreateColumn :: Statement -> Bool
                        isMatchingCreateColumn AddColumn { column = addColumn } = actualColumns
                                |> find \case
                                    Column { name } -> name == columnName
                                |> maybe False (\c -> (c :: Column) { name = addColumn.name } == addColumn)
                        isMatchingCreateColumn otherwise                          = False
                applyRenameColumn (statement:rest) = statement:(applyRenameColumn rest)
                applyRenameColumn [] = []

                -- | Emits 'ALTER TABLE table ADD UNIQUE (column);'
                --
                -- This function substitutes the following queries:
                --
                -- > ALTER TABLE table DROP COLUMN column;
                -- > ALTER TABLE table ADD COLUMN column UNIQUE;
                --
                -- With a more natural @ADD UNIQUE@:
                --
                -- > ALTER TABLE table ADD UNIQUE (column);
                --
                applyMakeUnique (s@(DropColumn { columnName }):statements) = case matchingCreateColumn of
                        Just matchingCreateColumn -> updateConstraint:(applyMakeUnique (filter ((/=) matchingCreateColumn) statements))
                        Nothing -> s:(applyMakeUnique statements)
                    where
                        dropColumn :: Column
                        (Just dropColumn) = actualColumns
                                |> find \case
                                    Column { name } -> name == columnName

                        updateConstraint = if dropColumn.isUnique
                            then DropConstraint { tableName, constraintName = tableName <> "_" <> (dropColumn.name) <> "_key" }
                            else AddConstraint { tableName, constraint = UniqueConstraint { name = Nothing, columnNames = [dropColumn.name] }, deferrable = Nothing, deferrableType = Nothing }

                        matchingCreateColumn :: Maybe Statement
                        matchingCreateColumn = find isMatchingCreateColumn statements

                        isMatchingCreateColumn :: Statement -> Bool
                        isMatchingCreateColumn AddColumn { column = addColumn } = addColumn { isUnique = False } == dropColumn { isUnique = False }
                        isMatchingCreateColumn otherwise                        = False
                applyMakeUnique (statement:rest) = statement:(applyMakeUnique rest)
                applyMakeUnique [] = []

                -- | Emits "ALTER TABLE table ALTER COLUMN column SET DEFAULT 'value'"
                --
                -- This function substitutes the following queries:
                --
                -- > ALTER TABLE table DROP COLUMN column;
                -- > ALTER TABLE table ADD COLUMN column;
                --
                -- With a more natural @SET DEFAULT@:
                --
                -- > ALTER TABLE table ALTER COLUMN column SET DEFAULT 'value'
                --
                applySetDefault (s@(DropColumn { columnName }):statements) = case matchingCreateColumn of
                        Just matchingCreateColumn -> case matchingCreateColumn.column.defaultValue of
                            Just value -> SetDefaultValue { tableName, columnName, value }:rest
                            Nothing -> DropDefaultValue { tableName, columnName }:rest
                            where
                                rest = applySetDefault (filter ((/=) matchingCreateColumn) statements)
                        Nothing -> s:(applySetDefault statements)
                    where
                        dropColumn :: Column
                        (Just dropColumn) = actualColumns
                                |> find \case
                                    Column { name } -> name == columnName

                        matchingCreateColumn :: Maybe Statement
                        matchingCreateColumn = find isMatchingCreateColumn statements

                        isMatchingCreateColumn :: Statement -> Bool
                        isMatchingCreateColumn AddColumn { column = addColumn } = (addColumn { defaultValue = Nothing } :: Column) == (dropColumn { defaultValue = Nothing } :: Column)
                        isMatchingCreateColumn otherwise                        = False
                applySetDefault (statement:rest) = statement:(applySetDefault rest)
                applySetDefault [] = []

                -- | Emits 'ALTER TABLE table ALTER COLUMN column DROP NOT NULL'
                --
                -- This function substitutes the following queries:
                --
                -- > ALTER TABLE table DROP COLUMN column;
                -- > ALTER TABLE table ADD COLUMN column;
                --
                -- With a more natural @DROP NOT NULL@:
                --
                -- > ALTER TABLE table ALTER COLUMN column DROP NOT NULL
                --
                applyToggleNull (s@(DropColumn { columnName }):statements) = case matchingCreateColumn of
                        Just matchingCreateColumn -> updateConstraint:(applyToggleNull (filter ((/=) matchingCreateColumn) statements))
                        Nothing -> s:(applyToggleNull statements)
                    where
                        dropColumn :: Column
                        (Just dropColumn) = actualColumns
                                |> find \case
                                    Column { name } -> name == columnName

                        updateConstraint = if dropColumn.notNull
                            then DropNotNull { tableName, columnName = dropColumn.name }
                            else SetNotNull { tableName, columnName = dropColumn.name }

                        matchingCreateColumn :: Maybe Statement
                        matchingCreateColumn = find isMatchingCreateColumn statements

                        isMatchingCreateColumn :: Statement -> Bool
                        isMatchingCreateColumn AddColumn { column = addColumn } = addColumn `eqColumnExceptNull` dropColumn
                        isMatchingCreateColumn otherwise                        = False

                        eqColumnExceptNull :: Column -> Column -> Bool
                        eqColumnExceptNull colA colB = (normalizeCol colA) == (normalizeCol colB)
                            where
                                normalizeCol col = col { notNull = False, defaultValue = Just (VarExpression "null") }
                applyToggleNull (statement:rest) = statement:(applyToggleNull rest)
                applyToggleNull [] = []

migrateEnum :: Statement -> Statement -> [Statement]
migrateEnum CreateEnumType { name, values = targetValues } CreateEnumType { values = actualValues } = map addValue newValues
    where
        newValues :: [Text]
        newValues = targetValues \\ actualValues

        addValue :: Text -> Statement
        addValue value = AddValueToEnumType { enumName = name, newValue = value, ifNotExists = True }

getAppDBSchema :: Text -> IO [Statement]
getAppDBSchema databaseUrl = do
    sql <- dumpAppDatabaseSchema databaseUrl
    case parseDumpedSql sql of
        Left error -> fail (cs error)
        Right result -> pure result

-- | Returns the DDL statements of the locally running dev db
--
-- Basically does the same as @make dumpdb@ but returns the output as a string
dumpAppDatabaseSchema :: Text -> IO Text
dumpAppDatabaseSchema databaseUrl = do
    projectDir <- Directory.getCurrentDirectory
    cs <$> Process.readProcess "pg_dump" ["-s", "--no-owner", "--no-acl", cs databaseUrl] []

parseDumpedSql :: Text -> (Either ByteString [Statement])
parseDumpedSql sql =
    case runParser Parser.parseDDL "pg_dump" sql of
        Left error -> Left (cs $ errorBundlePretty error)
        Right r -> Right r

normalizeSchema :: [Statement] -> [Statement]
normalizeSchema statements = map normalizeStatement statements
        |> concat
        |> normalizePrimaryKeys

normalizeStatement :: Statement -> [Statement]
normalizeStatement StatementCreateTable { unsafeGetCreateTable = table } = StatementCreateTable { unsafeGetCreateTable = normalizedTable } : normalizeTableRest
    where
        (normalizedTable, normalizeTableRest) = normalizeTable table
normalizeStatement AddConstraint { tableName, constraint, deferrable, deferrableType } = [ AddConstraint { tableName, constraint = normalizeConstraint tableName constraint, deferrable, deferrableType } ]
normalizeStatement CreateEnumType { name, values } = [ CreateEnumType { name = Text.toLower name, values = map Text.toLower values } ]
normalizeStatement CreatePolicy { name, action, tableName, using, check } = [ CreatePolicy { name = truncateIdentifier name, tableName, using = (unqualifyExpression tableName . normalizeExpression) <$> using, check = (unqualifyExpression tableName . normalizeExpression) <$> check, action = normalizePolicyAction action } ]
normalizeStatement CreateIndex { columns, indexType, indexName, .. } = [ CreateIndex { columns = map normalizeIndexColumn columns, indexType = normalizeIndexType indexType, indexName = truncateIdentifier indexName, .. } ]
normalizeStatement CreateFunction { .. } = [ CreateFunction { orReplace = False, language = Text.toUpper language, functionBody = removeIndentation $ normalizeNewLines functionBody, .. } ]
normalizeStatement otherwise = [otherwise]

normalizePolicyAction (Just PolicyForAll) = Nothing
normalizePolicyAction otherwise = otherwise

normalizeTable :: CreateTable -> (CreateTable, [Statement])
normalizeTable table@(CreateTable { .. }) = ( CreateTable { columns = fst normalizedColumns, constraints = normalizedTableConstraints, .. }, (concat $ (snd normalizedColumns)) <> normalizedConstraintsStatements )
    where
        normalizedColumns = columns
                |> map (normalizeColumn table)
                |> unzip

        -- pg_dump typically inlines the table constraints into the CREATE TABLE statement like this:
        --
        -- > CREATE TABLE public.a (
        -- >     id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
        -- >     CONSTRAINT c CHECK 1=1
        -- > );
        --
        -- In IHP we typically split this into a 'CREATE TABLE' statement and into a 'ALTER TABLE .. ADD CONSTRAINT ..' statement.
        --
        -- We normalize the above statement to this:
        --
        -- > CREATE TABLE public.a (
        -- >     id uuid DEFAULT public.uuid_generate_v4() NOT NULL
        -- > );
        -- > ALTER TABLE a ADD CONSTRAINT c CHECK 1=1;
        normalizedCheckConstraints :: [Either Statement Constraint]
        normalizedCheckConstraints = constraints
                |> map \case
                    checkConstraint@(CheckConstraint {}) -> Left AddConstraint { tableName = name, constraint = checkConstraint, deferrable = Nothing, deferrableType = Nothing }
                    otherConstraint -> Right otherConstraint

        normalizedTableConstraints :: [Constraint]
        normalizedTableConstraints =
            normalizedCheckConstraints
            |> mapMaybe \case
                Left _ -> Nothing
                Right c -> Just c

        normalizedConstraintsStatements :: [Statement]
        normalizedConstraintsStatements =
            normalizedCheckConstraints
            |> mapMaybe \case
                Right _ -> Nothing
                Left c -> Just c

normalizeConstraint :: Text -> Constraint -> Constraint
normalizeConstraint _ ForeignKeyConstraint { name, columnName, referenceTable, referenceColumn, onDelete } = ForeignKeyConstraint { name = truncateIdentifier <$> name, columnName = Text.toLower columnName, referenceTable = Text.toLower referenceTable, referenceColumn = fmap Text.toLower referenceColumn, onDelete = Just (fromMaybe NoAction onDelete) }
normalizeConstraint tableName constraint@(UniqueConstraint { name = Just uniqueName, columnNames }) | length columnNames > 1 =
        -- Single column UNIQUE constraints like:
        --
        -- > ALTER TABLE ONLY public.users ADD CONSTRAINT users_github_user_id_key UNIQUE (github_user_id);
        --
        -- are packed into the CREATE TABLE definition:
        --
        -- > CREATE TABLE users (
        -- >     id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
        -- >     github_user_id INT DEFAULT NULL UNIQUE
        -- > );
        --
        -- For multi columns we need to normalize the name, e.g.:
        --
        -- > ALTER TABLE days ADD UNIQUE (category_id, date);
        --
        -- Is the same as:
        --
        -- > ALTER TABLE ONLY public.days ADD CONSTRAINT days_category_id_date_key UNIQUE (category_id, date);
        --
        let
            defaultName = ([tableName] <> columnNames <> ["key"])
                    |> Text.intercalate "_" 
        in
            if uniqueName == defaultName
                then constraint { name = Nothing }
                else constraint
normalizeConstraint _ otherwise = otherwise

normalizeColumn :: CreateTable -> Column -> (Column, [Statement])
normalizeColumn table Column { name, columnType, defaultValue, notNull, isUnique, generator } = (Column { name = normalizeName name, columnType = normalizeSqlType columnType, defaultValue = normalizedDefaultValue, notNull, isUnique = False, generator = normalizeColumnGenerator <$> generator }, uniqueConstraint)
    where
        uniqueConstraint =
            if isUnique
                then [ AddConstraint { tableName = table.name, constraint = UniqueConstraint (Just $ (table.name) <>"_" <> name <> "_key") [name], deferrable = Nothing, deferrableType = Nothing } ]
                else []

        normalizeName :: Text -> Text
        normalizeName nane = Text.toLower name

        normalizedDefaultValue = case defaultValue of
            Just defaultValue -> Just (normalizeExpression defaultValue)
            Nothing -> if notNull || isJust generator
                then Nothing
                else Just (VarExpression "null") -- pg_dump columns don't have an explicit default null value

normalizeColumnGenerator :: ColumnGenerator -> ColumnGenerator
normalizeColumnGenerator generator@(ColumnGenerator { generate }) = generator { generate = normalizeExpression generate }

normalizeExpression :: Expression -> Expression
normalizeExpression e@(TextExpression {}) = e
normalizeExpression (VarExpression var) = VarExpression (Text.toLower var)
normalizeExpression (CallExpression function args) = CallExpression (Text.toLower function) (map normalizeExpression args)
normalizeExpression (NotEqExpression a b) = NotEqExpression (normalizeExpression a) (normalizeExpression b)
normalizeExpression (EqExpression a b) = EqExpression (normalizeExpression a) (normalizeExpression b)
normalizeExpression (AndExpression a b) = AndExpression (normalizeExpression a) (normalizeExpression b)
normalizeExpression (IsExpression a b) = IsExpression (normalizeExpression a) (normalizeExpression b)
normalizeExpression (InExpression a b) = InExpression (normalizeExpression a) (normalizeExpression b)
normalizeExpression (NotExpression a) = NotExpression (normalizeExpression a)
normalizeExpression (OrExpression a b) = OrExpression (normalizeExpression a) (normalizeExpression b)
normalizeExpression (LessThanExpression a b) = LessThanExpression (normalizeExpression a) (normalizeExpression b)
normalizeExpression (LessThanOrEqualToExpression a b) = LessThanOrEqualToExpression (normalizeExpression a) (normalizeExpression b)
normalizeExpression (GreaterThanExpression a b) = GreaterThanExpression (normalizeExpression a) (normalizeExpression b)
normalizeExpression (GreaterThanOrEqualToExpression a b) = GreaterThanOrEqualToExpression (normalizeExpression a) (normalizeExpression b)
normalizeExpression e@(DoubleExpression {}) = e
normalizeExpression e@(IntExpression {}) = e
normalizeExpression (ConcatenationExpression a b) = ConcatenationExpression (normalizeExpression a) (normalizeExpression b)
-- Enum default values from pg_dump always have an explicit type cast. Inside the Schema.sql they typically don't have those.
-- Therefore we remove these typecasts here
--
-- 'job_status_not_started'::public.job_status => 'job_status_not_started'
--
normalizeExpression (TypeCastExpression a b) = normalizeExpression a
normalizeExpression (SelectExpression Select { columns, from, whereClause, alias }) = SelectExpression Select { columns = resolveAlias' <$> (normalizeExpression <$> columns), from = normalizeFrom from, whereClause = resolveAlias' (normalizeExpression whereClause), alias = Nothing }
    where
        -- Turns a `SELECT 1 FROM a` into `SELECT 1 FROM public.a`
        normalizeFrom (VarExpression a) = DotExpression (VarExpression "public") a
        normalizeFrom otherwise = normalizeExpression otherwise

        resolveAlias' = resolveAlias alias (unqualifiedName from)

        unqualifiedName :: Expression -> Expression
        unqualifiedName (DotExpression (VarExpression _) name) = VarExpression name
        unqualifiedName name = name
normalizeExpression (DotExpression a b) = DotExpression (normalizeExpression a) b
normalizeExpression (ExistsExpression a) = ExistsExpression (normalizeExpression a)

-- | Replaces @table.field@ with just @field@
--
-- >>> unqualifyExpression "servers" (sql "SELECT * FROM servers WHERE servers.is_public")
-- sql "SELECT * FROM servers WHERE is_public"
--
unqualifyExpression :: Text -> Expression -> Expression
unqualifyExpression scope expression = doUnqualify expression
    where
        doUnqualify e@(TextExpression {}) = e
        doUnqualify e@(VarExpression {}) = e
        doUnqualify (CallExpression function args) = CallExpression function (map doUnqualify args)
        doUnqualify (NotEqExpression a b) = NotEqExpression (doUnqualify a) (doUnqualify b)
        doUnqualify (EqExpression a b) = EqExpression (doUnqualify a) (doUnqualify b)
        doUnqualify (AndExpression a b) = AndExpression (doUnqualify a) (doUnqualify b)
        doUnqualify (IsExpression a b) = IsExpression (doUnqualify a) (doUnqualify b)
        doUnqualify (InExpression a b) = InExpression (doUnqualify a) (doUnqualify b)
        doUnqualify (NotExpression a) = NotExpression (doUnqualify a)
        doUnqualify (OrExpression a b) = OrExpression (doUnqualify a) (doUnqualify b)
        doUnqualify (LessThanExpression a b) = LessThanExpression (doUnqualify a) (doUnqualify b)
        doUnqualify (LessThanOrEqualToExpression a b) = LessThanOrEqualToExpression (doUnqualify a) (doUnqualify b)
        doUnqualify (GreaterThanExpression a b) = GreaterThanExpression (doUnqualify a) (doUnqualify b)
        doUnqualify (GreaterThanOrEqualToExpression a b) = GreaterThanOrEqualToExpression (doUnqualify a) (doUnqualify b)
        doUnqualify e@(DoubleExpression {}) = e
        doUnqualify e@(IntExpression {}) = e
        doUnqualify (ConcatenationExpression a b) = ConcatenationExpression (doUnqualify a) (doUnqualify b)
        doUnqualify (TypeCastExpression a b) = TypeCastExpression (doUnqualify a) b
        doUnqualify e@(SelectExpression Select { columns, from, whereClause, alias }) =
            let recurse = case from of
                    VarExpression fromName -> doUnqualify . unqualifyExpression fromName
                    DotExpression (VarExpression "public") fromName -> doUnqualify . unqualifyExpression fromName
                    _ -> doUnqualify
            in
                SelectExpression Select { columns = (recurse <$> columns), from = from, whereClause = recurse whereClause, alias }
        doUnqualify (ExistsExpression a) = ExistsExpression (doUnqualify a)
        doUnqualify (DotExpression (VarExpression scope') b) | scope == scope' = VarExpression b
        doUnqualify (DotExpression a b) = DotExpression (doUnqualify a) b


resolveAlias :: Maybe Text -> Expression -> Expression -> Expression
resolveAlias (Just alias) fromExpression expression =
    let
        rec = resolveAlias (Just alias) fromExpression
    in case expression of
        e@(TextExpression {}) -> e
        e@(VarExpression var) -> if var == alias
                    then fromExpression
                    else e
        e@(CallExpression function args) -> CallExpression function (map rec args)
        e@(NotEqExpression a b) -> NotEqExpression (rec a) (rec b)
        e@(EqExpression a b) -> EqExpression (rec a) (rec b)
        e@(AndExpression a b) -> AndExpression (rec a) (rec b)
        e@(IsExpression a b) -> IsExpression (rec a) (rec b)
        e@(InExpression a b) -> InExpression (rec a) (rec b)
        e@(NotExpression a) -> NotExpression (rec a)
        e@(OrExpression a b) -> OrExpression (rec a) (rec b)
        e@(LessThanExpression a b) -> LessThanExpression (rec a) (rec b)
        e@(LessThanOrEqualToExpression a b) -> LessThanOrEqualToExpression (rec a) (rec b)
        e@(GreaterThanExpression a b) -> GreaterThanExpression (rec a) (rec b)
        e@(GreaterThanOrEqualToExpression a b) -> GreaterThanOrEqualToExpression (rec a) (rec b)
        e@(DoubleExpression {}) -> e
        e@(IntExpression {}) -> e
        e@(TypeCastExpression a b) -> (TypeCastExpression (rec a) b)
        e@(SelectExpression Select { columns, from, whereClause, alias }) -> SelectExpression Select { columns = rec <$> columns, from = rec from, whereClause = rec whereClause, alias = alias }
        e@(DotExpression a b) -> DotExpression (rec a) b
        e@(ExistsExpression a) -> ExistsExpression (rec a)
resolveAlias Nothing fromExpression expression = expression

normalizeSqlType :: PostgresType -> PostgresType
normalizeSqlType (PCustomType customType) = PCustomType (Text.toLower customType)
normalizeSqlType PBigserial = PBigInt
normalizeSqlType PSerial = PInt
normalizeSqlType otherwise = otherwise

migrationPathFromPlan :: [GeneratorAction] -> Text
migrationPathFromPlan plan =
        let (Just path) = plan
                |> find \case
                    CreateFile {} -> True
                    otherwise     -> False
                |> \case
                    Just CreateFile { filePath } -> Just filePath
                    otherwise                    -> Nothing
        in
            path

-- | Removes @ALTER TABLE .. ADD CONSTRAINT .._pkey PRIMARY KEY (id);@ and moves it into the 'primaryKeyConstraint' field of the 'CreateTable'  statement
--
-- pg_dump dumps a table like this:
--
-- > CREATE TABLE a (
-- >     id uuid DEFAULT uuid_generate_v4() NOT NULL
-- > );
-- >
-- > ALTER TABLE a ADD CONSTRAINT users_pkey PRIMARY KEY (id);
--
-- This function basically removes the @ALTER TABLE@ statements and moves the primary key directly into the @CREATE TABLE@ statement:
--
-- > CREATE TABLE a (
-- >     id uuid DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
-- > );
--
normalizePrimaryKeys :: [Statement] -> [Statement]
normalizePrimaryKeys statements = reverse $ normalizePrimaryKeys' [] statements
    where
        normalizePrimaryKeys' normalizedStatements ((AddConstraint { tableName, constraint = AlterTableAddPrimaryKey { primaryKeyConstraint } }):rest) =
            normalizePrimaryKeys'
                (normalizedStatements
                    |> map \case
                        StatementCreateTable { unsafeGetCreateTable = table@(CreateTable { name }) } | name == tableName -> StatementCreateTable { unsafeGetCreateTable = addPK primaryKeyConstraint table }
                        otherwise -> otherwise
                )
                (rest)
        normalizePrimaryKeys' normalizedStatements (statement:rest) = normalizePrimaryKeys' (statement:normalizedStatements) rest
        normalizePrimaryKeys' normalizedStatements [] = normalizedStatements

        addPK :: PrimaryKeyConstraint -> CreateTable -> CreateTable
        addPK PrimaryKeyConstraint { primaryKeyColumnNames } table@(CreateTable { primaryKeyConstraint = PrimaryKeyConstraint { primaryKeyColumnNames = existingPKs } }) = table { primaryKeyConstraint = PrimaryKeyConstraint { primaryKeyColumnNames = existingPKs <> primaryKeyColumnNames } }


-- | Removes @DROP INDEX ..@ statements and other that appear after a @DROP TABLE@ statement. The @DROP TABLE ..@ statement
-- itself already removes indexes and foreigns keys on that table. So an @DROP INDEX ..@ would then fail.
--
-- Shrinks a sequence like this:
--
-- > DROP TABLE a;
-- > DROP INDEX some_index_on_table_a;
-- > ALTER TABLE a DROP CONSTRAINT some_constraint_on_table_a;
--
-- Into this:
--
-- > DROP TABLE a;
--
removeImplicitDeletions :: [Statement] -> [Statement] -> [Statement]
removeImplicitDeletions actualSchema (statement@dropStatement:rest) | isDropStatement dropStatement = statement:(filter isImplicitlyDeleted rest)
    where
        isImplicitlyDeleted (DropIndex { indexName }) = case findIndexByName indexName of
                Just CreateIndex { tableName = indexTableName, columns = indexColumns } -> indexTableName /= dropTableName && (
                        case dropColumnName of
                            Just dropColumnName -> indexColumns
                                    |> find (\IndexColumn { column } -> column == VarExpression dropColumnName)
                                    |> isNothing
                            Nothing -> True
                    )
                Nothing -> True
        isImplicitlyDeleted (DropConstraint { tableName = constraintTableName }) = constraintTableName /= dropTableName
        isImplicitlyDeleted (DropPolicy { tableName = policyTableName }) = not (isNothing dropColumnName && policyTableName == dropTableName)
        isImplicitlyDeleted otherwise = True

        findIndexByName :: Text -> Maybe Statement
        findIndexByName name = find (isIndex name) actualSchema

        isIndex :: Text -> Statement -> Bool
        isIndex name CreateIndex { indexName } = indexName == name
        isIndex _    _                         = False

        isDropStatement DropTable {} = True
        isDropStatement DropColumn {} = True
        isDropStatement _ = False

        (dropTableName, dropColumnName) = case dropStatement of
            DropTable { tableName } -> (tableName, Nothing)
            DropColumn { tableName, columnName } -> (tableName, Just columnName)
removeImplicitDeletions actualSchema (statement:rest) = statement:(removeImplicitDeletions actualSchema rest)
removeImplicitDeletions actualSchema [] = []

-- | Moves statements that add enum values outside the database transaction
--
-- When IHP generates a migration that contains a statement like this:
--
-- > ALTER TYPE my_enum ADD VALUE 'some_value';
--
-- the migration will fail with this error:
--
-- > Query (89.238182ms): "BEGIN" ()
-- > migrate: SqlError {sqlState = "25001", sqlExecStatus = FatalError, sqlErrorMsg = "ALTER TYPE ... ADD cannot run inside a transaction block", sqlErrorDetail = "", sqlErrorHint = ""}
--
-- This function moves the @ADD VALUE@ statement outside the main database transaction:
--
-- > COMMIT; -- Commit the transaction previously started by IHP
-- > ALTER TYPE my_enum ADD VALUE 'some_value';
-- > BEGIN; -- Restart the connection as IHP will also try to run it's own COMMIT
--
disableTransactionWhileAddingEnumValues :: [Statement] -> [Statement]
disableTransactionWhileAddingEnumValues statements =
        if isEmpty addEnumValueStatements
            then otherStatements
            else [Comment " Commit the transaction previously started by IHP", Commit] <> (map enableIfNotExists addEnumValueStatements) <> [Comment " Restart the connection as IHP will also try to run it's own COMMIT", Begin] <> otherStatements
    where
        (addEnumValueStatements, otherStatements) = partition isAddEnumValueStatement statements

        isAddEnumValueStatement AddValueToEnumType {} = True
        isAddEnumValueStatement otherwise = False

        enableIfNotExists statement@(AddValueToEnumType { .. }) = statement { ifNotExists = True }
        enableIfNotExists otherwise = otherwise

normalizeIndexType :: Maybe IndexType -> Maybe IndexType
normalizeIndexType (Just Btree) = Nothing
normalizeIndexType indexType = indexType

normalizeIndexColumn :: IndexColumn -> IndexColumn
normalizeIndexColumn IndexColumn { column, columnOrder } =
    IndexColumn
        { column = normalizeExpression column
        , columnOrder = normalizeIndexColumnOrder columnOrder
        }

normalizeIndexColumnOrder :: [IndexColumnOrder] -> [IndexColumnOrder]
normalizeIndexColumnOrder columnOrder = columnOrder |> filter (/=Asc)

normalizeNewLines :: Text -> Text
normalizeNewLines text =
    text
    |> Text.replace "\r\n" "\n"
    |> Text.replace "\r" "\n"


removeIndentation :: Text -> Text
removeIndentation text =
        lines
        |> map (\line -> if line /= "BEGIN"
                then Text.drop spacesToDrop line
                else line)
        |> Text.unlines
        |> Text.dropAround (\c -> c == '\n')
    where
        lines :: [Text]
        lines = text
                |> Text.lines
        spaces :: [Int]
        spaces = lines
                |> filter (\line -> line /= "" && line /= "BEGIN")
                |> map (\line -> Text.length (Text.takeWhile Char.isSpace line))
        spacesToDrop = spaces |> minimum

-- | Postgres truncates identifiers longer than 63 characters.
--
-- This function truncates a Text to 63 chars max. This way we avoid unnecssary changes in the generated migrations.
truncateIdentifier :: Text -> Text
truncateIdentifier identifier =
    if Text.length identifier > 63
        then Text.take 63 identifier
        else identifier
