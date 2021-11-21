{-|
Module: IHP.IDE.CodeGen.MigrationGenerator
Description: Generates database migration sql files
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.IDE.CodeGen.MigrationGenerator where

import IHP.Prelude
import qualified System.Directory as Directory
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import IHP.ModelSupport hiding (withTransaction)
import qualified Data.Time.Clock.POSIX as POSIX
import qualified IHP.NameSupport as NameSupport
import qualified Data.Char as Char
import IHP.Log.Types
import IHP.SchemaMigration
import qualified System.Process as Process
import qualified IHP.IDE.SchemaDesigner.Parser as Parser
import IHP.IDE.SchemaDesigner.Types
import Text.Megaparsec

-- | Generates a new migration @.sql@ file in @Application/Migration@
createMigration :: Text -> IO Migration
createMigration description = do
    revision <- round <$> POSIX.getPOSIXTime
    let slug = NameSupport.toSlug description
    let migrationFile = tshow revision <> (if isEmpty slug then "" else "-" <> slug) <> ".sql"
    Directory.createDirectoryIfMissing False "Application/Migration"
    Text.writeFile ("Application/Migration/" <> cs migrationFile) "-- Write your SQL migration code in here\n"
    pure Migration { .. }

diffAppDatabase = do
    (Right targetSchema) <- Parser.parseSchemaSql
    actualSchema <- getAppDBSchema

    pure (diffSchemas targetSchema actualSchema)

diffSchemas :: [Statement] -> [Statement] -> [Statement]
diffSchemas targetSchema' actualSchema' =
    targetSchema
        |> removeComments -- SQL Comments are not used in the diff
        |> (\schema -> schema \\ actualSchema) -- Get rid of all statements that exist in both schemas, there's nothing we need to do to them
        |> map statementToMigration
        |> concat
    where
        targetSchema = normalizeSchema targetSchema'
        actualSchema = normalizeSchema actualSchema'
        removeComments = filter \case
                Comment {} -> False
                _          -> True

        statementToMigration :: Statement -> [Statement]
        statementToMigration statement@(StatementCreateTable { unsafeGetCreateTable = table }) = 
                case actualTable of
                    Just actualTable -> migrateTable statement actualTable
                    Nothing -> [statement]
            where
                actualTable = actualSchema |> find \case
                        StatementCreateTable { unsafeGetCreateTable = CreateTable { name } } -> name == get #name table
                        otherwise                                                            -> False
        statementToMigration statement@(CreateEnumType { name, values }) = 
                case actualEnum of
                    Just actualEnum -> migrateEnum statement actualEnum
                    Nothing -> [statement]
            where
                actualEnum = actualSchema |> find \case
                        CreateEnumType { name = actualName } -> name == actualName
                        otherwise                            -> False
        statementToMigration statement = [statement]

migrateTable :: Statement -> Statement -> [Statement]
migrateTable StatementCreateTable { unsafeGetCreateTable = targetTable } StatementCreateTable { unsafeGetCreateTable = actualTable } = migrateTable' targetTable actualTable
    where
        migrateTable' CreateTable { name = tableName, columns = targetColumns } CreateTable { columns = actualColumns } =
                (map createColumn createColumns <> map dropColumn dropColumns)
                    |> applyRenameColumn
            where

                createColumns :: [Column]
                createColumns = targetColumns \\ actualColumns

                dropColumns :: [Column]
                dropColumns = actualColumns \\ targetColumns

                createColumn :: Column -> Statement
                createColumn column = AddColumn { tableName, column }

                dropColumn :: Column -> Statement
                dropColumn column = DropColumn { tableName, columnName = get #name column }

                applyRenameColumn (s@(AddColumn { column }):statements) = case matchingDropColumn of
                        Just matchingDropColumn -> RenameColumn { tableName, from = get #columnName matchingDropColumn, to = get #name column } : (applyRenameColumn (filter ((/=) matchingDropColumn) statements))
                        Nothing -> s:(applyRenameColumn statements)
                    where
                        matchingDropColumn :: Maybe Statement
                        matchingDropColumn = find isMatchingDropColumn statements

                        isMatchingDropColumn :: Statement -> Bool
                        isMatchingDropColumn DropColumn { columnName } = actualColumns
                                |> find \case
                                    Column { name } -> name == columnName
                                    otherwise       -> False
                                |> maybe False (\c -> (c :: Column) { name = get #name column } == column)
                        isMatchingDropColumn otherwise                          = False
                applyRenameColumn (statement:rest) = statement:(applyRenameColumn rest)
                applyRenameColumn [] = []

migrateEnum :: Statement -> Statement -> [Statement]
migrateEnum CreateEnumType { name, values = targetValues } CreateEnumType { values = actualValues } = map addValue newValues
    where
        newValues :: [Text]
        newValues = targetValues \\ actualValues
        
        addValue :: Text -> Statement
        addValue value = AddValueToEnumType { enumName = name, newValue = value }

getAppDBSchema :: IO [Statement]
getAppDBSchema = do
    sql <- dumpAppDatabaseSchema
    case parseDumpedSql sql of
        Left error -> fail (cs error)
        Right result -> pure result

-- | Returns the DDL statements of the locally running dev db
--
-- Basically does the same as @make dumpdb@ but returns the output as a string
dumpAppDatabaseSchema :: IO Text
dumpAppDatabaseSchema = do
    projectDir <- Directory.getCurrentDirectory
    cs <$> Process.readProcess "pg_dump" ["-s", "--no-owner", "-h", projectDir <> "/build/db", "app"] []

parseDumpedSql :: Text -> (Either ByteString [Statement])
parseDumpedSql sql =
    case runParser Parser.parseDDL "pg_dump" sql of
        Left error -> Left (cs $ errorBundlePretty error)
        Right r -> Right r

normalizeSchema :: [Statement] -> [Statement]
normalizeSchema statements = map normalizeStatement statements

normalizeStatement :: Statement -> Statement
normalizeStatement StatementCreateTable { unsafeGetCreateTable = table } = StatementCreateTable { unsafeGetCreateTable = normalizeTable table }
normalizeStatement AddConstraint { tableName, constraintName, constraint } = AddConstraint { tableName, constraintName, constraint = normalizeConstraint constraint }
normalizeStatement CreateEnumType { name, values } = CreateEnumType { name = Text.toLower name, values = map Text.toLower values }
normalizeStatement otherwise = otherwise

normalizeTable :: CreateTable -> CreateTable
normalizeTable CreateTable { .. } = CreateTable { columns = map normalizeColumn columns, .. }

normalizeConstraint :: Constraint -> Constraint
normalizeConstraint ForeignKeyConstraint { columnName, referenceTable, referenceColumn, onDelete } = ForeignKeyConstraint { columnName = Text.toLower columnName, referenceTable = Text.toLower referenceTable, referenceColumn = fmap Text.toLower referenceColumn, onDelete = Just (fromMaybe NoAction onDelete) }
normalizeConstraint otherwise = otherwise

normalizeColumn :: Column -> Column
normalizeColumn Column { name, columnType, defaultValue, notNull, isUnique } = Column { name = normalizeName name, columnType = normalizeSqlType columnType, defaultValue = normalizedDefaultValue, notNull, isUnique }
    where
        normalizeName :: Text -> Text
        normalizeName nane = Text.toLower name

        normalizedDefaultValue = case defaultValue of
            Just defaultValue -> Just (normalizeExpression defaultValue)
            Nothing -> if notNull
                then Nothing
                else Just (VarExpression "null") -- pg_dump columns don't have an explicit default null value

        normalizeExpression :: Expression -> Expression
        normalizeExpression e@(TextExpression {}) = e
        normalizeExpression (VarExpression var) = VarExpression (Text.toLower var)
        normalizeExpression (CallExpression function args) = CallExpression (Text.toLower function) (map normalizeExpression args)
        normalizeExpression (NotEqExpression a b) = NotEqExpression (normalizeExpression a) (normalizeExpression b)
        normalizeExpression (EqExpression a b) = EqExpression (normalizeExpression a) (normalizeExpression b)
        normalizeExpression (AndExpression a b) = AndExpression (normalizeExpression a) (normalizeExpression b)
        normalizeExpression (IsExpression a b) = IsExpression (normalizeExpression a) (normalizeExpression b)
        normalizeExpression (NotExpression a) = NotExpression (normalizeExpression a)
        normalizeExpression (OrExpression a b) = OrExpression (normalizeExpression a) (normalizeExpression b)
        normalizeExpression (LessThanExpression a b) = LessThanExpression (normalizeExpression a) (normalizeExpression b)
        normalizeExpression (LessThanOrEqualToExpression a b) = LessThanOrEqualToExpression (normalizeExpression a) (normalizeExpression b)
        normalizeExpression (GreaterThanExpression a b) = GreaterThanExpression (normalizeExpression a) (normalizeExpression b)
        normalizeExpression (GreaterThanOrEqualToExpression a b) = GreaterThanOrEqualToExpression (normalizeExpression a) (normalizeExpression b)
        normalizeExpression e@(DoubleExpression {}) = e
        normalizeExpression e@(IntExpression {}) = e
        -- Enum default values from pg_dump always have an explicit type cast. Inside the Schema.sql they typically don't have those.
        -- Therefore we remove these typecasts here
        --
        -- 'job_status_not_started'::public.job_status => 'job_status_not_started'
        --
        normalizeExpression (TypeCastExpression a b) = normalizeExpression a

normalizeSqlType :: PostgresType -> PostgresType
normalizeSqlType (PCustomType customType) = PCustomType (Text.toLower customType)
normalizeSqlType otherwise = otherwise
