module IHP.IDE.CodeGen.Types where

import IHP.Prelude
import IHP.Postgres.Types
import qualified IHP.SchemaCompiler.Parser as SchemaDesigner

data GeneratorAction
    = CreateFile { filePath :: OsPath, fileContent :: Text }
    | AppendToFile { filePath :: OsPath, fileContent :: Text }
    | AppendToMarker { marker :: Text, filePath :: OsPath, fileContent :: Text }
    | AddImport { filePath :: OsPath, fileContent :: Text }
    | AddAction { filePath :: OsPath, fileContent :: Text }
    | AddToDataConstructor { dataConstructor :: Text, filePath :: OsPath, fileContent :: Text }
    | AddMountToFrontController { filePath :: OsPath, applicationName :: Text }
    | EnsureDirectory { directory :: OsPath }
    | RunShellCommand { shellCommand :: Text }
    deriving (Show, Eq)



fieldsForTable :: [Statement] -> Text -> Maybe [Text]
fieldsForTable database name =
    columnsForTable database name
        |> fmap (map (\col -> col.name |> columnNameToFieldName))
-- | Returns True when a column should be part of the generated controller or forms
--
-- Returrns @False@ for primary keys, or fields such as @created_at@
columnRelevantForCreateOrEdit :: PrimaryKeyConstraint -> Column -> Bool
columnRelevantForCreateOrEdit _ column
    | (column.columnType == PTimestamp || column.columnType == PTimestampWithTimezone)
    && (isJust (column.defaultValue))
    = False
columnRelevantForCreateOrEdit (PrimaryKeyConstraint primaryKeyColumns) column =
    column.name `notElem` primaryKeyColumns

getTable :: [Statement] -> Text -> Maybe Statement
getTable schema name = find isTable schema
    where
        isTable :: Statement -> Bool
        isTable table@(StatementCreateTable CreateTable { name = name' }) | name == name' = True
        isTable _ = False

-- | Like 'fieldsForTable' but returns full 'Column' records (filtered to exclude PKs and auto-timestamps)
columnsForTable :: [Statement] -> Text -> Maybe [Column]
columnsForTable database name =
    case getTable database name of
        Just (StatementCreateTable CreateTable { columns, primaryKeyConstraint }) -> columns
                |> filter (columnRelevantForCreateOrEdit primaryKeyConstraint)
                |> Just
        _ -> Nothing

-- | Returns @[(columnName, referenceTable)]@ for all foreign key constraints on a table.
--
-- Scans both inline table constraints and top-level 'AddConstraint' statements.
foreignKeysForTable :: [Statement] -> Text -> [(Text, Text)]
foreignKeysForTable schema tableName = inlineFK <> topLevelFK
    where
        inlineFK = case getTable schema tableName of
            Just (StatementCreateTable CreateTable { constraints }) ->
                [(c.columnName, c.referenceTable) | c@ForeignKeyConstraint {} <- constraints]
            _ -> []
        topLevelFK =
            [ (c.columnName, c.referenceTable)
            | AddConstraint { tableName = tbl, constraint = c@ForeignKeyConstraint {} } <- schema
            , tbl == tableName
            ]

-- | Returns column names that have single-column UNIQUE constraints (from 'AddConstraint' or inline table constraints).
--
-- Does not include primary key columns.
uniqueColumnsForTable :: [Statement] -> Text -> [Text]
uniqueColumnsForTable schema tableName = inlineUnique <> topLevelUnique
    where
        inlineUnique = case getTable schema tableName of
            Just (StatementCreateTable CreateTable { constraints }) ->
                [col | UniqueConstraint { columnNames = [col] } <- constraints]
            _ -> []
        topLevelUnique =
            [ col
            | AddConstraint { tableName = tbl, constraint = UniqueConstraint { columnNames = [col] } } <- schema
            , tbl == tableName
            ]

loadAppSchema :: IO [Statement]
loadAppSchema = SchemaDesigner.parseSchemaSql >>= \case
    Left _parserError -> pure []
    Right statements -> pure statements
