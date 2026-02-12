module IHP.IDE.CodeGen.Types (module IHP.IDE.CodeGen.Types, defaultUuidFunction) where

import IHP.Prelude
import qualified Data.Text as Text
import IHP.Postgres.Types
import qualified IHP.SchemaCompiler.Parser as SchemaDesigner
import IHP.IDE.CodeGen.DefaultUuidFunction (defaultUuidFunction)

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

-- | Ensures a name has the given suffix, returning both the suffixed and unsuffixed versions.
--
-- >>> ensureSuffix "View" "EditView"
-- ("EditView", "Edit")
-- >>> ensureSuffix "View" "Edit"
-- ("EditView", "Edit")
ensureSuffix :: Text -> Text -> (Text, Text)
ensureSuffix suffix name
    | suffix `isSuffixOf` name = (name, Text.dropEnd (Text.length suffix) name)
    | otherwise = (name <> suffix, name)

-- | Build a qualified Haskell module name from application, category, controller, and module name.
--
-- >>> qualifiedModuleName "Web" "View" "Users" "Edit"
-- "Web.View.Users.Edit"
qualifiedModuleName :: Text -> Text -> Text -> Text -> Text
qualifiedModuleName applicationName category controllerName moduleName =
    applicationName <> "." <> category <> "." <> controllerName <> "." <> moduleName

-- | Configuration for generating standard CRUD action bodies.
data ActionBodyConfig = ActionBodyConfig
    { singularName :: Text          -- ^ e.g. "User"
    , modelVariableSingular :: Text -- ^ e.g. "user"
    , idFieldName :: Text           -- ^ e.g. "userId"
    , model :: Text                 -- ^ e.g. "User"
    , indexAction :: Text           -- ^ redirect target for create/delete, e.g. "UsersAction"
    }

generateShowActionBody :: ActionBodyConfig -> Text
generateShowActionBody config =
    ""
    <> "    action Show" <> config.singularName <> "Action { " <> config.idFieldName <> " } = do\n"
    <> "        " <> config.modelVariableSingular <> " <- fetch " <> config.idFieldName <> "\n"
    <> "        render ShowView { .. }\n"

generateNewActionBody :: ActionBodyConfig -> Text
generateNewActionBody config =
    ""
    <> "    action New" <> config.singularName <> "Action = do\n"
    <> "        let " <> config.modelVariableSingular <> " = newRecord\n"
    <> "        render NewView { .. }\n"

generateEditActionBody :: ActionBodyConfig -> Text
generateEditActionBody config =
    ""
    <> "    action Edit" <> config.singularName <> "Action { " <> config.idFieldName <> " } = do\n"
    <> "        " <> config.modelVariableSingular <> " <- fetch " <> config.idFieldName <> "\n"
    <> "        render EditView { .. }\n"

generateUpdateActionBody :: ActionBodyConfig -> Text
generateUpdateActionBody config =
    ""
    <> "    action Update" <> config.singularName <> "Action { " <> config.idFieldName <> " } = do\n"
    <> "        " <> config.modelVariableSingular <> " <- fetch " <> config.idFieldName <> "\n"
    <> "        " <> config.modelVariableSingular <> "\n"
    <> "            |> build" <> config.singularName <> "\n"
    <> "            |> ifValid \\case\n"
    <> "                Left " <> config.modelVariableSingular <> " -> render EditView { .. }\n"
    <> "                Right " <> config.modelVariableSingular <> " -> do\n"
    <> "                    " <> config.modelVariableSingular <> " <- " <> config.modelVariableSingular <> " |> updateRecord\n"
    <> "                    setSuccessMessage \"" <> config.model <> " updated\"\n"
    <> "                    redirectTo Edit" <> config.singularName <> "Action { .. }\n"

generateCreateActionBody :: ActionBodyConfig -> Text
generateCreateActionBody config =
    ""
    <> "    action Create" <> config.singularName <> "Action = do\n"
    <> "        let " <> config.modelVariableSingular <> " = newRecord @" <> config.model <> "\n"
    <> "        " <> config.modelVariableSingular <> "\n"
    <> "            |> build" <> config.singularName <> "\n"
    <> "            |> ifValid \\case\n"
    <> "                Left " <> config.modelVariableSingular <> " -> render NewView { .. } \n"
    <> "                Right " <> config.modelVariableSingular <> " -> do\n"
    <> "                    " <> config.modelVariableSingular <> " <- " <> config.modelVariableSingular <> " |> createRecord\n"
    <> "                    setSuccessMessage \"" <> config.model <> " created\"\n"
    <> "                    redirectTo " <> config.indexAction <> "\n"

generateDeleteActionBody :: ActionBodyConfig -> Text
generateDeleteActionBody config =
    ""
    <> "    action Delete" <> config.singularName <> "Action { " <> config.idFieldName <> " } = do\n"
    <> "        " <> config.modelVariableSingular <> " <- fetch " <> config.idFieldName <> "\n"
    <> "        deleteRecord " <> config.modelVariableSingular <> "\n"
    <> "        setSuccessMessage \"" <> config.model <> " deleted\"\n"
    <> "        redirectTo " <> config.indexAction <> "\n"
