module IHP.IDE.ToolServer.Types where

import IHP.Prelude
import qualified IHP.Controller.Session as Session
import qualified IHP.ControllerSupport as ControllerSupport
import qualified IHP.ViewSupport as ViewSupport
import IHP.FrameworkConfig
import IHP.Environment
import qualified IHP.IDE.Types as DevServer
import IHP.FlashMessages.Types

data ToolServerApplication = ToolServerApplication { devServerContext :: DevServer.Context }

data SchemaController
    = PushToDbAction
    | DumpDbAction
    | UpdateDbAction
    | ShowCodeAction
    | SaveCodeAction
    | ShowGeneratedCodeAction { statementName :: Text }
    deriving (Eq, Show, Data)

data TablesController
    = TablesAction
    | ShowTableAction { tableName :: Text }
    | NewTableAction
    | CreateTableAction
    | EditTableAction { tableName :: Text, tableId :: Int }
    | UpdateTableAction
    | DeleteTableAction { tableId :: Int, tableName :: Text }
    deriving (Eq, Show, Data)

data ColumnsController
    = NewColumnAction { tableName :: Text }
    | CreateColumnAction
    | EditColumnAction { tableName :: Text, columnId :: Int }
    | UpdateColumnAction
    | DeleteColumnAction { tableName :: Text, columnId :: Int, columnName :: Text }
    | ToggleColumnUniqueAction { tableName :: Text, columnId :: Int }
    | NewForeignKeyAction { tableName :: Text, columnName :: Text }
    | CreateForeignKeyAction
    | EditForeignKeyAction { tableName :: Text, columnName :: Text, constraintName :: Text, referenceTable :: Text }
    | UpdateForeignKeyAction
    | DeleteForeignKeyAction { constraintName :: Text, tableName :: Text }
    deriving (Eq, Show, Data)

data EnumsController
    = ShowEnumAction { enumName :: Text }
    | NewEnumAction
    | CreateEnumAction
    | EditEnumAction { enumName :: Text, enumId :: Int }
    | UpdateEnumAction
    | DeleteEnumAction { tableId :: Int }
    deriving (Eq, Show, Data)

data EnumValuesController
    = NewEnumValueAction { enumName :: Text }
    | CreateEnumValueAction
    | EditEnumValueAction { enumName :: Text, valueId :: Int }
    | UpdateEnumValueAction
    | DeleteEnumValueAction { enumName :: Text, valueId :: Int }
    deriving (Eq, Show, Data)

data DataController
    = ShowDatabaseAction
    | ShowTableRowsAction { tableName :: Text }
    | ShowQueryAction
    | DeleteEntryAction { primaryKey :: UUID, tableName :: Text }
    | CreateRowAction
    | NewRowAction { tableName :: Text }
    | EditRowAction { tableName :: Text, targetPrimaryKey :: UUID }
    | UpdateRowAction
    | EditRowValueAction { tableName :: Text, targetName :: Text, id :: Text }
    | ToggleBooleanFieldAction { tableName :: Text, targetName :: Text, targetPrimaryKey :: UUID }
    | UpdateValueAction
    deriving (Eq, Show, Data)

data LogsController
    = AppLogsAction
    | PostgresLogsAction
    | OpenEditorAction
    deriving (Eq, Show, Data)

data CodeGenController
    = GeneratorsAction
    | NewControllerAction
    | NewScriptAction
    | NewViewAction
    | NewMailAction
    | NewActionAction
    | NewApplicationAction
    | NewMigrationAction
    | NewJobAction
    | CreateControllerAction
    | CreateScriptAction
    | CreateViewAction
    | CreateMailAction
    | CreateActionAction
    | CreateApplicationAction
    | CreateMigrationAction
    | CreateJobAction
    | OpenControllerAction
    deriving (Eq, Show, Data)


data DynamicField = DynamicField
    { fieldValue :: Maybe ByteString
    , fieldName :: ByteString
    } deriving (Show)

data ColumnDefinition = ColumnDefinition
    { columnName :: Text
    , columnType :: Text
    , columnDefault :: Maybe Text
    , isNullable :: Bool
    } deriving (Show)


-- | Keeps track of all all available apps in the projects. Used to display
-- the apps inside the sidebar navigation
--
-- Usually this list is like: @["Web"]@ or @["Web", "Admin"]@
newtype AvailableApps = AvailableApps [Text]

-- | Wrapper to pass the app url to the layout.
-- Usually "http://localhost:8000"
newtype AppUrl = AppUrl Text

-- | List of all controllers. Used inside e.g. the Schema Designer to decide whether to display
-- the 'Generate Controller' option
newtype WebControllers = WebControllers [Text]
