module IHP.IDE.ToolServer.Types where

import IHP.Prelude
import qualified IHP.Controller.Session as Session
import qualified IHP.ControllerSupport as ControllerSupport
import qualified IHP.ViewSupport as ViewSupport
import IHP.FrameworkConfig
import IHP.Environment
import qualified IHP.IDE.Types as DevServer

data ViewContext = ViewContext
    { requestContext :: ControllerSupport.RequestContext
    , flashMessages :: [Session.FlashMessage]
    , layout :: ViewSupport.Layout
    , controllerContext :: ControllerSupport.ControllerContext
    , appUrl :: Text
    , webControllers :: [Text]
    , appNames :: [Text]
    }


data ToolServerApplication = ToolServerApplication { devServerContext :: DevServer.Context } deriving (Eq)

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
    | DeleteEntryAction { primaryKey :: Text, tableName :: Text }
    | CreateRowAction
    | NewRowAction { tableName :: Text }
    | EditRowAction { tableName :: Text, targetPrimaryKey :: Text }
    | UpdateRowAction
    | EditRowValueAction { tableName :: Text, targetName :: Text, id :: Text }
    | ToggleBooleanFieldAction { tableName :: Text, targetName :: Text, targetPrimaryKey :: Text }
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
    | CreateControllerAction
    | CreateScriptAction
    | CreateViewAction
    | CreateMailAction
    | CreateActionAction
    | CreateApplicationAction
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

instance FrameworkConfig where
    environment = Development
    appHostname = "localhost"
