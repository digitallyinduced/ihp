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
    = ColumnsAction
    | NewColumnAction { tableName :: Text }
    | CreateColumnAction
    | EditColumnAction { tableName :: Text, columnId :: Int }
    | UpdateColumnAction
    | DeleteColumnAction { tableName :: Text, columnId :: Int }
    | ToggleColumnUniqueAction { tableName :: Text, columnId :: Int }
    | NewForeignKeyAction { tableName :: Text, columnName :: Text }
    | CreateForeignKeyAction
    | EditForeignKeyAction { tableName :: Text, columnName :: Text, constraintName :: Text, referenceTable :: Text }
    | UpdateForeignKeyAction
    | DeleteForeignKeyAction { constraintName :: Text, tableName :: Text }
    deriving (Eq, Show, Data)

data EnumsController
    = EnumsAction
    | ShowEnumAction { enumName :: Text }
    | NewEnumAction
    | CreateEnumAction
    | EditEnumAction { enumName :: Text, enumId :: Int }
    | UpdateEnumAction
    | DeleteEnumAction { tableId :: Int }
    deriving (Eq, Show, Data)

data EnumValuesController
    = EnumValuesAction
    | NewEnumValueAction { enumName :: Text }
    | CreateEnumValueAction
    | EditEnumValueAction { enumName :: Text, valueId :: Int }
    | UpdateEnumValueAction
    | DeleteEnumValueAction { enumName :: Text, valueId :: Int }
    deriving (Eq, Show, Data)


data DataController
    = ShowDatabaseAction
    | ShowTableRowsAction { tableName :: Text }
    | ShowQueryAction
    | DeleteEntryAction { fieldValue :: Text, tableName :: Text }
    | CreateRowAction
    | NewRowAction { tableName :: Text }
    | EditRowAction { tableName :: Text, id :: Text }
    | UpdateRowAction
    | EditRowValueAction { tableName :: Text, targetName :: Text, id :: Text }
    deriving (Eq, Show, Data)

data LogsController
    = AppLogsAction
    | PostgresLogsAction
    deriving (Eq, Show, Data)

data CodeGenController
    = GeneratorsAction
    | NewControllerAction
    | NewScriptAction
    | NewViewAction
    | NewActionAction
    | NewApplicationAction
    | CreateControllerAction
    | CreateScriptAction
    | CreateViewAction
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
    } deriving (Show)

instance FrameworkConfig where 
    environment = Development
    appHostname = "localhost"
