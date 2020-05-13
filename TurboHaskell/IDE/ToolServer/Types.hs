module TurboHaskell.IDE.ToolServer.Types where

import TurboHaskell.Prelude
import qualified TurboHaskell.Controller.Session as Session
import qualified TurboHaskell.ControllerSupport as ControllerSupport
import qualified TurboHaskell.ViewSupport as ViewSupport
import TurboHaskell.FrameworkConfig
import TurboHaskell.Environment
import qualified TurboHaskell.IDE.Types as DevServer

data ViewContext = ViewContext
    { requestContext :: ControllerSupport.RequestContext
    , flashMessages :: [Session.FlashMessage]
    , layout :: ViewSupport.Layout
    , controllerContext :: ControllerSupport.ControllerContext
    }


data ToolServerApplication = ToolServerApplication { devServerContext :: DevServer.Context } deriving (Eq)

data SchemaController
    = SchemaAction
    | PushToDbAction
    | ShowCodeAction
    | SaveCodeAction
    | ShowGeneratedCodeAction { tableName :: Text }
    deriving (Eq, Show, Data)

data TablesController
    = TablesAction
    | ShowTableAction { tableName :: Text }
    | NewTableAction
    | CreateTableAction
    | EditTableAction { tableName :: Text, tableId :: Int }
    | UpdateTableAction
    | DeleteTableAction { tableId :: Int }
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
    deriving (Eq, Show, Data)

data EnumsController
    = EnumsAction
    | ShowEnumAction { enumName :: Text }
    | NewEnumAction
    | CreateEnumAction
    | EditEnumAction { enumName :: Text, enumId :: Int }
    | UpdateEnumAction
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
    deriving (Eq, Show, Data)

data LogsController
    = AppLogsAction
    | PostgresLogsAction
    deriving (Eq, Show, Data)

instance FrameworkConfig where 
    environment = Development
    baseUrl = "http://localhost:8001"
