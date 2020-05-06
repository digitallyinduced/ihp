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

data SchemaDesignerController
    = TablesAction
    | ShowTableAction { tableName :: Text }
    | NewColumnAction { tableName :: Text }
    | CreateColumnAction
    | PushToDbAction
    | CreateTableAction
    | NewTableAction
    | EditColumnAction { tableName :: Text, columnId :: Int }
    | UpdateColumnAction
    | EditTableAction { tableName :: Text, tableId :: Int }
    | UpdateTableAction
    | DeleteTableAction { tableId :: Int }
    | ShowEnumAction { enumName :: Text }
    | NewEnumAction
    | CreateEnumAction
    | NewEnumValueAction { enumName :: Text }
    | CreateEnumValueAction
    | EditEnumValueAction { enumName :: Text, valueId :: Int }
    | UpdateEnumValueAction
    | DeleteEnumValueAction { enumName :: Text, valueId :: Int }
    | DeleteColumnAction { tableName :: Text, columnId :: Int }
    | EditEnumAction { enumName :: Text, enumId :: Int }
    | UpdateEnumAction
    | ToggleColumnUniqueAction { tableName :: Text, columnId :: Int }
    | ShowCodeAction
    | SaveCodeAction
    | NewForeignKeyAction { tableName :: Text, columnName :: Text }
    | CreateForeignKeyAction
    | EditForeignKeyAction { tableName :: Text, columnName :: Text, constraintName :: Text, referenceTable :: Text }
    | UpdateForeignKeyAction
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
