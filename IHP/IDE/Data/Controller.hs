module IHP.IDE.Data.Controller where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.ViewContext
import IHP.IDE.Data.View.ShowDatabase

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified IHP.FrameworkConfig as Config

instance Controller DataController where
    action ShowDatabaseAction = do
        databaseUrl <- Config.appDatabaseUrl
        connection <- PG.connectPostgreSQL databaseUrl
        values :: [[DynamicField]] <- PG.query_ connection "SELECT * FROM users"
        putStrLn (tshow values)
        PG.close connection
        render ShowDatabaseView

data DynamicField = DynamicField
    { fieldValue :: Maybe ByteString
    , fieldName :: ByteString
    } deriving (Show)

instance PG.FromField DynamicField where
    fromField field fieldValue = pure DynamicField { .. }
        where
            fieldName = fromMaybe "" (PG.name field)