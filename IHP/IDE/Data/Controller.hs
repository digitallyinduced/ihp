module IHP.IDE.Data.Controller where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.ViewContext
import IHP.IDE.Data.View.ShowDatabase
import IHP.IDE.Data.View.ShowTableRows
import IHP.IDE.Data.View.ShowQuery

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified IHP.FrameworkConfig as Config

instance Controller DataController where
    action ShowDatabaseAction = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        PG.close connection
        render ShowDatabaseView { .. }

    action ShowTableRowsAction { tableName } = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection

        rows :: [[DynamicField]] <- PG.query connection "SELECT * FROM ?" (PG.Only (PG.Identifier tableName))

        PG.close connection
        render ShowTableRowsView { .. }

    action ShowQueryAction { query } = do
        connection <- connectToAppDb

        rows :: [[DynamicField]] <- PG.query_ connection (fromString (cs query))

        PG.close connection
        render ShowQueryView { .. }

connectToAppDb = do
    databaseUrl <- Config.appDatabaseUrl
    PG.connectPostgreSQL databaseUrl

fetchTableNames :: PG.Connection -> IO [Text]
fetchTableNames connection = do
    values :: [[Text]] <- PG.query_ connection "SELECT tablename FROM pg_catalog.pg_tables where schemaname = 'public'"
    pure (join values)

instance PG.FromField DynamicField where
    fromField field fieldValue = pure DynamicField { .. }
        where
            fieldName = fromMaybe "" (PG.name field)