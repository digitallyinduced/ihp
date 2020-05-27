module IHP.IDE.Data.Controller where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.ViewContext
import IHP.IDE.Data.View.ShowDatabase
import IHP.IDE.Data.View.ShowTableRows
import IHP.IDE.Data.View.ShowQuery
import IHP.IDE.Data.View.NewRow
import IHP.IDE.Data.View.EditRow
import IHP.IDE.Data.View.EditValue

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.FromRow as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified IHP.FrameworkConfig as Config
import qualified Data.UUID as UUID

instance Controller DataController where
    action ShowDatabaseAction = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        PG.close connection
        render ShowDatabaseView { .. }

    action ShowTableRowsAction { tableName } = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection

        rows :: [[DynamicField]] <- PG.query connection "SELECT * FROM ? ORDER BY id" (PG.Only (PG.Identifier tableName))

        PG.close connection
        render ShowTableRowsView { .. }

    action ShowQueryAction = do
        connection <- connectToAppDb
        let query = (param @Text "query")
        rows :: [[DynamicField]] <- PG.query_ connection (fromString (cs query))

        PG.close connection
        render ShowQueryView { .. }

    action DeleteEntryAction { fieldValue, tableName } = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        let (Just id) = UUID.fromText fieldValue
        let query = "DELETE FROM " <> tableName <> " WHERE id = ?"
        PG.execute connection (PG.Query . cs $! query) (PG.Only id)
        PG.close connection
        redirectTo ShowTableRowsAction { .. }

    action NewRowAction { tableName } = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection

        rows :: [[DynamicField]] <- PG.query connection "SELECT * FROM ? ORDER BY id" (PG.Only (PG.Identifier tableName))

        tableCols <- fetchTableCols connection tableName

        PG.close connection
        render NewRowView { .. }

    action CreateRowAction = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        let tableName = param "tableName"
        tableCols <- fetchTableCols connection tableName
        let values :: [Text] = map (\col -> param @Text (cs (get #columnName col))) tableCols
        let query = "INSERT INTO " <> tableName <> " VALUES (" <> intercalate "," values <> ")"
        putStrLn (query)
        PG.execute_ connection (PG.Query . cs $! query)
        PG.close connection
        redirectTo ShowTableRowsAction { .. }

    action EditRowAction { tableName, id } = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection

        rows :: [[DynamicField]] <- PG.query connection "SELECT * FROM ? ORDER BY id" (PG.Only (PG.Identifier tableName))

        tableCols <- fetchTableCols connection tableName
        values <- fetchTable connection (cs tableName) ("'" <> cs id <> "'")
        let (Just rowValues) = head values
        PG.close connection
        render EditRowView { .. }
        
    action UpdateRowAction = do
        let id :: String = cs (param @Text "id")
        let tableName = param "tableName"
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        tableCols <- fetchTableCols connection tableName
        let values :: [Text] = map (\col -> param @Text (cs (get #columnName col))) tableCols
        let columns :: [Text] = map (\col -> cs (get #columnName col)) tableCols
        let query = "UPDATE " <> tableName <> " SET " <> intercalate ", " (updateValues (zip columns values)) <> " WHERE id = " <> cs id
        PG.execute_ connection (PG.Query . cs $! query)
        PG.close connection
        redirectTo ShowTableRowsAction { .. }

    action EditRowValueAction { tableName, targetName, id } = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        
        rows :: [[DynamicField]] <- PG.query connection "SELECT * FROM ? ORDER BY id" (PG.Only (PG.Identifier tableName))

        let targetId = cs id
        PG.close connection
        render EditValueView { .. }
        

connectToAppDb = do
    databaseUrl <- Config.appDatabaseUrl
    PG.connectPostgreSQL databaseUrl

fetchTableNames :: PG.Connection -> IO [Text]
fetchTableNames connection = do
    values :: [[Text]] <- PG.query_ connection "SELECT tablename FROM pg_catalog.pg_tables where schemaname = 'public'"
    pure (join values)

fetchTableCols :: PG.Connection -> Text -> IO [ColumnDefinition]
fetchTableCols connection tableName = do
    PG.query connection "SELECT column_name,data_type,column_default FROM information_schema.columns where table_name = ?" (PG.Only tableName)

fetchTable :: PG.Connection -> String -> String -> IO [[DynamicField]]
fetchTable connection tableName rowId = do
    PG.query_ connection (fromString ("SELECT * FROM " <> tableName <> " where id = " <> rowId))

updateValues list = map (\elem -> fst elem <> " = " <> snd elem) list

instance PG.FromField DynamicField where
    fromField field fieldValue = pure DynamicField { .. }
        where
            fieldName = fromMaybe "" (PG.name field)

instance PG.FromRow ColumnDefinition where
    fromRow = ColumnDefinition <$> PG.field <*> PG.field <*> PG.field