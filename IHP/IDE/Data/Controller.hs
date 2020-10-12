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
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified IHP.FrameworkConfig as Config
import qualified Data.Text as T

instance Controller DataController where
    action ShowDatabaseAction = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        PG.close connection
        render ShowDatabaseView { .. }

    action ShowTableRowsAction { tableName } = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        primaryKeyFields <- tablePrimaryKeyFields connection tableName

        rows :: [[DynamicField]] <- fetchRows connection tableName

        tableCols <- fetchTableCols connection tableName

        PG.close connection
        render ShowTableRowsView { .. }

    action ShowQueryAction = do
        connection <- connectToAppDb
        let query = (param @Text "query")
        when (query == "") do
            redirectTo ShowDatabaseAction
        rows :: [[DynamicField]] <- PG.query_ connection (fromString (cs query))

        PG.close connection
        render ShowQueryView { .. }

    action DeleteEntryAction { primaryKey, tableName } = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        primaryKeyFields <- tablePrimaryKeyFields connection tableName
        let primaryKeyValues = T.splitOn "---" primaryKey
        let query = "DELETE FROM " <> tableName <> " WHERE " <> intercalate " AND " ((<> " = ?") <$> primaryKeyFields)
        PG.execute connection (PG.Query . cs $! query) primaryKeyValues
        PG.close connection
        redirectTo ShowTableRowsAction { .. }

    action NewRowAction { tableName } = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection

        rows :: [[DynamicField]] <- fetchRows connection tableName

        tableCols <- fetchTableCols connection tableName

        PG.close connection
        render NewRowView { .. }

    action CreateRowAction = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        let tableName = param "tableName"
        tableCols <- fetchTableCols connection tableName
        putStrLn (tshow (param @Bool "id_"))
        let values :: [Text] = map (\col -> quoteIfLiteral (param @Bool (cs (get #columnName col) <> "_")) (param @Text (cs (get #columnName col)))) tableCols
        let query = "INSERT INTO " <> tableName <> " VALUES (" <> intercalate "," values <> ")"
        putStrLn (query)
        PG.execute_ connection (PG.Query . cs $! query)
        PG.close connection
        redirectTo ShowTableRowsAction { .. }

    action EditRowAction { tableName, targetPrimaryKey } = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        primaryKeyFields <- tablePrimaryKeyFields connection tableName

        rows :: [[DynamicField]] <- fetchRows connection tableName

        tableCols <- fetchTableCols connection tableName
        let targetPrimaryKeyValues = T.splitOn "---" targetPrimaryKey
        values <- fetchRow connection (cs tableName) targetPrimaryKeyValues
        let (Just rowValues) = head values
        PG.close connection
        render EditRowView { .. }

    action UpdateRowAction = do
        let tableName = param "tableName"
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        tableCols <- fetchTableCols connection tableName
        primaryKeyFields <- tablePrimaryKeyFields connection tableName

        let values = map (PG.Escape . cs . param @Text . cs . get #columnName) tableCols
        let columns :: [Text] = map (\col -> cs (get #columnName col)) tableCols
        let primaryKeyValues = map (PG.Escape . cs . param @Text . (<> "-pk") . cs) primaryKeyFields

        let query = PG.Query . cs $! "UPDATE " <> tableName <> " SET " <> intercalate ", " (map (<> " = ?") columns) <> " WHERE " <> intercalate " AND " ((<> " = ?") <$> primaryKeyFields)
        PG.execute connection query (values <> primaryKeyValues)
        PG.close connection
        redirectTo ShowTableRowsAction { .. }

    action EditRowValueAction { tableName, targetName, targetPrimaryKey } = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        primaryKeyFields <- tablePrimaryKeyFields connection tableName

        rows :: [[DynamicField]] <- fetchRows connection tableName

        PG.close connection
        render EditValueView { .. }

    action ToggleBooleanFieldAction { tableName, targetName, targetPrimaryKey } = do
        let id :: String = cs (param @Text "id")
        let tableName = param "tableName"
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        tableCols <- fetchTableCols connection tableName
        primaryKeyFields <- tablePrimaryKeyFields connection tableName
        let targetPrimaryKeyValues = PG.Escape . cs <$> T.splitOn "---" targetPrimaryKey
        let query = PG.Query . cs $! "UPDATE ? SET ? = NOT ? WHERE " <> intercalate " AND " ((<> " = ?") <$> primaryKeyFields)
        let params = [PG.toField $ PG.Identifier tableName, PG.toField $ PG.Identifier targetName, PG.toField $ PG.Identifier targetName] <> targetPrimaryKeyValues
        PG.execute connection query params
        PG.close connection
        redirectTo ShowTableRowsAction { .. }


connectToAppDb = do
    databaseUrl <- Config.appDatabaseUrl
    PG.connectPostgreSQL databaseUrl

fetchTableNames :: PG.Connection -> IO [Text]
fetchTableNames connection = do
    values :: [[Text]] <- PG.query_ connection "SELECT tablename FROM pg_catalog.pg_tables where schemaname = 'public'"
    pure (join values)

fetchTableCols :: PG.Connection -> Text -> IO [ColumnDefinition]
fetchTableCols connection tableName = do
    PG.query connection "SELECT column_name,data_type,column_default,CASE WHEN is_nullable='YES' THEN true ELSE false END FROM information_schema.columns where table_name = ?" (PG.Only tableName)

fetchRow :: PG.Connection -> Text -> [Text] -> IO [[DynamicField]]
fetchRow connection tableName primaryKeyValues = do
    pkFields <- tablePrimaryKeyFields connection tableName
    let query = "SELECT * FROM " <> tableName <> " WHERE " <> intercalate " AND " ((<> " = ?") <$> pkFields)
    PG.query connection (PG.Query . cs $! query) primaryKeyValues

instance PG.FromField DynamicField where
    fromField field fieldValue = pure DynamicField { .. }
        where
            fieldName = fromMaybe "" (PG.name field)

instance PG.FromRow ColumnDefinition where
    fromRow = ColumnDefinition <$> PG.field <*> PG.field <*> PG.field <*> PG.field

tablePrimaryKeyFields :: PG.Connection -> Text -> IO [Text]
tablePrimaryKeyFields connection tableName = do
    fields <- PG.query connection "SELECT a.attname FROM pg_index i JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY(i.indkey) WHERE i.indrelid = ?::regclass AND i.indisprimary" (PG.Only tableName) :: IO [PG.Only Text]
    pure $ PG.fromOnly <$> fields

fetchRows :: FromRow r => PG.Connection -> Text -> IO [r]
fetchRows connection tableName = do
    pkFields <- tablePrimaryKeyFields connection tableName

    let query = "SELECT * FROM " <> tableName <> " ORDER BY " <> intercalate ", " pkFields

    PG.query_ connection (PG.Query . cs $! query)

quoteIfLiteral :: Bool -> Text -> Text
quoteIfLiteral False text = "'" <> text <> "'"
quoteIfLiteral True text = text