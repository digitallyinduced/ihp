module IHP.IDE.Data.Controller where

import IHP.ControllerPrelude
import IHP.Controller.RequestContext
import IHP.IDE.ToolServer.Types
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
import qualified Data.Text as T
import qualified Data.ByteString.Builder

import qualified Data.ByteString.Char8 as BS

instance Controller DataController where
    action ShowDatabaseAction = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        PG.close connection
        render ShowDatabaseView { .. }

    action ShowTableRowsAction { tableName } = do
        let page :: Int = paramOrDefault @Int 1 "page"
        let pageSize :: Int = paramOrDefault @Int 20 "rows"
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        primaryKeyFields <- tablePrimaryKeyFields connection tableName
        rows :: [[DynamicField]] <- fetchRowsPage connection tableName page pageSize
        tableCols <- fetchTableCols connection tableName
        totalRows <- tableLength connection tableName
        PG.close connection
        render ShowTableRowsView { .. }

    action ShowQueryAction = do
        connection <- connectToAppDb
        let query = (param @Text "query")
        when (query == "") $ redirectTo ShowDatabaseAction
        rows :: [[DynamicField]] <- if isQuery query then PG.query_ connection (fromString (cs query)) else PG.execute_ connection (fromString (cs query)) >> return []
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
        let values :: [PG.Action] = map (\col -> parseValues (param @Bool (cs (get #columnName col) <> "_")) (param @Bool (cs (get #columnName col) <> "-isBoolean")) (param @Text (cs (get #columnName col)))) tableCols
        let query = "INSERT INTO " <> tableName <> " VALUES (" <> intercalate "," (map (const "?") values) <> ")"
        PG.execute connection (PG.Query . cs $! query) values
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

        let values :: [PG.Action] = map (\col -> parseValues (param @Bool (cs (get #columnName col) <> "_")) (param @Bool (cs (get #columnName col) <> "-isBoolean")) (param @Text (cs (get #columnName col)))) tableCols
        let columns :: [Text] = map (\col -> cs (get #columnName col)) tableCols
        let primaryKeyValues = map (\pkey -> "'" <> (param @Text (cs pkey <> "-pk")) <> "'") primaryKeyFields

        let query = "UPDATE " <> tableName <> " SET " <> intercalate ", " (updateValues (zip columns (map (const "?") values))) <> " WHERE " <> intercalate " AND " (updateValues (zip primaryKeyFields primaryKeyValues))
        PG.execute connection (PG.Query . cs $! query) values
        PG.close connection
        redirectTo ShowTableRowsAction { .. }

    action EditRowValueAction { tableName, targetName, id } = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection

        rows :: [[DynamicField]] <- fetchRows connection tableName

        let targetId = cs id
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

    action UpdateValueAction = do
        let id :: String = cs (param @Text "id")
        let tableName = param "tableName"
        connection <- connectToAppDb
        let targetCol = param "targetName"
        let targetValue = param "targetValue"
        let query = "UPDATE " <> tableName <> " SET " <> targetCol <> " = '" <> targetValue <> "' WHERE id = '" <> cs id <> "'"
        PG.execute_ connection (PG.Query . cs $! query)
        PG.close connection
        redirectTo ShowTableRowsAction { .. }

    action DeleteTableRowsAction { tableName } = do
        connection <- connectToAppDb
        let query = "TRUNCATE TABLE " <> tableName
        PG.execute_ connection (PG.Query . cs $! query)
        PG.close connection
        redirectTo ShowTableRowsAction { .. }

connectToAppDb :: (?context :: ControllerContext) => _
connectToAppDb = PG.connectPostgreSQL $ fromConfig databaseUrl

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

fetchRowsPage :: FromRow r => PG.Connection -> Text -> Int -> Int -> IO [r]
fetchRowsPage connection tableName page rows = do
    pkFields <- tablePrimaryKeyFields connection tableName
    let slice = " OFFSET " <> show (page * rows - rows) <> " ROWS FETCH FIRST " <> show rows <> " ROWS ONLY"
    let query = "SELECT * FROM " <> tableName <> " ORDER BY " <> intercalate ", " pkFields <> slice

    PG.query_ connection (PG.Query . cs $! query)

tableLength :: _ => PG.Connection -> Text -> IO Int
tableLength connection tableName = do
    [Only count] <- PG.query connection "SELECT COUNT(*) FROM ?" [PG.Identifier tableName]
    pure count


-- parseValues sqlMode isBoolField input
parseValues :: Bool -> Bool -> Text -> PG.Action
parseValues _ True "on" = PG.toField True
parseValues _ True "off" = PG.toField False
parseValues False _ text = PG.toField text
parseValues _ _ text = PG.Plain (Data.ByteString.Builder.byteString (cs text))

updateValues list = map (\elem -> fst elem <> " = " <> snd elem) list

isQuery sql = T.isInfixOf "SELECT" u
    where u = T.toUpper sql
