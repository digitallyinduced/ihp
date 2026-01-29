module IHP.IDE.Data.Controller where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.Data.View.ShowDatabase
import IHP.IDE.Data.View.ShowTableRows
import IHP.IDE.Data.View.ShowQuery
import IHP.IDE.Data.View.NewRow
import IHP.IDE.Data.View.EditRow
import IHP.IDE.Data.View.EditValue
import IHP.IDE.Data.View.ShowForeignKeyHoverCard

import qualified Hasql.Connection as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Decoders as Decoders
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Data.Text as T
import qualified Data.List
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap

instance Controller DataController where
    action ShowDatabaseAction = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        Hasql.release connection
        case headMay tableNames of
            Just tableName -> jumpToAction ShowTableRowsAction { tableName }
            Nothing -> render ShowDatabaseView { .. }

    action ShowTableRowsAction { tableName } = do
        let page :: Int = paramOrDefault @Int 1 "page"
        let pageSize :: Int = paramOrDefault @Int 20 "rows"
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        primaryKeyFields <- tablePrimaryKeyFields connection tableName
        rows :: [[DynamicField]] <- fetchRowsPage connection tableName page pageSize
        tableCols <- fetchTableCols connection tableName
        totalRows <- tableLength connection tableName
        Hasql.release connection
        render ShowTableRowsView { .. }

    action NewQueryAction = do
        let queryText = ""
        let queryResult = Nothing
        render ShowQueryView { .. }

    action QueryAction = do
        connection <- connectToAppDb
        let queryText = param @Text "query"
        when (isEmpty queryText) do
            redirectTo NewQueryAction

        queryResult :: Maybe (Either SomeException SqlConsoleResult) <- Just <$> if isQuery queryText then do
                let wrappedQuery = "SELECT row_to_json(t)::jsonb FROM (" <> queryText <> ") t"
                let decoder = Decoders.rowList (Decoders.column (Decoders.nonNullable (Decoders.jsonb)))
                let session = snippetToSession (Snippet.sql (cs wrappedQuery)) decoder
                (do
                    jsonRows <- runSessionOnConnection connection session
                    pure $ Right $ SelectQueryResult (map jsonToFields jsonRows)
                    ) `catch` (\(e :: SomeException) -> pure (Left e))
            else do
                let session = snippetToSession (Snippet.sql (cs queryText)) Decoders.noResult
                (do
                    runSessionOnConnection connection session
                    pure $ Right $ InsertOrUpdateResult 0
                    ) `catch` (\(e :: SomeException) -> pure (Left e))

        Hasql.release connection
        render ShowQueryView { .. }

    action DeleteEntryAction { primaryKey, tableName } = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        primaryKeyFields <- tablePrimaryKeyFields connection tableName
        let primaryKeyValues = T.splitOn "---" primaryKey
        let whereClause = intercalate " AND " (zipWith (\field val -> "\"" <> field <> "\" = " <> quoteSqlValue val) primaryKeyFields primaryKeyValues)
        let query = "DELETE FROM " <> tableName <> " WHERE " <> whereClause
        runSessionOnConnection connection (Hasql.sql (cs query))
        Hasql.release connection
        redirectTo ShowTableRowsAction { .. }

    action NewRowAction { tableName } = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection

        rows :: [[DynamicField]] <- fetchRows connection tableName

        tableCols <- fetchTableCols connection tableName

        Hasql.release connection
        render NewRowView { .. }

    action CreateRowAction = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        let tableName = param "tableName"
        tableCols <- fetchTableCols connection tableName
        let snippetValues :: [Snippet] = map (\col -> parseValues (param @Bool (cs (col.columnName) <> "_")) (param @Bool (cs (col.columnName) <> "-isBoolean")) (param @Text (cs (col.columnName)))) tableCols
        let valuesSnippet = mconcat $ Data.List.intersperse (Snippet.sql ",") snippetValues
        let theSnippet = Snippet.sql (cs ("INSERT INTO " <> tableName <> " VALUES (")) <> valuesSnippet <> Snippet.sql ")"
        let session = snippetToSession theSnippet Decoders.noResult
        runSessionOnConnection connection session
        Hasql.release connection
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
        Hasql.release connection
        render EditRowView { .. }

    action UpdateRowAction = do
        let tableName = param "tableName"
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        tableCols <- fetchTableCols connection tableName
        primaryKeyFields <- tablePrimaryKeyFields connection tableName

        let snippetValues :: [Snippet] = map (\col -> parseValues (param @Bool (cs (col.columnName) <> "_")) (param @Bool (cs (col.columnName) <> "-isBoolean")) (param @Text (cs (col.columnName)))) tableCols
        let columns :: [Text] = map (\col -> cs (col.columnName)) tableCols
        let primaryKeyValues = map (\pkey -> "'" <> (param @Text (cs pkey <> "-pk")) <> "'") primaryKeyFields

        let setSnippets = mconcat $ Data.List.intersperse (Snippet.sql ", ") $ zipWith (\colName val -> Snippet.sql (cs ("\"" <> colName <> "\" = ")) <> val) columns snippetValues
        let whereClause = intercalate " AND " (updateValues (zip primaryKeyFields primaryKeyValues))
        let theSnippet = Snippet.sql (cs ("UPDATE " <> tableName <> " SET ")) <> setSnippets <> Snippet.sql (cs (" WHERE " <> whereClause))
        let session = snippetToSession theSnippet Decoders.noResult
        runSessionOnConnection connection session
        Hasql.release connection
        redirectTo ShowTableRowsAction { .. }

    action EditRowValueAction { tableName, targetName, id } = do
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection

        rows :: [[DynamicField]] <- fetchRows connection tableName

        let targetId = cs id
        Hasql.release connection
        render EditValueView { .. }

    action ToggleBooleanFieldAction { tableName, targetName, targetPrimaryKey } = do
        let id :: String = cs (param @Text "id")
        let tableName = param "tableName"
        connection <- connectToAppDb
        tableNames <- fetchTableNames connection
        tableCols <- fetchTableCols connection tableName
        primaryKeyFields <- tablePrimaryKeyFields connection tableName
        let targetPrimaryKeyValues = T.splitOn "---" targetPrimaryKey
        let whereClause = intercalate " AND " (zipWith (\field val -> "\"" <> field <> "\" = " <> quoteSqlValue val) primaryKeyFields targetPrimaryKeyValues)
        let query = "UPDATE \"" <> tableName <> "\" SET \"" <> targetName <> "\" = NOT \"" <> targetName <> "\" WHERE " <> whereClause
        runSessionOnConnection connection (Hasql.sql (cs query))
        Hasql.release connection
        redirectTo ShowTableRowsAction { .. }

    action UpdateValueAction = do
        let id :: String = cs (param @Text "id")
        let tableName = param "tableName"
        connection <- connectToAppDb
        let targetCol = param "targetName"
        let targetValue = param "targetValue"
        let query = "UPDATE " <> tableName <> " SET " <> targetCol <> " = '" <> targetValue <> "' WHERE id = '" <> cs id <> "'"
        runSessionOnConnection connection (Hasql.sql (cs query))
        Hasql.release connection
        redirectTo ShowTableRowsAction { .. }

    action DeleteTableRowsAction { tableName } = do
        connection <- connectToAppDb
        let query = "TRUNCATE TABLE " <> tableName
        runSessionOnConnection connection (Hasql.sql (cs query))
        Hasql.release connection
        redirectTo ShowTableRowsAction { .. }

    action AutocompleteForeignKeyColumnAction { tableName, columnName, term } = do
        connection <- connectToAppDb
        rows :: Maybe [[DynamicField]] <- do
            foreignKeyInfo <- fetchForeignKeyInfo connection tableName columnName

            case foreignKeyInfo of
                Just (foreignTable, foreignColumn) -> Just <$> fetchRowsPage connection foreignTable 1 50
                Nothing -> pure Nothing

        Hasql.release connection

        case rows of
            Just rows -> renderJson rows
            Nothing -> renderNotFound

    action ShowForeignKeyHoverCardAction { tableName, id, columnName } = do
        connection <- connectToAppDb
        hovercardData <- do
            let selectQuery = "SELECT \"" <> columnName <> "\" FROM \"" <> tableName <> "\" WHERE id = " <> quoteSqlValue id
            let decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.uuid))
            foreignId :: UUID <- runSessionOnConnection connection (snippetToSession (Snippet.sql (cs selectQuery)) decoder)

            foreignKeyInfo <- fetchForeignKeyInfo connection tableName columnName

            case foreignKeyInfo of
                Just (foreignTable, foreignColumn) -> do
                    record <- fetchSingleRowAsJson connection ("SELECT * FROM \"" <> foreignTable <> "\" WHERE \"" <> foreignColumn <> "\" = '" <> tshow foreignId <> "' LIMIT 1")
                    pure $ Just (record, foreignTable)
                Nothing -> pure Nothing
        Hasql.release connection

        case hovercardData of
            Just (record, foreignTableName) -> render ShowForeignKeyHoverCardView { record, foreignTableName }
            Nothing -> renderNotFound

connectToAppDb :: (?context :: ControllerContext) => IO Hasql.Connection
connectToAppDb = do
    result <- Hasql.acquire (connectionSettingsFromDatabaseUrl (cs ?context.frameworkConfig.databaseUrl))
    case result of
        Left err -> error ("connectToAppDb: Failed to acquire connection: " <> show err)
        Right conn -> pure conn

-- | Run a hasql session on a connection, throwing on error
runSessionOnConnection :: Hasql.Connection -> Hasql.Session a -> IO a
runSessionOnConnection connection session = do
    result <- Hasql.run session connection
    case result of
        Left err -> throwIO (userError (cs (show err)))
        Right val -> pure val

fetchTableNames :: Hasql.Connection -> IO [Text]
fetchTableNames connection = do
    let session = snippetToSession
            (Snippet.sql "SELECT tablename FROM pg_catalog.pg_tables where schemaname = 'public'")
            (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.text)))
    runSessionOnConnection connection session

fetchTableCols :: Hasql.Connection -> Text -> IO [ColumnDefinition]
fetchTableCols connection tableName = do
    let theSnippet = Snippet.sql "SELECT column_name,data_type,column_default,CASE WHEN is_nullable='YES' THEN true ELSE false END FROM information_schema.columns where table_name = " <> Snippet.param tableName <> Snippet.sql " ORDER BY ordinal_position"
    let decoder = Decoders.rowList columnDefinitionDecoder
    runSessionOnConnection connection (snippetToSession theSnippet decoder)

fetchRow :: Hasql.Connection -> Text -> [Text] -> IO [[DynamicField]]
fetchRow connection tableName primaryKeyValues = do
    pkFields <- tablePrimaryKeyFields connection tableName
    let whereClause = intercalate " AND " (zipWith (\field val -> "\"" <> field <> "\" = " <> quoteSqlValue val) pkFields primaryKeyValues)
    let query = "SELECT row_to_json(t)::jsonb FROM (SELECT * FROM " <> tableName <> " WHERE " <> whereClause <> ") t"
    fetchJsonRows connection query

columnDefinitionDecoder :: Decoders.Row ColumnDefinition
columnDefinitionDecoder = ColumnDefinition
    <$> Decoders.column (Decoders.nonNullable Decoders.text)
    <*> Decoders.column (Decoders.nonNullable Decoders.text)
    <*> Decoders.column (Decoders.nullable Decoders.text)
    <*> Decoders.column (Decoders.nonNullable Decoders.bool)

tablePrimaryKeyFields :: Hasql.Connection -> Text -> IO [Text]
tablePrimaryKeyFields connection tableName = do
    let theSnippet = Snippet.sql "SELECT a.attname FROM pg_index i JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY(i.indkey) WHERE i.indrelid = " <> Snippet.param tableName <> Snippet.sql "::regclass AND i.indisprimary"
    let decoder = Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.text))
    runSessionOnConnection connection (snippetToSession theSnippet decoder)

fetchRows :: Hasql.Connection -> Text -> IO [[DynamicField]]
fetchRows connection tableName = do
    pkFields <- tablePrimaryKeyFields connection tableName

    let orderClause = if null pkFields
            then ""
            else " ORDER BY " <> intercalate ", " pkFields

    let query = "SELECT row_to_json(t)::jsonb FROM (SELECT * FROM "
            <> tableName
            <> orderClause
            <> ") t"

    fetchJsonRows connection query

fetchRowsPage :: Hasql.Connection -> Text -> Int -> Int -> IO [[DynamicField]]
fetchRowsPage connection tableName page rows = do
    pkFields <- tablePrimaryKeyFields connection tableName
    let slice = " OFFSET " <> show (page * rows - rows) <> " ROWS FETCH FIRST " <> show rows <> " ROWS ONLY"
    let orderClause = if null pkFields
            then ""
            else " ORDER BY " <> intercalate ", " pkFields

    let query = "SELECT row_to_json(t)::jsonb FROM (SELECT * FROM "
            <> tableName
            <> orderClause
            <> slice
            <> ") t"

    fetchJsonRows connection query

tableLength :: Hasql.Connection -> Text -> IO Int
tableLength connection tableName = do
    let query = "SELECT COUNT(*) FROM \"" <> tableName <> "\""
    let decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable (fromIntegral <$> Decoders.int8)))
    runSessionOnConnection connection (snippetToSession (Snippet.sql (cs query)) decoder)


-- | Fetch rows as JSON using row_to_json, then convert to [[DynamicField]]
fetchJsonRows :: Hasql.Connection -> Text -> IO [[DynamicField]]
fetchJsonRows connection query = do
    let decoder = Decoders.rowList (Decoders.column (Decoders.nonNullable (Decoders.jsonb)))
    let session = snippetToSession (Snippet.sql (cs query)) decoder
    jsonRows <- runSessionOnConnection connection session
    pure $ map jsonToFields jsonRows

-- | Fetch a single row as JSON using row_to_json, then convert to [DynamicField]
fetchSingleRowAsJson :: Hasql.Connection -> Text -> IO [DynamicField]
fetchSingleRowAsJson connection query = do
    let wrappedQuery = "SELECT row_to_json(t)::jsonb FROM (" <> query <> ") t"
    let decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.jsonb)))
    let session = snippetToSession (Snippet.sql (cs wrappedQuery)) decoder
    jsonRow <- runSessionOnConnection connection session
    pure $ jsonToFields jsonRow

-- | Convert a JSON object (from row_to_json) to a list of DynamicField
jsonToFields :: Aeson.Value -> [DynamicField]
jsonToFields (Aeson.Object obj) =
    map (\(key, val) -> DynamicField { fieldName = cs (Aeson.Key.toText key), fieldValue = jsonValueToBytes val })
        (Aeson.KeyMap.toList obj)
jsonToFields _ = []

-- | Convert a JSON value to Maybe ByteString for DynamicField
jsonValueToBytes :: Aeson.Value -> Maybe ByteString
jsonValueToBytes Aeson.Null = Nothing
jsonValueToBytes (Aeson.String t) = Just (cs t)
jsonValueToBytes (Aeson.Bool True) = Just "true"
jsonValueToBytes (Aeson.Bool False) = Just "false"
jsonValueToBytes other = Just (cs (encode other))

-- parseValues sqlMode isBoolField input
parseValues :: Bool -> Bool -> Text -> Snippet
parseValues _ True "on" = Snippet.param True
parseValues _ True "off" = Snippet.param False
parseValues False _ text = Snippet.param text
parseValues _ _ text = Snippet.sql (cs text)

updateValues list = map (\elem -> fst elem <> " = " <> snd elem) list

isQuery sql = T.isInfixOf "SELECT" u
    where u = T.toUpper sql

-- | Quote a SQL value for use in a raw SQL string
quoteSqlValue :: Text -> Text
quoteSqlValue val = "'" <> T.replace "'" "''" val <> "'"

fetchForeignKeyInfo :: Hasql.Connection -> Text -> Text -> IO (Maybe (Text, Text))
fetchForeignKeyInfo connection tableName columnName = do
    let theSnippet = Snippet.sql "SELECT ccu.table_name AS foreign_table_name, ccu.column_name AS foreign_column_name FROM information_schema.table_constraints AS tc JOIN information_schema.key_column_usage AS kcu ON tc.constraint_name = kcu.constraint_name AND tc.table_schema = kcu.table_schema JOIN information_schema.constraint_column_usage AS ccu ON ccu.constraint_name = tc.constraint_name AND ccu.table_schema = tc.table_schema WHERE tc.constraint_type = 'FOREIGN KEY' AND tc.table_name = " <> Snippet.param tableName <> Snippet.sql " AND kcu.column_name = " <> Snippet.param columnName
    let decoder = Decoders.rowList $ (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
    result <- runSessionOnConnection connection (snippetToSession theSnippet decoder)
    case result of
        [(foreignTableName, foreignColumnName)] -> pure $ Just (foreignTableName, foreignColumnName)
        otherwise -> pure $ Nothing

instance {-# OVERLAPS #-} ToJSON [DynamicField] where
    toJSON fields = object (map (\DynamicField { fieldName, fieldValue } -> (cs fieldName) .= (fieldValueToJSON fieldValue)) fields)
        where
            fieldValueToJSON (Just bs) = toJSON ((cs bs) :: Text)
            fieldValueToJSON Nothing = toJSON Null
    toEncoding fields = pairs $ foldl' (<>) mempty (encodedFields)
        where
            encodedFields = (map (\DynamicField { fieldName, fieldValue } -> (cs fieldName) .= (fieldValueToJSON fieldValue)) fields)
            fieldValueToJSON (Just bs) = toJSON ((cs bs) :: Text)
            fieldValueToJSON Nothing = toJSON Null
