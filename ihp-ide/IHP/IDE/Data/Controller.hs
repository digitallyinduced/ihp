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
import qualified Hasql.Connection.Settings as HasqlSettings
import qualified Hasql.Session as Session
import qualified Hasql.Errors as HasqlErrors
import qualified Hasql.Decoders as Decoders
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.List as List
import qualified Control.Exception.Safe as Exception
import Data.Int (Int64)

instance Controller DataController where
    action ShowDatabaseAction = do
        tableNames <- withAppDb fetchTableNames
        case headMay tableNames of
            Just tableName -> jumpToAction ShowTableRowsAction { tableName }
            Nothing -> render ShowDatabaseView { .. }

    action ShowTableRowsAction { tableName } = do
        let page :: Int = paramOrDefault @Int 1 "page"
        let pageSize :: Int = paramOrDefault @Int 20 "rows"
        (tableNames, primaryKeyFields, rows :: [[DynamicField]], tableCols, totalRows) <- withAppDb \connection -> do
            tableNames <- fetchTableNames connection
            primaryKeyFields <- tablePrimaryKeyFields connection tableName
            rows <- fetchRowsPage connection tableName page pageSize
            tableCols <- fetchTableCols connection tableName
            totalRows <- tableLength connection tableName
            pure (tableNames, primaryKeyFields, rows, tableCols, totalRows)
        render ShowTableRowsView { .. }

    action NewQueryAction = do
        let queryText = ""
        let queryResult = Nothing
        render ShowQueryView { .. }

    action QueryAction = do
        let queryText = param @Text "query"
        when (isEmpty queryText) do
            redirectTo NewQueryAction

        queryResult :: Maybe (Either SqlConsoleError SqlConsoleResult) <- withAppDb \connection ->
            Just <$> if isQuery queryText then do
                    let snippet = wrapDynamicQuery (Snippet.sql (cs queryText))
                    let statement = Snippet.toStatement snippet dynamicFieldDecoder
                    let session = Session.statement () statement
                    result <- Hasql.use connection session
                    case result of
                        Right rows -> pure (Right (SelectQueryResult rows))
                        Left err -> pure (Left (sessionErrorToConsoleError err))
                else do
                    let statement = Snippet.toStatement (Snippet.sql (cs queryText)) Decoders.rowsAffected
                    let session = Session.statement () statement
                    result <- Hasql.use connection session
                    case result of
                        Right count -> pure (Right (InsertOrUpdateResult count))
                        Left err -> pure (Left (sessionErrorToConsoleError err))

        render ShowQueryView { .. }

    action DeleteEntryAction { primaryKey, tableName } = do
        withAppDb \connection -> do
            primaryKeyFields <- tablePrimaryKeyFields connection tableName
            let primaryKeyValues = T.splitOn "---" primaryKey
            let whereClause = mconcat $ List.intersperse (Snippet.sql " AND ") $
                    zipWith (\field val -> quoteIdentifier field <> Snippet.sql " = " <> Snippet.param val) primaryKeyFields primaryKeyValues
            let snippet = Snippet.sql "DELETE FROM " <> quoteIdentifier tableName <> Snippet.sql " WHERE " <> whereClause
            runSnippetExec connection snippet
        redirectTo ShowTableRowsAction { .. }

    action NewRowAction { tableName } = do
        (tableNames, rows :: [[DynamicField]], tableCols) <- withAppDb \connection -> do
            tableNames <- fetchTableNames connection
            rows <- fetchRows connection tableName
            tableCols <- fetchTableCols connection tableName
            pure (tableNames, rows, tableCols)
        render NewRowView { .. }

    action CreateRowAction = do
        let tableName = param "tableName"
        withAppDb \connection -> do
            tableCols <- fetchTableCols connection tableName
            let values :: [Snippet] = map (\col -> parseValues (param @Bool (cs (col.columnName) <> "_")) (param @Bool (cs (col.columnName) <> "-isBoolean")) (param @Text (cs (col.columnName)))) tableCols
            let snippet = Snippet.sql "INSERT INTO " <> quoteIdentifier tableName <> Snippet.sql " VALUES (" <> (mconcat $ List.intersperse (Snippet.sql ", ") values) <> Snippet.sql ")"
            runSnippetExec connection snippet
        redirectTo ShowTableRowsAction { .. }

    action EditRowAction { tableName, targetPrimaryKey } = do
        (tableNames, primaryKeyFields, rows :: [[DynamicField]], tableCols, rowValues) <- withAppDb \connection -> do
            tableNames <- fetchTableNames connection
            primaryKeyFields <- tablePrimaryKeyFields connection tableName
            rows <- fetchRows connection tableName
            tableCols <- fetchTableCols connection tableName
            let targetPrimaryKeyValues = T.splitOn "---" targetPrimaryKey
            values <- fetchRow connection tableName targetPrimaryKeyValues
            let (Just rowValues) = head values
            pure (tableNames, primaryKeyFields, rows, tableCols, rowValues)
        render EditRowView { .. }

    action UpdateRowAction = do
        let tableName = param "tableName"
        withAppDb \connection -> do
            tableCols <- fetchTableCols connection tableName
            primaryKeyFields <- tablePrimaryKeyFields connection tableName

            let values :: [Snippet] = map (\col -> parseValues (param @Bool (cs (col.columnName) <> "_")) (param @Bool (cs (col.columnName) <> "-isBoolean")) (param @Text (cs (col.columnName)))) tableCols
            let columns :: [Text] = map (\col -> col.columnName) tableCols

            let setClause = mconcat $ List.intersperse (Snippet.sql ", ") $
                    zipWith (\col val -> quoteIdentifier col <> Snippet.sql " = " <> val) columns values
            let whereClause = mconcat $ List.intersperse (Snippet.sql " AND ") $
                    map (\pkey -> quoteIdentifier pkey <> Snippet.sql " = " <> Snippet.param (param @Text (cs pkey <> "-pk"))) primaryKeyFields

            let snippet = Snippet.sql "UPDATE " <> quoteIdentifier tableName <> Snippet.sql " SET " <> setClause <> Snippet.sql " WHERE " <> whereClause
            runSnippetExec connection snippet
        redirectTo ShowTableRowsAction { .. }

    action EditRowValueAction { tableName, targetName, id } = do
        (tableNames, rows :: [[DynamicField]]) <- withAppDb \connection -> do
            tableNames <- fetchTableNames connection
            rows <- fetchRows connection tableName
            pure (tableNames, rows)
        let targetId = cs id
        render EditValueView { .. }

    action ToggleBooleanFieldAction { tableName, targetName, targetPrimaryKey } = do
        let id :: String = cs (param @Text "id")
        let tableName = param "tableName"
        withAppDb \connection -> do
            primaryKeyFields <- tablePrimaryKeyFields connection tableName
            let targetPrimaryKeyValues = T.splitOn "---" targetPrimaryKey
            let whereClause = mconcat $ List.intersperse (Snippet.sql " AND ") $
                    zipWith (\field val -> quoteIdentifier field <> Snippet.sql " = " <> Snippet.param val) primaryKeyFields targetPrimaryKeyValues
            let snippet = Snippet.sql "UPDATE " <> quoteIdentifier tableName <> Snippet.sql " SET " <> quoteIdentifier targetName <> Snippet.sql " = NOT " <> quoteIdentifier targetName <> Snippet.sql " WHERE " <> whereClause
            runSnippetExec connection snippet
        redirectTo ShowTableRowsAction { .. }

    action UpdateValueAction = do
        let id :: String = cs (param @Text "id")
        let tableName = param "tableName"
        let targetCol = param @Text "targetName"
        let targetValue = param @Text "targetValue"
        let snippet = Snippet.sql "UPDATE " <> quoteIdentifier tableName <> Snippet.sql " SET " <> quoteIdentifier targetCol <> Snippet.sql " = " <> Snippet.param targetValue <> Snippet.sql " WHERE id = " <> Snippet.param (cs id :: Text)
        withAppDb \connection ->
            runSnippetExec connection snippet
        redirectTo ShowTableRowsAction { .. }

    action DeleteTableRowsAction { tableName } = do
        let snippet = Snippet.sql "TRUNCATE TABLE " <> quoteIdentifier tableName
        withAppDb \connection ->
            runSnippetExec connection snippet
        redirectTo ShowTableRowsAction { .. }

    action AutocompleteForeignKeyColumnAction { tableName, columnName, term } = do
        rows :: Maybe [[DynamicField]] <- withAppDb \connection -> do
            foreignKeyInfo <- fetchForeignKeyInfo connection tableName columnName

            case foreignKeyInfo of
                Just (foreignTable, foreignColumn) -> Just <$> fetchRowsPage connection foreignTable 1 50
                Nothing -> pure Nothing

        case rows of
            Just rows -> renderJson rows
            Nothing -> renderNotFound

    action ShowForeignKeyHoverCardAction { tableName, id, columnName } = do
        hovercardData <- withAppDb \connection -> do
            let fetchIdSnippet = Snippet.sql "SELECT " <> quoteIdentifier columnName <> Snippet.sql "::text FROM " <> quoteIdentifier tableName <> Snippet.sql " WHERE id = " <> Snippet.param id
            foreignIdResult <- runSnippetQuery connection fetchIdSnippet (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.text)))

            case foreignIdResult of
                [foreignId] -> do
                    foreignKeyInfo <- fetchForeignKeyInfo connection tableName columnName

                    case foreignKeyInfo of
                        Just (foreignTable, foreignColumn) -> do
                            let fetchRecordSnippet = wrapDynamicQuery (Snippet.sql "SELECT * FROM " <> quoteIdentifier foreignTable <> Snippet.sql " WHERE " <> quoteIdentifier foreignColumn <> Snippet.sql " = " <> Snippet.param foreignId <> Snippet.sql "::uuid LIMIT 1")
                            records <- runSnippetQuery connection fetchRecordSnippet dynamicFieldDecoder
                            case records of
                                [record] -> pure $ Just (record, foreignTable)
                                _ -> pure Nothing
                        Nothing -> pure Nothing
                _ -> pure Nothing

        case hovercardData of
            Just (record, foreignTableName) -> render ShowForeignKeyHoverCardView { record, foreignTableName }
            Nothing -> renderNotFound

withAppDb :: (?context :: ControllerContext) => (Hasql.Connection -> IO a) -> IO a
withAppDb action = do
    let databaseUrl = ?context.frameworkConfig.databaseUrl
    connResult <- Hasql.acquire (HasqlSettings.connectionString (cs databaseUrl))
    case connResult of
        Right conn -> action conn `Exception.finally` Hasql.release conn
        Left err -> error (HasqlErrors.toDetailedText err)

runSnippetQuery :: Hasql.Connection -> Snippet -> Decoders.Result a -> IO a
runSnippetQuery conn snippet decoder = do
    let statement = Snippet.toStatement snippet decoder
    let session = Session.statement () statement
    result <- Hasql.use conn session
    case result of
        Right a -> pure a
        Left err -> error (HasqlErrors.toDetailedText err)

runSnippetExec :: Hasql.Connection -> Snippet -> IO ()
runSnippetExec conn snippet = do
    let statement = Snippet.toStatement snippet Decoders.noResult
    let session = Session.statement () statement
    result <- Hasql.use conn session
    case result of
        Right () -> pure ()
        Left err -> error (HasqlErrors.toDetailedText err)

fetchTableNames :: Hasql.Connection -> IO [Text]
fetchTableNames connection =
    runSnippetQuery connection
        (Snippet.sql "SELECT tablename FROM pg_catalog.pg_tables WHERE schemaname = 'public'")
        (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.text)))

fetchTableCols :: Hasql.Connection -> Text -> IO [ColumnDefinition]
fetchTableCols connection tableName =
    runSnippetQuery connection
        (Snippet.sql "SELECT column_name, data_type, column_default, CASE WHEN is_nullable='YES' THEN true ELSE false END FROM information_schema.columns WHERE table_name = " <> Snippet.param tableName <> Snippet.sql " ORDER BY ordinal_position")
        columnDefinitionDecoder

fetchRow :: Hasql.Connection -> Text -> [Text] -> IO [[DynamicField]]
fetchRow connection tableName primaryKeyValues = do
    pkFields <- tablePrimaryKeyFields connection tableName
    let whereClause = mconcat $ List.intersperse (Snippet.sql " AND ") $
            zipWith (\field val -> quoteIdentifier field <> Snippet.sql " = " <> Snippet.param val) pkFields primaryKeyValues
    let snippet = wrapDynamicQuery (Snippet.sql "SELECT * FROM " <> quoteIdentifier tableName <> Snippet.sql " WHERE " <> whereClause)
    runSnippetQuery connection snippet dynamicFieldDecoder

columnDefinitionDecoder :: Decoders.Result [ColumnDefinition]
columnDefinitionDecoder = Decoders.rowList $
    ColumnDefinition
        <$> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.bool)

-- | Decoder for dynamic query results wrapped with 'wrapDynamicQuery'.
-- Each row comes back as a single JSONB column (from row_to_json), which is then
-- parsed into a list of DynamicField values.
dynamicFieldDecoder :: Decoders.Result [[DynamicField]]
dynamicFieldDecoder = Decoders.rowList $
    Decoders.column (Decoders.nonNullable Decoders.jsonb)
    |> fmap (\case
        Aeson.Object obj -> map jsonFieldToDynamicField (Aeson.toList obj)
        _ -> error "Expected JSON object from row_to_json"
    )

jsonFieldToDynamicField :: (Aeson.Key, Aeson.Value) -> DynamicField
jsonFieldToDynamicField (key, val) = DynamicField
    { fieldValue = case val of
        Aeson.Null -> Nothing
        Aeson.String t -> Just (cs t)
        Aeson.Bool True -> Just "true"
        Aeson.Bool False -> Just "false"
        other -> Just (cs (Aeson.encode other))
    , fieldName = cs (Aeson.toText key)
    }

tablePrimaryKeyFields :: Hasql.Connection -> Text -> IO [Text]
tablePrimaryKeyFields connection tableName =
    runSnippetQuery connection
        (Snippet.sql "SELECT a.attname FROM pg_index i JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY(i.indkey) WHERE i.indrelid = " <> Snippet.param tableName <> Snippet.sql "::regclass AND i.indisprimary")
        (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.text)))

fetchRows :: Hasql.Connection -> Text -> IO [[DynamicField]]
fetchRows connection tableName = do
    pkFields <- tablePrimaryKeyFields connection tableName

    let orderBy = if null pkFields
            then mempty
            else Snippet.sql " ORDER BY " <> (mconcat $ List.intersperse (Snippet.sql ", ") (map quoteIdentifier pkFields))

    let snippet = wrapDynamicQuery (Snippet.sql "SELECT * FROM " <> quoteIdentifier tableName <> orderBy)
    runSnippetQuery connection snippet dynamicFieldDecoder

fetchRowsPage :: Hasql.Connection -> Text -> Int -> Int -> IO [[DynamicField]]
fetchRowsPage connection tableName page rows = do
    pkFields <- tablePrimaryKeyFields connection tableName

    let orderBy = if null pkFields
            then mempty
            else Snippet.sql " ORDER BY " <> (mconcat $ List.intersperse (Snippet.sql ", ") (map quoteIdentifier pkFields))

    let snippet = wrapDynamicQuery (
            Snippet.sql "SELECT * FROM " <> quoteIdentifier tableName
            <> orderBy
            <> Snippet.sql " OFFSET " <> Snippet.param (fromIntegral (page * rows - rows) :: Int64)
            <> Snippet.sql " ROWS FETCH FIRST " <> Snippet.param (fromIntegral rows :: Int64)
            <> Snippet.sql " ROWS ONLY"
            )
    runSnippetQuery connection snippet dynamicFieldDecoder

tableLength :: Hasql.Connection -> Text -> IO Int
tableLength connection tableName = do
    count <- runSnippetQuery connection
        (Snippet.sql "SELECT COUNT(*) FROM " <> quoteIdentifier tableName)
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
    pure (fromIntegral count)

-- parseValues sqlMode isBoolField input
parseValues :: Bool -> Bool -> Text -> Snippet
parseValues _ True "on" = Snippet.param True
parseValues _ True "off" = Snippet.param False
parseValues False _ text = Snippet.param text
parseValues _ _ text = Snippet.sql (cs text)  -- raw SQL mode (for expressions like now(), DEFAULT, NULL)

isQuery :: Text -> Bool
isQuery sql = T.isInfixOf "SELECT" u
    where u = T.toUpper sql

-- | Quote a SQL identifier (table name, column name) to prevent SQL injection.
-- Duplicated from IHP.DataSync.DynamicQuery to avoid ihp-datasync dependency.
quoteIdentifier :: Text -> Snippet
quoteIdentifier name = Snippet.sql (cs ("\"" <> T.replace "\"" "\"\"" name <> "\""))

-- | Wraps a SQL query snippet so that each row is returned as a JSON object.
-- Duplicated from IHP.DataSync.DynamicQuery to avoid ihp-datasync dependency.
wrapDynamicQuery :: Snippet -> Snippet
wrapDynamicQuery innerQuery =
    Snippet.sql "WITH _ihp_dynamic_result AS (" <> innerQuery <> Snippet.sql ") SELECT row_to_json(t)::jsonb FROM _ihp_dynamic_result AS t"

fetchForeignKeyInfo :: Hasql.Connection -> Text -> Text -> IO (Maybe (Text, Text))
fetchForeignKeyInfo connection tableName columnName = do
    let snippet =
            Snippet.sql "SELECT ccu.table_name AS foreign_table_name, ccu.column_name AS foreign_column_name FROM information_schema.table_constraints AS tc JOIN information_schema.key_column_usage AS kcu ON tc.constraint_name = kcu.constraint_name AND tc.table_schema = kcu.table_schema JOIN information_schema.constraint_column_usage AS ccu ON ccu.constraint_name = tc.constraint_name AND ccu.table_schema = tc.table_schema WHERE tc.constraint_type = 'FOREIGN KEY' AND tc.table_name = "
            <> Snippet.param tableName
            <> Snippet.sql " AND kcu.column_name = "
            <> Snippet.param columnName
    let decoder = Decoders.rowList $
            (,) <$> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
    result <- runSnippetQuery connection snippet decoder
    case result of
        [(foreignTableName, foreignColumnName)] -> pure $ Just (foreignTableName, foreignColumnName)
        _ -> pure Nothing

sessionErrorToConsoleError :: HasqlErrors.SessionError -> SqlConsoleError
sessionErrorToConsoleError (HasqlErrors.StatementSessionError _ _ _ _ _ (HasqlErrors.ServerStatementError (HasqlErrors.ServerError code message detail hint _))) =
    SqlConsoleError { errorMessage = message, errorDetail = fromMaybe "" detail, errorHint = fromMaybe "" hint, errorState = code }
sessionErrorToConsoleError (HasqlErrors.ScriptSessionError _ (HasqlErrors.ServerError code message detail hint _)) =
    SqlConsoleError { errorMessage = message, errorDetail = fromMaybe "" detail, errorHint = fromMaybe "" hint, errorState = code }
sessionErrorToConsoleError err =
    SqlConsoleError { errorMessage = cs (show err), errorDetail = "", errorHint = "", errorState = "" }

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
