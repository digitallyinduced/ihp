{-# LANGUAGE UndecidableInstances #-}
module IHP.DataSync.REST.Controller where

import IHP.ControllerPrelude hiding (OrderByClause)
import IHP.DataSync.REST.Types
import Data.Aeson
import qualified Data.Vector as Vector
import qualified Data.ByteString.Char8 as ByteString
import qualified Control.Exception.Safe as Exception
import IHP.DataSync.RowLevelSecurity
import IHP.DataSync.DynamicQuery
import IHP.DataSync.Types
import Network.HTTP.Types (status400)
import IHP.DataSync.DynamicQueryCompiler
import IHP.DataSync.TypedEncoder (makeCachedColumnTypeLookup, typedAesonValueToSnippet, lookupColumnType)
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Data.Text as Text
import qualified Data.List as List

import qualified Data.ByteString.Builder as ByteString
import qualified Data.Aeson.Encoding.Internal as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Hasql.Decoders as Decoders

instance (
    Show (Id' (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Controller ApiController where
    action CreateRecordAction { table } = do
        let hasqlPool = ?modelContext.hasqlPool
        ensureRLSEnabled hasqlPool table

        columnTypeLookup <- makeCachedColumnTypeLookup hasqlPool
        columnTypes <- columnTypeLookup table

        payload <- requestBodyJSON

        case payload of
            Object hashMap -> do
                let pairsList = hashMap
                        |> Aeson.toList
                        |> map (\(key, val) ->
                            let col = fieldNameToColumnName (Aeson.toText key)
                            in (col, lookupColumnType columnTypes col, val)
                        )

                let columns = map (\(c,_,_) -> c) pairsList
                let valueSnippets = map (\(_, colType, val) -> typedAesonValueToSnippet colType val) pairsList

                let insertResult = compileInsert table columns valueSnippets camelCaseRenamer columnTypes
                let stmt = compiledQueryStatement insertResult

                result :: Either SomeException [[Field]] <- Exception.try do
                    sqlQueryWriteWithRLS hasqlPool stmt

                case result of
                    Left e -> renderErrorJson (show e :: Text)
                    Right result -> renderJson result

            Array objects -> do
                let objectList = Vector.toList objects
                case objectList of
                    [] -> renderErrorJson ("At least one record is required" :: Text)
                    (firstElement:_) -> case firstElement of
                        Object firstHashMap -> do
                            let columns = firstHashMap
                                    |> Aeson.keys
                                    |> map (fieldNameToColumnName . Aeson.toText)

                            let parseObject value = case value of
                                    Object hashMap -> Right hashMap
                                    _otherwise -> Left ("Expected object, got: " <> show value)

                            case mapM parseObject objectList of
                                Left err -> renderErrorJson (cs err :: Text)
                                Right hashMaps -> do
                                    let encodeRow hashMap = map
                                            (\col ->
                                                let fieldName = columnNameToFieldName col
                                                    val = fromMaybe Data.Aeson.Null (Aeson.lookup (Aeson.fromText fieldName) hashMap)
                                                in typedAesonValueToSnippet (lookupColumnType columnTypes col) val)
                                            columns
                                    let valueRows = map encodeRow hashMaps

                                    let insertResult = compileInsertMany table columns valueRows camelCaseRenamer columnTypes
                                    let stmt = compiledQueryStatement insertResult

                                    result :: [[Field]] <- sqlQueryWriteWithRLS hasqlPool stmt
                                    renderJson result
                        _otherwise -> renderErrorJson ("Expected object" :: Text)

            _ -> error "Expected JSON object or array"

    action UpdateRecordAction { table, id } = do
        let hasqlPool = ?modelContext.hasqlPool
        ensureRLSEnabled hasqlPool table

        columnTypeLookup <- makeCachedColumnTypeLookup hasqlPool
        columnTypes <- columnTypeLookup table

        payload <- requestBodyJSON
        let hashMap = case payload of
                Object hm -> hm
                _ -> error "Expected JSON object"

        let setSql = encodeKeyMapToSetSql columnTypes hashMap
        let updateResult = compileUpdate table setSql (Snippet.sql "id = " <> uuidParam id) camelCaseRenamer columnTypes
        let stmt = compiledQueryStatement updateResult

        result :: [[Field]] <- sqlQueryWriteWithRLS hasqlPool stmt

        renderJson (head result)

    -- DELETE /api/:table/:id
    action DeleteRecordAction { table, id } = do
        let hasqlPool = ?modelContext.hasqlPool
        ensureRLSEnabled hasqlPool table

        let deleteSnippet = Snippet.sql ("DELETE FROM " <> quoteIdentifier table <> " WHERE id = ") <> uuidParam id
        let stmt = Snippet.toPreparedStatement deleteSnippet Decoders.noResult
        sqlExecWithRLS hasqlPool stmt

        renderJson True

    -- GET /api/:table/:id
    action ShowRecordAction { table, id } = do
        let hasqlPool = ?modelContext.hasqlPool
        ensureRLSEnabled hasqlPool table

        columnTypeLookup <- makeCachedColumnTypeLookup hasqlPool
        columnTypes <- columnTypeLookup table
        let selectColumns = compileSelectedColumns camelCaseRenamer columnTypes SelectAll
        let selectSnippet = Snippet.sql ("SELECT " <> selectColumns <> " FROM " <> quoteIdentifier table <> " WHERE id = ") <> uuidParam id
        let stmt = Snippet.toPreparedStatement (wrapDynamicQuery selectSnippet) dynamicRowDecoder
        result :: [[Field]] <- sqlQueryWithRLS hasqlPool stmt

        renderJson (head result)

    -- GET /api/:table
    -- GET /api/:table?orderBy=createdAt
    -- GET /api/:table?fields=id,title
    action ListRecordsAction { table } = do
        let hasqlPool = ?modelContext.hasqlPool
        ensureRLSEnabled hasqlPool table

        columnTypeLookup <- makeCachedColumnTypeLookup hasqlPool
        columnTypes <- columnTypeLookup table
        let querySnippet = compileQueryTyped camelCaseRenamer columnTypes (buildDynamicQueryFromRequest table)
        let stmt = compiledQueryStatement querySnippet
        result :: [[Field]] <- sqlQueryWithRLS hasqlPool stmt

        renderJson result

    action GraphQLQueryAction = do
        error "GraphQLQueryAction is handled by the GraphQL middleware"

buildDynamicQueryFromRequest table = DynamicSQLQuery
    { table
    , selectedColumns = paramOrDefault SelectAll "fields"
    , whereCondition = Nothing
    , orderByClause = paramList "orderBy"
    , distinctOnColumn = paramOrNothing "distinctOnColumn"
    , limit = paramOrNothing "limit"
    , offset = paramOrNothing "offset"
    }

instance ParamReader SelectedColumns where
    readParameter byteString = pure $
        byteString
            |> cs
            |> Text.split (\char -> char == ',')
            |> SelectSpecific

instance ParamReader OrderByClause where
    readParameter byteString = case ByteString.split ',' byteString of
            [orderByColumn, order] -> do
                orderByDirection <- parseOrder order
                pure OrderByClause { orderByColumn, orderByDirection }
            [orderByColumn] -> pure OrderByClause { orderByColumn, orderByDirection = Asc }
            _ -> Left "Invalid order by clause"
        where
            parseOrder "asc" = Right Asc
            parseOrder "desc" = Right Desc
            parseOrder otherwise = Left ("Invalid order " <> cs otherwise)

-- | Encode an Aeson KeyMap (from REST JSON payload) into a SQL SET clause 'Snippet'.
encodeKeyMapToSetSql :: ColumnTypeInfo -> Aeson.KeyMap Value -> Snippet
encodeKeyMapToSetSql columnTypes hashMap =
    let pairsList = hashMap
            |> Aeson.toList
            |> map (\(key, val) ->
                let col = fieldNameToColumnName (Aeson.toText key)
                in (col, lookupColumnType columnTypes col, val)
            )
        encodeSetClause (col, colType, val) =
            Snippet.sql (quoteIdentifier col <> " = ") <> typedAesonValueToSnippet colType val
        setSnippets = map encodeSetClause pairsList
    in mconcat $ List.intersperse (Snippet.sql ", ") setSnippets

renderErrorJson :: (?context :: ControllerContext, ?request :: Request) => ToJSON json => json -> IO ()
renderErrorJson json = renderJsonWithStatusCode status400 json
{-# INLINABLE renderErrorJson #-}

instance ToJSON GraphQLResult where
    toJSON GraphQLResult { requestId, graphQLResult } = object [ "tag" .= ("GraphQLResult" :: Text), "requestId" .= requestId, "graphQLResult" .= ("" :: Text) ]
    toEncoding GraphQLResult { requestId, graphQLResult } = Aeson.econcat
        [ Aeson.unsafeToEncoding "{\"tag\":\"GraphQLResult\",\"requestId\":"
        , Aeson.int requestId
        , Aeson.unsafeToEncoding ",\"graphQLResult\":"
        , toEncoding graphQLResult
        , Aeson.unsafeToEncoding "}"
        ]
instance ToJSON UndecodedJSON where
    toJSON (UndecodedJSON _) = error "Not implemented"
    toEncoding (UndecodedJSON json) = Aeson.unsafeToEncoding (ByteString.byteString json)
