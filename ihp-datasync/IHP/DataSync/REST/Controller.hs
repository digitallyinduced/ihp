{-# LANGUAGE UndecidableInstances #-}
module IHP.DataSync.REST.Controller where

import IHP.ControllerPrelude hiding (OrderByClause)
import Network.Wai (Request)
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
import IHP.DataSync.TypedEncoder (ColumnTypeInfo(..), makeCachedColumnTypeLookup, typedAesonValueToSnippet, lookupColumnType)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.List as List

import qualified Data.ByteString.Builder as ByteString
import qualified Data.Aeson.Encoding.Internal as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import IHP.ModelSupport.Types (ModelContext(..))

instance (
    Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Controller ApiController where
    action CreateRecordAction { table } = do
        let hasqlPool = ?modelContext.hasqlPool
        ensureRLSEnabled hasqlPool table

        columnTypeLookup <- makeCachedColumnTypeLookup hasqlPool
        columnTypes <- columnTypeLookup table

        let payload = requestBodyJSON

        case payload of
            Object hashMap -> do
                let pairs = hashMap
                        |> Aeson.toList
                        |> map (\(key, val) ->
                            let col = fieldNameToColumnName (Aeson.toText key)
                            in (col, typedAesonValueToSnippet (lookupColumnType columnTypes col) val)
                        )

                let columns = map fst pairs
                let values = map snd pairs

                let snippet = compileInsert table columns values camelCaseRenamer columnTypes

                result :: Either SomeException [[Field]] <- Exception.try do
                    sqlQueryWriteWithRLS hasqlPool (wrapDynamicQuery snippet) dynamicRowDecoder

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
                                    let values = hashMaps
                                            |> map (\hashMap ->
                                                    columns
                                                    |> map (\col ->
                                                        let fieldName = columnNameToFieldName col
                                                            val = fromMaybe Data.Aeson.Null (Aeson.lookup (Aeson.fromText fieldName) hashMap)
                                                        in typedAesonValueToSnippet (lookupColumnType columnTypes col) val
                                                    )
                                                )

                                    let snippet = compileInsertMany table columns values camelCaseRenamer columnTypes

                                    result :: [[Field]] <- sqlQueryWriteWithRLS hasqlPool (wrapDynamicQuery snippet) dynamicRowDecoder
                                    renderJson result
                        _otherwise -> renderErrorJson ("Expected object" :: Text)

            _ -> error "Expected JSON object or array"

    action UpdateRecordAction { table, id } = do
        let hasqlPool = ?modelContext.hasqlPool
        ensureRLSEnabled hasqlPool table

        columnTypeLookup <- makeCachedColumnTypeLookup hasqlPool
        columnTypes <- columnTypeLookup table

        let payload = requestBodyJSON
                |> \case
                    Object hashMap -> hashMap
                    _ -> error "Expected JSON object"

        let keyValues = payload
                |> Aeson.toList
                |> map (\(key, val) ->
                    let col = fieldNameToColumnName (Aeson.toText key)
                    in (col, typedAesonValueToSnippet (lookupColumnType columnTypes col) val)
                )

        let setCalls = keyValues
                |> map (\(col, val) -> quoteIdentifier col <> Snippet.sql " = " <> val)
        let setSnippet = mconcat $ List.intersperse (Snippet.sql ", ") setCalls
        let whereSnippet = Snippet.sql "id = " <> Snippet.param id
        let snippet = compileUpdate table setSnippet whereSnippet camelCaseRenamer columnTypes

        result :: [[Field]] <- sqlQueryWriteWithRLS hasqlPool (wrapDynamicQuery snippet) dynamicRowDecoder

        renderJson (head result)

    -- DELETE /api/:table/:id
    action DeleteRecordAction { table, id } = do
        let hasqlPool = ?modelContext.hasqlPool
        ensureRLSEnabled hasqlPool table

        sqlExecWithRLS hasqlPool (Snippet.sql "DELETE FROM " <> quoteIdentifier table <> Snippet.sql " WHERE id = " <> Snippet.param id)

        renderJson True

    -- GET /api/:table/:id
    action ShowRecordAction { table, id } = do
        let hasqlPool = ?modelContext.hasqlPool
        ensureRLSEnabled hasqlPool table

        columnTypeLookup <- makeCachedColumnTypeLookup hasqlPool
        columnTypes <- columnTypeLookup table
        let selectColumns = compileSelectedColumns camelCaseRenamer columnTypes SelectAll
        result :: [[Field]] <- sqlQueryWithRLS hasqlPool (wrapDynamicQuery (Snippet.sql "SELECT " <> selectColumns <> Snippet.sql " FROM " <> quoteIdentifier table <> Snippet.sql " WHERE id = " <> Snippet.param id)) dynamicRowDecoder

        renderJson (head result)

    -- GET /api/:table
    -- GET /api/:table?orderBy=createdAt
    -- GET /api/:table?fields=id,title
    action ListRecordsAction { table } = do
        let hasqlPool = ?modelContext.hasqlPool
        ensureRLSEnabled hasqlPool table

        columnTypeLookup <- makeCachedColumnTypeLookup hasqlPool
        columnTypes <- columnTypeLookup table
        let theSnippet = compileQueryTyped camelCaseRenamer columnTypes (buildDynamicQueryFromRequest table)
        result :: [[Field]] <- sqlQueryWithRLS hasqlPool (wrapDynamicQuery theSnippet) dynamicRowDecoder

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
