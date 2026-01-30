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
import qualified Data.Text as Text
import qualified Data.Scientific as Scientific
import qualified Data.List as List
import Data.Int (Int32)
import IHP.Postgres.Point (Point(..))

import qualified Data.ByteString.Builder as ByteString
import qualified Data.Aeson.Encoding.Internal as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.Decoders as Decoders
import Hasql.Implicits.Encoders (DefaultParamEncoder(..))


instance (
    Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Controller ApiController where
    action CreateRecordAction { table } = do
        ensureRLSEnabled table

        let payload = requestBodyJSON

        case payload of
            Object hashMap -> do
                let columns = hashMap
                        |> Aeson.keys
                        |> map (fieldNameToColumnName . Aeson.toText)

                let values = hashMap
                        |> Aeson.elems
                        |> map aesonValueToSnippet

                let columnSnippets = mconcat $ List.intersperse (Snippet.sql ", ") (map quoteIdentifier columns)
                let valueSnippets = mconcat $ List.intersperse (Snippet.sql ", ") values

                let snippet = Snippet.sql "INSERT INTO " <> quoteIdentifier table <> Snippet.sql " (" <> columnSnippets <> Snippet.sql ") VALUES (" <> valueSnippets <> Snippet.sql ") RETURNING *"

                result :: Either SomeException [[Field]] <- Exception.try do
                    sqlQueryWithRLS (wrapDynamicQuery snippet) dynamicRowDecoder

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
                                                    hashMap
                                                    |> Aeson.elems
                                                    |> map aesonValueToSnippet
                                                )

                                    let columnSnippets = mconcat $ List.intersperse (Snippet.sql ", ") (map quoteIdentifier columns)
                                    let valueRowSnippets = map (\row -> Snippet.sql "(" <> mconcat (List.intersperse (Snippet.sql ", ") row) <> Snippet.sql ")") values
                                    let valuesSnippet = mconcat $ List.intersperse (Snippet.sql ", ") valueRowSnippets

                                    let snippet = Snippet.sql "INSERT INTO " <> quoteIdentifier table <> Snippet.sql " (" <> columnSnippets <> Snippet.sql ") VALUES " <> valuesSnippet <> Snippet.sql " RETURNING *"

                                    result :: [[Field]] <- sqlQueryWithRLS (wrapDynamicQuery snippet) dynamicRowDecoder
                                    renderJson result
                        _otherwise -> renderErrorJson ("Expected object" :: Text)

            _ -> error "Expected JSON object or array"

    action UpdateRecordAction { table, id } = do
        ensureRLSEnabled table

        let payload = requestBodyJSON
                |> \case
                    Object hashMap -> hashMap
                    _ -> error "Expected JSON object"

        let columns = payload
                |> Aeson.keys
                |> map (fieldNameToColumnName . Aeson.toText)

        let values = payload
                |> Aeson.elems
                |> map aesonValueToSnippet

        let keyValues = zip columns values

        let setCalls = keyValues
                |> map (\(col, val) -> quoteIdentifier col <> Snippet.sql " = " <> val)
        let setSnippet = mconcat $ List.intersperse (Snippet.sql ", ") setCalls
        let snippet = Snippet.sql "UPDATE " <> quoteIdentifier table <> Snippet.sql " SET " <> setSnippet <> Snippet.sql " WHERE id = " <> Snippet.param id <> Snippet.sql " RETURNING *"

        result :: [[Field]] <- sqlQueryWithRLS (wrapDynamicQuery snippet) dynamicRowDecoder

        renderJson (head result)

    -- DELETE /api/:table/:id
    action DeleteRecordAction { table, id } = do
        ensureRLSEnabled table

        sqlExecWithRLS (Snippet.sql "DELETE FROM " <> quoteIdentifier table <> Snippet.sql " WHERE id = " <> Snippet.param id)

        renderJson True

    -- GET /api/:table/:id
    action ShowRecordAction { table, id } = do
        ensureRLSEnabled table

        result :: [[Field]] <- sqlQueryWithRLS (wrapDynamicQuery (Snippet.sql "SELECT * FROM " <> quoteIdentifier table <> Snippet.sql " WHERE id = " <> Snippet.param id)) dynamicRowDecoder

        renderJson (head result)

    -- GET /api/:table
    -- GET /api/:table?orderBy=createdAt
    -- GET /api/:table?fields=id,title
    action ListRecordsAction { table } = do
        ensureRLSEnabled table

        let theSnippet = compileQuery (buildDynamicQueryFromRequest table)
        result :: [[Field]] <- sqlQueryWithRLS (wrapDynamicQuery theSnippet) dynamicRowDecoder

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

renderErrorJson :: (?context :: ControllerContext) => ToJSON json => json -> IO ()
renderErrorJson json = renderJsonWithStatusCode status400 json
{-# INLINABLE renderErrorJson #-}

-- | Convert an Aeson Value to a Snippet parameter for use in dynamic SQL queries
aesonValueToSnippet :: Value -> Snippet
aesonValueToSnippet (String text) = Snippet.param text
aesonValueToSnippet (Bool value) = Snippet.param value
aesonValueToSnippet (Number value) = case Scientific.floatingOrInteger value of -- Hacky, we should make this function "Schema.sql"-aware in the future
    Left (floating :: Double) -> Snippet.param floating
    Right (integer :: Integer) -> Snippet.param (fromIntegral integer :: Int32)
aesonValueToSnippet Data.Aeson.Null = Snippet.sql "NULL"
aesonValueToSnippet (Data.Aeson.Array values) =
    Snippet.sql "ARRAY[" <> mconcat (List.intersperse (Snippet.sql ", ") (map aesonValueToSnippet (Vector.toList values))) <> Snippet.sql "]"
aesonValueToSnippet object@(Object values) =
    let
        tryDecodeAsPoint :: Maybe Point
        tryDecodeAsPoint = do
                xValue <- Aeson.lookup "x" values
                yValue <- Aeson.lookup "y" values
                x <- case xValue of
                        Number number -> pure (Scientific.toRealFloat number)
                        otherwise -> Nothing
                y <- case yValue of
                        Number number -> pure (Scientific.toRealFloat number)
                        otherwise -> Nothing
                pure Point { x, y }
    in
        -- This is really hacky and is mostly duck typing. We should refactor this in the future to
        -- become more type aware by passing the DDL of the table to 'aesonValueToSnippet'.
        if Aeson.size values == 2
            then fromMaybe (Snippet.param (toJSON object)) (pointToSnippet <$> tryDecodeAsPoint)
            else Snippet.param (toJSON object)
    where
        pointToSnippet (Point x y) = Snippet.sql ("point(" <> cs (tshow x) <> "," <> cs (tshow y) <> ")")


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
