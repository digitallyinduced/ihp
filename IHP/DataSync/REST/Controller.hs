{-# LANGUAGE UndecidableInstances #-}
module IHP.DataSync.REST.Controller where

import IHP.ControllerPrelude hiding (OrderByClause)
import IHP.DataSync.REST.Types
import Data.Aeson
import Data.Aeson.TH
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple as PG
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import qualified Data.ByteString.Char8 as ByteString
import qualified Control.Exception as Exception
import IHP.DataSync.RowLevelSecurity
import IHP.DataSync.DynamicQuery
import IHP.DataSync.Types
import Network.HTTP.Types (status400)
import IHP.DataSync.DynamicQueryCompiler
import qualified Data.Text as Text
import qualified Data.Scientific as Scientific

instance (
    PG.ToField (PrimaryKey (GetTableName CurrentUserRecord))
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Controller ApiController where
    action CreateRecordAction { table } = do
        ensureRLSEnabled table

        let payload = requestBodyJSON

        case payload of
            Object hashMap -> do
                let query = "INSERT INTO ? ? VALUES ? RETURNING *"
                let columns = hashMap
                        |> HashMap.keys
                        |> map fieldNameToColumnName

                let values = hashMap
                        |> HashMap.elems
                        |> map aesonValueToPostgresValue

                let params = (PG.Identifier table, PG.In (map PG.Identifier columns), PG.In values)
                
                result :: Either EnhancedSqlError [[Field]] <- Exception.try do
                    sqlQueryWithRLS query params

                case result of
                    Left error -> renderErrorJson error
                    Right result -> renderJson result

            Array objects -> do
                let query = "INSERT INTO ? ? ? RETURNING *"
                let columns = objects
                        |> Vector.toList
                        |> head
                        |> \case
                            Just value -> value
                            Nothing -> error "Atleast one record is required"
                        |> \case
                            Object hashMap -> hashMap
                            otherwise -> error "Expected object"
                        |> HashMap.keys
                        |> map fieldNameToColumnName

                let values = objects
                        |> Vector.toList
                        |> map (\object ->
                                object
                                |> \case
                                    Object hashMap -> hashMap
                                    otherwise -> error "Expected object"
                                |> HashMap.elems
                                |> map aesonValueToPostgresValue
                            )
                        

                let params = (PG.Identifier table, PG.In (map PG.Identifier columns), PG.Values [] values)

                result :: [[Field]] <- sqlQueryWithRLS query params
                renderJson result

        

    action UpdateRecordAction { table, id } = do
        ensureRLSEnabled table

        let payload = requestBodyJSON
                |> \case
                    Object hashMap -> hashMap

        let columns = payload
                |> HashMap.keys
                |> map fieldNameToColumnName
                |> map PG.Identifier

        let values = payload
                |> HashMap.elems
                |> map aesonValueToPostgresValue

        let keyValues = zip columns values

        let setCalls = keyValues
                |> map (\_ -> "? = ?")
                |> ByteString.intercalate ", "
        let query = "UPDATE ? SET " <> setCalls <> " WHERE id = ? RETURNING *"

        let params = [PG.toField (PG.Identifier table)]
                <> (join (map (\(key, value) -> [PG.toField key, value]) keyValues))
                <> [PG.toField id]

        result :: [[Field]] <- sqlQueryWithRLS (PG.Query query) params

        renderJson (head result)

    -- DELETE /api/:table/:id
    action DeleteRecordAction { table, id } = do
        ensureRLSEnabled table

        sqlExecWithRLS "DELETE FROM ? WHERE id = ?" (PG.Identifier table, id)

        renderJson True

    -- GET /api/:table/:id
    action ShowRecordAction { table, id } = do
        ensureRLSEnabled table

        result :: [[Field]] <- sqlQueryWithRLS "SELECT * FROM ? WHERE id = ?" (PG.Identifier table, id)

        renderJson (head result)

    -- GET /api/:table
    -- GET /api/:table?orderBy=createdAt
    -- GET /api/:table?fields=id,title
    action ListRecordsAction { table } = do
        ensureRLSEnabled table

        let (theQuery, theParams) = compileQuery (buildDynamicQueryFromRequest table)
        result :: [[Field]] <- sqlQueryWithRLS theQuery theParams

        renderJson result

buildDynamicQueryFromRequest table = DynamicSQLQuery
    { table
    , selectedColumns = paramOrDefault SelectAll "fields"
    , whereCondition = Nothing
    , orderByClause = paramList "orderBy"
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
        where
            parseOrder "asc" = Right Asc
            parseOrder "desc" = Right Desc
            parseOrder otherwise = Left ("Invalid order " <> cs otherwise)

instance ToJSON PG.SqlError where
    toJSON PG.SqlError { sqlState, sqlErrorMsg, sqlErrorDetail, sqlErrorHint } = object
                [ "state" .= ((cs sqlState) :: Text)
                , "errorMsg" .= ((cs sqlErrorMsg) :: Text)
                , "errorDetail" .= ((cs sqlErrorDetail) :: Text)
                , "errorHint" .= ((cs sqlErrorHint) :: Text)
                ]
        where
            fieldValueToJSON (IntValue value) = toJSON value
            fieldValueToJSON (TextValue value) = toJSON value
            fieldValueToJSON (BoolValue value) = toJSON value
            fieldValueToJSON (UUIDValue value) = toJSON value
            fieldValueToJSON (DateTimeValue value) = toJSON value

instance ToJSON EnhancedSqlError where
    toJSON EnhancedSqlError { sqlError } = toJSON sqlError

renderErrorJson :: (?context :: ControllerContext) => Data.Aeson.ToJSON json => json -> IO ()
renderErrorJson json = renderJsonWithStatusCode status400 json
{-# INLINABLE renderErrorJson #-}

aesonValueToPostgresValue :: Value -> PG.Action
aesonValueToPostgresValue (String text) = PG.toField text
aesonValueToPostgresValue (Bool value) = PG.toField value
aesonValueToPostgresValue (Number value) = case Scientific.floatingOrInteger value of -- Hacky, we should make this function "Schema.sql"-aware in the future
    Left (floating :: Double) -> PG.toField floating
    Right (integer :: Integer) -> PG.toField integer
aesonValueToPostgresValue Data.Aeson.Null = PG.toField PG.Null
aesonValueToPostgresValue object@(Object values) = 
    let
        tryDecodeAsPoint :: Maybe Point
        tryDecodeAsPoint = do
                xValue <- HashMap.lookup "x" values
                yValue <- HashMap.lookup "y" values
                x <- case xValue of
                        Number number -> pure (Scientific.toRealFloat number)
                        otherwise -> Nothing 
                y <- case yValue of
                        Number number -> pure (Scientific.toRealFloat number)
                        otherwise -> Nothing 
                pure Point { x, y }
    in
        -- This is really hacky and is mostly duck typing. We should refactor this in the future to
        -- become more type aware by passing the DDL of the table to 'aesonValueToPostgresValue'.
        if HashMap.size values == 2
            then fromMaybe (PG.toField $ toJSON object) (PG.toField <$> tryDecodeAsPoint)
            else PG.toField (toJSON object)