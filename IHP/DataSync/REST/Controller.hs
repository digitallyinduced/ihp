{-# LANGUAGE UndecidableInstances #-}
module IHP.DataSync.REST.Controller where

import IHP.ControllerPrelude
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
                        |> map \case
                            String text -> PG.toField text

                let params = (PG.Identifier table, PG.In (map PG.Identifier columns), PG.In values)
                
                result :: Either PG.SqlError [[Field]] <- Exception.try do
                    withRLS do
                        sqlQuery query params

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
                                |> map \case
                                    String text -> PG.toField text
                            )
                        

                let params = (PG.Identifier table, PG.In (map PG.Identifier columns), PG.Values [] values)

                result :: [[Field]] <- withRLS $ sqlQuery query params
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
                |> map \case
                    String text -> PG.toField text
                    Bool value -> PG.toField value

        let keyValues = zip columns values

        let setCalls = keyValues
                |> map (\_ -> "? = ?")
                |> ByteString.intercalate ", "
        let query = "UPDATE ? SET " <> setCalls <> " WHERE id = ? RETURNING *"

        let params = [PG.toField (PG.Identifier table)]
                <> (join (map (\(key, value) -> [PG.toField key, value]) keyValues))
                <> [PG.toField id]

        result :: [[Field]] <- withRLS $ sqlQuery (PG.Query query) params

        renderJson (head result)

    -- DELETE /api/:table/:id
    action DeleteRecordAction { table, id } = do
        ensureRLSEnabled table

        withRLS do
            sqlExec "DELETE FROM ? WHERE id = ?" (PG.Identifier table, id)

        renderJson True

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

renderErrorJson :: (?context :: ControllerContext) => Data.Aeson.ToJSON json => json -> IO ()
renderErrorJson json = renderJsonWithStatusCode status400 json
{-# INLINABLE renderErrorJson #-}