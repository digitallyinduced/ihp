module IHP.GraphQL.Patch where

import IHP.Prelude
import IHP.GraphQL.Types
import IHP.GraphQL.Analysis
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import qualified Data.UUID as UUID

insertRecord :: Text -> UUID -> HashMap Text Aeson.Value -> Document -> Aeson.Value -> Aeson.Value
insertRecord tableName id object document result = foldl' (\json path -> applyFunctionAtNode insertRecordsAtNode path json) result paths 
    where
        paths = nodePathsForTable tableName document
        insertRecordsAtNode (Aeson.Array vector) = Aeson.Array (Vector.snoc vector (Aeson.Object object))

updateRecord :: Text -> UUID -> HashMap Text Aeson.Value -> Document -> Aeson.Value -> Aeson.Value
updateRecord tableName id patch document result = foldl' (\json path -> applyFunctionAtNode updateRecordsAtNode path json) result paths 
    where
        paths = nodePathsForTable tableName document
        updateRecordsAtNode (Aeson.Array vector) = Aeson.Array (Vector.map updateRecordAtNode vector)
        updateRecordAtNode value@(Aeson.Object hashMap) =
            if isRecordIdEq id value
                then Aeson.Object (HashMap.union patch hashMap)
                else value

deleteRecord :: Text -> UUID -> Document -> Aeson.Value -> Aeson.Value
deleteRecord tableName id document result = foldl' (\json path -> applyFunctionAtNode deleteRecordAtNode path json) result paths 
    where
        paths = nodePathsForTable tableName document
        deleteRecordAtNode (Aeson.Array vector) = Aeson.Array (Vector.filter (not . isRecordIdEq id) vector)

isRecordIdEq :: UUID -> Aeson.Value -> Bool
isRecordIdEq id (Aeson.Object hashMap) =
    case HashMap.lookup "id" hashMap of
        Just (Aeson.String uuid) ->
            case UUID.fromText uuid of
                Just uuid -> uuid == id
                Nothing -> False
        otherwise -> False