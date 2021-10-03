{-|
Module: IHP.DataSync.DynamicQuery
Description: The normal IHP query functionality is type-safe. This module provides type-unsafe access to the database.
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.DataSync.DynamicQuery where

import IHP.ControllerPrelude
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.FromRow as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.Notification as PG

data Field = Field { fieldName :: Text, fieldValue :: DynamicValue }

data DynamicValue
    = IntValue Int
    | TextValue Text
    | BoolValue Bool
    | UUIDValue UUID
    | DateTimeValue UTCTime

instance {-# OVERLAPS #-} ToJSON [Field] where
    toJSON fields = object (map (\Field { fieldName, fieldValue } -> (cs fieldName) .= (fieldValueToJSON fieldValue)) fields)
        where
            fieldValueToJSON (IntValue value) = toJSON value
            fieldValueToJSON (TextValue value) = toJSON value
            fieldValueToJSON (BoolValue value) = toJSON value
            fieldValueToJSON (UUIDValue value) = toJSON value
            fieldValueToJSON (DateTimeValue value) = toJSON value

instance PG.FromField Field where
    fromField field fieldValue' = do
            fieldValue <- fieldValue
            pure Field { .. }
        where
            fieldName = (PG.name field)
                |> fmap (columnNameToFieldName . cs)
                |> fromMaybe ""
            fieldValue =
                    (IntValue <$> PG.fromField field fieldValue')
                <|> (TextValue <$> PG.fromField field fieldValue')
                <|> (BoolValue <$> PG.fromField field fieldValue')
                <|> (UUIDValue <$> PG.fromField field fieldValue')
                <|> (DateTimeValue <$> PG.fromField field fieldValue')


-- | Returns a list of all id's in a result
recordIds :: [[Field]] -> [UUID]
recordIds result = result
        |> concat
        |> filter (\Field { fieldName } -> fieldName == "id")
        |> map (get #fieldValue)
        |> mapMaybe \case
            UUIDValue uuid -> Just uuid
            otherwise      -> Nothing



-- Here you can add functions which are available in all your controllers

-- | Transforms the keys of a JSON object from field name to column name
--
-- >>> transformColumnNamesToFieldNames [json|{"isCompleted": true}|]
-- [json|{"is_completed": true}|]
transformColumnNamesToFieldNames :: Value -> Value
transformColumnNamesToFieldNames (Object hashMap) =
        hashMap
        |> HashMap.toList
        |> map (\(key, value) -> (columnNameToFieldName key, value))
        |> HashMap.fromList
        |> Object
