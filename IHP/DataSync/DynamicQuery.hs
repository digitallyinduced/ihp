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
import qualified IHP.QueryBuilder as QueryBuilder
import Data.Aeson.TH

data Field = Field { fieldName :: Text, fieldValue :: DynamicValue }

data DynamicValue
    = IntValue Int
    | TextValue Text
    | BoolValue Bool
    | UUIDValue UUID
    | DateTimeValue UTCTime

-- | Similiar to IHP.QueryBuilder.SQLQuery, but is designed to be accessed by external users
--
-- When compiling to SQL we have to be extra careful to escape all identifers and variables in the query.
-- The normal IHP.QueryBuilder doesn't need to be that careful as parts of the input are derived from
-- generated code from the Schema.sql.
--
data DynamicSQLQuery = DynamicSQLQuery
    { table :: !Text
    , selectedColumns :: SelectedColumns
    , whereCondition :: !(Maybe Condition)
    , orderByClause :: ![OrderByClause]
    , limitClause :: !(Maybe Text)
    , offsetClause :: !(Maybe Text)
    } deriving (Show, Eq)

data SelectedColumns
    = SelectAll -- ^ SELECT * FROM table
    | SelectSpecific [Text] -- ^ SELECT a, b, c FROM table
    deriving (Show, Eq)

instance FromJSON ByteString where
    parseJSON (String v) = pure $ cs v

instance FromJSON PG.Action where
    parseJSON (String v) = pure (PG.Escape (cs v))

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


$(deriveFromJSON defaultOptions 'QueryBuilder.OrCondition)
$(deriveFromJSON defaultOptions 'QueryBuilder.Join)
$(deriveFromJSON defaultOptions 'QueryBuilder.OrderByClause)
$(deriveFromJSON defaultOptions 'QueryBuilder.Asc)
$(deriveFromJSON defaultOptions 'SelectAll)
$(deriveFromJSON defaultOptions 'DynamicSQLQuery)
