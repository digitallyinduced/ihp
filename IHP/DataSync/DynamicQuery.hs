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
    = IntValue !Int
    | DoubleValue !Double
    | TextValue !Text
    | BoolValue !Bool
    | UUIDValue !UUID
    | DateTimeValue !UTCTime
    | PointValue !Point
    | Null
    deriving (Show, Eq)

-- | Similiar to IHP.QueryBuilder.SQLQuery, but is designed to be accessed by external users
--
-- When compiling to SQL we have to be extra careful to escape all identifers and variables in the query.
-- The normal IHP.QueryBuilder doesn't need to be that careful as parts of the input are derived from
-- generated code from the Schema.sql.
--
data DynamicSQLQuery = DynamicSQLQuery
    { table :: !Text
    , selectedColumns :: SelectedColumns
    , whereCondition :: !(Maybe ConditionExpression)
    , orderByClause :: ![OrderByClause]
    , limit :: !(Maybe Int)
    , offset :: !(Maybe Int)
    } deriving (Show, Eq)

-- | Represents a WHERE conditions of a 'DynamicSQLQuery'
data ConditionExpression
    = ColumnExpression { field :: !Text }
    | NullExpression
    | InfixOperatorExpression { left :: !ConditionExpression, op :: !ConditionOperator, right :: !ConditionExpression }
    | LiteralExpression { value :: !DynamicValue }
    deriving (Show, Eq)

-- | Operators available in WHERE conditions
data ConditionOperator
    = OpEqual -- ^ a = b
    | OpGreaterThan -- ^ a > b
    | OpLessThan -- ^ a < b
    | OpGreaterThanOrEqual -- ^ a >= b
    | OpLessThanOrEqual -- ^ a <= b
    | OpNotEqual -- ^ a <> b
    | OpAnd -- ^ a AND b
    | OpOr -- ^ a OR b
    | OpIs -- ^ a IS b
    | OpIsNot -- ^ a IS NOT b
    deriving (Show, Eq)

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
            fieldValueToJSON (DoubleValue value) = toJSON value
            fieldValueToJSON (TextValue value) = toJSON value
            fieldValueToJSON (BoolValue value) = toJSON value
            fieldValueToJSON (UUIDValue value) = toJSON value
            fieldValueToJSON (DateTimeValue value) = toJSON value
            fieldValueToJSON (PointValue value) = toJSON value
            fieldValueToJSON IHP.DataSync.DynamicQuery.Null = toJSON Data.Aeson.Null

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
                <|> (DoubleValue <$> PG.fromField field fieldValue')
                <|> (DateTimeValue <$> PG.fromField field fieldValue')
                <|> (PointValue <$> PG.fromField field fieldValue')
                <|> (PG.fromField @PG.Null field fieldValue' >> pure IHP.DataSync.DynamicQuery.Null)
                <|> fromFieldCustomEnum field fieldValue'

            fromFieldCustomEnum field (Just value) = pure (TextValue (cs value))
            fromFieldCustomEnum field Nothing      = pure IHP.DataSync.DynamicQuery.Null


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
$(deriveFromJSON defaultOptions ''ConditionOperator)
$(deriveFromJSON defaultOptions ''ConditionExpression)
$(deriveFromJSON defaultOptions ''DynamicValue)

instance FromJSON DynamicSQLQuery where
    parseJSON = withObject "DynamicSQLQuery" $ \v -> DynamicSQLQuery
        <$> v .: "table"
        <*> v .: "selectedColumns"
        <*> v .: "whereCondition"
        <*> v .: "orderByClause"
        <*> v .:? "limit" -- Limit can be absent in older versions of ihp-datasync.js
        <*> v .:? "offset" -- Offset can be absent in older versions of ihp-datasync.js