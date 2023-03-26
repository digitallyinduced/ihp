{-# LANGUAGE DeriveAnyClass #-}
{-|
Module: IHP.DataSync.DynamicQuery
Description: The normal IHP query functionality is type-safe. This module provides type-unsafe access to the database.
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.DataSync.DynamicQuery where

import IHP.ControllerPrelude hiding (OrderByClause)
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
import qualified GHC.Generics
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Key as Aeson

data Field = Field { fieldName :: Text, fieldValue :: DynamicValue }

data DynamicValue
    = IntValue !Int
    | DoubleValue !Double
    | TextValue !Text
    | BoolValue !Bool
    | UUIDValue !UUID
    | DateTimeValue !UTCTime
    | PointValue !Point
    | IntervalValue !PGInterval
    | ArrayValue ![DynamicValue]
    | Null
    deriving (Show, Eq)

newtype UndecodedJSON = UndecodedJSON ByteString
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
    , distinctOnColumn :: !(Maybe ByteString)
    , limit :: !(Maybe Int)
    , offset :: !(Maybe Int)
    } deriving (Show, Eq)

data OrderByClause
    = OrderByClause
        { orderByColumn :: !ByteString
        , orderByDirection :: !OrderByDirection }
    | OrderByTSRank { tsvector :: Text, tsquery :: !Text }
    deriving (Show, Eq, GHC.Generics.Generic, DeepSeq.NFData)

-- | Represents a WHERE conditions of a 'DynamicSQLQuery'
data ConditionExpression
    = ColumnExpression { field :: !Text }
    | InfixOperatorExpression
        { left :: !ConditionExpression
        , op :: !ConditionOperator
        , right :: !ConditionExpression
        }
    | LiteralExpression { value :: !DynamicValue }
    | CallExpression { functionCall :: !FunctionCall }
    | ListExpression { values :: ![DynamicValue] }
    deriving (Show, Eq)

data FunctionCall
    = ToTSQuery { text :: !Text } -- ^ to_tsquery('english', text)
    deriving (Show, Eq, GHC.Generics.Generic, DeepSeq.NFData)

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
    | OpTSMatch -- ^ tsvec_a @@ tsvec_b
    | OpIn -- ^ a IN b
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
    toJSON fields = object (map (\Field { fieldName, fieldValue } -> (cs fieldName) .= (toJSON fieldValue)) fields)
    toEncoding fields = pairs $ foldl' (<>) mempty encodedFields
        where
            encodedFields = (map (\Field { fieldName, fieldValue } -> (cs fieldName) .= (toJSON fieldValue)) fields)

instance ToJSON DynamicValue where
    toJSON (IntValue value) = toJSON value
    toJSON (DoubleValue value) = toJSON value
    toJSON (TextValue value) = toJSON value
    toJSON (BoolValue value) = toJSON value
    toJSON (UUIDValue value) = toJSON value
    toJSON (DateTimeValue value) = toJSON value
    toJSON (PointValue value) = toJSON value
    toJSON (IntervalValue value) = toJSON value
    toJSON (ArrayValue value) = toJSON value
    toJSON IHP.DataSync.DynamicQuery.Null = toJSON Data.Aeson.Null

instance PG.FromField Field where
    fromField field fieldValue' = do
            fieldValue <- PG.fromField field fieldValue'
            pure Field { .. }
        where
            fieldName = (PG.name field)
                |> fmap (columnNameToFieldName . cs)
                |> fromMaybe ""

instance PG.FromField DynamicValue where
    fromField field fieldValue' = fieldValue
        where
            fieldValue =
                    (IntValue <$> PG.fromField field fieldValue')
                <|> (TextValue <$> PG.fromField field fieldValue')
                <|> (BoolValue <$> PG.fromField field fieldValue')
                <|> (UUIDValue <$> PG.fromField field fieldValue')
                <|> (DoubleValue <$> PG.fromField field fieldValue')
                <|> (DateTimeValue <$> PG.fromField field fieldValue')
                <|> (PointValue <$> PG.fromField field fieldValue')
                <|> (IntervalValue <$> PG.fromField field fieldValue')
                <|> (ArrayValue <$> PG.fromField field fieldValue')
                <|> (PG.fromField @PG.Null field fieldValue' >> pure IHP.DataSync.DynamicQuery.Null)
                <|> fromFieldCustomEnum field fieldValue'

            fromFieldCustomEnum field (Just value) = pure (TextValue (cs value))
            fromFieldCustomEnum field Nothing      = pure IHP.DataSync.DynamicQuery.Null

instance PG.FromField UndecodedJSON where
    fromField field (Just value) = pure (UndecodedJSON value)
    fromField field Nothing = pure (UndecodedJSON "null")

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
        |> Aeson.toList
        |> map (\(key, value) -> (applyKey columnNameToFieldName key, value))
        |> Aeson.fromList
        |> Object
    where
        applyKey function key =
            key
                |> Aeson.toText
                |> function
                |> Aeson.fromText


$(deriveFromJSON defaultOptions ''FunctionCall)
$(deriveFromJSON defaultOptions 'QueryBuilder.OrCondition)
$(deriveFromJSON defaultOptions 'QueryBuilder.Join)
$(deriveFromJSON defaultOptions ''QueryBuilder.OrderByDirection)
$(deriveFromJSON defaultOptions 'QueryBuilder.OrderByClause)
$(deriveFromJSON defaultOptions 'SelectAll)
$(deriveFromJSON defaultOptions ''ConditionOperator)
$(deriveFromJSON defaultOptions ''DynamicValue)
$(deriveFromJSON defaultOptions ''ConditionExpression)

instance FromJSON DynamicSQLQuery where
    parseJSON = withObject "DynamicSQLQuery" $ \v -> DynamicSQLQuery
        <$> v .: "table"
        <*> v .: "selectedColumns"
        <*> v .: "whereCondition"
        <*> v .: "orderByClause"
        <*> v .:? "distinctOnColumn" -- distinctOnColumn can be absent in older versions of ihp-datasync.js
        <*> v .:? "limit" -- Limit can be absent in older versions of ihp-datasync.js
        <*> v .:? "offset" -- Offset can be absent in older versions of ihp-datasync.js


instance FromJSON OrderByClause where
    parseJSON = withObject "OrderByClause" $ \v -> do
        let oldFormat = OrderByClause
                <$> v .: "orderByColumn"
                <*> v .: "orderByDirection"
        let tagged = do
                tag <- v .: "tag"
                case tag of
                    "OrderByClause" -> oldFormat
                    "OrderByTSRank" -> OrderByTSRank <$> v .: "tsvector" <*> v .: "tsquery"
                    otherwise -> error ("Invalid tag: " <> otherwise)
        tagged <|> oldFormat
