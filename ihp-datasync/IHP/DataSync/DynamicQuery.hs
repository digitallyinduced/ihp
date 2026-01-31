{-# LANGUAGE DeriveAnyClass #-}
{-|
Module: IHP.DataSync.DynamicQuery
Description: The normal IHP query functionality is type-safe. This module provides type-unsafe access to the database.
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.DataSync.DynamicQuery where

import IHP.ControllerPrelude hiding (OrderByClause, Null)
import Data.Aeson hiding (Null)
import qualified Data.Aeson as Aeson
import qualified IHP.QueryBuilder as QueryBuilder
import qualified Hasql.Decoders as Decoders
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import Data.Aeson.TH
import qualified GHC.Generics
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Scientific as Scientific
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import IHP.Postgres.Point (Point(..))
import Data.Int (Int64)

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
    parseJSON invalid = fail $ cs ("Expected String for ByteString, got: " <> tshow invalid)

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
    toJSON Null = toJSON Aeson.Null

-- | Wraps a SQL query snippet so that each row is returned as a JSON object.
--
-- This is needed because hasql decoders are positional and don't provide column name metadata.
-- By wrapping with @row_to_json@, we get column names in the JSON keys, which we can then
-- decode into @[Field]@.
--
-- Uses a CTE (Common Table Expression) which works for both SELECT queries
-- and DML statements (INSERT, UPDATE, DELETE) with RETURNING:
--
-- @
-- WITH _ihp_dynamic_result AS (...original query...) SELECT row_to_json(t) FROM _ihp_dynamic_result AS t
-- @
wrapDynamicQuery :: Snippet -> Snippet
wrapDynamicQuery innerQuery =
    Snippet.sql "WITH _ihp_dynamic_result AS (" <> innerQuery <> Snippet.sql ") SELECT row_to_json(t)::jsonb FROM _ihp_dynamic_result AS t"

-- | Decoder for dynamic query results wrapped with 'wrapDynamicQuery'.
--
-- Each row comes back as a single JSON column (from @row_to_json@), which is then
-- parsed into a list of @Field@ values.
dynamicRowDecoder :: Decoders.Result [[Field]]
dynamicRowDecoder = Decoders.rowList dynamicRowJsonDecoder

-- | Decodes a single row from a @row_to_json@ wrapped query.
-- The row consists of a single JSONB column containing the original row as a JSON object.
dynamicRowJsonDecoder :: Decoders.Row [Field]
dynamicRowJsonDecoder = do
    jsonValue <- Decoders.column (Decoders.nonNullable Decoders.jsonb)
    case jsonToFields jsonValue of
        Right fields -> pure fields
        Left err -> error ("dynamicRowJsonDecoder: Failed to decode JSON row: " <> cs err)

-- | Converts a JSON object (from @row_to_json@) into a list of Fields
jsonToFields :: Value -> Either String [Field]
jsonToFields (Object obj) = Right $ map toField (Aeson.toList obj)
    where
        toField (key, value) = Field
            { fieldName = Aeson.toText key
            , fieldValue = aesonToDynamicValue value
            }
jsonToFields other = Left (cs ("Expected JSON object but got: " <> show other))

-- | Converts an Aeson Value to a DynamicValue
aesonToDynamicValue :: Value -> DynamicValue
aesonToDynamicValue Aeson.Null = Null
aesonToDynamicValue (Bool b) = BoolValue b
aesonToDynamicValue (String t) =
    -- Try to parse as UUID first (since postgres UUID columns come back as strings in JSON)
    case UUID.fromText t of
        Just uuid -> UUIDValue uuid
        Nothing -> TextValue t
aesonToDynamicValue (Number n) =
    case Scientific.floatingOrInteger n of
        Left (d :: Double) -> DoubleValue d
        Right (i :: Integer) -> IntValue (fromIntegral i)
aesonToDynamicValue (Array arr) = ArrayValue (map aesonToDynamicValue (Vector.toList arr))
aesonToDynamicValue (Object obj) = TextValue (cs (encode (Object obj))) -- Fallback for nested objects (e.g. JSONB columns)



-- | Returns a list of all id's in a result
recordIds :: [[Field]] -> [UUID]
recordIds result = result
        |> concat
        |> filter (\Field { fieldName } -> fieldName == "id")
        |> map (.fieldValue)
        |> mapMaybe \case
            UUIDValue uuid -> Just uuid
            otherwise      -> Nothing



-- | A map from column name to PostgreSQL type name (e.g. @"uuid"@, @"int4"@, @"timestamptz"@)
type ColumnTypeMap = HashMap.HashMap Text Text

-- | Encode a 'DynamicValue' as a typed Snippet parameter using the native Haskell type.
--
-- Used for expressions without column context (e.g. bare literals in WHERE clauses).
-- When column types are known, prefer 'IHP.DataSync.TypedEncoder.typedDynamicValueParam'
-- for correctly typed parameters.
dynamicValueParam :: DynamicValue -> Snippet
dynamicValueParam (IntValue i) = Snippet.param (fromIntegral i :: Int64)
dynamicValueParam (DoubleValue d) = Snippet.param d
dynamicValueParam (TextValue t) = Snippet.param t
dynamicValueParam (BoolValue b) = Snippet.param b
dynamicValueParam (UUIDValue u) = Snippet.param u
dynamicValueParam (DateTimeValue t) = Snippet.param t
dynamicValueParam (PointValue (Point x y)) = Snippet.sql ("point(" <> cs (tshow x) <> "," <> cs (tshow y) <> ")")
dynamicValueParam (IntervalValue (PGInterval bs)) = Snippet.param (cs bs :: Text)
dynamicValueParam (ArrayValue values) = Snippet.sql "ARRAY[" <> mconcat (List.intersperse (Snippet.sql ", ") (map dynamicValueParam values)) <> Snippet.sql "]"
dynamicValueParam Null = Snippet.sql "NULL"

-- | Quote a SQL identifier (table name, column name) to prevent SQL injection
quoteIdentifier :: Text -> Snippet
quoteIdentifier name = Snippet.sql (cs ("\"" <> Text.replace "\"" "\"\"" name <> "\""))

$(deriveFromJSON defaultOptions ''FunctionCall)
$(deriveFromJSON defaultOptions ''QueryBuilder.OrderByDirection)
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
