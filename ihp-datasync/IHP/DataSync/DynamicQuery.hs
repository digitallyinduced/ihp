{-# LANGUAGE DeriveAnyClass #-}
{-|
Module: IHP.DataSync.DynamicQuery
Description: The normal IHP query functionality is type-safe. This module provides type-unsafe access to the database.
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.DataSync.DynamicQuery where

import IHP.ControllerPrelude hiding (OrderByClause)
import qualified Data.Aeson as Aeson
import qualified IHP.QueryBuilder as QueryBuilder
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Statement as Hasql
import Data.Aeson.TH
import qualified GHC.Generics
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Scientific as Scientific
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Functor.Contravariant (contramap)
import IHP.QueryBuilder.HasqlCompiler (CompilerState(..), emptyCompilerState, nextParam)

data Field = Field { fieldName :: Text, fieldValue :: Value }

newtype UndecodedJSON = UndecodedJSON ByteString
    deriving (Show, Eq)

-- | A compiled dynamic query: SQL text with accumulated parameter state.
data CompiledQuery = CompiledQuery !Text !CompilerState

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
    | LiteralExpression { value :: !Value }
    | CallExpression { functionCall :: !FunctionCall }
    | ListExpression { values :: ![Value] }
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
    toJSON fields = object (map (\Field { fieldName, fieldValue } -> (cs fieldName) .= fieldValue) fields)
    toEncoding fields = pairs $ foldl' (<>) mempty encodedFields
        where
            encodedFields = (map (\Field { fieldName, fieldValue } -> (cs fieldName) .= fieldValue) fields)

-- | Decoder for dynamic query results wrapped with 'wrapDynamicQuery'.
--
-- Each row comes back as a single JSON column (from @row_to_json@), which is then
-- parsed into a list of @Field@ values.
dynamicRowDecoder :: Decoders.Result [[Field]]
dynamicRowDecoder = Decoders.rowList dynamicRowJsonDecoder

-- | Decodes a single row from a @row_to_json@ wrapped query.
-- The row consists of a single JSONB column containing the original row as a JSON object.
dynamicRowJsonDecoder :: Decoders.Row [Field]
dynamicRowJsonDecoder =
    Decoders.column (Decoders.nonNullable Decoders.jsonb)
    |> fmap (\jsonValue ->
        case jsonToFields jsonValue of
            Right fields -> fields
            Left err -> error ("dynamicRowJsonDecoder: Failed to decode JSON row: " <> cs err)
    )

-- | Converts a JSON object (from @row_to_json@) into a list of Fields
jsonToFields :: Value -> Either String [Field]
jsonToFields (Object obj) = Right $ map toField (Aeson.toList obj)
    where
        toField (key, value) = Field
            { fieldName = Aeson.toText key
            , fieldValue = value
            }
jsonToFields other = Left (cs ("Expected JSON object but got: " <> show other))


-- | Returns a list of all id's in a result
recordIds :: [[Field]] -> [UUID]
recordIds result = result
        |> concat
        |> filter (\Field { fieldName } -> fieldName == "id")
        |> map (.fieldValue)
        |> mapMaybe \case
            Aeson.String text -> UUID.fromText text
            otherwise         -> Nothing



-- | A map from column name to PostgreSQL type name (e.g. @"uuid"@, @"int4"@, @"timestamptz"@)
type ColumnTypeMap = HashMap.HashMap Text Text

-- | Column type information with both O(1) type lookup and database column ordering.
--
-- The 'typeMap' provides fast type lookups for WHERE clause compilation,
-- while 'orderedColumns' preserves the order columns were defined in the schema
-- (from @pg_attribute.attnum@), ensuring @id@ naturally appears first when
-- expanding @SELECT *@.
data ColumnTypeInfo = ColumnTypeInfo
    { typeMap :: !ColumnTypeMap
    , orderedColumns :: ![Text]
    } deriving (Show, Eq)

-- | Encode an Aeson 'Value' as a parameterized SQL fragment, threading 'CompilerState'.
--
-- Used for expressions without column context (e.g. bare literals in WHERE clauses).
-- When column types are known, prefer 'IHP.DataSync.TypedEncoder.typedValueParam'
-- for correctly typed parameters.
dynamicValueParam :: Value -> CompilerState -> (Text, CompilerState)
dynamicValueParam Aeson.Null cc = ("NULL", cc)
dynamicValueParam (Aeson.Bool b) cc = nextParam (contramap (const b) (Encoders.param (Encoders.nonNullable Encoders.bool))) cc
dynamicValueParam (Aeson.String t) cc = nextParam (contramap (const t) (Encoders.param (Encoders.nonNullable Encoders.text))) cc
dynamicValueParam (Aeson.Number n) cc =
    case Scientific.floatingOrInteger n of
        Left (d :: Double) -> nextParam (contramap (const d) (Encoders.param (Encoders.nonNullable Encoders.float8))) cc
        Right (i :: Integer) -> nextParam (contramap (const (fromIntegral i :: Int64)) (Encoders.param (Encoders.nonNullable Encoders.int8))) cc
dynamicValueParam (Aeson.Array arr) cc =
    let (cc', elemTexts) = List.mapAccumL (\st v -> let (t, st') = dynamicValueParam v st in (st', t)) cc (Vector.toList arr)
    in ("ARRAY[" <> mconcat (List.intersperse ", " elemTexts) <> "]", cc')
dynamicValueParam (Aeson.Object obj) cc = nextParam (contramap (const (cs (encode (Object obj)) :: Text)) (Encoders.param (Encoders.nonNullable Encoders.text))) cc

-- | Extracts all column names referenced in a 'ConditionExpression'
conditionColumns :: ConditionExpression -> Set.Set Text
conditionColumns (ColumnExpression field) = Set.singleton field
conditionColumns (InfixOperatorExpression left _ right) = Set.union (conditionColumns left) (conditionColumns right)
conditionColumns (LiteralExpression _) = Set.empty
conditionColumns (CallExpression _) = Set.empty
conditionColumns (ListExpression _) = Set.empty

-- | Wraps a SQL query so that each row is returned as a JSON object.
--
-- Uses a CTE (Common Table Expression) which works for both SELECT queries
-- and DML statements (INSERT, UPDATE, DELETE) with RETURNING.
wrapDynamicQuery :: Text -> Text
wrapDynamicQuery innerQuery =
    "WITH _ihp_dynamic_result AS (" <> innerQuery <> ") SELECT row_to_json(t)::jsonb FROM _ihp_dynamic_result AS t"

-- | Quote a SQL identifier (table name, column name) to prevent SQL injection.
--
-- Wraps the identifier in double quotes and escapes any embedded double quotes
-- by doubling them, following the SQL standard.
quoteIdentifier :: Text -> Text
quoteIdentifier name = "\"" <> Text.replace "\"" "\"\"" name <> "\""

-- | Extract the encoder from a 'CompilerState'.
ccEncoder :: CompilerState -> Encoders.Params ()
ccEncoder (CompilerState _ enc) = enc
{-# INLINE ccEncoder #-}

-- | Build a 'Hasql.Statement' from compiled SQL, state, and decoder.
toStatement :: Text -> CompilerState -> Decoders.Result a -> Hasql.Statement () a
toStatement sql cc decoder = Hasql.preparable sql (ccEncoder cc) decoder
{-# INLINE toStatement #-}

-- | Build an encoder for a single UUID parameter.
uuidParam :: UUID -> Encoders.Params ()
uuidParam id = contramap (const id) (Encoders.param (Encoders.nonNullable Encoders.uuid))
{-# INLINE uuidParam #-}

$(deriveFromJSON defaultOptions ''FunctionCall)
$(deriveFromJSON defaultOptions ''QueryBuilder.OrderByDirection)
$(deriveFromJSON defaultOptions 'SelectAll)
$(deriveFromJSON defaultOptions ''ConditionOperator)
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
