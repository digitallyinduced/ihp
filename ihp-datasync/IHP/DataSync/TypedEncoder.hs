{-|
Module: IHP.DataSync.TypedEncoder
Description: Schema-aware parameter encoding for DataSync queries
Copyright: (c) digitally induced GmbH, 2025

Queries column types from @pg_attribute@/@pg_type@ at runtime, caches per-table,
and uses 'Snippet.encoderAndParam' with the correct typed encoder (e.g. 'Encoders.uuid'
for UUID columns). This avoids type mismatches like @operator does not exist: uuid = text@
that occur in the extended query protocol when sending text-typed parameters for non-text columns.
-}
module IHP.DataSync.TypedEncoder
( ColumnTypeMap
, makeCachedColumnTypeLookup
, typedDynamicValueParam
, typedAesonValueToSnippet
) where

import IHP.Prelude
import IHP.DataSync.DynamicQuery (DynamicValue(..), ColumnTypeMap, aesonToDynamicValue, dynamicValueParam, quoteIdentifier)
import IHP.Postgres.Point (Point(..))
import IHP.Postgres.TimeParser (PGInterval(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Hasql.Pool
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import IHP.DataSync.Hasql (runSession)
import Data.Aeson (Value(Object, Array, String, Number, Bool))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.UUID as UUID
import qualified Data.Scientific as Scientific
import Data.Int (Int16, Int32, Int64)
import Data.Time (UTCTime, LocalTime, Day)
import qualified Data.Time.Format.ISO8601 as ISO8601
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
import qualified Data.List as List

-- | Creates a cached lookup function that queries column types from @pg_attribute@/@pg_type@
-- and caches the result per table name.
--
-- Follows the same caching pattern as 'makeCachedEnsureRLSEnabled'.
makeCachedColumnTypeLookup :: Hasql.Pool.Pool -> IO (Text -> IO ColumnTypeMap)
makeCachedColumnTypeLookup pool = do
    cache <- newIORef HashMap.empty
    pure \tableName -> do
        cached <- HashMap.lookup tableName <$> readIORef cache
        case cached of
            Just types -> pure types
            Nothing -> do
                types <- runSession pool (Session.statement tableName columnTypesStatement)
                modifyIORef' cache (HashMap.insert tableName types)
                pure types

-- | Prepared statement that queries column types for a table.
--
-- Uses @pg_attribute@ joined with @pg_type@ to get clean type names.
-- Filters out dropped columns and system columns (@attnum > 0@).
columnTypesStatement :: Statement.Statement Text ColumnTypeMap
columnTypesStatement = Statement.Statement
    "SELECT a.attname::text, t.typname::text FROM pg_attribute a JOIN pg_type t ON a.atttypid = t.oid WHERE a.attrelid = quote_ident($1)::regclass AND a.attnum > 0 AND NOT a.attisdropped"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (HashMap.fromList <$> Decoders.rowList columnTypeDecoder)
    True
  where
    columnTypeDecoder = do
        colName <- Decoders.column (Decoders.nonNullable Decoders.text)
        typName <- Decoders.column (Decoders.nonNullable Decoders.text)
        pure (colName, typName)

-- | Encode a 'DynamicValue' as a typed Snippet parameter.
--
-- When a column type is known (from 'ColumnTypeMap'), uses 'Snippet.encoderAndParam'
-- with the correct typed encoder. Falls back to 'dynamicValueParam' when no type info
-- is available.
typedDynamicValueParam :: Maybe Text -> DynamicValue -> Snippet
typedDynamicValueParam _ Null = Snippet.sql "NULL"
typedDynamicValueParam _ (PointValue (Point x y)) = Snippet.sql ("point(" <> cs (tshow x) <> "," <> cs (tshow y) <> ")")
typedDynamicValueParam _ (ArrayValue values) = Snippet.sql "ARRAY[" <> mconcat (List.intersperse (Snippet.sql ", ") (map (typedDynamicValueParam Nothing) values)) <> Snippet.sql "]"
typedDynamicValueParam (Just pgType) value = encodeWithType pgType value
typedDynamicValueParam Nothing value = dynamicValueParam value

-- | Encode a 'DynamicValue' using a specific PostgreSQL type encoder.
--
-- Maps PostgreSQL type names to the corresponding hasql encoder and converts
-- the 'DynamicValue' to the appropriate Haskell type.
encodeWithType :: Text -> DynamicValue -> Snippet
encodeWithType "uuid"        val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.uuid) (toUUID val)
encodeWithType "text"        val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.text) (toText val)
encodeWithType "varchar"     val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.text) (toText val)
encodeWithType "bpchar"      val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.text) (toText val)
encodeWithType "name"        val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.text) (toText val)
encodeWithType "int4"        val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.int4) (toInt32 val)
encodeWithType "int8"        val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.int8) (toInt64 val)
encodeWithType "int2"        val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.int2) (toInt16 val)
encodeWithType "bool"        val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.bool) (toBool val)
encodeWithType "timestamptz" val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.timestamptz) (toUTCTime val)
encodeWithType "timestamp"   val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.timestamp) (toLocalTime val)
encodeWithType "date"        val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.date) (toDay val)
encodeWithType "float8"      val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.float8) (toDouble val)
encodeWithType "float4"      val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.float4) (toFloat val)
encodeWithType "numeric"     val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.numeric) (toScientific val)
encodeWithType "jsonb"       val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.jsonb) (toJSON val)
encodeWithType "json"        val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.json) (toJSON val)
encodeWithType "bytea"       val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.bytea) (toByteString val)
encodeWithType pgType        val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.text) (toText val) <> Snippet.sql "::" <> quoteIdentifier pgType

-- | Encode an Aeson 'Value' as a typed Snippet parameter for INSERT/UPDATE operations.
--
-- JSON/JSONB columns get special handling to preserve the Aeson Value as-is,
-- rather than converting through DynamicValue (which would lose structure).
typedAesonValueToSnippet :: Maybe Text -> Value -> Snippet
typedAesonValueToSnippet (Just "jsonb") val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.jsonb) val
typedAesonValueToSnippet (Just "json")  val = Snippet.encoderAndParam (Encoders.nonNullable Encoders.json) val
typedAesonValueToSnippet _ (Aeson.Null) = Snippet.sql "NULL"
typedAesonValueToSnippet colType (Object values)
    | colType == Just "point" =
        let
            tryDecodeAsPoint :: Maybe Point
            tryDecodeAsPoint = do
                    xValue <- Aeson.lookup "x" values
                    yValue <- Aeson.lookup "y" values
                    x <- case xValue of
                            Aeson.Number number -> pure (Scientific.toRealFloat number)
                            _ -> Nothing
                    y <- case yValue of
                            Aeson.Number number -> pure (Scientific.toRealFloat number)
                            _ -> Nothing
                    pure Point { x, y }
        in
            if Aeson.size values == 2
                then fromMaybe (typedDynamicValueParam colType (aesonToDynamicValue (Object values))) (pointToSnippet <$> tryDecodeAsPoint)
                else typedDynamicValueParam colType (aesonToDynamicValue (Object values))
    | otherwise = typedDynamicValueParam colType (aesonToDynamicValue (Object values))
    where
        pointToSnippet (Point x y) = Snippet.sql ("point(" <> cs (tshow x) <> "," <> cs (tshow y) <> ")")
typedAesonValueToSnippet colType value = typedDynamicValueParam colType (aesonToDynamicValue value)

-- Conversion helpers

toUUID :: DynamicValue -> UUID
toUUID (UUIDValue u) = u
toUUID (TextValue t) = fromMaybe (error ("Invalid UUID: " <> cs t)) (UUID.fromText t)
toUUID v = error ("Cannot convert to UUID: " <> show v)

toText :: DynamicValue -> Text
toText (TextValue t) = t
toText (IntValue i) = tshow i
toText (DoubleValue d) = tshow d
toText (BoolValue b) = if b then "true" else "false"
toText (UUIDValue u) = UUID.toText u
toText (DateTimeValue t) = tshow t
toText (IntervalValue (PGInterval bs)) = cs bs
toText (PointValue (Point x y)) = "(" <> tshow x <> "," <> tshow y <> ")"
toText (ArrayValue vs) = "{" <> mconcat (List.intersperse "," (map toText vs)) <> "}"
toText v = error ("Cannot convert to Text: " <> show v)

toInt32 :: DynamicValue -> Int32
toInt32 (IntValue i) = fromIntegral i
toInt32 (DoubleValue d) = round d
toInt32 (TextValue t) = case Attoparsec.parseOnly (Attoparsec.signed Attoparsec.decimal) t of
    Right i -> i
    Left _ -> error ("Cannot parse Int32 from text: " <> cs t)
toInt32 v = error ("Cannot convert to Int32: " <> show v)

toInt64 :: DynamicValue -> Int64
toInt64 (IntValue i) = fromIntegral i
toInt64 (DoubleValue d) = round d
toInt64 (TextValue t) = case Attoparsec.parseOnly (Attoparsec.signed Attoparsec.decimal) t of
    Right i -> i
    Left _ -> error ("Cannot parse Int64 from text: " <> cs t)
toInt64 v = error ("Cannot convert to Int64: " <> show v)

toInt16 :: DynamicValue -> Int16
toInt16 (IntValue i) = fromIntegral i
toInt16 (DoubleValue d) = round d
toInt16 (TextValue t) = case Attoparsec.parseOnly (Attoparsec.signed Attoparsec.decimal) t of
    Right i -> i
    Left _ -> error ("Cannot parse Int16 from text: " <> cs t)
toInt16 v = error ("Cannot convert to Int16: " <> show v)

toBool :: DynamicValue -> Bool
toBool (BoolValue b) = b
toBool (TextValue "true") = True
toBool (TextValue "false") = False
toBool (TextValue "t") = True
toBool (TextValue "f") = False
toBool (IntValue 0) = False
toBool (IntValue _) = True
toBool v = error ("Cannot convert to Bool: " <> show v)

toUTCTime :: DynamicValue -> UTCTime
toUTCTime (DateTimeValue t) = t
toUTCTime (TextValue t) = case ISO8601.iso8601ParseM (cs t) of
    Just utc -> utc
    Nothing -> error ("Cannot parse UTCTime from text: " <> cs t)
toUTCTime v = error ("Cannot convert to UTCTime: " <> show v)

toLocalTime :: DynamicValue -> LocalTime
toLocalTime (DateTimeValue t) = utcToLocalTime utc t
toLocalTime (TextValue t) = case ISO8601.iso8601ParseM (cs t) of
    Just lt -> lt
    Nothing -> error ("Cannot parse LocalTime from text: " <> cs t)
toLocalTime v = error ("Cannot convert to LocalTime: " <> show v)

toDay :: DynamicValue -> Day
toDay (DateTimeValue t) = utctDay t
toDay (TextValue t) = case ISO8601.iso8601ParseM (cs t) of
    Just d -> d
    Nothing -> error ("Cannot parse Day from text: " <> cs t)
toDay v = error ("Cannot convert to Day: " <> show v)

toDouble :: DynamicValue -> Double
toDouble (DoubleValue d) = d
toDouble (IntValue i) = fromIntegral i
toDouble (TextValue t) = case Attoparsec.parseOnly Attoparsec.double t of
    Right d -> d
    Left _ -> error ("Cannot parse Double from text: " <> cs t)
toDouble v = error ("Cannot convert to Double: " <> show v)

toFloat :: DynamicValue -> Float
toFloat (DoubleValue d) = realToFrac d
toFloat (IntValue i) = fromIntegral i
toFloat (TextValue t) = case Attoparsec.parseOnly Attoparsec.double t of
    Right d -> realToFrac d
    Left _ -> error ("Cannot parse Float from text: " <> cs t)
toFloat v = error ("Cannot convert to Float: " <> show v)

toScientific :: DynamicValue -> Scientific.Scientific
toScientific (DoubleValue d) = Scientific.fromFloatDigits d
toScientific (IntValue i) = fromIntegral i
toScientific (TextValue t) = case Attoparsec.parseOnly Attoparsec.scientific t of
    Right s -> s
    Left _ -> error ("Cannot parse Scientific from text: " <> cs t)
toScientific v = error ("Cannot convert to Scientific: " <> show v)

toJSON :: DynamicValue -> Aeson.Value
toJSON (TextValue t) = Aeson.String t
toJSON (IntValue i) = Aeson.Number (fromIntegral i)
toJSON (DoubleValue d) = Aeson.Number (Scientific.fromFloatDigits d)
toJSON (BoolValue b) = Aeson.Bool b
toJSON Null = Aeson.Null
toJSON v = Aeson.String (toText v)

toByteString :: DynamicValue -> ByteString
toByteString (TextValue t) = cs t
toByteString v = cs (toText v)
