{-|
Module: IHP.DataSync.TypedEncoder
Description: Schema-aware parameter encoding for DataSync queries
Copyright: (c) digitally induced GmbH, 2025

Queries column types from @pg_attribute@/@pg_type@ at runtime, caches per-table,
and uses typed encoders (e.g. 'Encoders.uuid' for UUID columns) via 'CompilerState'.
This avoids type mismatches like @operator does not exist: uuid = text@
that occur in the extended query protocol when sending text-typed parameters for non-text columns.
-}
module IHP.DataSync.TypedEncoder
( ColumnTypeMap
, ColumnTypeInfo(..)
, makeCachedColumnTypeLookup
, typedValueParam
, typedAesonValueToSnippet
, lookupColumnType
) where

import IHP.Prelude
import IHP.DataSync.DynamicQuery (ColumnTypeMap, ColumnTypeInfo(..), quoteIdentifier)
import IHP.QueryBuilder.HasqlCompiler (CompilerState(..), emptyCompilerState, nextParam)
import PostgresqlTypes.Point (Point, fromCoordinates)
import qualified Hasql.Mapping.IsScalar as Mapping
import Hasql.PostgresqlTypes ()
import qualified Data.HashMap.Strict as HashMap
import qualified Hasql.Pool
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import IHP.DataSync.Hasql (runSession)
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.UUID as UUID
import qualified Data.Scientific as Scientific
import Data.Int (Int16)
import qualified Data.Time.Format.ISO8601 as ISO8601
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Vector as Vector
import Data.Functor.Contravariant (contramap)

-- | Creates a cached lookup function that queries column types from @pg_attribute@/@pg_type@
-- and caches the result per table name.
--
-- Returns 'ColumnTypeInfo' which includes both a type map for O(1) lookups and
-- an ordered column list matching the database schema order (from @attnum@).
--
-- Follows the same caching pattern as 'makeCachedEnsureRLSEnabled'.
makeCachedColumnTypeLookup :: Hasql.Pool.Pool -> IO (Text -> IO ColumnTypeInfo)
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
-- Results are ordered by @attnum@ to preserve the database schema column order.
columnTypesStatement :: Statement.Statement Text ColumnTypeInfo
columnTypesStatement = Statement.preparable
    "SELECT a.attname::text, t.typname::text FROM pg_attribute a JOIN pg_type t ON a.atttypid = t.oid WHERE a.attrelid = quote_ident($1)::regclass AND a.attnum > 0 AND NOT a.attisdropped ORDER BY a.attnum"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (buildColumnTypeInfo <$> Decoders.rowList columnTypeDecoder)
  where
    buildColumnTypeInfo rows = ColumnTypeInfo
        { typeMap = HashMap.fromList rows
        , orderedColumns = map fst rows
        }
    columnTypeDecoder =
        (,) <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)

-- | Encode a value with the given encoder, threading 'CompilerState'.
encodeValue :: Encoders.NullableOrNot Encoders.Value a -> a -> CompilerState -> (Text, CompilerState)
encodeValue enc val cc = nextParam (contramap (const val) (Encoders.param enc)) cc
{-# INLINE encodeValue #-}

-- | Encode an Aeson 'Value' as a typed parameter, threading 'CompilerState'.
--
-- When a column type is known (from 'ColumnTypeMap'), uses the correct typed encoder.
-- Errors when no type info is available, since 'makeCachedColumnTypeLookup' should
-- always provide column types.
typedValueParam :: Maybe Text -> Value -> CompilerState -> (Text, CompilerState)
typedValueParam _ Aeson.Null cc = ("NULL", cc)
typedValueParam colType (Object values) cc
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
                    pure (fromCoordinates x y)
        in
            if Aeson.size values == 2
                then case tryDecodeAsPoint of
                    Just point -> encodeValue (Encoders.nonNullable Mapping.encoder) point cc
                    Nothing -> error "Cannot decode as Point"
                else error "Cannot decode as Point: expected {x, y} object"
typedValueParam pgType (Array arr) cc =
    let elemType = case pgType of
            Just t | "_" `Text.isPrefixOf` t -> Just (Text.drop 1 t)
            _ -> pgType
        (cc', elemTexts) = List.mapAccumL (\st v -> let (t, st') = typedValueParam elemType v st in (st', t)) cc (Vector.toList arr)
    in ("ARRAY[" <> mconcat (List.intersperse ", " elemTexts) <> "]", cc')
typedValueParam (Just pgType) value cc = encodeWithType pgType value cc
typedValueParam Nothing value _cc = error ("typedValueParam: No column type available for value: " <> show value)

-- | Encode an Aeson 'Value' using a specific PostgreSQL type encoder,
-- threading 'CompilerState'.
encodeWithType :: Text -> Value -> CompilerState -> (Text, CompilerState)
encodeWithType _             Aeson.Null cc = ("NULL", cc)
encodeWithType "uuid"        val cc = encodeValue (Encoders.nonNullable Encoders.uuid) (toUUID val) cc
encodeWithType "text"        val cc = encodeValue (Encoders.nonNullable Encoders.text) (toText val) cc
encodeWithType "varchar"     val cc = encodeValue (Encoders.nonNullable Encoders.text) (toText val) cc
encodeWithType "bpchar"      val cc = encodeValue (Encoders.nonNullable Encoders.text) (toText val) cc
encodeWithType "name"        val cc = encodeValue (Encoders.nonNullable Encoders.text) (toText val) cc
encodeWithType "int4"        val cc = encodeValue (Encoders.nonNullable Encoders.int4) (toInt32 val) cc
encodeWithType "int8"        val cc = encodeValue (Encoders.nonNullable Encoders.int8) (toInt64 val) cc
encodeWithType "int2"        val cc = encodeValue (Encoders.nonNullable Encoders.int2) (toInt16 val) cc
encodeWithType "bool"        val cc = encodeValue (Encoders.nonNullable Encoders.bool) (toBool val) cc
encodeWithType "timestamptz" val cc = encodeValue (Encoders.nonNullable Encoders.timestamptz) (toUTCTime val) cc
encodeWithType "timestamp"   val cc = encodeValue (Encoders.nonNullable Encoders.timestamp) (toLocalTime val) cc
encodeWithType "date"        val cc = encodeValue (Encoders.nonNullable Encoders.date) (toDay val) cc
encodeWithType "float8"      val cc = encodeValue (Encoders.nonNullable Encoders.float8) (toDouble val) cc
encodeWithType "float4"      val cc = encodeValue (Encoders.nonNullable Encoders.float4) (toFloat val) cc
encodeWithType "numeric"     val cc = encodeValue (Encoders.nonNullable Encoders.numeric) (toScientific val) cc
encodeWithType "jsonb"       val cc = encodeValue (Encoders.nonNullable Encoders.jsonb) val cc
encodeWithType "json"        val cc = encodeValue (Encoders.nonNullable Encoders.json) val cc
encodeWithType "bytea"       val cc = encodeValue (Encoders.nonNullable Encoders.bytea) (toByteString val) cc
-- Interval uses text+cast for dynamic DataSync queries where values come as JSON text.
encodeWithType "interval"    val cc =
    let (ph, cc') = encodeValue (Encoders.nonNullable Encoders.text) (toText val) cc
    in (ph <> "::interval", cc')
encodeWithType pgType        val cc =
    let (ph, cc') = encodeValue (Encoders.nonNullable Encoders.text) (toText val) cc
    in (ph <> "::" <> quoteIdentifier pgType, cc')

-- | Encode an Aeson 'Value' as a typed parameter for INSERT/UPDATE operations.
--
-- Delegates directly to 'typedValueParam'.
typedAesonValueToSnippet :: Maybe Text -> Value -> CompilerState -> (Text, CompilerState)
typedAesonValueToSnippet = typedValueParam

-- | Look up a column's type from ColumnTypeInfo.
lookupColumnType :: ColumnTypeInfo -> Text -> Maybe Text
lookupColumnType info col = HashMap.lookup col info.typeMap

-- Conversion helpers (from Aeson Value to Haskell types)

toUUID :: Value -> UUID
toUUID (String t) = fromMaybe (error ("Invalid UUID: " <> cs t)) (UUID.fromText t)
toUUID v = error ("Cannot convert to UUID: " <> show v)

toText :: Value -> Text
toText (String t) = t
toText (Number n) = case Scientific.floatingOrInteger n of
    Left (d :: Double) -> tshow d
    Right (i :: Integer) -> tshow i
toText (Bool b) = if b then "true" else "false"
toText v = error ("Cannot convert to Text: " <> show v)

toInt32 :: Value -> Int32
toInt32 (Number n) = case Scientific.floatingOrInteger n of
    Left (d :: Double) -> round d
    Right (i :: Integer) -> fromIntegral i
toInt32 (String t) = case Attoparsec.parseOnly (Attoparsec.signed Attoparsec.decimal) t of
    Right i -> i
    Left _ -> error ("Cannot parse Int32 from text: " <> cs t)
toInt32 v = error ("Cannot convert to Int32: " <> show v)

toInt64 :: Value -> Int64
toInt64 (Number n) = case Scientific.floatingOrInteger n of
    Left (d :: Double) -> round d
    Right (i :: Integer) -> fromIntegral i
toInt64 (String t) = case Attoparsec.parseOnly (Attoparsec.signed Attoparsec.decimal) t of
    Right i -> i
    Left _ -> error ("Cannot parse Int64 from text: " <> cs t)
toInt64 v = error ("Cannot convert to Int64: " <> show v)

toInt16 :: Value -> Int16
toInt16 (Number n) = case Scientific.floatingOrInteger n of
    Left (d :: Double) -> round d
    Right (i :: Integer) -> fromIntegral i
toInt16 (String t) = case Attoparsec.parseOnly (Attoparsec.signed Attoparsec.decimal) t of
    Right i -> i
    Left _ -> error ("Cannot parse Int16 from text: " <> cs t)
toInt16 v = error ("Cannot convert to Int16: " <> show v)

toBool :: Value -> Bool
toBool (Bool b) = b
toBool (String "true") = True
toBool (String "false") = False
toBool (String "t") = True
toBool (String "f") = False
toBool (Number 0) = False
toBool (Number _) = True
toBool v = error ("Cannot convert to Bool: " <> show v)

toUTCTime :: Value -> UTCTime
toUTCTime (String t) = case ISO8601.iso8601ParseM (cs t) of
    Just utc -> utc
    Nothing -> error ("Cannot parse UTCTime from text: " <> cs t)
toUTCTime v = error ("Cannot convert to UTCTime: " <> show v)

toLocalTime :: Value -> LocalTime
toLocalTime (String t) = case ISO8601.iso8601ParseM (cs t) of
    Just lt -> lt
    Nothing -> error ("Cannot parse LocalTime from text: " <> cs t)
toLocalTime v = error ("Cannot convert to LocalTime: " <> show v)

toDay :: Value -> Day
toDay (String t) = case ISO8601.iso8601ParseM (cs t) of
    Just d -> d
    Nothing -> error ("Cannot parse Day from text: " <> cs t)
toDay v = error ("Cannot convert to Day: " <> show v)

toDouble :: Value -> Double
toDouble (Number n) = Scientific.toRealFloat n
toDouble (String t) = case Attoparsec.parseOnly Attoparsec.double t of
    Right d -> d
    Left _ -> error ("Cannot parse Double from text: " <> cs t)
toDouble v = error ("Cannot convert to Double: " <> show v)

toFloat :: Value -> Float
toFloat (Number n) = Scientific.toRealFloat n
toFloat (String t) = case Attoparsec.parseOnly Attoparsec.double t of
    Right d -> realToFrac d
    Left _ -> error ("Cannot parse Float from text: " <> cs t)
toFloat v = error ("Cannot convert to Float: " <> show v)

toScientific :: Value -> Scientific.Scientific
toScientific (Number n) = n
toScientific (String t) = case Attoparsec.parseOnly Attoparsec.scientific t of
    Right s -> s
    Left _ -> error ("Cannot parse Scientific from text: " <> cs t)
toScientific v = error ("Cannot convert to Scientific: " <> show v)

toByteString :: Value -> ByteString
toByteString (String t) = cs t
toByteString v = cs (toText v)
