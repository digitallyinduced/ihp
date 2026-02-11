{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: IHP.Hasql.Encoders
Description: DefaultParamEncoder instances for common types
Copyright: (c) digitally induced GmbH, 2025

This module provides orphan 'DefaultParamEncoder' instances for types that
hasql-implicits doesn't support out of the box, most importantly 'Int'.

These instances are needed because hasql-implicits only provides instances for
fixed-width integer types ('Int16', 'Int32', 'Int64'), not Haskell's
platform-dependent 'Int'. Since most IHP applications use 'Int' for
integer columns, we provide these instances to make the transition seamless.
-}
module IHP.Hasql.Encoders
( ToSnippetParams(..)
, sqlToSnippet
) where

import Prelude
import Data.Int (Int64)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word32, Word8)
import Data.Bits (shiftR)
import qualified Hasql.Encoders as Encoders
import Hasql.Implicits.Encoders (DefaultParamEncoder(..))
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import Database.PostgreSQL.Simple (Only(..), (:.)(..))
import Data.Functor.Contravariant (contramap, (>$<))
import Data.Functor.Contravariant.Divisible (divide)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import IHP.ModelSupport.Types (Id'(..), PrimaryKey)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.Types (Binary(..))
import qualified Hasql.Mapping.IsScalar as Mapping
import Hasql.PostgresqlTypes ()
import PostgresqlTypes.Point (Point)
import PostgresqlTypes.Polygon (Polygon)
import qualified PostgresqlTypes.Inet as PgInet
import IHP.Postgres.TimeParser (PGInterval(..))
import IHP.Postgres.TSVector (Tsvector)
import qualified Net.IP
import qualified Net.IPv4
import qualified Net.IPv6
import qualified Data.WideWord.Word128 as Word128

-- | Encode 'Int' as PostgreSQL int8 (bigint)
--
-- This treats Haskell's 'Int' as 'Int64', which is safe on 64-bit platforms
-- (where 'Int' is 64 bits) and may truncate on 32-bit platforms (where 'Int'
-- is 32 bits but int8 can hold the full range).
instance DefaultParamEncoder Int where
    defaultParam = Encoders.nonNullable (contramap (fromIntegral :: Int -> Int64) Encoders.int8)

-- | Encode '[Int]' as PostgreSQL int8[] (bigint array)
instance DefaultParamEncoder [Int] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable (contramap (fromIntegral :: Int -> Int64) Encoders.int8)

-- | Encode 'Maybe Int' as nullable PostgreSQL int8
instance DefaultParamEncoder (Maybe Int) where
    defaultParam = Encoders.nullable (contramap (fromIntegral :: Int -> Int64) Encoders.int8)

-- | Encode '[Maybe Int]' as PostgreSQL int8[] with nullable elements
instance DefaultParamEncoder [Maybe Int] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nullable (contramap (fromIntegral :: Int -> Int64) Encoders.int8)

-- | Encode 'Vector Int' as PostgreSQL int8[] (bigint array)
instance DefaultParamEncoder (Vector Int) where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable (contramap (fromIntegral :: Int -> Int64) Encoders.int8)

-- | Encode 'Id' table' for tables with UUID primary keys
-- This covers the common case in IHP where most tables use UUID as the primary key.
instance PrimaryKey table ~ UUID => DefaultParamEncoder (Id' table) where
    defaultParam = Encoders.nonNullable (contramap (\(Id uuid) -> uuid) Encoders.uuid)

-- | Encode list of 'Id' table' for tables with UUID primary keys
-- Used by filterWhereIdIn for simple primary keys
instance PrimaryKey table ~ UUID => DefaultParamEncoder [Id' table] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable (contramap (\(Id uuid) -> uuid) Encoders.uuid)

-- | Encode 'Maybe (Id' table)' for nullable foreign keys
instance PrimaryKey table ~ UUID => DefaultParamEncoder (Maybe (Id' table)) where
    defaultParam = Encoders.nullable (contramap (\(Id uuid) -> uuid) Encoders.uuid)

-- | Encode '[Maybe (Id' table)]' for filterWhereIn with nullable foreign keys
instance PrimaryKey table ~ UUID => DefaultParamEncoder [Maybe (Id' table)] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nullable (contramap (\(Id uuid) -> uuid) Encoders.uuid)

-- | Encode '(UUID, UUID)' as PostgreSQL composite/record type
-- Used for composite primary keys with two UUID columns
instance DefaultParamEncoder (UUID, UUID) where
    defaultParam = Encoders.nonNullable $ Encoders.composite (Nothing :: Maybe Text) "" uuidPairComposite

-- | Encode '[(UUID, UUID)]' as PostgreSQL array of composite types
-- Used by filterWhereIdIn for tables with composite primary keys of two UUIDs
instance DefaultParamEncoder [(UUID, UUID)] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable $ Encoders.composite (Nothing :: Maybe Text) "" uuidPairComposite

-- | Encode '(Id' a, Id' b)' as PostgreSQL composite/record type
-- Used for composite primary keys with two Id columns (where both resolve to UUID)
instance (PrimaryKey a ~ UUID, PrimaryKey b ~ UUID) => DefaultParamEncoder (Id' a, Id' b) where
    defaultParam = Encoders.nonNullable $ Encoders.composite (Nothing :: Maybe Text) "" $
        contramap (\(Id a, Id b) -> (a, b)) uuidPairComposite

-- | Encode '[(Id' a, Id' b)]' as PostgreSQL array of composite types
-- Used by filterWhereIdIn for tables with composite primary keys of two Id columns
instance (PrimaryKey a ~ UUID, PrimaryKey b ~ UUID) => DefaultParamEncoder [(Id' a, Id' b)] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable $ Encoders.composite (Nothing :: Maybe Text) "" $
        contramap (\(Id a, Id b) -> (a, b)) uuidPairComposite

-- | Helper: composite encoder for a pair of UUIDs
uuidPairComposite :: Encoders.Composite (UUID, UUID)
uuidPairComposite = divide id (Encoders.field (Encoders.nonNullable Encoders.uuid)) (Encoders.field (Encoders.nonNullable Encoders.uuid))

-- | Encode 'Binary ByteString' as PostgreSQL bytea
-- IHP wraps bytea columns in Binary, so we need to unwrap before encoding
instance DefaultParamEncoder (Binary ByteString) where
    defaultParam = Encoders.nonNullable (contramap (\(Binary bs) -> bs) Encoders.bytea)

-- | Encode 'Maybe (Binary ByteString)' as nullable PostgreSQL bytea
instance DefaultParamEncoder (Maybe (Binary ByteString)) where
    defaultParam = Encoders.nullable (contramap (\(Binary bs) -> bs) Encoders.bytea)

-- | Encode 'Integer' as PostgreSQL int8 (bigint)
-- Used for BigInt and BigSerial columns
instance DefaultParamEncoder Integer where
    defaultParam = Encoders.nonNullable (contramap fromInteger Encoders.int8)

-- | Encode 'Maybe Integer' as nullable PostgreSQL int8
instance DefaultParamEncoder (Maybe Integer) where
    defaultParam = Encoders.nullable (contramap fromInteger Encoders.int8)

-- | Encode 'Point' as PostgreSQL point via postgresql-types binary encoder
instance DefaultParamEncoder Point where
    defaultParam = Encoders.nonNullable Mapping.encoder

-- | Encode 'Maybe Point' as nullable PostgreSQL point
instance DefaultParamEncoder (Maybe Point) where
    defaultParam = Encoders.nullable Mapping.encoder

-- | Encode 'Polygon' as PostgreSQL polygon via postgresql-types binary encoder
instance DefaultParamEncoder Polygon where
    defaultParam = Encoders.nonNullable Mapping.encoder

-- | Encode 'Maybe Polygon' as nullable PostgreSQL polygon
instance DefaultParamEncoder (Maybe Polygon) where
    defaultParam = Encoders.nullable Mapping.encoder

-- | Encode 'PGInterval' as PostgreSQL interval
-- Uses 'unknown' OID so PostgreSQL coerces the text representation to interval
instance DefaultParamEncoder PGInterval where
    defaultParam = Encoders.nonNullable (contramap (\(PGInterval bs) -> bs) Encoders.unknown)

-- | Encode 'Maybe PGInterval' as nullable PostgreSQL interval
instance DefaultParamEncoder (Maybe PGInterval) where
    defaultParam = Encoders.nullable (contramap (\(PGInterval bs) -> bs) Encoders.unknown)

-- | Encode 'Tsvector' as PostgreSQL tsvector via postgresql-types binary encoder
instance DefaultParamEncoder Tsvector where
    defaultParam = Encoders.nonNullable Mapping.encoder

-- | Encode 'Maybe Tsvector' as nullable PostgreSQL tsvector
instance DefaultParamEncoder (Maybe Tsvector) where
    defaultParam = Encoders.nullable Mapping.encoder

-- | Encode 'Net.IP.IP' as PostgreSQL inet via postgresql-types binary encoder
instance DefaultParamEncoder Net.IP.IP where
    defaultParam = Encoders.nonNullable (contramap ipToInet Mapping.encoder)

-- | Encode 'Maybe Net.IP.IP' as nullable PostgreSQL inet
instance DefaultParamEncoder (Maybe Net.IP.IP) where
    defaultParam = Encoders.nullable (contramap ipToInet Mapping.encoder)

-- | Convert 'Net.IP.IP' to 'PostgresqlTypes.Inet.Inet'
ipToInet :: Net.IP.IP -> PgInet.Inet
ipToInet ip = Net.IP.case_ ipv4ToInet ipv6ToInet ip
  where
    ipv4ToInet :: Net.IPv4.IPv4 -> PgInet.Inet
    ipv4ToInet addr = PgInet.normalizeFromV4 (Net.IPv4.getIPv4 addr) 32
    ipv6ToInet :: Net.IPv6.IPv6 -> PgInet.Inet
    ipv6ToInet addr =
        let w128 = Net.IPv6.getIPv6 addr
            hi = Word128.word128Hi64 w128
            lo = Word128.word128Lo64 w128
            a = fromIntegral (hi `shiftR` 32)
            b = fromIntegral hi
            c = fromIntegral (lo `shiftR` 32)
            d = fromIntegral lo
        in PgInet.normalizeFromV6 a b c d 128

-- | Converts parameter tuples into a list of hasql 'Snippet' values.
--
-- This mirrors postgresql-simple's 'ToRow' typeclass, allowing @sqlQuery@ and @sqlExec@
-- to use hasql's native parameterized queries instead of 'PG.formatQuery'.
class ToSnippetParams a where
    toSnippetParams :: a -> [Snippet]

instance ToSnippetParams () where
    toSnippetParams () = []

instance DefaultParamEncoder a => ToSnippetParams (Only a) where
    toSnippetParams (Only a) = [Snippet.param a]

instance (DefaultParamEncoder a, DefaultParamEncoder b) => ToSnippetParams (a, b) where
    toSnippetParams (a, b) = [Snippet.param a, Snippet.param b]

instance (DefaultParamEncoder a, DefaultParamEncoder b, DefaultParamEncoder c) => ToSnippetParams (a, b, c) where
    toSnippetParams (a, b, c) = [Snippet.param a, Snippet.param b, Snippet.param c]

instance (DefaultParamEncoder a, DefaultParamEncoder b, DefaultParamEncoder c, DefaultParamEncoder d) => ToSnippetParams (a, b, c, d) where
    toSnippetParams (a, b, c, d) = [Snippet.param a, Snippet.param b, Snippet.param c, Snippet.param d]

instance (DefaultParamEncoder a, DefaultParamEncoder b, DefaultParamEncoder c, DefaultParamEncoder d, DefaultParamEncoder e) => ToSnippetParams (a, b, c, d, e) where
    toSnippetParams (a, b, c, d, e) = [Snippet.param a, Snippet.param b, Snippet.param c, Snippet.param d, Snippet.param e]

instance (DefaultParamEncoder a, DefaultParamEncoder b, DefaultParamEncoder c, DefaultParamEncoder d, DefaultParamEncoder e, DefaultParamEncoder f) => ToSnippetParams (a, b, c, d, e, f) where
    toSnippetParams (a, b, c, d, e, f) = [Snippet.param a, Snippet.param b, Snippet.param c, Snippet.param d, Snippet.param e, Snippet.param f]

instance (DefaultParamEncoder a, DefaultParamEncoder b, DefaultParamEncoder c, DefaultParamEncoder d, DefaultParamEncoder e, DefaultParamEncoder f, DefaultParamEncoder g) => ToSnippetParams (a, b, c, d, e, f, g) where
    toSnippetParams (a, b, c, d, e, f, g) = [Snippet.param a, Snippet.param b, Snippet.param c, Snippet.param d, Snippet.param e, Snippet.param f, Snippet.param g]

instance (DefaultParamEncoder a, DefaultParamEncoder b, DefaultParamEncoder c, DefaultParamEncoder d, DefaultParamEncoder e, DefaultParamEncoder f, DefaultParamEncoder g, DefaultParamEncoder h) => ToSnippetParams (a, b, c, d, e, f, g, h) where
    toSnippetParams (a, b, c, d, e, f, g, h) = [Snippet.param a, Snippet.param b, Snippet.param c, Snippet.param d, Snippet.param e, Snippet.param f, Snippet.param g, Snippet.param h]

instance (DefaultParamEncoder a, DefaultParamEncoder b, DefaultParamEncoder c, DefaultParamEncoder d, DefaultParamEncoder e, DefaultParamEncoder f, DefaultParamEncoder g, DefaultParamEncoder h, DefaultParamEncoder i) => ToSnippetParams (a, b, c, d, e, f, g, h, i) where
    toSnippetParams (a, b, c, d, e, f, g, h, i) = [Snippet.param a, Snippet.param b, Snippet.param c, Snippet.param d, Snippet.param e, Snippet.param f, Snippet.param g, Snippet.param h, Snippet.param i]

instance (DefaultParamEncoder a, DefaultParamEncoder b, DefaultParamEncoder c, DefaultParamEncoder d, DefaultParamEncoder e, DefaultParamEncoder f, DefaultParamEncoder g, DefaultParamEncoder h, DefaultParamEncoder i, DefaultParamEncoder j) => ToSnippetParams (a, b, c, d, e, f, g, h, i, j) where
    toSnippetParams (a, b, c, d, e, f, g, h, i, j) = [Snippet.param a, Snippet.param b, Snippet.param c, Snippet.param d, Snippet.param e, Snippet.param f, Snippet.param g, Snippet.param h, Snippet.param i, Snippet.param j]

-- | Append two parameter lists (mirrors postgresql-simple's ':.' operator)
instance (ToSnippetParams a, ToSnippetParams b) => ToSnippetParams (a :. b) where
    toSnippetParams (a :. b) = toSnippetParams a <> toSnippetParams b

-- | Converts a SQL query with @?@ placeholders and a list of 'Snippet' parameters
-- into a single 'Snippet' with native hasql @$1, $2, ...@ parameterization.
--
-- This mirrors postgresql-simple's @?@ placeholder convention.
--
-- __Example:__
--
-- > sqlToSnippet "SELECT * FROM users WHERE id = ? AND name = ?" [Snippet.param id, Snippet.param name]
-- > -- becomes: Snippet.sql "SELECT * FROM users WHERE id = " <> Snippet.param id <> Snippet.sql " AND name = " <> Snippet.param name
--
sqlToSnippet :: ByteString -> [Snippet] -> Snippet
sqlToSnippet sql params = mconcat (interleave sqlParts params)
  where
    sqlParts = map (Snippet.sql . Text.decodeUtf8) (BS8.split '?' sql)
    interleave (s:ss) (p:ps) = s : p : interleave ss ps
    interleave ss [] = ss
    interleave [] _ = []
{-# INLINE sqlToSnippet #-}
