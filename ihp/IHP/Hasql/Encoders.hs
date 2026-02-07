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
module IHP.Hasql.Encoders () where

import Prelude
import Data.Int (Int64)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word32, Word8)
import Data.Bits (shiftR)
import qualified Hasql.Encoders as Encoders
import Hasql.Implicits.Encoders (DefaultParamEncoder(..))
import Data.Functor.Contravariant (contramap, (>$<))
import Data.Functor.Contravariant.Divisible (divide)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import IHP.ModelSupport.Types (Id'(..), PrimaryKey)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.Types (Binary(..))
import qualified Hasql.Mapping.IsScalar as Mapping
import Hasql.PostgresqlTypes ()
import qualified PostgresqlTypes.Point as PgPoint
import qualified PostgresqlTypes.Polygon as PgPolygon
import qualified PostgresqlTypes.Inet as PgInet
import IHP.Postgres.Point (Point(..))
import IHP.Postgres.Polygon (Polygon(..))
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

-- | Encode IHP 'Point' as PostgreSQL point via postgresql-types binary encoder
instance DefaultParamEncoder Point where
    defaultParam = Encoders.nonNullable (contramap ihpPointToPg Mapping.encoder)
      where
        ihpPointToPg :: Point -> PgPoint.Point
        ihpPointToPg (Point x y) = PgPoint.fromCoordinates x y

-- | Encode 'Maybe Point' as nullable PostgreSQL point
instance DefaultParamEncoder (Maybe Point) where
    defaultParam = Encoders.nullable (contramap ihpPointToPg Mapping.encoder)
      where
        ihpPointToPg :: Point -> PgPoint.Point
        ihpPointToPg (Point x y) = PgPoint.fromCoordinates x y

-- | Encode IHP 'Polygon' as PostgreSQL polygon via postgresql-types binary encoder
instance DefaultParamEncoder Polygon where
    defaultParam = Encoders.nonNullable (contramap ihpPolygonToPg Mapping.encoder)
      where
        ihpPolygonToPg :: Polygon -> PgPolygon.Polygon
        ihpPolygonToPg (Polygon points) =
            case PgPolygon.refineFromPointList (map (\(Point x y) -> (x, y)) points) of
                Just pg -> pg
                Nothing -> error "Polygon must have at least 3 points"

-- | Encode 'Maybe Polygon' as nullable PostgreSQL polygon
instance DefaultParamEncoder (Maybe Polygon) where
    defaultParam = Encoders.nullable (contramap ihpPolygonToPg Mapping.encoder)
      where
        ihpPolygonToPg :: Polygon -> PgPolygon.Polygon
        ihpPolygonToPg (Polygon points) =
            case PgPolygon.refineFromPointList (map (\(Point x y) -> (x, y)) points) of
                Just pg -> pg
                Nothing -> error "Polygon must have at least 3 points"

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
