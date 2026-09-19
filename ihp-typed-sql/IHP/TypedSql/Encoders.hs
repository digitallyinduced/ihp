{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: IHP.TypedSql.Encoders
Description: DefaultParamEncoder instances for postgresql-types values
Copyright: (c) digitally induced GmbH, 2026

'DefaultParamEncoder' instances for the @postgresql-types@ values that
"IHP.TypedSql.TypeMapping" maps PostgreSQL OIDs onto: @point@, @polygon@,
@inet@, @tsvector@ and @interval@, plus PostGIS @geometry@.

They live here, not in @ihp@, because typedSql /generates code naming these
types/, so the package must be able to encode them back — including for a
standalone user who has no @ihp@ dependency. Exactly one package owns each
instance: duplicate definitions compile fine in isolation, but every use site
then fails with an overlapping-instances error.

@ihp@ picks these up by importing this module in "IHP.Hasql.Encoders", so
existing IHP code sees them unchanged. Only genuinely IHP-specific instances
(job-queue enums, @postgresql-simple@'s 'Binary' wrapper) stay in @ihp@,
because this package only builds on hasql, never on @postgresql-simple@.
-}
module IHP.TypedSql.Encoders () where

import           Data.Int                 (Int64)
import           Data.Vector (Vector)
import           Data.Functor.Contravariant (contramap)
import qualified Hasql.Encoders           as Encoders
import           Hasql.Implicits.Encoders (DefaultParamEncoder (..))
import qualified Hasql.Mapping.IsScalar   as Mapping
import           Hasql.PostgresqlTypes    () -- IsScalar instances for the postgresql-types values below
import           PostgresqlTypes.Geometry (Geometry)
import           PostgresqlTypes.Inet     (Inet)
import           PostgresqlTypes.Interval (Interval)
import           PostgresqlTypes.Point    (Point)
import           PostgresqlTypes.Polygon  (Polygon)
import           PostgresqlTypes.Tsvector (Tsvector)
import           Prelude

-- | Encode 'Integer' as PostgreSQL int8 (bigint)
-- Used for BigInt and BigSerial columns
instance DefaultParamEncoder Integer where
    defaultParam = Encoders.nonNullable (contramap fromInteger Encoders.int8)

-- | Encode 'Maybe Integer' as nullable PostgreSQL int8
instance DefaultParamEncoder (Maybe Integer) where
    defaultParam = Encoders.nullable (contramap fromInteger Encoders.int8)

-- | Encode 'Int' as PostgreSQL int8 (bigint).
instance DefaultParamEncoder Int where
    defaultParam = Encoders.nonNullable (contramap (fromIntegral :: Int -> Int64) Encoders.int8)

-- | Encode '[Int]' as PostgreSQL int8[] (bigint array).
instance DefaultParamEncoder [Int] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable (contramap (fromIntegral :: Int -> Int64) Encoders.int8)

-- | Encode 'Maybe Int' as nullable PostgreSQL int8.
instance DefaultParamEncoder (Maybe Int) where
    defaultParam = Encoders.nullable (contramap (fromIntegral :: Int -> Int64) Encoders.int8)

-- | Encode '[Maybe Int]' as PostgreSQL int8[] with nullable elements.
instance DefaultParamEncoder [Maybe Int] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nullable (contramap (fromIntegral :: Int -> Int64) Encoders.int8)

-- | Encode 'Vector Int' as PostgreSQL int8[] (bigint array)
instance DefaultParamEncoder (Vector Int) where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable (contramap (fromIntegral :: Int -> Int64) Encoders.int8)

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

-- | Encode 'Interval' as PostgreSQL interval via postgresql-types binary encoder
instance DefaultParamEncoder Interval where
    defaultParam = Encoders.nonNullable Mapping.encoder

-- | Encode 'Maybe Interval' as nullable PostgreSQL interval
instance DefaultParamEncoder (Maybe Interval) where
    defaultParam = Encoders.nullable Mapping.encoder

-- | Encode 'Tsvector' as PostgreSQL tsvector via postgresql-types binary encoder
instance DefaultParamEncoder Tsvector where
    defaultParam = Encoders.nonNullable Mapping.encoder

-- | Encode 'Maybe Tsvector' as nullable PostgreSQL tsvector
instance DefaultParamEncoder (Maybe Tsvector) where
    defaultParam = Encoders.nullable Mapping.encoder

-- | Encode 'Inet' as PostgreSQL inet via postgresql-types binary encoder
instance DefaultParamEncoder Inet where
    defaultParam = Encoders.nonNullable Mapping.encoder

-- | Encode 'Maybe Inet' as nullable PostgreSQL inet
instance DefaultParamEncoder (Maybe Inet) where
    defaultParam = Encoders.nullable Mapping.encoder

-- | Encode PostGIS 'Geometry'. The 'Mapping.IsScalar' instance comes from
-- 'Hasql.PostgresqlTypes'; the OID is resolved by name at query time since
-- the PostGIS extension assigns it dynamically.
instance DefaultParamEncoder Geometry where
    defaultParam = Encoders.nonNullable Mapping.encoder

-- | Encode 'Maybe Geometry' as a nullable PostGIS geometry
instance DefaultParamEncoder (Maybe Geometry) where
    defaultParam = Encoders.nullable Mapping.encoder
