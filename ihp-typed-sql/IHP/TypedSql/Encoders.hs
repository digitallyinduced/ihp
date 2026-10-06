{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: IHP.TypedSql.Encoders
Description: DefaultParamEncoder instances for postgresql-types values
Copyright: (c) digitally induced GmbH, 2026

'DefaultParamEncoder' instances for the @postgresql-types@ values that
"IHP.TypedSql.TypeMapping" maps PostgreSQL OIDs onto: @point@, @polygon@,
@inet@, @tsvector@ and @interval@. (PostGIS @geometry@ stays in @ihp@,
"IHP.Hasql.Encoders", which keeps the @Mapping.IsScalar@ bridge matching
its @postgresql-types@ pin.) The scalar @Int@ family, 'Integer', and the
'Id'' encoders live here as well.

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
import           Data.Text                (Text)
import           Data.Vector (Vector)
import           Data.Functor.Contravariant (contramap)
import           Data.Functor.Contravariant.Divisible (divide)
import qualified Hasql.Encoders           as Encoders
import           Hasql.Implicits.Encoders (DefaultParamEncoder (..))
import qualified Hasql.Mapping.IsScalar   as Mapping
import           Hasql.PostgresqlTypes    () -- IsScalar instances for the postgresql-types values below
import           IHP.TypedSql.Id          (Id' (..), PrimaryKey)
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

-- | Encode 'Id' table' for tables with any primary key type that has an 'IsScalar' instance.
-- The 'Id'' type and the 'PrimaryKey' family live in "IHP.TypedSql.Id"; only
-- their encoders live here, next to every other 'DefaultParamEncoder' instance,
-- so @ihp@ can import the type without paying for the encoder machinery.
instance Mapping.IsScalar (PrimaryKey table) => DefaultParamEncoder (Id' table) where
    defaultParam = Encoders.nonNullable (contramap (\(Id pk) -> pk) Mapping.encoder)

-- | Encode list of 'Id' table' for tables with any encodable primary key type.
instance Mapping.IsScalar (PrimaryKey table) => DefaultParamEncoder [Id' table] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable (contramap (\(Id pk) -> pk) Mapping.encoder)

-- | Encode 'Maybe (Id' table)' for nullable foreign keys with any encodable primary key type.
instance Mapping.IsScalar (PrimaryKey table) => DefaultParamEncoder (Maybe (Id' table)) where
    defaultParam = Encoders.nullable (contramap (\(Id pk) -> pk) Mapping.encoder)

-- | Encode '[Maybe (Id' table)]' for @IN (...)@ queries with nullable foreign keys.
instance Mapping.IsScalar (PrimaryKey table) => DefaultParamEncoder [Maybe (Id' table)] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nullable (contramap (\(Id pk) -> pk) Mapping.encoder)

-- | Encode '(Id' a, Id' b)' as PostgreSQL composite/record type
-- Used for composite primary keys with two Id columns of any scalar PK type
instance (Mapping.IsScalar (PrimaryKey a), Mapping.IsScalar (PrimaryKey b)) => DefaultParamEncoder (Id' a, Id' b) where
    defaultParam = Encoders.nonNullable $ Encoders.composite (Nothing :: Maybe Text) "" $
        divide (\(Id a, Id b) -> (a, b))
            (Encoders.field (Encoders.nonNullable Mapping.encoder))
            (Encoders.field (Encoders.nonNullable Mapping.encoder))

-- | Encode '[(Id' a, Id' b)]' as PostgreSQL array of composite types
-- Used by filterWhereIdIn for tables with two-column composite primary keys
instance (Mapping.IsScalar (PrimaryKey a), Mapping.IsScalar (PrimaryKey b)) => DefaultParamEncoder [(Id' a, Id' b)] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable $ Encoders.composite (Nothing :: Maybe Text) "" $
        divide (\(Id a, Id b) -> (a, b))
            (Encoders.field (Encoders.nonNullable Mapping.encoder))
            (Encoders.field (Encoders.nonNullable Mapping.encoder))

-- | Encode '(Id' a, Id' b, Id' c)' as PostgreSQL composite/record type
-- Used for composite primary keys with three Id columns of any scalar PK type
instance (Mapping.IsScalar (PrimaryKey a), Mapping.IsScalar (PrimaryKey b), Mapping.IsScalar (PrimaryKey c)) => DefaultParamEncoder (Id' a, Id' b, Id' c) where
    defaultParam = Encoders.nonNullable $ Encoders.composite (Nothing :: Maybe Text) "" $
        divide (\(Id a, Id b, Id c) -> (a, (b, c)))
            (Encoders.field (Encoders.nonNullable Mapping.encoder))
            (divide id (Encoders.field (Encoders.nonNullable Mapping.encoder)) (Encoders.field (Encoders.nonNullable Mapping.encoder)))

-- | Encode '[(Id' a, Id' b, Id' c)]' as PostgreSQL array of composite types
-- Used by filterWhereIdIn for tables with three-column composite primary keys
instance (Mapping.IsScalar (PrimaryKey a), Mapping.IsScalar (PrimaryKey b), Mapping.IsScalar (PrimaryKey c)) => DefaultParamEncoder [(Id' a, Id' b, Id' c)] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable $ Encoders.composite (Nothing :: Maybe Text) "" $
        divide (\(Id a, Id b, Id c) -> (a, (b, c)))
            (Encoders.field (Encoders.nonNullable Mapping.encoder))
            (divide id (Encoders.field (Encoders.nonNullable Mapping.encoder)) (Encoders.field (Encoders.nonNullable Mapping.encoder)))

-- | Encode '(Id' a, Id' b, Id' c, Id' d)' as PostgreSQL composite/record type
-- Used for composite primary keys with four Id columns of any scalar PK type
instance (Mapping.IsScalar (PrimaryKey a), Mapping.IsScalar (PrimaryKey b), Mapping.IsScalar (PrimaryKey c), Mapping.IsScalar (PrimaryKey d)) => DefaultParamEncoder (Id' a, Id' b, Id' c, Id' d) where
    defaultParam = Encoders.nonNullable $ Encoders.composite (Nothing :: Maybe Text) "" $
        divide (\(Id a, Id b, Id c, Id d) -> (a, (b, c, d)))
            (Encoders.field (Encoders.nonNullable Mapping.encoder))
            (divide (\(b, c, d) -> (b, (c, d)))
                (Encoders.field (Encoders.nonNullable Mapping.encoder))
                (divide id (Encoders.field (Encoders.nonNullable Mapping.encoder)) (Encoders.field (Encoders.nonNullable Mapping.encoder))))

-- | Encode '[(Id' a, Id' b, Id' c, Id' d)]' as PostgreSQL array of composite types
-- Used by filterWhereIdIn for tables with four-column composite primary keys
instance (Mapping.IsScalar (PrimaryKey a), Mapping.IsScalar (PrimaryKey b), Mapping.IsScalar (PrimaryKey c), Mapping.IsScalar (PrimaryKey d)) => DefaultParamEncoder [(Id' a, Id' b, Id' c, Id' d)] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable $ Encoders.composite (Nothing :: Maybe Text) "" $
        divide (\(Id a, Id b, Id c, Id d) -> (a, (b, c, d)))
            (Encoders.field (Encoders.nonNullable Mapping.encoder))
            (divide (\(b, c, d) -> (b, (c, d)))
                (Encoders.field (Encoders.nonNullable Mapping.encoder))
                (divide id (Encoders.field (Encoders.nonNullable Mapping.encoder)) (Encoders.field (Encoders.nonNullable Mapping.encoder))))
