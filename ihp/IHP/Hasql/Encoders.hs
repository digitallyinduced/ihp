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
import qualified Hasql.Encoders as Encoders
import Hasql.Implicits.Encoders (DefaultParamEncoder(..))
import Data.Functor.Contravariant (contramap, (>$<))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import IHP.ModelSupport.Types (Id'(..), PrimaryKey)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.Types (Binary(..))

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
    defaultParam = Encoders.nonNullable $ Encoders.composite $
        (fst >$< Encoders.field (Encoders.nonNullable Encoders.uuid)) <>
        (snd >$< Encoders.field (Encoders.nonNullable Encoders.uuid))

-- | Encode '[(UUID, UUID)]' as PostgreSQL array of composite types
-- Used by filterWhereIdIn for tables with composite primary keys of two UUIDs
instance DefaultParamEncoder [(UUID, UUID)] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable $ Encoders.composite $
        (fst >$< Encoders.field (Encoders.nonNullable Encoders.uuid)) <>
        (snd >$< Encoders.field (Encoders.nonNullable Encoders.uuid))

-- | Encode '(Id' a, Id' b)' as PostgreSQL composite/record type
-- Used for composite primary keys with two Id columns (where both resolve to UUID)
instance (PrimaryKey a ~ UUID, PrimaryKey b ~ UUID) => DefaultParamEncoder (Id' a, Id' b) where
    defaultParam = Encoders.nonNullable $ Encoders.composite $
        ((\(Id uuid) -> uuid) . fst >$< Encoders.field (Encoders.nonNullable Encoders.uuid)) <>
        ((\(Id uuid) -> uuid) . snd >$< Encoders.field (Encoders.nonNullable Encoders.uuid))

-- | Encode '[(Id' a, Id' b)]' as PostgreSQL array of composite types
-- Used by filterWhereIdIn for tables with composite primary keys of two Id columns
instance (PrimaryKey a ~ UUID, PrimaryKey b ~ UUID) => DefaultParamEncoder [(Id' a, Id' b)] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable $ Encoders.composite $
        ((\(Id uuid) -> uuid) . fst >$< Encoders.field (Encoders.nonNullable Encoders.uuid)) <>
        ((\(Id uuid) -> uuid) . snd >$< Encoders.field (Encoders.nonNullable Encoders.uuid))

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
