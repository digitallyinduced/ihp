{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: IHP.Hasql.Encoder
Description: DefaultParamEncoder instances for common types
Copyright: (c) digitally induced GmbH, 2025

This module provides orphan 'DefaultParamEncoder' instances for types that
hasql-implicits doesn't support out of the box, most importantly 'Int'.

These instances are needed because hasql-implicits only provides instances for
fixed-width integer types ('Int16', 'Int32', 'Int64'), not Haskell's
platform-dependent 'Int'. Since most IHP applications use 'Int' for
integer columns, we provide these instances to make the transition seamless.
-}
module IHP.Hasql.Encoder () where

import Prelude
import Data.Int (Int64)
import qualified Hasql.Encoders as Encoders
import Hasql.Implicits.Encoders (DefaultParamEncoder(..))
import Data.Functor.Contravariant (contramap)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

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
