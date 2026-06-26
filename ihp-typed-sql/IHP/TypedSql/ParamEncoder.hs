{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module: IHP.TypedSql.ParamEncoder
Description: Turns a @${...}@ placeholder value into a hasql 'Snippet'
Copyright: (c) digitally induced GmbH, 2026

The @typedSql@ quasiquoter annotates every @${...}@ parameter with the column's
__scalar__ Haskell type (e.g. @Int@, @Id' table@, a generated enum) and emits a
call to 'typedSqlParam'. The 'TypedSqlParam' class then accepts the value in any
of four shapes and binds it through the matching 'DefaultParamEncoder':

  * @${x}@          — the bare column type, or any newtype coercible to it (e.g. @Id'@)
  * @${Just x}@     — a @Maybe@ of the column type (and @${Nothing}@ binds SQL @NULL@)
  * @${[x]}@        — a list, for @IN (...)@ / @= ANY(...)@
  * @${[Just x]}@   — a list of @Maybe@, for nullable-element arrays

Because the value's shape selects the instance, bare values keep working for every
column (nullable or not), while @Maybe@ / @[Maybe]@ values are now accepted too.
-}
module IHP.TypedSql.ParamEncoder
    ( TypedSqlParam (..)
    ) where

import Prelude
import Data.Coerce (Coercible, coerce)
import Data.Proxy (Proxy)
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.Implicits.Encoders (DefaultParamEncoder)
import IHP.Hasql.Encoders () -- DefaultParamEncoder instances for Int, Id', etc.

-- | Encode a @typedSql@ placeholder value into a 'Snippet', given the column's
-- scalar Haskell type @col@ (passed as a 'Proxy' by the quasiquoter).
--
-- See the module documentation for the supported value shapes.
class TypedSqlParam col a where
    typedSqlParam :: Proxy col -> a -> Snippet

-- | Bare value: the column type itself, or a newtype representationally coercible
-- to it (this is what lets @Id' table@ bind against its underlying primary key).
instance {-# OVERLAPPABLE #-} (DefaultParamEncoder col, Coercible a col) => TypedSqlParam col a where
    typedSqlParam _ value = Snippet.param (coerce value :: col)

-- | @Maybe@ value: binds @Just x@ as the value and @Nothing@ as SQL @NULL@.
instance DefaultParamEncoder (Maybe col) => TypedSqlParam col (Maybe col) where
    typedSqlParam _ = Snippet.param

-- | List value: for @column IN (...)@ and @column = ANY(...)@.
instance DefaultParamEncoder [col] => TypedSqlParam col [col] where
    typedSqlParam _ = Snippet.param

-- | List of @Maybe@ value: a nullable-element array.
instance DefaultParamEncoder [Maybe col] => TypedSqlParam col [Maybe col] where
    typedSqlParam _ = Snippet.param
