{-|
Module: IHP.TypedSql.Prelude
Description: Internal prelude for the ihp-typed-sql package
Copyright: (c) digitally induced GmbH, 2026

Internal to @ihp-typed-sql@ and deliberately not exposed. Every module in this
package needs the same base import and the '|>' operator, so they live here
once instead of being repeated — and, for '|>', re-defined — in each module.
-}
module IHP.TypedSql.Prelude
    ( module Prelude
    , (|>)
    ) where

import           Prelude

-- | Pipe operator: @a |> f@ is @f a@.
infixl 8 |>
(|>) :: a -> (a -> b) -> b
a |> f = f a
{-# INLINE (|>) #-}
