{-# LANGUAGE TypeFamilies, DataKinds, MultiParamTypeClasses, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, IncoherentInstances, AllowAmbiguousTypes, FunctionalDependencies #-}
module Foundation.HaskellSupport ((|>), isEmpty, whenEmpty, whenNonEmpty, (==>), get, ifOrEmpty) where

import ClassyPrelude
import Control.Monad (when)
import qualified Data.Default
import qualified Data.UUID
import Data.Proxy
import GHC.Records
import GHC.TypeLits
import GHC.OverloadedLabels

--(|>) :: a -> f -> f a
infixl 8 |>
a |> f = f a
{-# INLINE (|>) #-}

isEmpty :: (Eq a, Monoid a) => a -> Bool
isEmpty value = value == mempty
{-# INLINE isEmpty #-}

ifOrEmpty :: (Monoid a) => Bool -> a -> a
ifOrEmpty bool a = if bool then a else mempty
{-# INLINE ifOrEmpty #-}

whenEmpty condition = when (isEmpty condition)
whenNonEmpty condition = when (not (isEmpty condition))


instance Data.Default.Default Data.UUID.UUID where
    def = Data.UUID.nil

(==>) :: forall model attribute value. (KnownSymbol attribute, HasField attribute model value) => model -> Proxy attribute -> value
(==>) struct _ = getField @attribute struct

instance forall name name'. (KnownSymbol name, name' ~ name) => IsLabel name (Proxy name') where
    fromLabel = Proxy @name'

get :: forall model name value. (KnownSymbol name, HasField name model value) => Proxy name -> model -> value
get _ = getField @name
{-# INLINE get #-}
