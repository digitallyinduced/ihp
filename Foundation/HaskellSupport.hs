{-# LANGUAGE TypeFamilies, DataKinds, MultiParamTypeClasses, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, IncoherentInstances, AllowAmbiguousTypes, FunctionalDependencies #-}
module Foundation.HaskellSupport ((|>), isEmpty, whenEmpty, whenNonEmpty, (==>), get, ifOrEmpty, modify) where

import ClassyPrelude
import Control.Monad (when)
import qualified Data.Default
import qualified Data.UUID
import Data.Proxy
import GHC.TypeLits
import GHC.OverloadedLabels
import Control.Lens hiding ((|>))
import Data.Generics.Product

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

{-# INLINE whenEmpty #-}
whenEmpty condition = when (isEmpty condition)

{-# INLINE whenNonEmpty #-}
whenNonEmpty :: (Monoid a, Eq a, Applicative f) => a -> f () -> f ()
whenNonEmpty condition = when (not (isEmpty condition))


instance Data.Default.Default Data.UUID.UUID where
    def = Data.UUID.nil

(==>) :: forall model attribute value. (KnownSymbol attribute, HasField' attribute model value) => model -> Proxy attribute -> value
(==>) struct _ = getField @attribute struct

instance forall name name'. (KnownSymbol name, name' ~ name) => IsLabel name (Proxy name') where
    fromLabel = Proxy @name'

get :: forall model name value. (KnownSymbol name, HasField' name model value, Generic model) => Proxy name -> model -> value
get _ = getField @name
{-# INLINE get #-}

modify :: forall model name value updateFunction. (KnownSymbol name, HasField' name model value) => Proxy name -> (value -> value) -> model -> model
modify _ updateFunction model = let value = getField @name model in setField @name (updateFunction value) model
