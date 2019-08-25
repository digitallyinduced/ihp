{-# LANGUAGE TypeFamilies, DataKinds, MultiParamTypeClasses, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, IncoherentInstances, AllowAmbiguousTypes, FunctionalDependencies #-}
module TurboHaskell.HaskellSupport ((|>), isEmpty, whenEmpty, whenNonEmpty, (==>), get, set, ifOrEmpty, modify, SetField (..), UpdateField (..)) where

import ClassyPrelude
import Control.Monad (when)
import qualified Data.Default
import qualified Data.UUID
import Data.Proxy
import GHC.TypeLits
import GHC.OverloadedLabels
import qualified GHC.Records as Record

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

(==>) :: forall model attribute value. (KnownSymbol attribute, Record.HasField attribute model value) => model -> Proxy attribute -> value
(==>) struct _ = Record.getField @attribute struct

instance forall name name'. (KnownSymbol name, name' ~ name) => IsLabel name (Proxy name') where
    fromLabel = Proxy @name'

{-# INLINE get #-}
get :: forall model name value. (KnownSymbol name, Record.HasField name model value) => Proxy name -> model -> value
get _ record = Record.getField @name record

{-# INLINE set #-}
set :: forall model name value. (KnownSymbol name, SetField name model value) => Proxy name -> value -> model -> model
set name value record = setField @name value record

{-# INLINE modify #-}
modify :: forall model name value updateFunction. (KnownSymbol name, Record.HasField name model value, SetField name model value) => Proxy name -> (value -> value) -> model -> model
modify _ updateFunction model = let value = Record.getField @name model in setField @name (updateFunction value) model

-- UpdateField field model (Include field model) fieldValue (FetchResult fieldValue fetchModel),
class SetField (field :: GHC.TypeLits.Symbol) model value | field model -> value where
    setField :: value -> model -> model

class Record.HasField field model value => UpdateField (field :: GHC.TypeLits.Symbol) model model' value value' | model model' value' -> value where
    updateField :: value' -> model -> model'