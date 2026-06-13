{-# LANGUAGE TypeFamilies, DataKinds, MultiParamTypeClasses, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, AllowAmbiguousTypes, FunctionalDependencies #-}

{-|
Module: IHP.Record
Description: Type-level record field operations
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Record
( (|>)
, get
, set
, setJust
, setMaybe
, modify
, modifyJust
, SetField (..)
, UpdateField (..)
, incrementField
, decrementField
, CopyFields (..)
) where

import Data.Proxy
import GHC.TypeLits
import qualified GHC.Records as Record
import Prelude

-- | Pipe operator
infixl 8 |>
(|>) :: a -> (a -> b) -> b
a |> f = f a
{-# INLINE (|>) #-}

-- | Returns the field value for a field name
get :: forall model name value. (KnownSymbol name, Record.HasField name model value) => Proxy name -> model -> value
get _ record = Record.getField @name record
{-# INLINE get #-}

-- | Sets a field of a record and returns the new record.
set :: forall model name value. (KnownSymbol name, SetField name model value) => Proxy name -> value -> model -> model
set name value record = setField @name value record
{-# INLINE set #-}

-- | Like 'set' but doesn't set the value if it's 'Nothing'.
setMaybe :: forall model name value. (KnownSymbol name, SetField name model (Maybe value)) => Proxy name -> Maybe value -> model -> model
setMaybe name value record = case value of
    Just value -> setField @name (Just value) record
    Nothing    -> record
{-# INLINE setMaybe #-}

-- | Like 'set' but wraps the value with a 'Just'.
setJust :: forall model name value. (KnownSymbol name, SetField name model (Maybe value)) => Proxy name -> value -> model -> model
setJust name value record = setField @name (Just value) record
{-# INLINE setJust #-}

modify :: forall model name value. (KnownSymbol name, Record.HasField name model value, SetField name model value) => Proxy name -> (value -> value) -> model -> model
modify _ updateFunction model = let value = Record.getField @name model in setField @name (updateFunction value) model
{-# INLINE modify #-}

-- | Like 'modify', but only modifies the value if it's not Nothing.
modifyJust :: forall model name value. (KnownSymbol name, Record.HasField name model (Maybe value), SetField name model (Maybe value)) => Proxy name -> (value -> value) -> model -> model
modifyJust _ updateFunction model = case Record.getField @name model of
        Just value -> setField @name (Just (updateFunction value)) model
        Nothing -> model
{-# INLINE modifyJust #-}

-- | Plus @1@ on record field.
incrementField :: forall model name value. (KnownSymbol name, Record.HasField name model value, SetField name model value, Num value) => Proxy name -> model -> model
incrementField _ model = let value = Record.getField @name model in setField @name (value + 1) model
{-# INLINE incrementField #-}

-- | Minus @1@ on a record field.
decrementField :: forall model name value. (KnownSymbol name, Record.HasField name model value, SetField name model value, Num value) => Proxy name -> model -> model
decrementField _ model = let value = Record.getField @name model in setField @name (value - 1) model
{-# INLINE decrementField #-}

class SetField (field :: GHC.TypeLits.Symbol) model value | field model -> value where
    setField :: value -> model -> model

class Record.HasField field model value => UpdateField (field :: GHC.TypeLits.Symbol) model model' value value' | model model' value' -> value where
    updateField :: value' -> model -> model'

-- | Generic 'SetField' for every record field, defined in terms of 'UpdateField'.
--
-- The schema compiler emits one 'UpdateField' instance per field (it carries the
-- actual record update and the polymorphic belongs-to variants). 'SetField' is
-- the non-type-changing special case, so rather than generating a second, nearly
-- identical instance per field, we delegate to 'UpdateField' here once. The
-- @HasField@ constraint (built in for every record field) satisfies the
-- @field model -> value@ functional dependency. The delegation is @INLINE@, so
-- after inlining this compiles to exactly the same code as a hand-written
-- per-field instance — zero runtime cost.
--
-- Marked @OVERLAPPABLE@ so the framework's bespoke 'SetField' instances for
-- non-model types (e.g. @SQLQuery@, @FormContext@, @MetaBag@) still win.
instance {-# OVERLAPPABLE #-} (Record.HasField field model value, UpdateField field model model value value) => SetField field model value where
    setField = updateField @field
    {-# INLINE setField #-}

class CopyFields (fields :: [Symbol]) destinationRecord sourceRecord where
    copyFields :: sourceRecord -> destinationRecord -> destinationRecord

instance CopyFields ('[]) destinationRecord sourceRecord where
    copyFields sourceRecord destinationRecord = destinationRecord
    {-# INLINE copyFields #-}

instance (CopyFields rest destinationRecord sourceRecord
    , KnownSymbol fieldName
    , SetField fieldName destinationRecord fieldType
    , Record.HasField fieldName sourceRecord fieldType
    ) => CopyFields (fieldName:rest) destinationRecord sourceRecord where
    copyFields sourceRecord destinationRecord =
            destinationRecord
            |> set (Proxy @fieldName) (Record.getField @fieldName sourceRecord)
            |> copyFields @rest sourceRecord
    {-# INLINE copyFields #-}
