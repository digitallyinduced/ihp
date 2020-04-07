{-|
Module: TurboHaskell.ValidationSupport.ValidateFieldIO
Description: IO validations for records
Copyright: (c) digitally induced GmbH, 2020

For complex validation logic which requires IO and database access.

For simpler validation use 'TurboHaskell.ValidationSupport.ValidateField.validateField'.

Also take a look at 'TurboHaskell.ValidationSupport.ValidateIsUnique.validateIsUnique' for e.g. checking that an email is unique.
-}
module TurboHaskell.ValidationSupport.ValidateFieldIO (validateFieldIO) where

import ClassyPrelude
import Data.Proxy
import TurboHaskell.ModelSupport
import TurboHaskell.ValidationSupport.Types
import GHC.Records
import GHC.TypeLits
import TurboHaskell.HaskellSupport

-- | A function taking some value and returning a 'IO ValidatorResult'
--
-- >>> ValidatorIO Text
-- Text -> IO ValidatorResult
--
-- >>> ValidatorIO Int
-- Int -> IO ValidatorResult
type ValidatorIO value = value -> IO ValidatorResult

-- | Validates a record field using a given validator function.
--
-- The same as 'TurboHaskell.ValidationSupport.ValidateField.validateField', but works with IO and can e.g. access the database.
--
-- When the validation fails, the validation error is saved inside the @meta :: MetaBag@ field of the record.
-- You can retrieve a possible validation error using 'TurboHaskell.ValidationSupport.Types.getValidationFailure'.
--
validateFieldIO :: forall field model fieldValue. (
        , ?modelContext :: ModelContext
        , KnownSymbol field
        , HasField field model fieldValue
        , HasField "meta" model MetaBag
        , SetField "meta" model MetaBag
    ) => Proxy field -> ValidatorIO fieldValue -> model -> IO model
validateFieldIO fieldProxy customValidation model = do
    let value :: fieldValue = getField @field model
    result <- liftIO (customValidation value)
    pure (attachValidatorResult fieldProxy result model)
{-# INLINE validateFieldIO #-}