{-|
Module: IHP.ValidationSupport.ValidateField
Description: Validation for records
Copyright: (c) digitally induced GmbH, 2020

Use 'validateField' and 'validateFieldIO' together with the validation functions to do simple validations.

Also take a look at 'IHP.ValidationSupport.ValidateIsUnique.validateIsUnique' for e.g. checking that an email is unique.
-}
module IHP.ValidationSupport.ValidateField where

import ClassyPrelude
import Data.Proxy
import IHP.ValidationSupport.Types
import GHC.TypeLits (KnownSymbol, Symbol)
import GHC.Records
import IHP.ModelSupport
import IHP.HaskellSupport

-- | A function taking some value and returning a 'ValidatorResult'
--
-- >>> Validator Text
-- Text -> ValidatorResult
--
-- >>> Validator Int
-- Int -> ValidatorResult
type Validator valueType = valueType -> ValidatorResult

-- | Validates a record field using a given validator function.
--
-- When the validation fails, the validation error is saved inside the @meta :: MetaBag@ field of the record.
-- You can retrieve a possible validation error using 'IHP.ValidationSupport.Types.getValidationFailure'.
--
-- __Example:__ 'nonEmpty' validation for a record
--
-- > let project :: NewProject = newRecord
-- > project
-- >     |> validateField #name nonEmpty
-- >     |> getValidationFailure #name -- Just "This field cannot be empty"
-- >
-- >
-- > project
-- >     |> set #name "Hello World"
-- >     |> validateField #name nonEmpty
-- >     |> getValidationFailure #name -- Nothing
--
--
-- __Example:__ Using 'IHP.Controller.Param.ifValid' for branching
--
-- > let project :: NewProject = newRecord
-- >
-- > project
-- >     |> validateField #name nonEmpty
-- >     |> ifValid \case
-- >         Left project -> do
-- >             putStrLn "Invalid project. Please try again"
-- >         Right project -> do
-- >             putStrLn "Project is valid. Saving to database."
-- >             createRecord project
validateField :: forall field fieldValue validator model. (
        KnownSymbol field
        , HasField field model fieldValue
        , HasField "meta" model MetaBag
        , SetField "meta" model MetaBag
    ) => Proxy field -> Validator fieldValue -> model -> model
validateField field validator model = attachValidatorResult field (validator (getField @field model)) model
{-# INLINE validateField #-}


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
-- The same as 'validateField', but works with IO and can e.g. access the database.
--
-- When the validation fails, the validation error is saved inside the @meta :: MetaBag@ field of the record.
-- You can retrieve a possible validation error using 'IHP.ValidationSupport.Types.getValidationFailure'.
--
validateFieldIO :: forall field model fieldValue. (
        ?modelContext :: ModelContext
        , KnownSymbol field
        , HasField field model fieldValue
        , HasField "meta" model MetaBag
        , SetField "meta" model MetaBag
    ) => Proxy field -> ValidatorIO fieldValue -> model -> IO model
validateFieldIO fieldProxy customValidation model = do
    let value :: fieldValue = getField @field model
    result <- customValidation value
    pure (attachValidatorResult fieldProxy result model)
{-# INLINE validateFieldIO #-}

-- | Validates value is not empty
--
-- >>> nonEmpty "hello world"
-- Success
--
-- >>> nonEmpty ""
-- Failure "This field cannot be empty"
--
-- >>> nonEmpty (Just "hello")
-- Success
--
-- >>> nonEmpty Nothing
-- Failure "This field cannot be empty"
nonEmpty :: MonoFoldable value => value -> ValidatorResult
nonEmpty value | null value = Failure "This field cannot be empty"
nonEmpty _ = Success
{-# INLINE nonEmpty #-}

-- | Validates value looks like a phone number
--
-- Values needs to start with @\+@ and has to have atleast 5 characters
--
-- >>> isPhoneNumber "1337"
-- Failure ".."
--
-- >>> isPhoneNumber "+49123456789"
-- Success
isPhoneNumber :: Text -> ValidatorResult
isPhoneNumber text | "+" `isPrefixOf` text && length text > 5 = Success
isPhoneNumber text = Failure "is not a valid phone number (has to start with +, at least 5 characters)"
{-# INLINE isPhoneNumber #-}

-- | Validates email by checking that there is an @\@@
--
-- >>> isEmail "marc@digitallyinduced.com"
-- Success
--
-- >>> isEmail "loremipsum"
-- Failure "is not a valid email"
--
-- >>> isEmail "someone@hostname"
-- Success
isEmail :: Text -> ValidatorResult
isEmail text | "@" `isInfixOf` text = Success
isEmail text = Failure "is not a valid email"
{-# INLINE isEmail #-}

-- | Validates value is between min and max
--
-- >>> isInRange (0, 10) 5
-- Success
--
-- >>> isInRange (0, 10) 0
-- Success
--
-- >>> isInRange (0, 10) 1337
-- Failure " has to be between 0 and 10"
--
-- >>> let isHumanAge = isInRange (0, 100)
-- >>> isHumanAge 22
-- Success
isInRange :: (Show value, Ord value) => (value, value) -> value -> ValidatorResult
isInRange (min, max) value | value >= min && value <= max = Success
isInRange (min, max) value = Failure (" has to be between " <> tshow min <> " and " <> tshow max)
{-# INLINE isInRange #-}

-- | Validates that value is a hax-based color string
--
-- >>> isColor "#ffffff"
--  Success
--
-- >>> isColor "rgb(0, 0, 0)"
-- Failure "is not a valid color"
isColor :: Text -> ValidatorResult
isColor text | ("#" `isPrefixOf` text) && (length text == 7) = Success
isColor text = Failure "is not a valid color"
{-# INLINE isColor #-}

-- | Validates string starts with @http://@ or @https://@
--
-- >>> isUrl "https://digitallyinduced.com"
-- Success
--
-- >>> isUrl "digitallyinduced.com"
-- Failure "is not a valid url. It needs to start with http:// or https://"
isUrl :: Text -> ValidatorResult
isUrl text | "http://" `isPrefixOf` text || "https://" `isPrefixOf` text = Success
isUrl text = Failure "is not a valid url. It needs to start with http:// or https://"
{-# INLINE isUrl #-}