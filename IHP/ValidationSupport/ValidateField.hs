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
import GHC.TypeLits (KnownSymbol)
import GHC.Records
import IHP.ModelSupport
import IHP.HaskellSupport
import Text.Regex.TDFA
import Data.List ((!!))

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
-- > let project :: Project = newRecord
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
-- > let project :: Project = newRecord
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

-- | Overrides the error message of a given validator function.
--
-- >>> (nonEmpty |> withCustomErrorMessage "Custom error message") ""
-- Failure "Custom error message"
--
--
-- >>> (isEmail |> withCustomErrorMessage "We only accept valid email addresses") "not valid email"
-- Failure "We only accept valid email addresses"
withCustomErrorMessage :: Text -> (value -> ValidatorResult) -> value -> ValidatorResult
withCustomErrorMessage errorMessage validator value =
    case validator value of
        Failure _ -> Failure errorMessage
        Success -> Success
{-# INLINABLE withCustomErrorMessage #-}


-- | Validates that value passes at least one of the given validators
--
-- >>> "ihp@example.com" |> validateAny([isEmptyValue, isEmail])
-- Success
--
-- >>> "" |> validateAny([isEmptyValue, isEmail])
-- Success
--
-- >>> "no spam plz" |> validateAny([empty, isEmail])
-- Failure "did not pass any validators"
validateAny :: [value -> ValidatorResult] -> value -> ValidatorResult
validateAny validators text =
  case any isSuccess $ map ($ text) validators of
    True -> Success
    False -> Failure "did not pass any validators"
{-# INLINABLE validateAny #-}


-- | Validates that value passes all of the given validators
--
-- In case of multiple failures, the first Failure is returned.
--
-- >>> 2016 |> validateAll([isGreaterThan(1900), isLessThan(2020)])
-- Success
--
-- >>> 1899 |> validateAll([isGreaterThan(1900), isLessThan(2020)])
-- Failure "has to be greater than 1900"
validateAll :: [value -> ValidatorResult] -> value -> ValidatorResult
validateAll validators text =
  let results = map ($ text) validators
  in case all isSuccess results of
    True -> Success
    False -> (filter isFailure results) !! 0
{-# INLINABLE validateAll #-}


-- | Validates that value is not empty
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
nonEmpty :: IsEmpty value => value -> ValidatorResult
nonEmpty value | isEmpty value = Failure "This field cannot be empty"
nonEmpty _ = Success
{-# INLINABLE nonEmpty #-}


-- | Validates that value is empty
--
-- >>> isEmptyValue "hello world"
-- Failure "This field must be empty"
--
-- >>> ieEmptyValue ""
-- Success
--
-- >>> isEmptyValue (Just "hello")
-- Failure "This field must be empty"
--
-- >>> isEmptyValue Nothing
-- Success
isEmptyValue :: IsEmpty value => value -> ValidatorResult
isEmptyValue value | isEmpty value = Success
isEmptyValue _ = Failure "This field must be empty"
{-# INLINABLE isEmptyValue #-}


-- | Validates that value looks like a phone number
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
{-# INLINABLE isPhoneNumber #-}


-- | Validates that value is an email address
--
-- The validation is not meant to be compliant with RFC 822. Its purpose is to
-- reject obviously invalid values without false-negatives.
--
-- >>> isEmail "marc@digitallyinduced.com"
-- Success
--
-- >>> isEmail "marc@secret.digitallyinduced.com" -- subdomains are fine
-- Success
--
-- >>> isEmail "ॐ@मणिपद्मे.हूँ"
-- Success
--
-- >>> isEmail "marc@localhost" -- Although discouraged by ICANN, dotless TLDs are legal. See https://www.icann.org/news/announcement-2013-08-30-en
-- Success
--
-- >>> isEmail "loremipsum"
-- Failure "is not a valid email"
--
-- >>> isEmail "A@b@c@domain.com"
-- Failure "is not a valid email"
isEmail :: Text -> ValidatorResult
isEmail text | text =~ ("^[^ @]+@[^ @_+]+\\.?[^ @_+-]+$" :: Text) = Success
isEmail text = Failure "is not a valid email"
{-# INLINABLE isEmail #-}


-- | Validates that value is between min and max
--
-- >>> isInRange (0, 10) 5
-- Success
--
-- >>> isInRange (0, 10) 0
-- Success
--
-- >>> isInRange (0, 10) 1337
-- Failure "has to be between 0 and 10"
--
-- >>> let isHumanAge = isInRange (0, 100)
-- >>> isHumanAge 22
-- Success
isInRange :: (Show value, Ord value) => (value, value) -> value -> ValidatorResult
isInRange (min, max) value | value >= min && value <= max = Success
isInRange (min, max) value = Failure ("has to be between " <> tshow min <> " and " <> tshow max)
{-# INLINABLE isInRange #-}


-- | Validates that value is less than a max value
--
-- >>> isLessThan 10 5
-- Success
--
-- >>> isLessThan 10 20
-- Failure "has to be less than 10"
isLessThan :: (Show value, Ord value) => value -> value -> ValidatorResult
isLessThan max value | value < max = Success
isLessThan max value = Failure ("has to be less than " <> tshow max)
{-# INLINABLE isLessThan #-}


-- | Validates that value is greater than a min value
--
-- >>> isGreaterThan 10 20
-- Success
--
-- >>> isGreaterThan 10 5
-- Failure "has to be greater than 10"
isGreaterThan :: (Show value, Ord value) => value -> value -> ValidatorResult
isGreaterThan min value | value > min = Success
isGreaterThan min value = Failure ("has to be greater than " <> tshow min)
{-# INLINABLE isGreaterThan #-}


-- | Validates that value has a max length
--
-- >>> hasMaxLength 10 "IHP"
-- Success
--
-- >>> hasMaxLength 2 "IHP"
-- Failure "is longer than 2 characters"
hasMaxLength :: Int -> Text -> ValidatorResult
hasMaxLength max text | length text <= max = Success
hasMaxLength max text = Failure ("is longer than " <> tshow max <> " characters")
{-# INLINABLE hasMaxLength #-}


-- | Validates that value has a min length
--
-- >>> hasMinLength 2 "IHP"
-- Success
--
-- >>> hasMinLength 10 "IHP"
-- Failure "is shorter than 10 characters"
hasMinLength :: Int -> Text -> ValidatorResult
hasMinLength min text | length text >= min = Success
hasMinLength min text = Failure ("is shorter than " <> tshow min <> " characters")
{-# INLINABLE hasMinLength #-}


-- | Validates that value is a hex-based rgb color string
--
-- >>> isRgbHexColor "#ffffff"
-- Success
--
-- >>> isRgbHexColor "#fff"
-- Success
--
-- >>> isRgbHexColor "rgb(0, 0, 0)"
-- Failure "is not a valid rgb hex color"
isRgbHexColor :: Text -> ValidatorResult
isRgbHexColor text | text =~ ("^#([0-9a-f]{3}|[0-9a-f]{6})$" :: Text) = Success
isRgbHexColor text = Failure "is not a valid rgb hex color"
{-# INLINABLE isRgbHexColor #-}


-- | Validates that value is a hex-based rgb color string
--
-- >>> isRgbaHexColor "#ffffffff"
-- Success
--
-- >>> isRgbaHexColor "#ffff"
-- Success
--
-- >>> isRgbaHexColor "rgb(0, 0, 0, 1)"
-- Failure "is not a valid rgba hex color"
isRgbaHexColor :: Text -> ValidatorResult
isRgbaHexColor text | text =~ ("^#([0-9a-f]{4}|[0-9a-f]{8})$" :: Text) = Success
isRgbaHexColor text = Failure "is not a valid rgba hex color"
{-# INLINABLE isRgbaHexColor #-}


-- | Validates that value is a hex-based rgb(a) color string
--
-- >>> isHexColor "#ffffff"
-- Success
--
-- >>> isHexColor "#ffffffff"
-- Success
--
-- >>> isHexColor "rgb(0, 0, 0)"
-- Failure "is not a valid hex color"
isHexColor :: Text -> ValidatorResult
isHexColor = validateAny [isRgbHexColor, isRgbaHexColor]
  |> withCustomErrorMessage "is not a valid hex color"
{-# INLINABLE isHexColor #-}


-- | Validates that value is a rgb() color string
--
-- >>> isRgbColor "rgb(255, 0, 0)"
-- Success
--
-- >>> isRgbColor "#f00"
-- Failure "is not a valid rgb() color"
isRgbColor :: Text -> ValidatorResult
isRgbColor text | text =~ ("^rgb\\( *([0-9]*\\.)?[0-9]+ *, *([0-9]*\\.)?[0-9]+ *, *([0-9]*\\.)?[0-9]+ *\\)$" :: Text) = Success
isRgbColor text = Failure "is not a valid rgb() color"
{-# INLINABLE isRgbColor #-}


-- | Validates that value is a rgba() color string
--
-- >>> isRgbaColor "rgb(255, 0, 0, 1.0)"
-- Success
--
-- >>> isRgbaColor "#f00f"
-- Failure "is not a valid rgba() color"
isRgbaColor :: Text -> ValidatorResult
isRgbaColor text | text =~ ("^rgba\\( *([0-9]*\\.)?[0-9]+ *, *([0-9]*\\.)?[0-9]+ *, *([0-9]*\\.)?[0-9]+ *, *([0-9]*\\.)?[0-9]+ *\\)$" :: Text) = Success
isRgbaColor text = Failure "is not a valid rgba() color"
{-# INLINABLE isRgbaColor #-}


-- | Validates that value is a hex-based or rgb(a) color string
--
-- >>> isColor "#ffffff"
-- Success
--
-- >>> isColor "rgba(255, 0, 0, 0.5)"
-- Success
--
-- >>> isColor "rgb(0, 0, 0)"
-- Failure "is not a valid color"
isColor :: Text -> ValidatorResult
isColor = validateAny [isRgbHexColor, isRgbaHexColor, isRgbColor, isRgbaColor]
  |> withCustomErrorMessage "is not a valid color"
{-# INLINABLE isColor #-}


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
{-# INLINABLE isUrl #-}


isInList :: (Eq value, Show value) => [value] -> value -> ValidatorResult
isInList list value | list |> includes value = Success
isInList list value = Failure ("is not allowed. It needs to be one of the following: " <> (tshow list))
{-# INLINABLE isInList #-}

-- | Validates that value is True
--
-- >>> isTrue True
-- Success
--
-- >>> isTrue False
-- Failure "This field cannot be false"
isTrue :: Bool -> ValidatorResult
isTrue value = if value then Success else Failure "This field cannot be false"
{-# INLINABLE isTrue #-}


-- | Validates that value is False
--
-- >>> isFalse False
-- Success
--
-- >>> isFalse True
-- Failure "This field cannot be true"
isFalse :: Bool -> ValidatorResult
isFalse value = if not value then Success else Failure "This field cannot be true"
{-# INLINABLE isFalse #-}


-- | Validates that value is matched by the regular expression
--
-- >>> matchesRegex "^[0-9]{4}$" "2016"
-- Success
--
-- >>> matchesRegex "^[0-9]{4}$" "16"
-- Failure "This field does not match the regular expression \"^[0-9]{4}$\""
--
-- >>> matchesRegex "[0-9]{4}" "xx2016xx"
-- Success -- regex is missing ^ and $
--
matchesRegex :: Text -> Text -> ValidatorResult
matchesRegex regex text = if text =~ regex then Success else Failure $ "This field does not match the regular expression \"" <> regex <> "\""
{-# INLINABLE matchesRegex #-}


-- | Validates that value is a valid slug
--
-- >>> isSlug "i-am-a-slug"
-- Success
--
-- >>> isSlug "I-AM-A-Slug (Copy)"
-- Failure "is not a valid slug (consisting of only letters, numbers, underscores or hyphens)"
isSlug :: Text -> ValidatorResult
isSlug text | text =~ ("^[a-zA-Z0-9_-]+$" :: Text) = Success
isSlug text = Failure "is not a valid slug (consisting of only letters, numbers, underscores or hyphens)"
{-# INLINABLE isSlug #-}
