{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Foundation.ValidationSupport where
import           ClassyPrelude
import           Foundation.ModelSupport
import qualified Data.Text as Text
import Foundation.NameSupport (humanize)
import GHC.Records
import Data.Proxy
import GHC.TypeLits (KnownSymbol, Symbol)

data ValidatorIdentity a = ValidatorIdentity deriving (Show)

data ValidatorResult = Success | Failure Text deriving (Show, Eq)

isSuccess Success = True
isSuccess _       = False

class Validator constraint where
    type ValidatorValue constraint :: *
    validateField :: FormField modelField => constraint -> modelField -> ValidatorValue constraint -> ValidatorResult

data NonEmpty = NonEmpty deriving (Show)
instance Validator NonEmpty where
    type ValidatorValue NonEmpty = Text
    validateField NonEmpty field "" = Failure (humanize (formFieldName field) <> " cannot be empty")
    validateField NonEmpty _ _      = Success

data IsPhoneNumber = IsPhoneNumber deriving (Show)
instance Validator IsPhoneNumber where
    type ValidatorValue IsPhoneNumber = Text
    validateField IsPhoneNumber _ text | "+" `isPrefixOf` text && length text > 5 = Success
    validateField IsPhoneNumber field _ = Failure (humanize (formFieldName field) <> " is not a valid phone number (has to start with +, at least 5 characters)")

data IsInRange = IsInRange (Int, Int) deriving (Show)
instance Validator IsInRange where
    type ValidatorValue IsInRange = Int
    validateField (IsInRange (min, max)) _ value | value >= min && value <= max = Success
    validateField (IsInRange (min, max)) field _ = Failure $ (humanize (formFieldName field)) <> " has to be between " <> tshow min <> " and " <> tshow max

instance Validator (ValidatorIdentity fieldType) where
    type ValidatorValue (ValidatorIdentity fieldType) = fieldType
    validateField _ _ _ = Success


class CanValidate model where
    type ValidateModelResult model :: *
    type ModelValidator model :: *
    validate :: model -> ValidateModelResult model
    isValid :: model -> Bool

class CanValidateField model where
    type ModelFieldType model :: *
    validateModelField :: (FormFieldValue (ModelFieldType model) model) => model -> ModelFieldType model -> ValidatorResult
