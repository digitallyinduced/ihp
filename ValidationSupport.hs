{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Foundation.ValidationSupport where
import           ClassyPrelude
import           Foundation.ModelSupport

data ValidatorIdentity a = ValidatorIdentity deriving (Show)
data NonEmpty = NonEmpty deriving (Show)

data ValidatorResult = Success | Failure Text deriving (Show, Eq)

isSuccess Success = True
isSuccess _       = False

class Validator constraint where
    type ValidatorValue constraint :: *
    validateField :: FormField modelField => constraint -> modelField -> ValidatorValue constraint -> ValidatorResult

instance Validator NonEmpty where
    type ValidatorValue NonEmpty = Text
    validateField NonEmpty field "" = Failure (formFieldName field <> " cannot be empty")
    validateField NonEmpty _ _      = Success

instance Validator (ValidatorIdentity fieldType) where
    type ValidatorValue (ValidatorIdentity fieldType) = fieldType
    validateField _ _ _ = Success


class CanValidate model where
    type ValidateModelResult model :: *
    type ModelValidator model :: *
    validate :: model -> ValidateModelResult model
    isValid :: model -> Bool

class CanValidateField model field where
    validateModelField :: (FormFieldValue field model) => model -> field -> ValidatorResult
