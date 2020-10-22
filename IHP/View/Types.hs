{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs, UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables, IncoherentInstances  #-}

module IHP.View.Types
( FormField (..)
, SubmitButton (..)
, FormContext (..)
, InputType (..)
, ModelFormAction (..)
, CSSFramework (..)
)
where

import IHP.Prelude hiding (div)
import qualified Text.Blaze.Html5 as Blaze
import qualified IHP.Controller.Session as Session

class ModelFormAction application record where
    modelFormAction :: record -> Text

data FormField = FormField
    { fieldType :: !InputType
    , fieldName :: !Blaze.AttributeValue
    , fieldLabel :: !Text
    , fieldValue :: !Text
    , fieldInputId :: !Text
    , validatorResult :: !(Maybe Text)
    , fieldInput :: !(FormField -> Blaze.Html)
    , fieldClass :: !Text
    , labelClass :: !Text
    , disableLabel :: !Bool
    , disableGroup :: !Bool
    , disableValidationResult :: !Bool
    , cssFramework :: CSSFramework
    , helpText :: !Text
    , placeholder :: !Text
    , required :: Bool
    }

data SubmitButton = SubmitButton
    { label :: Blaze.Html
    , buttonClass :: Text
    , cssFramework :: CSSFramework
    }

data FormContext model = FormContext
    { model :: model
    , formAction :: !Text
    , cssFramework :: CSSFramework
    }

data InputType
    = TextInput
    | NumberInput
    | CheckboxInput
    | ColorInput
    | EmailInput
    | HiddenInput
    | TextareaInput
    | DateInput
    | DateTimeInput
    | PasswordInput
    | SelectInput { options :: ![(Text, Text)] }


-- | Render functions to render with bootstrap etc.
--
-- We call this functions with the cssFramework passed to have late binding (like from OOP languages)
data CSSFramework = CSSFramework
    { styledFlashMessage :: CSSFramework -> Session.FlashMessage -> Blaze.Html
    , styledFlashMessages :: CSSFramework -> [Session.FlashMessage] -> Blaze.Html
    -- | Renders the full form field calling other functions below
    , styledFormField :: CSSFramework -> FormField -> Blaze.Html
    -- | The primary form submit button
    , styledSubmitButton :: CSSFramework -> SubmitButton -> Blaze.Html
    -- | Class for the primary form submit button
    , styledSubmitButtonClass :: Text
    -- | Renders the help text below an input field. Used with @[hsx|{(textField #firstname) { helpText = "Your first name" } }|]@
    , styledFormFieldHelp :: CSSFramework -> FormField -> Blaze.Html
    -- | First class attached to @<input/>@ elements, e.g. @<input class="form-control"/>@
    , styledInputClass :: FormField -> Text
    -- | When the form validation failed, invalid inputs will have this class
    , styledInputInvalidClass :: FormField -> Text
    -- | Class applied to the div wrapping the label and input, e.g. @"form-group"@
    , styledFormGroupClass :: Text
    -- | Elements that containers the validation error message for a invalid form field
    , styledValidationResult :: CSSFramework -> FormField -> Blaze.Html
    -- | Class name for container of validation error message
    , styledValidationResultClass :: Text
    }

