{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs, UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables, IncoherentInstances  #-}

module IHP.View.Form where

import IHP.Prelude hiding (div)
import           Data.String.Conversions            (cs)
import           IHP.ValidationSupport
import           IHP.View.ConvertibleStrings ()
import           IHP.ViewErrorMessages
import           IHP.ViewSupport
import           Text.Blaze.Html5                   (a, body, button, code, div, docTypeHtml, footer, form, h1, h2, h3, h4, h5, h6, head, hr, html, iframe, img,
                                                     input, label, li, link, meta, nav, ol, p, pre, script, small, span, table, tbody, td, th, thead, title, tr,
                                                     ul, (!), (!?))
import qualified Text.Blaze.Html5                   as H
import qualified Text.Blaze.Html5                   as Html5
import           Text.Blaze.Html5.Attributes        (autocomplete, autofocus, charset, class_, content, href, httpEquiv, id, lang, method, name,
                                                     onclick, placeholder, rel, src, style, type_, value)
import qualified Text.Blaze.Html5.Attributes        as A

import IHP.HtmlSupport.ToHtml
import qualified IHP.NameSupport
import GHC.Types
import qualified Text.Inflections
import qualified Data.Text as Text
import Data.Maybe (fromJust)
import IHP.Controller.RequestContext
import IHP.RouterSupport hiding (get)
import IHP.ModelSupport (getModelName, inputValue, isNew, GetModelName, Id', NormalizeModel, MetaBag, InputValue)
import IHP.HtmlSupport.QQ (hsx)
import IHP.View.Types
import IHP.View.Classes 
import IHP.FrameworkConfig (ConfigProvider)
import qualified Network.Wai as Wai
import IHP.Controller.RequestContext
import IHP.Controller.Context

class ModelFormAction application record where
    modelFormAction :: (?context :: ControllerContext) => record -> Text

-- | Returns the form's action attribute for a given record.
--
-- Expects that AutoRoute is used. Otherwise you need to use @formFor'@ or specify
-- a manual ModelFormAction instance.
--
-- We guess the form submitt action based on the current url
-- It's a @New..Action@ or @Edit..Action@. We guess the corresponding
-- @Create..Action@ name or @Update..Action@ name based on the AutoRoute rules
--
-- In case the routing is not based on AutoRoute, a manual ModelFormAction instance needs
-- to be defined
instance (
    HasField "id" record id
    , Eq id
    , Default id
    , KnownSymbol (GetModelName record)
    , Show id
    ) => ModelFormAction application record where
    modelFormAction record =
        let
            path = theRequest |> get #pathInfo
            action = if isNew record
                then "Create" <> getModelName @record
                else "Update" <> getModelName @record <> "?" <> lcfirst (getModelName @record) <> "Id=" <> tshow (get #id record)
        in
            init path
                |> (\path -> [""] <> (fromMaybe [] path) <> [action])
                |> intercalate "/"


formFor :: forall record parent id application. (
    ?context :: ControllerContext
    , Eq record
    , Typeable record
    , ModelFormAction application record
    , HasField "id" record id
    , HasField "meta" record MetaBag
    , Default id
    , Eq id
    ) => record -> ((?context :: ControllerContext, ?formContext :: FormContext record) => Html5.Html) -> Html5.Html
formFor record = buildForm (createFormContext record) { formAction = modelFormAction @application record }
{-# INLINE formFor #-}


formFor' :: forall record parent id application. (
    ?context :: ControllerContext
    , Eq record
    , Typeable record
    , HasField "id" record id
    , HasField "meta" record MetaBag
    , Default id
    , Eq id
    ) => record -> Text -> ((?context :: ControllerContext, ?formContext :: FormContext record) => Html5.Html) -> Html5.Html
formFor' record action = buildForm (createFormContext record) { formAction = action }
{-# INLINE formFor' #-}

createFormContext :: forall record viewContext parent id application. (
        ?context :: ControllerContext
        , Eq record
        , Typeable record
        , HasField "id" record id
        , HasField "meta" record MetaBag
        ) => record -> FormContext record
createFormContext record =
    FormContext
        { model = record
        , formAction = ""
        , cssFramework = theCSSFramework
        }
{-# INLINE createFormContext #-}

{-# INLINE buildForm #-}
buildForm :: forall model  parent id. (?context :: ControllerContext, HasField "id" model id, Default id, Eq id) => FormContext model -> ((?context :: ControllerContext, ?formContext :: FormContext model) => Html5.Html) -> Html5.Html
buildForm formContext inner =
    let
        theModel = model formContext
        action = formAction formContext
        isNewRecord = IHP.ModelSupport.isNew theModel
        formId = if isNewRecord then "" else formAction formContext
        formClass :: Text = if isNewRecord then "new-form" else "edit-form"
        formInner = let ?formContext = formContext in inner
    in
        [hsx|<form method="POST" action={action} id={formId} class={formClass}>{formInner}</form>|]


{-# INLINE submitButton #-}
submitButton :: forall model id. (?formContext :: FormContext model, HasField "id" model id, KnownSymbol (GetModelName model), Eq id, Default id) => SubmitButton
submitButton =
    let
        modelName = IHP.ModelSupport.getModelName @model
        isNew = IHP.ModelSupport.isNew (model ?formContext)
    in SubmitButton
    { label = cs $ (if isNew then "Create " else "Save ") <> modelName
    , buttonClass = mempty
    , cssFramework = get #cssFramework ?formContext
    }

{-# INLINE fieldNameToFieldLabel #-}
fieldNameToFieldLabel :: Text -> Text
fieldNameToFieldLabel fieldName = cs (let (Right parts) = Text.Inflections.parseCamelCase [] fieldName in Text.Inflections.titleize parts)

{-# INLINE columnNameToFieldLabel #-}
columnNameToFieldLabel :: Text -> Text
columnNameToFieldLabel columnName = cs (let (Right parts) = Text.Inflections.parseSnakeCase [] columnName in Text.Inflections.titleize parts)

{-# INLINE removeIdSuffix #-}
removeIdSuffix :: Text -> Text
removeIdSuffix text = fromMaybe text (Text.stripSuffix " Id" text)

textField :: forall fieldName model value.
    ( ?formContext :: FormContext model
    , HasField fieldName model value
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , InputValue value
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
textField field = FormField
        { fieldType = TextInput
        , fieldName = cs fieldName
        , fieldLabel = fieldNameToFieldLabel (cs fieldName)
        , fieldValue =  inputValue ((getField @fieldName model) :: value)
        , fieldInputId = cs (IHP.NameSupport.lcfirst (getModelName @model) <> "_" <> cs fieldName)
        , validatorResult = getValidationFailure field model
        , fieldClass = ""
        , labelClass = ""
        , disableLabel = False
        , disableGroup = False
        , disableValidationResult = False
        , fieldInput = const Html5.input
        , cssFramework = get #cssFramework ?formContext
        , helpText = ""
        , placeholder = ""
        , required = False
        , autofocus = False
        }
    where
        fieldName = symbolVal field
        FormContext { model } = ?formContext
{-# INLINE textField #-}

numberField :: forall fieldName model value.
    ( ?formContext :: FormContext model
    , HasField fieldName model value
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , InputValue value
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
numberField field = (textField field) { fieldType = NumberInput }
{-# INLINE numberField #-}

textareaField :: forall fieldName model value.
    ( ?formContext :: FormContext model
    , HasField fieldName model value
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , InputValue value
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
textareaField field = (textField field) { fieldType = TextareaInput, fieldInput = \formField -> Html5.textarea (cs (fieldValue formField)) }
{-# INLINE textareaField #-}

colorField :: forall fieldName model value.
    ( ?formContext :: FormContext model
    , HasField fieldName model value
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , InputValue value
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
colorField field = (textField field) { fieldType = ColorInput }
{-# INLINE colorField #-}

emailField :: forall fieldName model.
    ( ?formContext :: FormContext model
    , HasField fieldName model Text
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
emailField field = (textField field) { fieldType = EmailInput }
{-# INLINE emailField #-}

dateField :: forall fieldName model value.
    ( ?formContext :: FormContext model
    , HasField fieldName model value
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , InputValue value
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
dateField field = (textField field) { fieldType = DateInput }
{-# INLINE dateField #-}

passwordField :: forall fieldName model.
    ( ?formContext :: FormContext model
    , HasField fieldName model Text
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
passwordField field = (textField field) { fieldType = PasswordInput }
{-# INLINE passwordField #-}


dateTimeField :: forall fieldName model value.
    ( ?formContext :: FormContext model
    , HasField fieldName model value
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , InputValue value
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
dateTimeField alpha = (textField alpha) { fieldType = DateTimeInput }
{-# INLINE dateTimeField #-}


hiddenField :: forall fieldName model value.
    ( ?formContext :: FormContext model
    , HasField fieldName model value
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , InputValue value
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
hiddenField field = (textField field) { fieldType = HiddenInput }
{-# INLINE hiddenField #-}


checkboxField :: forall fieldName model.
    ( ?formContext :: FormContext model
    , HasField fieldName model Bool
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
checkboxField field = FormField
        { fieldType = CheckboxInput
        , fieldName = cs fieldName
        , fieldLabel = fieldNameToFieldLabel (cs fieldName)
        , fieldValue =  if getField @fieldName model then "yes" else "no"
        , fieldInputId = cs (IHP.NameSupport.lcfirst (getModelName @model) <> "_" <> cs fieldName)
        , validatorResult = getValidationFailure field model
        , fieldClass = ""
        , labelClass = ""
        , disableLabel = False
        , disableGroup = False
        , disableValidationResult = False
        , fieldInput = const Html5.input
        , cssFramework = get #cssFramework ?formContext
        , helpText = ""
        , placeholder = ""
        , required = False
        , autofocus = False
        }
    where
        fieldName = symbolVal field
        FormContext { model } = ?formContext
{-# INLINE checkboxField #-}


selectField :: forall fieldName model item.
    ( ?formContext :: FormContext model
    , HasField fieldName model (SelectValue item)
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , KnownSymbol (GetModelName model)
    , CanSelect item
    , InputValue (SelectValue item)
    ) => Proxy fieldName -> [item] -> FormField
selectField field items = FormField
        { fieldType =
            let
                itemToTuple :: item -> (Text, Text)
                itemToTuple item = (selectLabel item, inputValue (selectValue item))
            in
                 SelectInput (map itemToTuple items)
        , fieldName = cs fieldName
        , fieldLabel = removeIdSuffix $ fieldNameToFieldLabel (cs fieldName)
        , fieldValue = inputValue ((getField @fieldName model :: SelectValue item))
        , fieldInputId = cs (IHP.NameSupport.lcfirst (getModelName @model) <> "_" <> cs fieldName)
        , validatorResult = getValidationFailure field model
        , fieldClass = ""
        , labelClass = ""
        , disableLabel = False
        , disableGroup = False
        , disableValidationResult = False
        , fieldInput = const (Html5.select mempty)
        , cssFramework = get #cssFramework ?formContext
        , helpText = ""
        , placeholder = "Please select"
        , required = False
        , autofocus = False
    }
    where
        fieldName = symbolVal field
        FormContext { model } = ?formContext
{-# INLINE selectField #-}

class CanSelect model where
    type SelectValue model :: GHC.Types.Type
    selectLabel :: model -> Text
    default selectLabel :: Show model => model -> Text
    selectLabel = tshow
    selectValue :: model -> SelectValue model
    default selectValue :: HasField "id" model (SelectValue model) => model -> SelectValue model
    selectValue = getField @"id"

instance ToHtml FormField where
    {-# INLINE toHtml #-}
    toHtml ::  FormField -> Html5.Html
    toHtml formField@(FormField { cssFramework }) = styledFormField cssFramework cssFramework formField

instance ToHtml SubmitButton where
    {-# INLINE toHtml #-}
    toHtml submitButton@(SubmitButton { cssFramework }) = styledSubmitButton cssFramework cssFramework submitButton
