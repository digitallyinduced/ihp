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

class ModelFormAction application record where
    modelFormAction :: (?context :: context, HasField "requestContext" context RequestContext) => record -> Text

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
    ) => ModelFormAction application record where
    modelFormAction record =
        let
            path = theRequest |> get #pathInfo
            action = if isNew record
                then "Create" <> getModelName @record
                else "Update" <> getModelName @record
        in
            init path
                |> (\path -> path <> [action])
                |> intercalate "/"


formFor :: forall record viewContext parent id application. (
    ?context :: viewContext
    , Eq record
    , Typeable record
    , ModelFormAction application record
    , HasField "id" record id
    , application ~ ViewApp viewContext
    , HasField "meta" record MetaBag
    , Default id
    , Eq id
    , ConfigProvider viewContext
    , HasField "requestContext" viewContext RequestContext
    ) => record -> ((?context :: viewContext, ?formContext :: FormContext record) => Html5.Html) -> Html5.Html
formFor record = buildForm (createFormContext record) { formAction = modelFormAction @application record }
{-# INLINE formFor #-}


formFor' :: forall record viewContext parent id application. (
    ?context :: viewContext
    , Eq record
    , Typeable record
    , HasField "id" record id
    , application ~ ViewApp viewContext
    , HasField "meta" record MetaBag
    , Default id
    , Eq id
    , ConfigProvider viewContext
    ) => record -> Text -> ((?context :: viewContext, ?formContext :: FormContext record) => Html5.Html) -> Html5.Html
formFor' record action = buildForm (createFormContext record) { formAction = action }
{-# INLINE formFor' #-}

horizontalFormFor :: forall record viewContext parent id application. (
        ?context :: viewContext
        , Eq record
        , Typeable record
        , ModelFormAction application record
        , HasField "id" record id
        , application ~ ViewApp viewContext
        , HasField "meta" record MetaBag
        , Default id
        , Eq id
        , ConfigProvider viewContext
        ) => record -> ((?viewContext :: viewContext, ?formContext :: FormContext record) => Html5.Html) -> Html5.Html
horizontalFormFor record = undefined
-- 
--  buildForm (createFormContext record)
--         { renderFormField = renderHorizontalBootstrapFormField
--         , renderSubmit = renderHorizontalBootstrapSubmitButton
--         }
-- {-# INLINE horizontalFormFor #-}


createFormContext :: forall record viewContext parent id application. (
        ?context :: viewContext
        , Eq record
        , Typeable record
        , HasField "id" record id
        , application ~ ViewApp viewContext
        , HasField "meta" record MetaBag
        , ConfigProvider viewContext
        ) => record -> FormContext record
createFormContext record =
    FormContext
        { model = record
        , formAction = ""
        , cssFramework = theCSSFramework
        }
{-# INLINE createFormContext #-}

{-# INLINE buildForm #-}
buildForm :: forall model viewContext parent id. (?context :: viewContext, HasField "id" model id, Default id, Eq id) => FormContext model -> ((?context :: viewContext, ?formContext :: FormContext model) => Html5.Html) -> Html5.Html
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



-- {-# INLINE renderHorizontalBootstrapFormField #-}
-- renderHorizontalBootstrapFormField :: FormField -> Html5.Html
-- renderHorizontalBootstrapFormField formField@(FormField { fieldType }) =
--         case fieldType of
--             TextInput -> renderTextField "text" formField
--             NumberInput -> renderTextField "number" formField
--             PasswordInput -> renderTextField "password" formField
--             ColorInput -> renderTextField "color" formField
--             EmailInput -> renderTextField "email" formField
--             DateInput -> renderTextField "date" formField
--             DateTimeInput -> renderTextField "datetime-local" formField
--             CheckboxInput -> renderCheckboxFormField formField
--             HiddenInput -> renderTextField "hidden" formField { disableLabel = True, disableGroup = True, disableValidationResult = True }
--             TextareaInput -> renderTextField "text" formField
--             SelectInput {} -> renderSelectField formField
--     where
--         renderCheckboxFormField :: FormField -> Html5.Html
--         renderCheckboxFormField formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, validatorResult, fieldClass, disableLabel, disableValidationResult, fieldInput, required }) = div ! class_ "form-group" $ div ! class_ "form-check" $ do
--             (if disableLabel then div else H.label ! class_ "form-check-label") $ do
--                 let theInput = input
--                         ! A.type_ "checkbox"
--                         ! A.name fieldName
--                         ! A.class_ (cs $ classes ["form-check-input", ("is-invalid", isJust validatorResult), (fieldClass, not (null fieldClass))])
--                         !? (required, A.required "required")
--                         !? (fieldValue == "yes", A.checked "checked")
--                 theInput
--                 input ! type_ "hidden" ! name fieldName ! A.value (cs $ inputValue False)
--                 Html5.text fieldLabel
--                 unless disableValidationResult (renderValidationResult formField)
--         renderTextField :: Html5.AttributeValue -> FormField -> Html5.Html
--         renderTextField inputType formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disableLabel, disableValidationResult, fieldInput, labelClass, placeholder, required }) = if disableLabel then renderInner else div ! A.class_ "form-group row" $ renderInner
--             where
--                 renderInner = do
--                     unless (disableLabel || null fieldLabel) [hsx|<label class={classes ["col-sm-4 col-form-label ", (labelClass, True)]} for={fieldInputId}>{fieldLabel}</label>|]
--                     div ! class_ "col-sm-8" $ do
--                         (fieldInput formField)
--                                 ! A.type_ inputType
--                                 ! A.name fieldName
--                                 ! A.placeholder (cs placeholder)
--                                 ! A.id (cs fieldInputId)
--                                 ! A.class_ (cs $ classes ["form-control", ("is-invalid", isJust validatorResult), (fieldClass, not (null fieldClass))])
--                                 ! A.value (cs fieldValue)
--                                 !? (required, A.required "required")
--                         if disableValidationResult then mempty else renderValidationResult formField
--                         renderHelpText formField
--         renderSelectField :: FormField -> Html5.Html
--         renderSelectField formField@(FormField {fieldType, placeholder, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disableLabel, disableValidationResult, labelClass, required }) = if disableLabel then renderInner else div ! A.class_ "form-group row" $ renderInner
--             where
--                 renderInner = do
--                     if disableLabel || fieldLabel == "" then pure () else H.label ! A.class_ (cs $ "col-sm-4 col-form-label " <> labelClass) ! A.for (cs fieldInputId) $ cs fieldLabel
--                     div ! class_ "col-sm-8" $ do
--                         Html5.select ! name fieldName ! A.id (cs fieldInputId) ! A.class_ (cs $ classes ["form-control", ("is-invalid", isJust validatorResult), (fieldClass, not (null fieldClass))]) ! value (cs fieldValue) !? (required, A.required "required") $ do
--                             --Html5.option ! A.disabled "disabled" ! A.selected "selected" $ Html5.text ("Bitte auswählen" :: Text)
--                             let isValueSelected = isJust $ find (\(optionLabel, optionValue) -> optionValue == fieldValue) (options fieldType)
--                             (if isValueSelected then Html5.option else Html5.option ! A.selected "selected") ! A.disabled "disabled" $ Html5.text (if null placeholder then "Please select" else placeholder)
--                             forEach (options fieldType) $ \(optionLabel, optionValue) -> (let option = Html5.option ! A.value (cs optionValue) in (if optionValue == fieldValue then option ! A.selected "selected" else option) $ cs optionLabel)
--                         unless disableValidationResult (renderValidationResult formField)
--                         renderHelpText formField
-- 
-- {-# INLINE renderHorizontalBootstrapSubmitButton #-}
-- renderHorizontalBootstrapSubmitButton SubmitButton { modelIsNew, modelName } = div ! class_ "form-group row" $ do
--     div ! class_ "offset-sm-5 col-sm-3 text-left" $ do
--         button ! class_ "btn btn-primary btn-lg pl-4 pr-4 w-100" $ (if modelIsNew then "Create " else "Save ") <> (cs $ modelName)

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
