{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances, StandaloneDeriving,InstanceSigs, UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables, IncoherentInstances  #-}

module Foundation.View.Form where

import           ClassyPrelude                      hiding (div)
import           Data.String.Conversions            (cs)
import           Foundation.HaskellSupport
import qualified Foundation.ModelSupport
import Foundation.ModelSupport (ModelFieldType)
import           Foundation.ValidationSupport
import           Foundation.View.ConvertibleStrings ()
import           Foundation.ViewErrorMessages
import           Foundation.ViewSupport
import           Network.HTTP.Types.Method          (methodPost)
import           Network.Wai                        (requestMethod)
import qualified Network.Wai
import           Text.Blaze                         (Attribute, dataAttribute, preEscapedText, stringValue, text)
import           Text.Blaze.Html5                   (a, body, button, code, div, docTypeHtml, footer, form, h1, h2, h3, h4, h5, h6, head, hr, html, iframe, img,
                                                     input, label, li, link, meta, nav, ol, p, pre, script, small, span, table, tbody, td, th, thead, title, tr,
                                                     ul)
import           Text.Blaze.Html5                   ((!))
import qualified Text.Blaze.Html5                   as Html5
import           Text.Blaze.Html5.Attributes        (action, autocomplete, autofocus, charset, class_, content, href, httpEquiv, id, lang, method, name,
                                                     onclick, placeholder, rel, src, style, type_, value)
import qualified Text.Blaze.Html5.Attributes        as A

import           UrlGenerator
import qualified View.Context
import qualified Text.Blaze.Internal
import Foundation.HtmlSupport.ToHtml
import qualified Foundation.Controller.Session
import qualified Foundation.NameSupport
import GHC.OverloadedLabels
import GHC.Types
import GHC.TypeLits
import Data.Proxy
import qualified Data.Text
import qualified Text.Inflections
import GHC.Records
import qualified Data.Text as Text

data FormField = FormField {
        fieldType :: !InputType,
        fieldName :: Html5.AttributeValue,
        fieldLabel :: Text,
        fieldValue :: Text,
        fieldInputId :: Text,
        validatorResult :: ValidatorResult,
        fieldInput :: Html5.Html,
        fieldClass :: Html5.AttributeValue,
        labelClass :: Html5.AttributeValue,
        disableLabel :: Bool,
        disableGroup :: Bool,
        disableValidationResult :: Bool,
        modelIsNew :: Bool,
        formIsSubmitted :: Bool,
        renderFormField :: FormField -> Html5.Html,
        helpText :: Text,
        placeholder :: Text
    }

data SubmitButton = SubmitButton { modelIsNew :: Bool, modelName :: Text, renderSubmit :: SubmitButton -> Html5.Html }

data FormContext model = FormContext { model :: model, renderFormField :: FormField -> Html5.Html, renderSubmit :: SubmitButton -> Html5.Html }

formFor :: (?viewContext :: ViewContext) => (Foundation.ModelSupport.IsNew model, Foundation.ModelSupport.HasModelName model) => model -> Text -> ((?viewContext :: ViewContext, ?formContext :: FormContext model) => Html5.Html) -> Html5.Html
formFor model = formFor' (FormContext { model, renderFormField = renderBootstrapFormField, renderSubmit = renderBootstrapSubmitButton })

horizontalFormFor :: (?viewContext :: ViewContext) => (Foundation.ModelSupport.IsNew model, Foundation.ModelSupport.HasModelName model) => model -> Text -> ((?viewContext :: ViewContext, ?formContext :: FormContext model) => Html5.Html) -> Html5.Html
horizontalFormFor model = formFor' (FormContext { model, renderFormField = renderHorizontalBootstrapFormField, renderSubmit = renderHorizontalBootstrapSubmitButton })

formFor' :: (?viewContext :: ViewContext) => (Foundation.ModelSupport.IsNew model, Foundation.ModelSupport.HasModelName model) => FormContext model -> Text -> ((?viewContext :: ViewContext, ?formContext :: FormContext model) => Html5.Html) -> Html5.Html
formFor' formContext url inner = form ! method "POST" ! action (cs url) $ do
    let ?formContext = formContext in inner


submitButton :: (?formContext :: FormContext model, Foundation.ModelSupport.IsNew model, Foundation.ModelSupport.HasModelName model) => SubmitButton
submitButton = SubmitButton { modelIsNew = Foundation.ModelSupport.isNew (model ?formContext), modelName = Foundation.ModelSupport.getModelName (model ?formContext), renderSubmit = let FormContext { renderSubmit } = ?formContext in renderSubmit }

data InputType = TextInput | CheckboxInput | ColorInput | HiddenInput | TextareaInput | SelectInput { options :: [(Text, Text)] }

renderHelpText (FormField { helpText }) =
    case helpText of
        "" -> mempty
        helpText -> small ! A.class_ "form-text text-muted" $ text helpText

renderValidationResult (FormField { modelIsNew, validatorResult }) = when modelIsNew $ case validatorResult of
                Success         -> return ()
                Failure message -> div ! class_ "invalid-feedback" $ cs message

isInvalid (FormField { modelIsNew, formIsSubmitted, validatorResult }) =
        if formIsSubmitted
            then case validatorResult of
                    Success         -> False
                    Failure _       -> True
            else False

renderBootstrapFormField :: FormField -> Html5.Html
renderBootstrapFormField formField@(FormField { fieldType }) =
        case fieldType of
            TextInput -> renderTextField "text" formField
            ColorInput -> renderTextField "color" formField
            CheckboxInput -> renderCheckboxFormField formField
            HiddenInput -> renderTextField "hidden" formField { disableLabel = True, disableGroup = True, disableValidationResult = True }
            TextareaInput -> renderTextField "text" formField { fieldInput = Html5.textarea (cs $ fieldValue formField) }
            SelectInput {} -> renderSelectField formField
    where
        maybeWithFormGroup (FormField { fieldInputId, disableGroup }) renderInner = if disableGroup then renderInner else div ! A.class_ "form-group" ! A.id (cs $ "form-group-" <> fieldInputId) $ renderInner
        renderCheckboxFormField :: FormField -> Html5.Html
        renderCheckboxFormField formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, validatorResult, fieldClass, disableLabel, disableValidationResult, fieldInput, modelIsNew, formIsSubmitted, labelClass }) = div ! class_ "form-group" $ div ! class_ "form-check" $ do
            (if disableLabel then div else label ! class_ "form-check-label") $ do
                let theInput = input ! type_ "checkbox" ! name fieldName ! class_ ("form-check-input " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass)
                if fieldValue == "yes" then theInput ! A.checked "checked" else theInput
                Html5.text fieldLabel
                if disableValidationResult then mempty else renderValidationResult formField
        renderTextField :: Html5.AttributeValue -> FormField -> Html5.Html
        renderTextField inputType formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disableLabel, disableValidationResult, fieldInput, modelIsNew, formIsSubmitted, labelClass, placeholder }) =
            maybeWithFormGroup formField $ do
                if disableLabel || fieldLabel == "" then return () else label ! A.class_ labelClass ! A.for (cs fieldInputId) $ cs fieldLabel
                let theInput = fieldInput ! type_ inputType ! name fieldName ! A.placeholder (cs placeholder) ! A.id (cs fieldInputId) ! class_ ("form-control " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass)
                if fieldValue == "" then theInput else theInput ! value (cs fieldValue)
                if disableValidationResult then mempty else renderValidationResult formField
                renderHelpText formField
        renderSelectField :: FormField -> Html5.Html
        renderSelectField formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disableLabel, disableValidationResult, fieldInput, modelIsNew, formIsSubmitted, labelClass }) =
            maybeWithFormGroup formField $ do
                if disableLabel then return () else label ! A.class_ labelClass ! A.for (cs fieldInputId) $ cs fieldLabel
                Html5.select ! name fieldName ! A.id (cs fieldInputId) ! class_ ("form-control " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass) ! value (cs fieldValue) $ do
                    --Html5.option ! A.disabled "disabled" ! A.selected "selected" $ Html5.text ("Bitte auswählen" :: Text)
                    let isValueSelected = isJust $ find (\(optionLabel, optionValue) -> optionValue == fieldValue) (options fieldType)
                    (if isValueSelected then Html5.option else Html5.option ! A.selected "selected") $ Html5.text ("Kein Team")
                    forM_ (options fieldType) $ \(optionLabel, optionValue) -> (let option = Html5.option ! A.value (cs optionValue) in (if optionValue == fieldValue then option ! A.selected "selected" else option) $ cs optionLabel)
                renderHelpText formField
                if disableValidationResult then mempty else renderValidationResult formField

renderBootstrapSubmitButton SubmitButton { modelIsNew, modelName }= button ! class_ "btn btn-primary" $ (if modelIsNew then "Create " else "Save ") <> (cs $ modelName)


renderHorizontalBootstrapFormField :: FormField -> Html5.Html
renderHorizontalBootstrapFormField formField@(FormField { fieldType }) =
        case fieldType of
            TextInput -> renderTextField "text" formField
            ColorInput -> renderTextField "color" formField
            CheckboxInput -> renderCheckboxFormField formField
            HiddenInput -> renderTextField "hidden" formField { disableLabel = True, disableGroup = True, disableValidationResult = True }
            TextareaInput -> renderTextField "text" formField { fieldInput = Html5.textarea (cs $ fieldValue formField) }
            SelectInput {} -> renderSelectField formField
    where
        renderCheckboxFormField :: FormField -> Html5.Html
        renderCheckboxFormField formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, validatorResult, fieldClass, disableLabel, disableValidationResult, fieldInput, modelIsNew, formIsSubmitted }) = div ! class_ "form-group" $ div ! class_ "form-check" $ do
            (if disableLabel then div else label ! class_ "form-check-label") $ do
                let theInput = input ! type_ "checkbox" ! name fieldName ! class_ ("form-check-input " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass)
                if fieldValue == "yes" then theInput ! A.checked "checked" else theInput
                Html5.text fieldLabel
                if disableValidationResult then mempty else renderValidationResult formField
        renderTextField :: Html5.AttributeValue -> FormField -> Html5.Html
        renderTextField inputType formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disableLabel, disableValidationResult, fieldInput, modelIsNew, formIsSubmitted, labelClass, placeholder }) = if disableLabel then renderInner else div ! A.class_ "form-group row" $ renderInner
            where
                renderInner = do
                    if disableLabel || fieldLabel == "" then return () else label ! A.class_ ("col-sm-2 col-form-label " <> labelClass) ! A.for (cs fieldInputId) $ cs fieldLabel
                    div ! class_ "col-sm-6" $ do
                        fieldInput ! type_ inputType ! name fieldName ! A.placeholder (cs placeholder) ! A.id (cs fieldInputId) ! class_ ("form-control " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass) ! value (cs fieldValue)
                        if disableValidationResult then mempty else renderValidationResult formField
                        renderHelpText formField
        renderSelectField :: FormField -> Html5.Html
        renderSelectField formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disableLabel, disableValidationResult, fieldInput, modelIsNew, formIsSubmitted, labelClass }) = if disableLabel then renderInner else div ! A.class_ "form-group row" $ renderInner
            where
                renderInner = do
                    if disableLabel || fieldLabel == "" then return () else label ! A.class_ ("col-sm-2 col-form-label " <> labelClass) ! A.for (cs fieldInputId) $ cs fieldLabel
                    div ! class_ "col-sm-6" $ do
                        Html5.select ! name fieldName ! A.id (cs fieldInputId) ! class_ ("form-control " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass) ! value (cs fieldValue) $ do
                            --Html5.option ! A.disabled "disabled" ! A.selected "selected" $ Html5.text ("Bitte auswählen" :: Text)
                            let isValueSelected = isJust $ find (\(optionLabel, optionValue) -> optionValue == fieldValue) (options fieldType)
                            (if isValueSelected then Html5.option else Html5.option ! A.selected "selected") $ Html5.text ("Kein Team")
                            forM_ (options fieldType) $ \(optionLabel, optionValue) -> (let option = Html5.option ! A.value (cs optionValue) in (if optionValue == fieldValue then option ! A.selected "selected" else option) $ cs optionLabel)
                        if disableValidationResult then mempty else renderValidationResult formField
                        renderHelpText formField

renderHorizontalBootstrapSubmitButton SubmitButton { modelIsNew, modelName }= div ! class_ "form-group row" $ do
    div ! class_ "offset-sm-5 col-sm-3 text-left" $ do
        button ! class_ "btn btn-primary btn-lg pl-4 pr-4 w-100" $ (if modelIsNew then "Create " else "Save ") <> (cs $ modelName)




instance (KnownSymbol symbol, Foundation.ModelSupport.IsNew model, Foundation.ModelSupport.HasModelName model, HasField symbol model value, IsLabel symbol (ModelFieldType model), CanValidateField model, Foundation.ModelSupport.FormFieldValue (ModelFieldType model) model, Foundation.ModelSupport.InputValue value ) => IsLabel symbol ((FormContext model, ViewContext, Proxy value) -> FormField) where
    fromLabel = \(formContext, viewContext, _) -> let fieldName = cs (symbolVal @symbol Proxy) in FormField {
                        fieldType = TextInput,
                        fieldName = cs (Foundation.NameSupport.fieldNameToColumnName fieldName),
                        fieldLabel = fieldNameToFieldLabel fieldName,
                        fieldValue =  let value :: value = getField @(symbol) (model formContext) in Foundation.ModelSupport.inputValue value,
                        fieldInputId = cs (Foundation.NameSupport.lcfirst (Foundation.ModelSupport.getModelName (model formContext)) <> "_" <> Foundation.NameSupport.fieldNameToColumnName fieldName),
                        validatorResult = (validateModelField (model formContext) :: ModelFieldType model -> ValidatorResult) (fromLabel @symbol),
                        fieldClass = "",
                        labelClass = "",
                        disableLabel = False,
                        disableGroup = False,
                        disableValidationResult = False,
                        fieldInput = input,
                        modelIsNew = Foundation.ModelSupport.isNew (model formContext),
                        formIsSubmitted = let ?viewContext = viewContext in isSubmitted,
                        renderFormField = let FormContext { renderFormField } = formContext in renderFormField,
                        helpText = "",
                        placeholder = ""
                    }

instance (KnownSymbol symbol, Foundation.ModelSupport.IsNew model, Foundation.ModelSupport.HasModelName model, HasField symbol model Bool) => IsLabel symbol ((FormContext model, ViewContext, Proxy Bool) -> FormField) where
    fromLabel = \(formContext, viewContext, _) -> let fieldName = cs (symbolVal @symbol Proxy) in FormField {
                        fieldType = CheckboxInput,
                        fieldName = cs (Foundation.NameSupport.fieldNameToColumnName fieldName),
                        fieldLabel = fieldNameToFieldLabel fieldName,
                        fieldValue =  let value = getField @(symbol) (model formContext) in if value then "yes" else "no",
                        fieldInputId = cs (Foundation.NameSupport.lcfirst (Foundation.ModelSupport.getModelName (model formContext)) <> "_" <> Foundation.NameSupport.fieldNameToColumnName fieldName),
                        validatorResult = Success,
                        fieldClass = "",
                        labelClass = "",
                        disableLabel = False,
                        disableGroup = False,
                        disableValidationResult = False,
                        fieldInput = input,
                        modelIsNew = Foundation.ModelSupport.isNew (model formContext),
                        formIsSubmitted = let ?viewContext = viewContext in isSubmitted,
                        renderFormField = let FormContext { renderFormField } = formContext in renderFormField,
                        helpText = "",
                        placeholder = ""
                    }

instance (KnownSymbol symbol, Foundation.ModelSupport.IsNew model, Foundation.ModelSupport.HasModelName model, HasField symbol model ((SelectValue item)), CanSelect item, Foundation.ModelSupport.InputValue (SelectValue item)) => IsLabel symbol ((FormContext model, ViewContext, [item], Proxy value) -> FormField) where
    fromLabel = \(formContext, viewContext, items, _) -> let fieldName = cs (symbolVal @symbol Proxy) in FormField {
                        fieldType =
                            let
                                itemToTuple :: item -> (Text, Text)
                                itemToTuple item = (selectLabel item, Foundation.ModelSupport.inputValue (selectValue item))
                            in
                                 SelectInput $ map itemToTuple items
                            ,
                        fieldName = cs (Foundation.NameSupport.fieldNameToColumnName fieldName),
                        fieldLabel = removeIdSuffix $ fieldNameToFieldLabel fieldName,
                        fieldValue =
                            let value = ((getField @(symbol) (model formContext)) :: (SelectValue item))
                            in Foundation.ModelSupport.inputValue value,
                        fieldInputId = cs (Foundation.NameSupport.lcfirst (Foundation.ModelSupport.getModelName (model formContext)) <> "_" <> Foundation.NameSupport.fieldNameToColumnName fieldName),
                        validatorResult = Success,
                        fieldClass = "",
                        labelClass = "",
                        disableLabel = False,
                        disableGroup = False,
                        disableValidationResult = False,
                        fieldInput = Html5.select mempty,
                        modelIsNew = Foundation.ModelSupport.isNew (model formContext),
                        formIsSubmitted = let ?viewContext = viewContext in isSubmitted,
                        renderFormField = let FormContext { renderFormField } = formContext in renderFormField,
                        helpText = "",
                        placeholder = ""
                    }

fieldNameToFieldLabel :: Text -> Text
fieldNameToFieldLabel fieldName = cs (let (Right parts) = Text.Inflections.parseCamelCase [] fieldName in Text.Inflections.titleize parts)

removeIdSuffix :: Text -> Text
removeIdSuffix text = fromMaybe text (Text.stripSuffix " Id" text)

textField :: forall alpha attributeName model value. (?formContext :: FormContext model, ?viewContext :: ViewContext) => (alpha ~ ((FormContext model, ViewContext, Proxy value) -> FormField)) => alpha -> FormField
textField alpha = alpha (?formContext, ?viewContext, Proxy :: Proxy value)

textareaField :: forall alpha attributeName model value. (?formContext :: FormContext model, ?viewContext :: ViewContext) => (alpha ~ ((FormContext model, ViewContext, Proxy value) -> FormField)) => alpha -> FormField
textareaField alpha = (textField alpha) { fieldType = TextareaInput }

colorField :: forall alpha attributeName model. (?formContext :: FormContext model, ?viewContext :: ViewContext) => (alpha ~ ((FormContext model, ViewContext, Proxy Text) -> FormField)) => alpha -> FormField
colorField alpha = (textField alpha) { fieldType = ColorInput }

hiddenField :: forall alpha attributeName model value. (?formContext :: FormContext model, ?viewContext :: ViewContext) => (alpha ~ ((FormContext model, ViewContext, Proxy value) -> FormField)) => alpha -> FormField
hiddenField alpha = (textField alpha) { fieldType = HiddenInput }

checkboxField :: forall alpha attributeName model. (?formContext :: FormContext model, ?viewContext :: ViewContext) => (alpha ~ ((FormContext model, ViewContext, Proxy Bool) -> FormField)) => alpha -> FormField
checkboxField alpha = alpha (?formContext, ?viewContext, Proxy :: Proxy Bool)

selectField :: forall alpha attributeName model value item. (?formContext :: FormContext model, ?viewContext :: ViewContext) => (alpha ~ ((FormContext model, ViewContext, [item], Proxy value) -> FormField), CanSelect item) => alpha -> [item] -> FormField
selectField alpha items = alpha (?formContext, ?viewContext, items, Proxy :: Proxy value)

class CanSelect model where
    type SelectValue model :: GHC.Types.*
    selectLabel :: model -> Text
    selectValue :: model -> SelectValue model

instance ToHtml FormField where
    toHtml ::  FormField -> Html5.Html
    toHtml formField@(FormField { renderFormField }) = renderFormField formField

instance ToHtml SubmitButton where
    toHtml submitButton@(SubmitButton { renderSubmit }) = renderSubmit submitButton

renderFlashMessages :: Html
renderFlashMessages =
    let View.Context.ViewContext{flashMessages} = ?viewContext
    in
        forM_ flashMessages $ \flashMessage -> do
            case flashMessage of
                Foundation.Controller.Session.SuccessFlashMessage message -> div ! class_ "alert alert-success" $ cs message
                Foundation.Controller.Session.ErrorFlashMessage message -> div ! class_ "alert alert-danger" $ cs message

isSubmitted :: (?viewContext :: ViewContext) => Bool
isSubmitted = let ViewContext {request} = ?viewContext in requestMethod request == methodPost
