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
        fieldLabel :: Html5.Html,
        fieldValue :: Text,
        fieldInputId :: Text,
        validatorResult :: ValidatorResult,
        fieldInput :: Html5.Html,
        fieldClass :: Html5.AttributeValue,
        labelClass :: Html5.AttributeValue,
        disableLabel :: Bool,
        modelIsNew :: Bool,
        formIsSubmitted :: Bool,
        renderFormField :: FormField -> Html5.Html
    }

data SubmitButton = SubmitButton { modelIsNew :: Bool, modelName :: Text, renderSubmit :: SubmitButton -> Html5.Html }

data FormContext model = FormContext { model :: model, renderFormField :: FormField -> Html5.Html, renderSubmit :: SubmitButton -> Html5.Html }

formFor :: (?viewContext :: ViewContext) => (Foundation.ModelSupport.IsNew model, Foundation.ModelSupport.HasModelName model) => model -> Text -> ((?viewContext :: ViewContext, ?formContext :: FormContext model) => Html5.Html) -> Html5.Html
formFor model = formFor' (FormContext { model, renderFormField = renderBootstrapFormField, renderSubmit = renderBootstrapSubmitButton })

horizontalFormFor :: (?viewContext :: ViewContext) => (Foundation.ModelSupport.IsNew model, Foundation.ModelSupport.HasModelName model) => model -> Text -> ((?viewContext :: ViewContext, ?formContext :: FormContext model) => Html5.Html) -> Html5.Html
horizontalFormFor model = formFor' (FormContext { model, renderFormField = renderHorizontalBootstrapFormField, renderSubmit = renderHorizontalBootstrapSubmitButton })

formFor' :: (?viewContext :: ViewContext) => (Foundation.ModelSupport.IsNew model, Foundation.ModelSupport.HasModelName model) => FormContext model -> Text -> ((?viewContext :: ViewContext, ?formContext :: FormContext model) => Html5.Html) -> Html5.Html
formFor' formContext url inner = form ! method "POST" ! action (cs url) $ do
    renderFlashMessages
    let ?formContext = formContext in inner



submitButton :: (?formContext :: FormContext model, Foundation.ModelSupport.IsNew model, Foundation.ModelSupport.HasModelName model) => SubmitButton
submitButton = SubmitButton { modelIsNew = Foundation.ModelSupport.isNew (model ?formContext), modelName = Foundation.ModelSupport.getModelName (model ?formContext), renderSubmit = let FormContext { renderSubmit } = ?formContext in renderSubmit }

data InputType = TextInput | CheckboxInput | ColorInput | HiddenInput | TextareaInput | SelectInput { options :: [(Text, Text)] }

renderValidationResult (FormField { modelIsNew, validatorResult })= when modelIsNew $ case validatorResult of
                Success         -> return ()
                Failure message -> div ! class_ "invalid-feedback" $ cs message

renderBootstrapFormField :: FormField -> Html5.Html
renderBootstrapFormField formField@(FormField { fieldType }) =
        case fieldType of
            TextInput -> renderTextField "text" formField
            ColorInput -> renderTextField "color" formField
            CheckboxInput -> renderCheckboxFormField formField
            HiddenInput -> renderTextField "hidden" formField { disableLabel = True }
            TextareaInput -> renderTextField "text" formField { fieldInput = Html5.textarea (cs $ fieldValue formField) }
            SelectInput {} -> renderSelectField formField
    where
        maybeWithFormGroup (FormField { disableLabel }) renderInner = if disableLabel then renderInner else div ! A.class_ "form-group" $ renderInner
        renderCheckboxFormField :: FormField -> Html5.Html
        renderCheckboxFormField formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, validatorResult, fieldClass, disableLabel, fieldInput, modelIsNew, formIsSubmitted, labelClass }) = div ! class_ "form-group" $ div ! class_ "form-check" $ do
            (if disableLabel then div else label ! class_ "form-check-label") $ do
                let theInput = input ! type_ "checkbox" ! name fieldName ! class_ ("form-check-input " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass)
                if fieldValue == "yes" then theInput ! A.checked "checked" else theInput
                fieldLabel
                renderValidationResult formField
        renderTextField :: Html5.AttributeValue -> FormField -> Html5.Html
        renderTextField inputType formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disableLabel, fieldInput, modelIsNew, formIsSubmitted, labelClass }) =
            maybeWithFormGroup formField $ do
                if disableLabel then return () else label ! A.class_ labelClass ! A.for (cs fieldInputId) $ fieldLabel
                fieldInput ! type_ inputType ! name fieldName ! A.id (cs fieldInputId) ! class_ ("form-control " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass) ! value (cs fieldValue)
                renderValidationResult formField
        renderSelectField :: FormField -> Html5.Html
        renderSelectField formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disableLabel, fieldInput, modelIsNew, formIsSubmitted, labelClass }) =
            maybeWithFormGroup formField $ do
                if disableLabel then return () else label ! A.class_ labelClass ! A.for (cs fieldInputId) $ fieldLabel
                Html5.select ! name fieldName ! A.id (cs fieldInputId) ! class_ ("form-control " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass) ! value (cs fieldValue) $ do
                    --Html5.option ! A.disabled "disabled" ! A.selected "selected" $ Html5.text ("Bitte auswählen" :: Text)
                    let isValueSelected = isJust $ find (\(optionLabel, optionValue) -> optionValue == fieldValue) (options fieldType)
                    (if isValueSelected then Html5.option else Html5.option ! A.selected "selected") $ Html5.text ("Kein Team")
                    forM_ (options fieldType) $ \(optionLabel, optionValue) -> (let option = Html5.option ! A.value (cs optionValue) in (if optionValue == fieldValue then option ! A.selected "selected" else option) $ cs optionLabel)
                renderValidationResult formField

renderBootstrapSubmitButton SubmitButton { modelIsNew, modelName }= button ! class_ "btn btn-primary" $ (if modelIsNew then "Create " else "Save ") <> (cs $ modelName)


renderHorizontalBootstrapFormField :: FormField -> Html5.Html
renderHorizontalBootstrapFormField formField@(FormField { fieldType }) =
        case fieldType of
            TextInput -> renderTextField "text" formField
            ColorInput -> renderTextField "color" formField
            CheckboxInput -> renderCheckboxFormField formField
            HiddenInput -> renderTextField "hidden" formField { disableLabel = True }
    where
        renderCheckboxFormField :: FormField -> Html5.Html
        renderCheckboxFormField formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, validatorResult, fieldClass, disableLabel, fieldInput, modelIsNew, formIsSubmitted }) = div ! class_ "form-group" $ div ! class_ "form-check" $ do
            (if disableLabel then div else label ! class_ "form-check-label") $ do
                let theInput = input ! type_ "checkbox" ! name fieldName ! class_ ("form-check-input " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass)
                if fieldValue == "yes" then theInput ! A.checked "checked" else theInput
                fieldLabel
                renderValidationResult formField
        renderTextField :: Html5.AttributeValue -> FormField -> Html5.Html
        renderTextField inputType formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disableLabel, fieldInput, modelIsNew, formIsSubmitted, labelClass }) = if disableLabel then renderInner else div ! A.class_ "form-group row" $ renderInner
            where
                renderInner = do
                    if disableLabel then return () else label ! A.class_ ("col-sm-2 col-form-label " <> labelClass) ! A.for (cs fieldInputId) $ fieldLabel
                    div ! class_ "col-sm-6" $ do
                        fieldInput ! type_ inputType ! name fieldName ! A.id (cs fieldInputId) ! class_ ("form-control " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass) ! value (cs fieldValue)
                        renderValidationResult formField

renderHorizontalBootstrapSubmitButton SubmitButton { modelIsNew, modelName }= div ! class_ "form-group row" $ do
    div ! class_ "offset-sm-5 col-sm-3 text-left" $ do
        button ! class_ "btn btn-primary btn-lg pl-4 pr-4 w-100" $ (if modelIsNew then "Create " else "Save ") <> (cs $ modelName)




instance (KnownSymbol symbol, Foundation.ModelSupport.IsNew model, Foundation.ModelSupport.HasModelName model, HasField symbol model value, IsLabel symbol (ModelFieldType model), CanValidateField model, Foundation.ModelSupport.FormFieldValue (ModelFieldType model) model, Foundation.ModelSupport.InputValue value ) => IsLabel symbol ((FormContext model, ViewContext, Proxy value) -> FormField) where
    fromLabel = \(formContext, viewContext, _) -> let fieldName = cs (symbolVal @symbol Proxy) in FormField {
                        fieldType = TextInput,
                        fieldName = cs (Foundation.NameSupport.fieldNameToColumnName fieldName),
                        fieldLabel = cs $ fieldNameToFieldLabel fieldName,
                        fieldValue =  Foundation.ModelSupport.inputValue $ getField @(symbol) (model formContext),
                        fieldInputId = cs (Foundation.NameSupport.lcfirst (Foundation.ModelSupport.getModelName (model formContext)) <> "_" <> Foundation.NameSupport.fieldNameToColumnName fieldName),
                        validatorResult = (validateModelField (model formContext) :: ModelFieldType model -> ValidatorResult) (fromLabel @symbol),
                        fieldClass = "",
                        labelClass = "",
                        disableLabel = False,
                        fieldInput = input,
                        modelIsNew = Foundation.ModelSupport.isNew (model formContext),
                        formIsSubmitted = let ?viewContext = viewContext in isSubmitted,
                        renderFormField = let FormContext { renderFormField } = formContext in renderFormField
                    }

instance (KnownSymbol symbol, Foundation.ModelSupport.IsNew model, Foundation.ModelSupport.HasModelName model, HasField symbol model Bool) => IsLabel symbol ((FormContext model, ViewContext, Proxy Bool) -> FormField) where
    fromLabel = \(formContext, viewContext, _) -> let fieldName = cs (symbolVal @symbol Proxy) in FormField {
                        fieldType = CheckboxInput,
                        fieldName = cs (Foundation.NameSupport.fieldNameToColumnName fieldName),
                        fieldLabel = cs $ fieldNameToFieldLabel fieldName,
                        fieldValue =  let value = getField @(symbol) (model formContext) in if value then "yes" else "no",
                        fieldInputId = cs (Foundation.NameSupport.lcfirst (Foundation.ModelSupport.getModelName (model formContext)) <> "_" <> Foundation.NameSupport.fieldNameToColumnName fieldName),
                        validatorResult = Success,
                        fieldClass = "",
                        labelClass = "",
                        disableLabel = False,
                        fieldInput = input,
                        modelIsNew = Foundation.ModelSupport.isNew (model formContext),
                        formIsSubmitted = let ?viewContext = viewContext in isSubmitted,
                        renderFormField = let FormContext { renderFormField } = formContext in renderFormField
                    }

instance (KnownSymbol symbol, Foundation.ModelSupport.IsNew model, Foundation.ModelSupport.HasModelName model, HasField symbol model (Maybe (SelectValue item)), CanSelect item, Foundation.ModelSupport.NewTypeWrappedUUID (SelectValue item)) => IsLabel symbol ((FormContext model, ViewContext, [item], Proxy value) -> FormField) where
    fromLabel = \(formContext, viewContext, items, _) -> let fieldName = cs (symbolVal @symbol Proxy) in FormField {
                        fieldType =
                            let
                                itemToTuple item = (selectLabel item, (Foundation.ModelSupport.inputValue . Foundation.ModelSupport.unwrap) (selectValue item))
                            in
                                 SelectInput $ map itemToTuple items
                            ,
                        fieldName = cs (Foundation.NameSupport.fieldNameToColumnName fieldName),
                        fieldLabel = cs . removeIdSuffix $ fieldNameToFieldLabel fieldName,
                        fieldValue =
                            let value = ((getField @(symbol) (model formContext)) :: Maybe (SelectValue item))
                            in maybe "" (Foundation.ModelSupport.inputValue . Foundation.ModelSupport.unwrap) value,
                        fieldInputId = cs (Foundation.NameSupport.lcfirst (Foundation.ModelSupport.getModelName (model formContext)) <> "_" <> Foundation.NameSupport.fieldNameToColumnName fieldName),
                        validatorResult = Success,
                        fieldClass = "",
                        labelClass = "",
                        disableLabel = False,
                        fieldInput = Html5.select mempty,
                        modelIsNew = Foundation.ModelSupport.isNew (model formContext),
                        formIsSubmitted = let ?viewContext = viewContext in isSubmitted,
                        renderFormField = let FormContext { renderFormField } = formContext in renderFormField
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
