{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances, StandaloneDeriving,InstanceSigs, UndecidableInstances, AllowAmbiguousTypes  #-}

module Foundation.View.Form where

import           ClassyPrelude                      hiding (div)
import           Data.String.Conversions            (cs)
import           Foundation.HaskellSupport
import qualified Foundation.ModelSupport
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

inputFieldWithLabel inputType labelText fieldName = div ! class_ "form-group" $ do
    label labelText
    input ! type_ inputType ! name fieldName ! class_ "form-control"

textFieldWithLabel :: Html5.Html -> Html5.AttributeValue -> Html5.Html
textFieldWithLabel = inputFieldWithLabel "text"

colorFieldWithLabel :: Html5.Html -> Html5.AttributeValue -> Html5.Html
colorFieldWithLabel = inputFieldWithLabel "color"

emailFieldWithLabel :: Html5.Html -> Html5.AttributeValue -> Html5.Html
emailFieldWithLabel = inputFieldWithLabel "email"

numberFieldWithLabel :: Html5.Html -> Html5.AttributeValue -> Html5.Html
numberFieldWithLabel = inputFieldWithLabel "number"

renderFormGroupFormField :: FormField -> Html5.Html
renderFormGroupFormField (FormField {fieldType, fieldName, fieldLabel, fieldValue, validatorResult, fieldClass, disableLabel, fieldInput, modelIsNew, formIsSubmitted }) = div ! class_ "form-group" $ do
    if disableLabel then return () else label fieldLabel
    input ! type_ fieldType ! name fieldName ! class_ ("form-control " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass) ! value (cs fieldValue)
    when modelIsNew $ case validatorResult of
        Success         -> return ()
        Failure message -> div ! class_ "invalid-feedback" $ cs message

renderCheckboxFormField :: FormField -> Html5.Html
renderCheckboxFormField (FormField {fieldType, fieldName, fieldLabel, fieldValue, validatorResult, fieldClass, disableLabel, fieldInput, modelIsNew, formIsSubmitted }) = div ! class_ "form-group" $ div ! class_ "form-check" $ do
    (if disableLabel then div else label ! class_ "form-check-label") $ do
        let theInput = input ! type_ fieldType ! name fieldName ! class_ ("form-check-input " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass)
        if fieldValue == "yes" then theInput ! A.checked "checked" else theInput
        fieldLabel
    when modelIsNew $ case validatorResult of
        Success         -> return ()
        Failure message -> div ! class_ "invalid-feedback" $ cs message


renderFormField2 :: FormField -> Html5.Html
renderFormField2 (FormField {fieldType, fieldName, fieldLabel, fieldValue, validatorResult, fieldClass, disableLabel, fieldInput, modelIsNew, formIsSubmitted }) = do
    if disableLabel then return () else label fieldLabel
    fieldInput ! type_ fieldType ! name fieldName ! class_ ("form-control " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass) ! value (cs fieldValue)
    when modelIsNew $ case validatorResult of
        Success         -> return ()
        Failure message -> div ! class_ "invalid-feedback" $ cs message

formFor :: (?viewContext :: ViewContext) => (Foundation.ModelSupport.IsNew model, Foundation.ModelSupport.HasModelName model) => model -> Html5.AttributeValue -> [model -> FormField] -> Html5.Html
formFor model url fields = form ! method "POST" ! action url $ do
    renderFlashMessages
    forM_ (map (\field -> field model) fields) (\field@(FormField { renderFormField }) -> renderFormField field)
    button ! class_ "btn btn-primary" $ (if Foundation.ModelSupport.isNew model then "Create " else "Save ") <> (cs $ Foundation.ModelSupport.getModelName model)

formFor2 :: (?viewContext :: ViewContext) => (Foundation.ModelSupport.IsNew model, Foundation.ModelSupport.HasModelName model) => model -> Text -> ((?viewContext :: ViewContext, ?formContext :: FormContext model) => Html5.Html) -> Html5.Html
formFor2 model url inner = form ! method "POST" ! action (cs url) $ do
    renderFlashMessages
    let ?formContext = FormContext { model } in inner


submitButton :: (?formContext :: FormContext model, Foundation.ModelSupport.IsNew model, Foundation.ModelSupport.HasModelName model) => Html5.Html
submitButton = button ! class_ "btn btn-primary" $ (if Foundation.ModelSupport.isNew (model ?formContext) then "Create " else "Save ") <> (cs $ Foundation.ModelSupport.getModelName (model ?formContext))


data FormField = FormField {
        fieldType :: Html5.AttributeValue,
        fieldName :: Html5.AttributeValue,
        fieldLabel :: Html5.Html,
        fieldValue :: Text,
        validatorResult :: ValidatorResult,
        fieldInput :: Html5.Html,
        fieldClass :: Html5.AttributeValue,
        disableLabel :: Bool,
        modelIsNew :: Bool,
        formIsSubmitted :: Bool,
        renderFormField :: FormField -> Html5.Html
    }

fieldFactory :: (?viewContext :: ViewContext, Foundation.ModelSupport.FormField attribute, Foundation.ModelSupport.FormFieldValue attribute model, CanValidateField model attribute, Foundation.ModelSupport.IsNew model) => Html5.AttributeValue -> (FormField -> Html5.Html) -> attribute -> model -> FormField
fieldFactory fieldType renderFormField param model = FormField {
        fieldType = fieldType,
        fieldName = cs (Foundation.ModelSupport.formFieldName param),
        fieldLabel = cs (Foundation.ModelSupport.formFieldLabel param),
        fieldValue = (Foundation.ModelSupport.formFieldValue param model),
        validatorResult = validateModelField model param,
        renderFormField = renderFormField,
        fieldClass = "",
        disableLabel = False,
        fieldInput = input,
        modelIsNew = Foundation.ModelSupport.isNew model,
        formIsSubmitted = isSubmitted
    }

textField :: (?viewContext :: ViewContext, Foundation.ModelSupport.FormField attribute, Foundation.ModelSupport.FormFieldValue attribute model, CanValidateField model attribute, Foundation.ModelSupport.IsNew model) => attribute -> model -> FormField
textField = fieldFactory "text" renderFormGroupFormField

data FormContext model = FormContext { model :: model }

renderTextField :: (?formContext :: FormContext model) => (Foundation.ModelSupport.FormField attribute, Foundation.ModelSupport.FormFieldValue attribute model, CanValidateField model attribute, Foundation.ModelSupport.IsNew model) => attribute -> Html
renderTextField attribute = renderFormField2 (fieldFactory "text" renderFormField2 attribute (model ?formContext))

makeField :: (?formContext :: FormContext model, ?viewContext :: ViewContext) => (Foundation.ModelSupport.FormField attribute, Foundation.ModelSupport.FormFieldValue attribute model, CanValidateField model attribute, Foundation.ModelSupport.IsNew model) => attribute -> FormField
makeField param = FormField {
                    fieldType = "text",
                    fieldName = cs (Foundation.ModelSupport.formFieldName param),
                    fieldLabel = cs (Foundation.ModelSupport.formFieldLabel param),
                    fieldValue = (Foundation.ModelSupport.formFieldValue param (model ?formContext)),
                    validatorResult = validateModelField (model ?formContext) param,
                    fieldClass = "",
                    disableLabel = False,
                    fieldInput = input,
                    modelIsNew = Foundation.ModelSupport.isNew (model ?formContext),
                    formIsSubmitted = isSubmitted
                }

instance ToHtml FormField where
    toHtml ::  FormField -> Html5.Html
    toHtml = renderFormField2

colorField :: (?viewContext :: ViewContext, Foundation.ModelSupport.FormField attribute, Foundation.ModelSupport.FormFieldValue attribute model, CanValidateField model attribute, Foundation.ModelSupport.IsNew model) => attribute -> model -> FormField
colorField = fieldFactory "color" renderFormGroupFormField

checkboxField :: (?viewContext :: ViewContext, Foundation.ModelSupport.FormField attribute, Foundation.ModelSupport.FormFieldValue attribute model, CanValidateField model attribute, Foundation.ModelSupport.IsNew model) => attribute -> model -> FormField
checkboxField = fieldFactory "checkbox" renderCheckboxFormField

renderFlashMessages :: Html
renderFlashMessages =
    let View.Context.ViewContext{flashMessage} = ?viewContext
    in
        case flashMessage of
            Just message -> div ! class_ "alert alert-success" $ cs message
            Nothing      -> mempty

isSubmitted :: (?viewContext :: ViewContext) => Bool
isSubmitted = let ViewContext {request} = ?viewContext in requestMethod request == methodPost
