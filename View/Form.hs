{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

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

renderFormField :: (?viewContext :: ViewContext) => Foundation.ModelSupport.IsNew model => FormField -> model -> Html5.Html
renderFormField (FormField fieldType fieldName fieldLabel fieldValue validatorResult) model = div ! class_ "form-group" $ do
    label fieldLabel
    input ! type_ fieldType ! name fieldName ! class_ ("form-control " <> if not isSubmitted || isSuccess validatorResult then "" else "is-invalid") ! value fieldValue
    when (Foundation.ModelSupport.isNew model) $ case validatorResult of
        Success         -> return ()
        Failure message -> div ! class_ "invalid-feedback" $ cs message

formFor :: (?viewContext :: ViewContext) => (Foundation.ModelSupport.IsNew model) => model -> Html5.AttributeValue -> [model -> FormField] -> Html5.Html
formFor model url fields = form ! method "POST" ! action url $ do

    renderFlashMessages
    forM_ fields (\field -> renderFormField (field model) model)
    button ! class_ "btn btn-primary" $ "Submit"

data FormField = FormField { fieldType :: Html5.AttributeValue, fieldName :: Html5.AttributeValue, fieldLabel :: Html5.Html, fieldValue :: Html5.AttributeValue, validatorResult :: ValidatorResult }

fieldFactory :: (Foundation.ModelSupport.FormField attribute, Foundation.ModelSupport.FormFieldValue attribute model, CanValidateField model attribute) => Html5.AttributeValue -> attribute -> model -> FormField
fieldFactory fieldType param model = FormField {
        fieldType = fieldType,
        fieldName = cs (Foundation.ModelSupport.formFieldName param),
        fieldLabel = cs (Foundation.ModelSupport.formFieldLabel param),
        fieldValue = cs (Foundation.ModelSupport.formFieldValue param model),
        validatorResult = validateModelField model param
    }

textField :: (Foundation.ModelSupport.FormField attribute, Foundation.ModelSupport.FormFieldValue attribute model, CanValidateField model attribute) => attribute -> model -> FormField
textField = fieldFactory "text"

colorField :: (Foundation.ModelSupport.FormField attribute, Foundation.ModelSupport.FormFieldValue attribute model, CanValidateField model attribute) => attribute -> model -> FormField
colorField = fieldFactory "color"

renderFlashMessages :: Html
renderFlashMessages =
    let View.Context.ViewContext{flashMessage} = ?viewContext
    in
        case flashMessage of
            Just message -> div ! class_ "alert alert-success" $ cs message
            Nothing      -> mempty

isSubmitted :: (?viewContext :: ViewContext) => Bool
isSubmitted = let ViewContext {request} = ?viewContext in requestMethod request == methodPost
