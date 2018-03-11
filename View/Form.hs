{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Foundation.View.Form where

import ClassyPrelude hiding (div)
import           Data.String.Conversions      (cs)
import           Foundation.HaskellSupport
import qualified Foundation.ModelSupport
import           Foundation.ViewErrorMessages
import           Foundation.ViewSupport
import qualified Network.Wai
import           Text.Blaze                   (Attribute, dataAttribute, preEscapedText, stringValue, text)
import           Text.Blaze.Html5             (a, body, button, code, div, docTypeHtml, footer, form, h1, h2, h3, h4, h5, h6, head, hr, html, iframe, img,
                                               input, label, li, link, meta, nav, ol, p, pre, script, small, span, table, tbody, td, th, thead, title, tr, ul)
import           Text.Blaze.Html5             ((!))
import qualified Text.Blaze.Html5             as Html5
import           Text.Blaze.Html5.Attributes  (action, autocomplete, autofocus, charset, class_, content, href, httpEquiv, id, lang, method, name, onclick,
                                               placeholder, rel, src, style, type_, value)
import qualified Text.Blaze.Html5.Attributes  as A
import           UrlGenerator
import qualified View.Context
import Foundation.View.ConvertibleStrings ()
import Foundation.ValidationSupport





inputFieldWithLabel inputType labelText fieldName = div ! class_ "form-group" $ do
    label labelText
    input ! type_ inputType ! name fieldName ! class_ "form-control"

textFieldWithLabel = inputFieldWithLabel "text"
colorFieldWithLabel = inputFieldWithLabel "color"
emailFieldWithLabel = inputFieldWithLabel "email"
numberFieldWithLabel = inputFieldWithLabel "number"

renderFormField (FormField fieldType fieldName fieldLabel fieldValue validatorResult) model = div ! class_ "form-group" $ do
    label fieldLabel
    input ! type_ fieldType ! name fieldName ! class_ ("form-control " <> if isSuccess validatorResult then "" else "is-invalid") ! value fieldValue
    case validatorResult of
        Success -> return ()
        Failure message -> div ! class_ "invalid-feedback" $ cs message

formFor model url fields = form ! method "POST" ! action url $ do
    renderFlashMessages
    forM_ fields (\field -> renderFormField (field model) model)
    button ! class_ "btn btn-primary" $ "Submit"

data FormField = FormField { fieldType :: Html5.AttributeValue, fieldName :: Html5.AttributeValue, fieldLabel :: Html5.Html, fieldValue :: Html5.AttributeValue, validatorResult :: ValidatorResult }

fieldFactory :: (Foundation.ModelSupport.FormField a, CanValidateField a) => Html5.AttributeValue -> a -> Foundation.ModelSupport.Model a -> FormField
fieldFactory fieldType param model = FormField {
        fieldType = fieldType,
        fieldName = cs (Foundation.ModelSupport.formFieldName param),
        fieldLabel = cs (Foundation.ModelSupport.formFieldLabel param),
        fieldValue = cs (Foundation.ModelSupport.formFieldValue param model),
        validatorResult = validateModelField model param
    }

textField :: (Foundation.ModelSupport.FormField a, CanValidateField a) => a -> Foundation.ModelSupport.Model a -> FormField
textField = fieldFactory "text"

colorField :: (Foundation.ModelSupport.FormField a, CanValidateField a) => a -> Foundation.ModelSupport.Model a -> FormField
colorField = fieldFactory "color"

renderFlashMessages :: Html
renderFlashMessages =
    let View.Context.ViewContext{flashMessage} = ?viewContext
    in
        case flashMessage of
            Just message -> div ! class_ "alert alert-success" $ cs message
            Nothing -> mempty
