{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Foundation.ViewPrelude (
    Html, div, span, p, a, href, nav, h1, h2, h3, h4, h5, ul, ol, id, li, head, meta, title, link, docTypeHtml, script, body, form, input, label, button, text, value, hr, footer, table, thead, tr, th, tbody, td, onClick, preEscapedText, iframe, placeholder, autofocus, autocomplete, img, httpEquiv, content, small,
    
    src, class_, lang, rel, charset, type_, method, action, name, style,
    
    ($), (!), forM_, mempty,

    module Foundation.HaskellSupport,
    (<>),
    Show (show),
    stringValue,

    StyleRule (BackgroundColor, FontSize),

    cs,

    textFieldWithLabel, colorFieldWithLabel, emailFieldWithLabel, numberFieldWithLabel,

    renderFormField,
    FormField (..),
    textField,
    formFor,

    isActivePath,
    when,

    module UrlGenerator
) where

import ClassyPrelude (($), forM_, mempty, Text, (<>), fromString, fmap, Show (show), (.), String, (==))
import Text.Blaze.Html5 (html, div, span, p, a, nav, h1, h2, h3, h4, h5, ul, ol, li, head, meta, title, link, docTypeHtml, script, body, form, input, label, button, hr, footer, table, thead, tr, th, tbody, td, iframe, img, small)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as Html5
import Text.Blaze.Html5.Attributes (class_, src, id, lang, rel, charset, type_, method, action, name, href, onclick, style, autofocus, placeholder, autocomplete, value, httpEquiv, content)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze (text, Attribute, stringValue, preEscapedText)
import Foundation.HaskellSupport
import Data.Text (intercalate, Text)
import Data.String.Conversions (cs, ConvertibleStrings (convertString))
import Foundation.ViewSupport
import qualified Foundation.ModelSupport
import Foundation.ViewErrorMessages
import qualified Network.Wai
import qualified ClassyPrelude
import UrlGenerator
import Control.Monad (when)
type Style = [StyleRule]
data StyleRule = BackgroundColor Text | FontSize Text

style2 :: Style -> Attribute
style2 theStyle = A.style (stringValue (cs (intercalate "; " (fmap compile theStyle))))
    where
        compile :: StyleRule -> Text
        compile (BackgroundColor color) = "background-color: " <> color
        compile (FontSize size) = "font-size: " <> size

instance ConvertibleStrings String Html5.AttributeValue where
    convertString = stringValue

instance ConvertibleStrings Text Html5.AttributeValue where
    convertString = Html5.textValue

instance ConvertibleStrings String Html5.Html where
    convertString = Html5.string

instance ConvertibleStrings Text Html5.Html where
    convertString = Html5.text

onClick = onclick

inputFieldWithLabel inputType labelText fieldName = div ! class_ "form-group" $ do
    label labelText
    input ! type_ inputType ! name fieldName ! class_ "form-control"

textFieldWithLabel = inputFieldWithLabel "text"
colorFieldWithLabel = inputFieldWithLabel "color"
emailFieldWithLabel = inputFieldWithLabel "email"
numberFieldWithLabel = inputFieldWithLabel "number"

renderFormField (FormField fieldType fieldName fieldLabel fieldValue) = div ! class_ "form-group" $ do
    label fieldLabel
    input ! type_ fieldType ! name fieldName ! class_ "form-control" ! value fieldValue

formFor model url fields = form ! method "POST" ! action url $ do
    forM_ fields (\field -> renderFormField (field model))
    button ! class_ "btn btn-primary" $ "Submit"

data FormField = FormField { fieldType :: Html5.AttributeValue, fieldName :: Html5.AttributeValue, fieldLabel :: Html5.Html, fieldValue :: Html5.AttributeValue }

textField :: Foundation.ModelSupport.FormField a => a -> Foundation.ModelSupport.Model a -> FormField
textField param model = FormField {
        fieldType = "text",
        fieldName = cs (Foundation.ModelSupport.formFieldName param),
        fieldLabel = cs (Foundation.ModelSupport.formFieldLabel param),
        fieldValue = cs (Foundation.ModelSupport.formFieldValue param model)
    }

isActivePath :: (?viewContext :: ViewContext) => String -> ClassyPrelude.Bool
isActivePath path =
    let
        (ViewContext request) = ?viewContext
        currentPath = Network.Wai.rawPathInfo request
    in
        currentPath == (cs path)
