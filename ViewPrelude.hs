{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Foundation.ViewPrelude (
    Html, div, span, p, a, href, nav, h1, h2, h3, h4, h5, ul, ol, id, li, head, meta, title, link, docTypeHtml, script, body, form, input, label, button, text, value, hr, footer, table, thead, tr, th, tbody, td, onClick, preEscapedText, iframe, placeholder, autofocus, autocomplete, img, httpEquiv, content, small, dataAttribute, h6,

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
    Int,

    Maybe (..),
    viewContext,

    module UrlGenerator
) where

import           ClassyPrelude                (Int, Maybe (..), Show (show), String, Text, fmap, forM_, fromString, mempty, ($), (.), (<>), (==))
import qualified ClassyPrelude
import           Control.Monad                (when)
import           Data.String.Conversions      (ConvertibleStrings (convertString), cs)
import           Data.Text                    (Text, intercalate)
import           Foundation.HaskellSupport
import qualified Foundation.ModelSupport
import           Foundation.ViewErrorMessages
import           Foundation.ViewSupport
import qualified Network.Wai
import           Text.Blaze                   (Attribute, dataAttribute, preEscapedText, stringValue, text)
import           Text.Blaze.Html5             (a, body, button, div, docTypeHtml, footer, form, h1, h2, h3, h4, h5, h6, head, hr, html, iframe, img, input,
                                               label, li, link, meta, nav, ol, p, script, small, span, table, tbody, td, th, thead, title, tr, ul)
import           Text.Blaze.Html5             ((!))
import qualified Text.Blaze.Html5             as Html5
import           Text.Blaze.Html5.Attributes  (action, autocomplete, autofocus, charset, class_, content, href, httpEquiv, id, lang, method, name, onclick,
                                               placeholder, rel, src, style, type_, value)
import qualified Text.Blaze.Html5.Attributes  as A
import           UrlGenerator
import qualified View.Context

type Style = [StyleRule]
data StyleRule = BackgroundColor Text | FontSize Text

style2 :: Style -> Attribute
style2 theStyle = A.style (stringValue (cs (intercalate "; " (fmap compile theStyle))))
    where
        compile :: StyleRule -> Text
        compile (BackgroundColor color) = "background-color: " <> color
        compile (FontSize size)         = "font-size: " <> size

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

isActivePath :: (?viewContext :: View.Context.ViewContext) => Text -> ClassyPrelude.Bool
isActivePath path =
    let
        currentPath = Network.Wai.rawPathInfo (View.Context.request ?viewContext)
    in
        currentPath == (cs path)

viewContext :: (?viewContext :: View.Context.ViewContext) => View.Context.ViewContext
viewContext = ?viewContext
