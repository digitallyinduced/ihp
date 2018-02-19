{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Foundation.ViewPrelude (
    Html, div, span, p, a, href, nav, h1, h2, h3, h4, h5, ul, ol, id, li, head, meta, title, link, docTypeHtml, script, body, form, input, label, button, text, value, hr, footer, table, thead, tr, th, tbody, td, onClick, preEscapedText,
    
    src, class_, lang, rel, charset, type_, method, action, name, style,
    
    ($), (!), forM_, mempty,

    (|>),
    (<>),
    Show (show),
    stringValue,

    StyleRule (BackgroundColor, FontSize),

    cs
) where

import ClassyPrelude (($), forM_, mempty, Text, (<>), fromString, fmap, Show (show), (.), String)
import Text.Blaze.Html5 (html, div, span, p, a, nav, h1, h2, h3, h4, h5, ul, ol, li, head, meta, title, link, docTypeHtml, script, body, form, input, label, button, hr, footer, table, thead, tr, th, tbody, td)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as Html5
import Text.Blaze.Html5.Attributes (class_, src, id, lang, rel, charset, type_, method, action, name, href, onclick)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze (text, Attribute, stringValue, preEscapedText)
import Foundation.HaskellSupport
import Data.Text (intercalate, Text)
import Data.String.Conversions (cs, ConvertibleStrings (convertString))
import Foundation.ViewSupport

import Foundation.ViewErrorMessages

type Style = [StyleRule]
data StyleRule = BackgroundColor Text | FontSize Text

style :: Style -> Attribute
style theStyle = A.style (stringValue (cs (intercalate "; " (fmap compile theStyle))))
    where
        compile :: StyleRule -> Text
        compile (BackgroundColor color) = "background-color: " <> color
        compile (FontSize size) = "font-size: " <> size

value :: ConvertibleStrings a String => a -> Attribute
value = A.value . stringValue . cs

instance ConvertibleStrings String Html5.AttributeValue where
    convertString = stringValue

instance ConvertibleStrings String Html5.Html where
    convertString = Html5.string

instance ConvertibleStrings Text Html5.Html where
    convertString = Html5.text

onClick = onclick