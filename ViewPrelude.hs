module Foundation.ViewPrelude (
    Html, div, span, p, a, href, nav, h1, h2, h3, h4, h5, ul, id, li, head, meta, title, link, docTypeHtml, script, body, form, input, label, button, text,
    
    src, class_, lang, rel, charset, type_, method, action, name, style,
    
    ($), (!), forM_, mempty,

    (|>),

    StyleRule (BackgroundColor, FontSize)
) where

import ClassyPrelude (($), forM_, mempty, Text, (<>), fromString, fmap)
import Text.Blaze.Html5 (Html, html, div, span, p, a, nav, h1, h2, h3, h4, h5, ul, li, head, meta, title, link, docTypeHtml, script, body, form, input, label, button)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes (class_, href, src, id, lang, rel, charset, type_, method, action, name)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze (text, Attribute, stringValue)
import Foundation.HaskellSupport
import Data.Text (intercalate)
import Data.String.Conversions (cs)

type Style = [StyleRule]
data StyleRule = BackgroundColor Text | FontSize Text

style :: Style -> Attribute
style theStyle = A.style (stringValue (cs (intercalate "; " (fmap compile theStyle))))
    where
        compile :: StyleRule -> Text
        compile (BackgroundColor color) = "background-color: " <> color
        compile (FontSize size) = "font-size: " <> size
