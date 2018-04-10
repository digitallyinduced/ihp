{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Foundation.HtmlSupport.QQ (hsx) where

import           ClassyPrelude
import           Foundation.HtmlSupport.Parser
import           Language.Haskell.Meta         (parseExp)
import qualified Language.Haskell.TH           as TH
import           Language.Haskell.TH.Quote
import           Text.Blaze.Html5              ((!))
import qualified Text.Blaze.Html5              as Html5
import           Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html (Html)
import Text.Blaze.Internal (attribute, MarkupM (Parent, Leaf), StaticString)
import Data.String.Conversions (cs)
import Foundation.HtmlSupport.ToHtml

hsx :: QuasiQuoter
hsx = QuasiQuoter {
        quoteExp = quoteHsxExpression,
        quotePat = error "quotePat: not defined",
        quoteDec = error "quoteDec: not defined",
        quoteType = error "quoteType: not defined"
    }

quoteHsxExpression :: String -> TH.ExpQ
quoteHsxExpression code = do
        expression <- monadicParseHsx code
        compileToHaskell expression
    where
        monadicParseHsx code =
            case parseHsx code of
                Left error   -> fail (show error)
                Right result -> return result

compileToHaskell :: Node -> TH.ExpQ
compileToHaskell (Node name attributes children) =
    let
        renderedChildren = TH.listE $ map compileToHaskell children
    in case attributes of
        StaticAttributes attributes ->
            let
                stringAttributes = TH.listE $ map toStringAttribute attributes
            in [| (applyAttributes (makeElement name $(renderedChildren)) $(stringAttributes)) |]

compileToHaskell (Children children) =
    let
        renderedChildren = TH.listE $ map compileToHaskell children
    in [| foldl' (>>) mempty $(renderedChildren) |]

compileToHaskell (TextNode value) = [| Html5.string value |]
compileToHaskell (SplicedNode code) =
    case parseExp code of
        Right expression -> [| toHtml $(return expression) |]
        Left error -> fail ("compileToHaskell(" <> code <> "): " <> show error)

toStringAttribute :: (String, AttributeValue) -> TH.ExpQ
toStringAttribute (name, TextValue value) = do
    let nameWithSuffix = " " <> name <> "=\""
    if name `elem` attributes || ("data-" `isPrefixOf` name) then return () else fail ("Invalid attribute: " <> name)
    [| (attribute name nameWithSuffix) value |]

toStringAttribute (name, ExpressionValue code) = do
    let nameWithSuffix = " " <> name <> "=\""
    if name `elem` attributes || ("data-" `isPrefixOf` name) then return () else fail ("Invalid attribute: " <> name)
    case parseExp code of
        Right expression -> [| (attribute name nameWithSuffix) (cs $(return expression)) |]
        Left error -> fail ("toStringAttribute.compileToHaskell(" <> code <> "): " <> show error)


applyAttributes :: Html5.Html -> [Html5.Attribute] -> Html5.Html
applyAttributes el [] = el
applyAttributes el (x:xs) = applyAttributes (el ! x) xs

makeElement :: String -> [Html] -> Html
makeElement name children =
    let
        element :: Html -> Html
        element = (Parent (fromString name) (fromString $ "<" <> name) (fromString $ "</" <> name <> ">"))
        leaf = (Leaf (fromString name) (fromString $ "<" <> name) (fromString $ ">"))
    in if name `elem` parents then
            let children' = (foldl' (<>) (unsafeHead children) (unsafeTail children)) in element children'
        else
            if name `elem` leafs then
                leaf ()
            else
                error ("makeElement: Unknown tag ")

attributes =
        [ "accept", "accept-charset", "accesskey", "action", "alt", "async"
        , "autocomplete", "autofocus", "autoplay", "challenge", "charset"
        , "checked", "cite", "class", "cols", "colspan", "content"
        , "contenteditable", "contextmenu", "controls", "coords", "data"
        , "datetime", "defer", "dir", "disabled", "draggable", "enctype", "for"
        , "form", "formaction", "formenctype", "formmethod", "formnovalidate"
        , "formtarget", "headers", "height", "hidden", "high", "href"
        , "hreflang", "http-equiv", "icon", "id", "ismap", "item", "itemprop"
        , "itemscope", "itemtype"
        , "keytype", "label", "lang", "list", "loop", "low", "manifest", "max"
        , "maxlength", "media", "method", "min", "multiple", "name"
        , "novalidate", "onbeforeonload", "onbeforeprint", "onblur", "oncanplay"
        , "oncanplaythrough", "onchange", "oncontextmenu", "onclick"
        , "ondblclick", "ondrag", "ondragend", "ondragenter", "ondragleave"
        , "ondragover", "ondragstart", "ondrop", "ondurationchange", "onemptied"
        , "onended", "onerror", "onfocus", "onformchange", "onforminput"
        , "onhaschange", "oninput", "oninvalid", "onkeydown", "onkeyup"
        , "onload", "onloadeddata", "onloadedmetadata", "onloadstart"
        , "onmessage", "onmousedown", "onmousemove", "onmouseout", "onmouseover"
        , "onmouseup", "onmousewheel", "ononline", "onpagehide", "onpageshow"
        , "onpause", "onplay", "onplaying", "onprogress", "onpropstate"
        , "onratechange", "onreadystatechange", "onredo", "onresize", "onscroll"
        , "onseeked", "onseeking", "onselect", "onstalled", "onstorage"
        , "onsubmit", "onsuspend", "ontimeupdate", "onundo", "onunload"
        , "onvolumechange", "onwaiting", "open", "optimum", "pattern", "ping"
        , "placeholder", "preload", "pubdate", "radiogroup", "readonly", "rel"
        , "required", "reversed", "rows", "rowspan", "sandbox", "scope"
        , "scoped", "seamless", "selected", "shape", "size", "sizes", "span"
        , "spellcheck", "src", "srcdoc", "start", "step", "style", "subject"
        , "summary", "tabindex", "target", "title", "type", "usemap", "value"
        , "width", "wrap", "xmlns"
        , "ontouchstart", "download"
        , "allowtransparency", "minlength", "maxlength"
        ]


parents =
        [ "a", "abbr", "address", "article", "aside", "audio", "b"
        , "bdo", "blockquote", "body", "button", "canvas", "caption", "cite"
        , "code", "colgroup", "command", "datalist", "dd", "del", "details"
        , "dfn", "div", "dl", "dt", "em", "fieldset", "figcaption", "figure"
        , "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header"
        , "hgroup", "html", "i", "iframe", "ins", "kbd", "label"
        , "legend", "li", "main", "map", "mark", "menu", "meter", "nav"
        , "noscript", "object", "ol", "optgroup", "option", "output", "p"
        , "pre", "progress", "q", "rp", "rt", "ruby", "samp", "script"
        , "section", "select", "small", "span", "strong", "style", "sub"
        , "summary", "sup", "table", "tbody", "td", "textarea", "tfoot", "th"
        , "thead", "time", "title", "tr", "ul", "var", "video"
        ]

leafs =
        [ "area", "br", "col", "hr", "link", "img", "input",  "meta", "param"
        ]