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
import qualified Debug.Trace
import qualified Language.Haskell.Exts.Syntax as HS


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
        Right expression -> let patched = Debug.Trace.traceShow (patchExpr expression)  (patchExpr expression) in [| toHtml $(return patched) |]
        Left error -> fail ("compileToHaskell(" <> code <> "): " <> show error)

patchExpr :: TH.Exp -> TH.Exp
patchExpr (TH.UInfixE (TH.VarE varName) (TH.VarE hash) (TH.VarE labelValue)) | hash == TH.mkName "#" = TH.AppE (TH.VarE varName) fromLabel
    where
            fromLabel = TH.AppTypeE (TH.VarE (TH.mkName "fromLabel")) (TH.LitT (TH.StrTyLit (show labelValue)))
--- UInfixE (UInfixE a (VarE |>) (VarE get)) (VarE #) (VarE firstName)
patchExpr input@(TH.UInfixE (TH.UInfixE a (TH.VarE arrow) (TH.VarE get)) (TH.VarE hash) (TH.VarE labelValue)) | (hash == TH.mkName "#") && (arrow == TH.mkName "|>") && (get == TH.mkName "get") =
        (TH.UInfixE (patchExpr a) (TH.VarE arrow) (TH.AppE (TH.VarE get) fromLabel))
    where
            fromLabel = TH.AppTypeE (TH.VarE (TH.mkName "fromLabel")) (TH.LitT (TH.StrTyLit (show labelValue)))
-- UInfixE (UInfixE a (VarE $) (VarE get)) (VarE #) (AppE (VarE id) (VarE checklist))
patchExpr (TH.UInfixE (TH.UInfixE a b get) (TH.VarE hash) (TH.AppE (TH.VarE labelValue) (TH.VarE d))) | (hash == TH.mkName "#") =
        TH.UInfixE (patchExpr a) (patchExpr b) (TH.AppE (TH.AppE get fromLabel) (TH.VarE d))
    where
            fromLabel = TH.AppTypeE (TH.VarE (TH.mkName "fromLabel")) (TH.LitT (TH.StrTyLit (show labelValue)))
patchExpr (TH.UInfixE (TH.VarE varName) (TH.VarE hash) (TH.AppE (TH.VarE labelValue) arg)) | hash == TH.mkName "#" = TH.AppE (TH.AppE (TH.VarE varName) fromLabel) arg
    where
            fromLabel = TH.AppTypeE (TH.VarE (TH.mkName "fromLabel")) (TH.LitT (TH.StrTyLit (show labelValue)))
patchExpr (TH.UInfixE (TH.VarE a) (TH.VarE hash) (TH.AppE (TH.VarE labelValue) (TH.VarE b))) | hash == TH.mkName "#" =
        TH.AppE (TH.AppE (TH.VarE a) fromLabel) (TH.VarE b)
    where
            fromLabel = TH.AppTypeE (TH.VarE (TH.mkName "fromLabel")) (TH.LitT (TH.StrTyLit (show labelValue)))

patchExpr (TH.UInfixE a b c) = TH.UInfixE (patchExpr a) (patchExpr b) (patchExpr c)
patchExpr (TH.ParensE e) = TH.ParensE (patchExpr e)
patchExpr (TH.RecUpdE a b) = TH.RecUpdE (patchExpr a) b
patchExpr (TH.AppE a b) = TH.AppE (patchExpr a) (patchExpr b)
patchExpr (TH.LamE a b) = TH.LamE a (patchExpr b)
patchExpr (TH.LetE a b) = TH.LetE a' (patchExpr b)
    where
        a' = map patchDec a
        patchDec (TH.ValD a (TH.NormalB b) c) = (TH.ValD a (TH.NormalB (patchExpr b)) c)
        patchDec a = a
patchExpr (TH.CondE a b c) = TH.CondE (patchExpr a) (patchExpr b) (patchExpr c)
patchExpr (TH.SigE a b) = TH.SigE (patchExpr a) b
patchExpr e = e

-- UInfixE (VarE get) (VarE #) (AppE (VarE id) (VarE step))

-- UInfixE (UInfixE (UInfixE (UInfixE (UInfixE (UInfixE (VarE currentUser) (VarE |>) (VarE get)) (VarE #) (VarE firstName)) (VarE <>) (LitE (StringL " "))) (VarE <>) (VarE currentUser)) (VarE |>) (VarE get)) (VarE #) (VarE lastName)
-- UInfixE (UInfixE (VarE tshow) (VarE $) (VarE get)) (VarE #) (AppE (VarE id) (VarE checklist))


toStringAttribute :: (String, AttributeValue) -> TH.ExpQ
toStringAttribute (name, TextValue value) = do
    let nameWithSuffix = " " <> name <> "=\""
    if name `elem` attributes || ("data-" `isPrefixOf` name) then return () else fail ("Invalid attribute: " <> name)
    [| (attribute name nameWithSuffix) value |]

toStringAttribute (name, ExpressionValue code) = do
    let nameWithSuffix = " " <> name <> "=\""
    if name `elem` attributes || ("data-" `isPrefixOf` name) then return () else fail ("Invalid attribute: " <> name)
    case parseExp code of
        Right expression -> let patched = Debug.Trace.traceShow (patchExpr expression) (patchExpr expression) in [| (attribute name nameWithSuffix) (cs $(return patched)) |]
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
            let children' = case length children of
                    0 -> mempty
                    1 -> unsafeHead children
                    _ -> (foldl' (<>) (unsafeHead children) (unsafeTail children))
            in element children'
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
        , "allowtransparency", "minlength", "maxlength", "property"
        , "role"
        , "d", "viewBox", "fill", "cx", "cy", "r", "x", "y", "text-anchor", "alignment-baseline"
        , "line-spacing", "letter-spacing"
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
        , "svg", "path", "text", "circle"
        ]

leafs =
        [ "area", "br", "col", "hr", "link", "img", "input",  "meta", "param"
        ]
