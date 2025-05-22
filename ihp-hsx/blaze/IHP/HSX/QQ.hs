{-# LANGUAGE TemplateHaskell, UndecidableInstances, BangPatterns, PackageImports, FlexibleInstances, OverloadedStrings #-}

{-|
Module: IHP.HSX.QQ
Description: Defines the @[hsx||]@ syntax
Copyright: (c) digitally induced GmbH, 2022
-}
module IHP.HSX.QQ
  ( hsx
  , uncheckedHsx
  , customHsx
  , quoteHsxExpression
  ) where

import           Prelude
import Data.Text (Text)
import           IHP.HSX.Parser
import qualified "template-haskell" Language.Haskell.TH           as TH
import qualified "template-haskell" Language.Haskell.TH.Syntax           as TH
import           Language.Haskell.TH.Quote
import           Text.Blaze.Html5              ((!))
import qualified Text.Blaze.Html5              as Html5
import Text.Blaze.Html (Html)
import Text.Blaze.Internal (attribute, MarkupM (Parent, Leaf), StaticString (..))
import Data.String.Conversions
import IHP.HSX.ToHtml
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Blaze.Html.Renderer.String as BlazeString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.List (foldl')
import IHP.HSX.Attribute
import qualified Text.Blaze.Html5.Attributes as Attributes
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set

hsx :: QuasiQuoter
hsx = customHsx 
        (HsxSettings 
            { checkMarkup = True
            , additionalTagNames = Set.empty
            , additionalAttributeNames = Set.empty
            }
        )

uncheckedHsx :: QuasiQuoter
uncheckedHsx = customHsx
        (HsxSettings 
            { checkMarkup = False
            , additionalTagNames = Set.empty
            , additionalAttributeNames = Set.empty
            }
        )

customHsx :: HsxSettings -> QuasiQuoter
customHsx settings = 
    QuasiQuoter 
        { quoteExp = quoteHsxExpression settings
        , quotePat = error "quotePat: not defined"
        , quoteDec = error "quoteDec: not defined"
        , quoteType = error "quoteType: not defined"
        }

quoteHsxExpression :: HsxSettings -> String -> TH.ExpQ
quoteHsxExpression settings code = do
        hsxPosition <- findHSXPosition
        extensions <- TH.extsEnabled
        expression <- case parseHsx settings hsxPosition extensions (cs code) of
                Left error   -> fail (Megaparsec.errorBundlePretty error)
                Right result -> pure result
        compileToHaskell expression
    where

        findHSXPosition = do
            loc <- TH.location
            let (line, col) = TH.loc_start loc
            pure $ Megaparsec.SourcePos (TH.loc_filename loc) (Megaparsec.mkPos line) (Megaparsec.mkPos col)

compileToHaskell :: Node -> TH.ExpQ
compileToHaskell (Node "!DOCTYPE" [StaticAttribute "html" (TextValue "html")] [] True) = [| Html5.docType |]
compileToHaskell (Node name attributes children isLeaf) =
    let
        renderedChildren = TH.listE $ map compileToHaskell children
        stringAttributes = TH.listE $ map toStringAttribute attributes
    in
        if isLeaf
            then
                let
                    element = nodeToBlazeLeaf name
                in
                    [| applyAttributes $element $stringAttributes |]
            else
                let
                    element = nodeToBlazeElement name
                in [| applyAttributes ($element (mconcat $renderedChildren)) $stringAttributes |]
compileToHaskell (Children children) =
    let
        renderedChildren = TH.listE $ map compileToHaskell children
    in [| mconcat $(renderedChildren) |]

compileToHaskell (TextNode value) = [| Html5.preEscapedText value |]
compileToHaskell (PreEscapedTextNode value) = [| Html5.preEscapedText value |]
compileToHaskell (SplicedNode expression) = [| toHtml $(pure expression) |]
compileToHaskell (CommentNode value) = [| Html5.textComment value |]
compileToHaskell (NoRenderCommentNode) = [| mempty |]

nodeToBlazeElement :: Text -> TH.Q TH.Exp
nodeToBlazeElement name =
    HashMap.findWithDefault (nodeToBlazeElementGeneric name) name knownElements

knownElements :: HashMap.HashMap Text TH.ExpQ
knownElements =
    HashMap.fromList
        [ ("a", [| Html5.a |])
        , ("abbr", [| Html5.abbr |])
        , ("address", [| Html5.address |])
        , ("article", [| Html5.article |])
        , ("aside", [| Html5.aside |])
        , ("audio", [| Html5.audio |])
        , ("b", [| Html5.b |])
        , ("blockquote", [| Html5.blockquote |])
        , ("body", [| Html5.body |])
        , ("button", [| Html5.button |])
        , ("canvas", [| Html5.canvas |])
        , ("caption", [| Html5.caption |])
        , ("cite", [| Html5.cite |])
        , ("code", [| Html5.code |])
        , ("colgroup", [| Html5.colgroup |])
        , ("datalist", [| Html5.datalist |])
        , ("dd", [| Html5.dd |])
        , ("del", [| Html5.del |])
        , ("details", [| Html5.details |])
        , ("dfn", [| Html5.dfn |])
        , ("div", [| Html5.div |])
        , ("dl", [| Html5.dl |])
        , ("dt", [| Html5.dt |])
        , ("em", [| Html5.em |])
        , ("fieldset", [| Html5.fieldset |])
        , ("figcaption", [| Html5.figcaption |])
        , ("figure", [| Html5.figure |])
        , ("footer", [| Html5.footer |])
        , ("form", [| Html5.form |])
        , ("h1", [| Html5.h1 |])
        , ("h2", [| Html5.h2 |])
        , ("h3", [| Html5.h3 |])
        , ("h4", [| Html5.h4 |])
        , ("h5", [| Html5.h5 |])
        , ("h6", [| Html5.h6 |])
        , ("head", [| Html5.head |])
        , ("header", [| Html5.header |])
        , ("hgroup", [| Html5.hgroup |])
        , ("html", [| Html5.html |])
        , ("i", [| Html5.i |])
        , ("iframe", [| Html5.iframe |])
        , ("ins", [| Html5.ins |])
        , ("kbd", [| Html5.kbd |])
        , ("label", [| Html5.label |])
        , ("legend", [| Html5.legend |])
        , ("li", [| Html5.li |])
        , ("main", [| Html5.main |])
        , ("map", [| Html5.map |])
        , ("mark", [| Html5.mark |])
        , ("menu", [| Html5.menu |])
        , ("menuitem", [| Html5.menuitem |])
        , ("meter", [| Html5.meter |])
        , ("nav", [| Html5.nav |])
        , ("noscript", [| Html5.noscript |])
        , ("object", [| Html5.object |])
        , ("ol", [| Html5.ol |])
        , ("optgroup", [| Html5.optgroup |])
        , ("option", [| Html5.option |])
        , ("output", [| Html5.output |])
        , ("p", [| Html5.p |])
        , ("pre", [| Html5.pre |])
        , ("progress", [| Html5.progress |])
        , ("q", [| Html5.q |])
        , ("rp", [| Html5.rp |])
        , ("rt", [| Html5.rt |])
        , ("ruby", [| Html5.ruby |])
        , ("s", [| Html5.s |])
        , ("samp", [| Html5.samp |])
        , ("script", [| Html5.script |])
        , ("section", [| Html5.section |])
        , ("select", [| Html5.select |])
        , ("small", [| Html5.small |])
        , ("span", [| Html5.span |])
        , ("strong", [| Html5.strong |])
        , ("style", [| Html5.style |])
        , ("sub", [| Html5.sub |])
        , ("summary", [| Html5.summary |])
        , ("sup", [| Html5.sup |])
        , ("table", [| Html5.table |])
        , ("tbody", [| Html5.tbody |])
        , ("td", [| Html5.td |])
        , ("textarea", [| Html5.textarea |])
        , ("tfoot", [| Html5.tfoot |])
        , ("th", [| Html5.th |])
        , ("thead", [| Html5.thead |])
        , ("time", [| Html5.time |])
        , ("title", [| Html5.title |])
        , ("tr", [| Html5.tr |])
        , ("u", [| Html5.u |])
        , ("ul", [| Html5.ul |])
        , ("var", [| Html5.var |])
        , ("video", [| Html5.video |])
        ]

nodeToBlazeLeaf :: Text -> TH.Q TH.Exp
nodeToBlazeLeaf name =
    HashMap.findWithDefault (nodeToBlazeLeafGeneric name) name knownLeafs

knownLeafs :: HashMap.HashMap Text TH.ExpQ
knownLeafs =
    HashMap.fromList
        [ ("area", [| Html5.area |])
        , ("base", [| Html5.base |])
        , ("br", [| Html5.br |])
        , ("col", [| Html5.col |])
        , ("embed", [| Html5.embed |])
        , ("hr", [| Html5.hr |])
        , ("img", [| Html5.img |])
        , ("input", [| Html5.input |])
        , ("keygen", [| Html5.keygen |])
        , ("link", [| Html5.link |])
        , ("meta", [| Html5.meta |])
        , ("param", [| Html5.param |])
        , ("source", [| Html5.source |])
        , ("track", [| Html5.track |])
        , ("wbr", [| Html5.wbr |])
        ]

nodeToBlazeElementGeneric :: Text -> TH.Q TH.Exp
nodeToBlazeElementGeneric name =
    let
        openTag :: Text
        openTag = "<" <> tag
        
        tag :: Text
        tag = cs name

        closeTag :: Text
        closeTag = "</" <> tag <> ">"
    in
        [| makeParent (textToStaticString $(TH.lift name)) (textToStaticString $(TH.lift openTag)) (textToStaticString $(TH.lift closeTag)) |]

nodeToBlazeLeafGeneric :: Text -> TH.Q TH.Exp
nodeToBlazeLeafGeneric name =
    let
        openTag :: Text
        openTag = "<" <> tag

        closeTag :: Text
        closeTag = ">"
        
        tag :: Text
        tag = cs name
    in
        [| (Leaf (textToStaticString $(TH.lift tag)) (textToStaticString $(TH.lift openTag)) (textToStaticString $(TH.lift closeTag)) ()) |]

toStringAttribute :: Attribute -> TH.ExpQ
toStringAttribute (StaticAttribute name (TextValue value)) =
    attributeFromName name value

toStringAttribute (StaticAttribute name (ExpressionValue expression)) = let nameWithSuffix = " " <> name <> "=\"" in [| applyAttribute name nameWithSuffix $(pure expression) |]
toStringAttribute (SpreadAttributes expression) = [| spreadAttributes $(pure expression) |]

attributeFromName :: Text -> Text -> TH.ExpQ
attributeFromName name value =
    let
        value' :: TH.ExpQ
        value' = if Text.null value then [| mempty |] else [| Html5.preEscapedTextValue value |] 

        attr = attributeFromName' name
    in
        [| (! $attr $value') |]

attributeFromName' :: Text -> TH.ExpQ
attributeFromName' name =
    HashMap.findWithDefault (attributeFromNameGeneric name) name knownAttributes

knownAttributes :: HashMap.HashMap Text TH.ExpQ
knownAttributes =
    HashMap.fromList
        [ ("accept", [| Attributes.accept |])
        , ( "accept-charset", [| Attributes.acceptCharset |])
        , ( "accesskey", [| Attributes.accesskey |])
        , ( "action", [| Attributes.action |])
        , ( "alt", [| Attributes.alt |])
        , ( "async", [| Attributes.async |])
        , ( "autocomplete", [| Attributes.autocomplete |])
        , ( "autofocus", [| Attributes.autofocus |])
        , ( "autoplay", [| Attributes.autoplay |])
        , ( "challenge", [| Attributes.challenge |])
        , ( "charset", [| Attributes.charset |])
        , ( "checked", [| Attributes.checked |])
        , ( "cite", [| Attributes.cite |])
        , ( "class", [| Attributes.class_ |])
        , ( "cols", [| Attributes.cols |])
        , ( "colspan", [| Attributes.colspan |])
        , ( "content", [| Attributes.content |])
        , ( "contenteditable", [| Attributes.contenteditable |])
        , ( "contextmenu", [| Attributes.contextmenu |])
        , ( "controls", [| Attributes.controls |])
        , ( "coords", [| Attributes.coords |])
        , ( "data", [| Attributes.data_ |])
        , ( "datetime", [| Attributes.datetime |])
        , ( "defer", [| Attributes.defer |])
        , ( "dir", [| Attributes.dir |])
        , ( "disabled", [| Attributes.disabled |])
        , ( "download", [| Attributes.download |])
        , ( "draggable", [| Attributes.draggable |])
        , ( "enctype", [| Attributes.enctype |])
        , ( "for", [| Attributes.for |])
        , ( "form", [| Attributes.form |])
        , ( "formaction", [| Attributes.formaction |])
        , ( "formenctype", [| Attributes.formenctype |])
        , ( "formmethod", [| Attributes.formmethod |])
        , ( "formnovalidate", [| Attributes.formnovalidate |])
        , ( "formtarget", [| Attributes.formtarget |])
        , ( "headers", [| Attributes.headers |])
        , ( "height", [| Attributes.height |])
        , ( "hidden", [| Attributes.hidden |])
        , ( "high", [| Attributes.high |])
        , ( "href", [| Attributes.href |])
        , ( "hreflang", [| Attributes.hreflang |])
        , ( "http-equiv", [| Attributes.httpEquiv |])
        , ( "icon", [| Attributes.icon |])
        , ( "id", [| Attributes.id |])
        , ( "ismap", [| Attributes.ismap |])
        , ( "item", [| Attributes.item |])
        , ( "itemprop", [| Attributes.itemprop |])
        , ( "itemscope", [| Attributes.itemscope |])
        , ( "itemtype", [| Attributes.itemtype |])
        , ( "keytype", [| Attributes.keytype |])
        , ( "label", [| Attributes.label |])
        , ( "lang", [| Attributes.lang |])
        , ( "list", [| Attributes.list |])
        , ( "loop", [| Attributes.loop |])
        , ( "low", [| Attributes.low |])
        , ( "manifest", [| Attributes.manifest |])
        , ( "max", [| Attributes.max |])
        , ( "maxlength", [| Attributes.maxlength |])
        , ( "media", [| Attributes.media |])
        , ( "method", [| Attributes.method |])
        , ( "min", [| Attributes.min |])
        , ( "minlength", [| Attributes.minlength |])
        , ( "multiple", [| Attributes.multiple |])
        , ( "muted", [| Attributes.muted |])
        , ( "name", [| Attributes.name |])
        , ( "novalidate", [| Attributes.novalidate |])
        , ( "onbeforeonload", [| Attributes.onbeforeonload |])
        , ( "onbeforeprint", [| Attributes.onbeforeprint |])
        , ( "onblur", [| Attributes.onblur |])
        , ( "oncanplay", [| Attributes.oncanplay |])
        , ( "oncanplaythrough", [| Attributes.oncanplaythrough |])
        , ( "onchange", [| Attributes.onchange |])
        , ( "onclick", [| Attributes.onclick |])
        , ( "oncontextmenu", [| Attributes.oncontextmenu |])
        , ( "ondblclick", [| Attributes.ondblclick |])
        , ( "ondrag", [| Attributes.ondrag |])
        , ( "ondragend", [| Attributes.ondragend |])
        , ( "ondragenter", [| Attributes.ondragenter |])
        , ( "ondragleave", [| Attributes.ondragleave |])
        , ( "ondragover", [| Attributes.ondragover |])
        , ( "ondragstart", [| Attributes.ondragstart |])
        , ( "ondrop", [| Attributes.ondrop |])
        , ( "ondurationchange", [| Attributes.ondurationchange |])
        , ( "onemptied", [| Attributes.onemptied |])
        , ( "onended", [| Attributes.onended |])
        , ( "onerror", [| Attributes.onerror |])
        , ( "onfocus", [| Attributes.onfocus |])
        , ( "onformchange", [| Attributes.onformchange |])
        , ( "onforminput", [| Attributes.onforminput |])
        , ( "onhaschange", [| Attributes.onhaschange |])
        , ( "oninput", [| Attributes.oninput |])
        , ( "oninvalid", [| Attributes.oninvalid |])
        , ( "onkeydown", [| Attributes.onkeydown |])
        , ( "onkeypress", [| Attributes.onkeypress |])
        , ( "onkeyup", [| Attributes.onkeyup |])
        , ( "onload", [| Attributes.onload |])
        , ( "onloadeddata", [| Attributes.onloadeddata |])
        , ( "onloadedmetadata", [| Attributes.onloadedmetadata |])
        , ( "onloadstart", [| Attributes.onloadstart |])
        , ( "onmessage", [| Attributes.onmessage |])
        , ( "onmousedown", [| Attributes.onmousedown |])
        , ( "onmousemove", [| Attributes.onmousemove |])
        , ( "onmouseout", [| Attributes.onmouseout |])
        , ( "onmouseover", [| Attributes.onmouseover |])
        , ( "onmouseup", [| Attributes.onmouseup |])
        , ( "onmousewheel", [| Attributes.onmousewheel |])
        , ( "ononline", [| Attributes.ononline |])
        , ( "onpagehide", [| Attributes.onpagehide |])
        , ( "onpageshow", [| Attributes.onpageshow |])
        , ( "onpause", [| Attributes.onpause |])
        , ( "onplay", [| Attributes.onplay |])
        , ( "onplaying", [| Attributes.onplaying |])
        , ( "onprogress", [| Attributes.onprogress |])
        , ( "onpropstate", [| Attributes.onpropstate |])
        , ( "onratechange", [| Attributes.onratechange |])
        , ( "onreadystatechange", [| Attributes.onreadystatechange |])
        , ( "onredo", [| Attributes.onredo |])
        , ( "onresize", [| Attributes.onresize |])
        , ( "onscroll", [| Attributes.onscroll |])
        , ( "onseeked", [| Attributes.onseeked |])
        , ( "onseeking", [| Attributes.onseeking |])
        , ( "onselect", [| Attributes.onselect |])
        , ( "onstalled", [| Attributes.onstalled |])
        , ( "onstorage", [| Attributes.onstorage |])
        , ( "onsubmit", [| Attributes.onsubmit |])
        , ( "onsuspend", [| Attributes.onsuspend |])
        , ( "ontimeupdate", [| Attributes.ontimeupdate |])
        , ( "onundo", [| Attributes.onundo |])
        , ( "onunload", [| Attributes.onunload |])
        , ( "onvolumechange", [| Attributes.onvolumechange |])
        , ( "onwaiting", [| Attributes.onwaiting |])
        , ( "open", [| Attributes.open |])
        , ( "optimum", [| Attributes.optimum |])
        , ( "pattern", [| Attributes.pattern |])
        , ( "ping", [| Attributes.ping |])
        , ( "placeholder", [| Attributes.placeholder |])
        , ( "poster", [| Attributes.poster |])
        , ( "preload", [| Attributes.preload |])
        , ( "property", [| Attributes.property |])
        , ( "pubdate", [| Attributes.pubdate |])
        , ( "radiogroup", [| Attributes.radiogroup |])
        , ( "readonly", [| Attributes.readonly |])
        , ( "rel", [| Attributes.rel |])
        , ( "required", [| Attributes.required |])
        , ( "reversed", [| Attributes.reversed |])
        , ( "role", [| Attributes.role |])
        , ( "rows", [| Attributes.rows |])
        , ( "rowspan", [| Attributes.rowspan |])
        , ( "sandbox", [| Attributes.sandbox |])
        , ( "scope", [| Attributes.scope |])
        , ( "scoped", [| Attributes.scoped |])
        , ( "seamless", [| Attributes.seamless |])
        , ( "selected", [| Attributes.selected |])
        , ( "shape", [| Attributes.shape |])
        , ( "size", [| Attributes.size |])
        , ( "sizes", [| Attributes.sizes |])
        , ( "span", [| Attributes.span |])
        , ( "spellcheck", [| Attributes.spellcheck |])
        , ( "src", [| Attributes.src |])
        , ( "srcdoc", [| Attributes.srcdoc |])
        , ( "start", [| Attributes.start |])
        , ( "step", [| Attributes.step |])
        , ( "style", [| Attributes.style |])
        , ( "subject", [| Attributes.subject |])
        , ( "summary", [| Attributes.summary |])
        , ( "tabindex", [| Attributes.tabindex |])
        , ( "target", [| Attributes.target |])
        , ( "title", [| Attributes.title |])
        , ( "type", [| Attributes.type_ |])
        , ( "usemap", [| Attributes.usemap |])
        , ( "value", [| Attributes.value |])
        , ( "width", [| Attributes.width |])
        , ( "wrap", [| Attributes.wrap |])
        , ( "xmlns", [| Attributes.xmlns |])
        ]

attributeFromNameGeneric :: Text -> TH.ExpQ
attributeFromNameGeneric name =
    let
        nameWithSuffix = " " <> name <> "=\""
    in
        [| attribute (Html5.textTag name) (Html5.textTag nameWithSuffix) |]

spreadAttributes :: ApplyAttribute value => [(Text, value)] -> Html5.Html -> Html5.Html
spreadAttributes attributes html = applyAttributes html $ map (\(name, value) -> applyAttribute name (" " <> name <> "=\"") value) attributes
{-# INLINE spreadAttributes #-}

applyAttributes :: Html5.Html -> [Html5.Html -> Html5.Html] -> Html5.Html
applyAttributes element (attribute:rest) = applyAttributes (attribute element) rest
applyAttributes element [] = element
{-# INLINE applyAttributes #-}

makeParent :: StaticString -> StaticString -> StaticString -> Html -> Html
makeParent tag openTag closeTag children = Parent tag openTag closeTag children
{-# INLINE makeParent #-}

textToStaticString :: Text -> StaticString
textToStaticString text = StaticString (Text.unpack text ++) (Text.encodeUtf8 text) text
{-# INLINE textToStaticString #-}

instance Show (MarkupM ()) where
    show html = BlazeString.renderHtml html
