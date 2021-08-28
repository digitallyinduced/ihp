module IHP.HSX.Parser
( parseHsx
, Node (..)
, Attribute (..)
, AttributeValue (..)
, collapseSpace
) where

import CorePrelude
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Char as Char
import qualified Data.Text as Text
import Control.Monad.Fail
import Data.String.Conversions
import qualified Data.List as List
import Control.Monad (unless)
import Prelude (show)
import qualified Language.Haskell.Meta as Haskell
import qualified Language.Haskell.TH.Syntax as Haskell
import qualified "template-haskell" Language.Haskell.TH as TH
import qualified Data.Set as Set

data AttributeValue = TextValue !Text | ExpressionValue !Haskell.Exp deriving (Eq, Show)

data Attribute = StaticAttribute !Text !AttributeValue | SpreadAttributes !Haskell.Exp deriving (Eq, Show)

data Node = Node !Text ![Attribute] ![Node] !Bool
    | TextNode !Text
    | PreEscapedTextNode !Text -- ^ Used in @script@ or @style@ bodies
    | SplicedNode !Haskell.Exp -- ^ Inline haskell expressions like @{myVar}@ or @{f "hello"}@
    | Children ![Node]
    | CommentNode !Text
    deriving (Eq, Show)

-- | Parses a HSX text and returns a 'Node'
--
-- __Example:__
--
-- > let filePath = "my-template"
-- > let line = 0
-- > let col = 0
-- > let position = Megaparsec.SourcePos filePath (Megaparsec.mkPos line) (Megaparsec.mkPos col)
-- > let hsxText = "<strong>Hello</strong>"
-- >
-- > let (Right node) = parseHsx position hsxText
parseHsx :: SourcePos -> Text -> Either (ParseErrorBundle Text Void) Node
parseHsx position code = runParser (setPosition position *> parser) "" code

type Parser = Parsec Void Text

setPosition pstateSourcePos = updateParserState (\state -> state {
        statePosState = (statePosState state) { pstateSourcePos }
    })

parser :: Parser Node
parser = do
    space
    node <- manyHsxElement <|> hsxElement
    space
    eof
    pure node

hsxElement = try hsxComment <|> try hsxSelfClosingElement <|> hsxNormalElement

manyHsxElement = do
    children <- many hsxChild
    pure (Children (stripTextNodeWhitespaces children))

hsxSelfClosingElement = do
    _ <- char '<'
    name <- hsxElementName
    let isLeaf = name `Set.member` leafs
    attributes <-
      if isLeaf
        then hsxNodeAttributes (string ">" <|> string "/>")
        else hsxNodeAttributes (string "/>")
    space
    pure (Node name attributes [] isLeaf)

hsxNormalElement = do
    (name, attributes) <- hsxOpeningElement
    let parsePreEscapedTextChildren transformText = do
                    let closingElement = "</" <> name <> ">"
                    text <- cs <$> manyTill anySingle (string closingElement)
                    pure [PreEscapedTextNode (transformText text)]
    let parseNormalHSXChildren = stripTextNodeWhitespaces <$> (space >> (manyTill (try hsxChild) (try (space >> hsxClosingElement name))))

    -- script and style tags have special handling for their children. Inside those tags
    -- we allow any kind of content. Using a haskell expression like @<script>{myHaskellExpr}</script>@
    -- will just literally output the string @{myHaskellExpr}@ without evaluating the haskell expression itself.
    --
    -- Here is an example HSX code explaining the problem:
    -- [hsx|<style>h1 { color: red; }</style>|]
    -- The @{ color: red; }@ would be parsed as a inline haskell expression without the special handling
    --
    -- Additionally we don't do the usual escaping for style and script bodies, as this will make e.g. the
    -- javascript unusuable.
    children <- case name of
            "script" -> parsePreEscapedTextChildren Text.strip
            "style" -> parsePreEscapedTextChildren (collapseSpace . Text.strip)
            otherwise -> parseNormalHSXChildren
    pure (Node name attributes children False)

hsxOpeningElement = do
    char '<'
    name <- hsxElementName
    space
    attributes <- hsxNodeAttributes (char '>')
    pure (name, attributes)

hsxComment :: Parser Node
hsxComment = do
    string "<!--"
    body :: String <- manyTill (satisfy (const True)) (string "-->")
    space
    pure (CommentNode (cs body))


hsxNodeAttributes :: Parser a -> Parser [Attribute]
hsxNodeAttributes end = staticAttributes
    where
        staticAttributes = do
            attributes <- manyTill (hsxNodeAttribute <|> hsxSplicedAttributes) end
            let staticAttributes = List.filter isStaticAttribute attributes
            let keys = List.map (\(StaticAttribute name _) -> name) staticAttributes
            let uniqueKeys = List.nub keys
            unless (keys == uniqueKeys) (fail $ "Duplicate attribute found in tag: " <> show (keys List.\\ uniqueKeys))
            pure attributes

isStaticAttribute (StaticAttribute _ _) = True
isStaticAttribute _ = False

hsxSplicedAttributes :: Parser Attribute
hsxSplicedAttributes = do
    name <- between (string "{...") (string "}") (takeWhile1P Nothing (\c -> c /= '}'))
    space
    haskellExpression <- case Haskell.parseExp (cs name) of
            Right expression -> pure (patchExpr expression)
            Left error -> fail (show error)
    pure (SpreadAttributes haskellExpression)

hsxNodeAttribute = do
    key <- hsxAttributeName
    space

    -- Boolean attributes like <input disabled/> will be represented as <input disabled="disabled"/>
    -- as there is currently no other way to represent them with blaze-html.
    --
    -- There's a special case for data attributes: Data attributes like <form data-disable-javascript-submission/> will be represented as <form data-disable-javascript-submission="true"/>
    --
    -- See: https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#boolean-attributes
    let attributeWithoutValue = do
            let value = if "data-" `Text.isPrefixOf` key
                    then "true"
                    else key
            pure (StaticAttribute key (TextValue value))

    -- Parsing normal attributes like <input value="Hello"/>
    let attributeWithValue = do
            _ <- char '='
            space
            value <- hsxQuotedValue <|> hsxSplicedValue
            space
            pure (StaticAttribute key value)

    attributeWithValue <|> attributeWithoutValue


hsxAttributeName :: Parser Text
hsxAttributeName = do
        name <- rawAttribute
        unless (isValidAttributeName name) (fail $ "Invalid attribute name: " <> cs name)
        pure name
    where
        isValidAttributeName name =
            "data-" `Text.isPrefixOf` name
            || "aria-" `Text.isPrefixOf` name
            || "hx-" `Text.isPrefixOf` name
            || "hx-" `Text.isPrefixOf` name
            || name `Set.member` attributes

        rawAttribute = takeWhile1P Nothing (\c -> Char.isAlphaNum c || c == '-')


hsxQuotedValue :: Parser AttributeValue
hsxQuotedValue = do
    value <- between (char '"') (char '"') (takeWhileP Nothing (\c -> c /= '\"'))
    pure (TextValue value)

hsxSplicedValue :: Parser AttributeValue
hsxSplicedValue = do
    value <- between (char '{') (char '}') (takeWhile1P Nothing (\c -> c /= '}'))
    haskellExpression <- case Haskell.parseExp (cs value) of
            Right expression -> pure (patchExpr expression)
            Left error -> fail (show error)
    pure (ExpressionValue haskellExpression)

hsxClosingElement name = (hsxClosingElement' name) <?> friendlyErrorMessage
    where
        friendlyErrorMessage = show (Text.unpack ("</" <> name <> ">"))
        hsxClosingElement' name = do
            _ <- string ("</" <> name)
            space
            char ('>')
            pure ()

hsxChild = hsxElement <|> hsxSplicedNode <|> try (space >> hsxElement) <|> hsxText

-- | Parses a hsx text node
--
-- Stops parsing when hitting a variable, like `{myVar}`
hsxText :: Parser Node
hsxText = buildTextNode <$> takeWhile1P (Just "text") (\c -> c /= '{' && c /= '}' && c /= '<' && c /= '>')

-- | Builds a TextNode and strips all surround whitespace from the input string
buildTextNode :: Text -> Node
buildTextNode value = TextNode (collapseSpace value)

data TokenTree = TokenLeaf Text | TokenNode [TokenTree] deriving (Show)

hsxSplicedNode :: Parser Node
hsxSplicedNode = do
        expression <- doParse
        haskellExpression <- case Haskell.parseExp (cs expression) of
                Right expression -> pure (patchExpr expression)
                Left error -> fail (show error)
        pure (SplicedNode haskellExpression)
    where
        doParse = do
            tree <- node
            let value = (treeToString "" tree)
            pure $ Text.init $ Text.tail value

        parseTree = node <|> leaf
        node = TokenNode <$> between (char '{') (char '}') (many parseTree)
        leaf = TokenLeaf <$> takeWhile1P Nothing (\c -> c /= '{' && c /= '}')
        treeToString :: Text -> TokenTree -> Text
        treeToString acc (TokenLeaf value)  = acc <> value
        treeToString acc (TokenNode [])     = acc
        treeToString acc (TokenNode (x:xs)) = ((treeToString (acc <> "{") x) <> (Text.concat $ fmap (treeToString "") xs)) <> "}"


hsxElementName :: Parser Text
hsxElementName = do
    name <- takeWhile1P (Just "identifier") (\c -> Char.isAlphaNum c || c == '_' || c == '-')
    let isValidParent = name `Set.member` parents
    let isValidLeaf = name `Set.member` leafs
    let isValidCustomWebComponent = "-" `Text.isInfixOf` name
    unless (isValidParent || isValidLeaf || isValidCustomWebComponent) (fail $ "Invalid tag name: " <> cs name)
    space
    pure name

hsxIdentifier :: Parser Text
hsxIdentifier = do
    name <- takeWhile1P (Just "identifier") (\c -> Char.isAlphaNum c || c == '_')
    space
    pure name


attributes :: Set Text
attributes = Set.fromList
        [ "accept", "accept-charset", "accesskey", "action", "alt", "async"
        , "autocomplete", "autofocus", "autoplay", "challenge", "charset"
        , "checked", "cite", "class", "cols", "colspan", "content"
        , "contenteditable", "contextmenu", "controls", "coords", "data"
        , "datetime", "defer", "dir", "disabled", "draggable", "enctype"
        , "form", "formaction", "formenctype", "formmethod", "formnovalidate"
        , "for"
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
        , "d", "viewBox", "cx", "cy", "r", "x", "y", "text-anchor", "alignment-baseline"
        , "line-spacing", "letter-spacing"
        , "integrity", "crossorigin", "poster"
        , "accent-height", "accumulate", "additive", "alphabetic", "amplitude"
        , "arabic-form", "ascent", "attributeName", "attributeType", "azimuth"
        , "baseFrequency", "baseProfile", "bbox", "begin", "bias", "by", "calcMode"
        , "cap-height", "class", "clipPathUnits", "contentScriptType"
        , "contentStyleType", "cx", "cy", "d", "descent", "diffuseConstant", "divisor"
        , "dur", "dx", "dy", "edgeMode", "elevation", "end", "exponent"
        , "externalResourcesRequired", "filterRes", "filterUnits", "font-family"
        , "font-size", "font-stretch", "font-style", "font-variant", "font-weight"
        , "format", "from", "fx", "fy", "g1", "g2", "glyph-name", "glyphRef"
        , "gradientTransform", "gradientUnits", "hanging", "height", "horiz-adv-x"
        , "horiz-origin-x", "horiz-origin-y", "id", "ideographic", "in", "in2"
        , "intercept", "k", "k1", "k2", "k3", "k4", "kernelMatrix", "kernelUnitLength"
        , "keyPoints", "keySplines", "keyTimes", "lang", "lengthAdjust"
        , "limitingConeAngle", "local", "markerHeight", "markerUnits", "markerWidth"
        , "maskContentUnits", "maskUnits", "mathematical", "max", "media", "method"
        , "min", "mode", "name", "numOctaves", "offset", "onabort", "onactivate"
        , "onbegin", "onclick", "onend", "onerror", "onfocusin", "onfocusout", "onload"
        , "onmousedown", "onmousemove", "onmouseout", "onmouseover", "onmouseup"
        , "onrepeat", "onresize", "onscroll", "onunload", "onzoom", "operator", "order"
        , "orient", "orientation", "origin", "overline-position", "overline-thickness"
        , "panose-1", "path", "pathLength", "patternContentUnits", "patternTransform"
        , "patternUnits", "points", "pointsAtX", "pointsAtY", "pointsAtZ"
        , "preserveAlpha", "preserveAspectRatio", "primitiveUnits", "r", "radius"
        , "refX", "refY", "rendering-intent", "repeatCount", "repeatDur"
        , "requiredExtensions", "requiredFeatures", "restart", "result", "rotate", "rx"
        , "ry", "scale", "seed", "slope", "spacing", "specularConstant"
        , "specularExponent", "spreadMethod", "startOffset", "stdDeviation", "stemh"
        , "stemv", "stitchTiles", "strikethrough-position", "strikethrough-thickness"
        , "string", "style", "surfaceScale", "systemLanguage", "tableValues", "target"
        , "targetX", "targetY", "textLength", "title", "to", "transform", "type", "u1"
        , "u2", "underline-position", "underline-thickness", "unicode", "unicode-range"
        , "units-per-em", "v-alphabetic", "v-hanging", "v-ideographic", "v-mathematical"
        , "values", "version", "vert-adv-y", "vert-origin-x", "vert-origin-y", "viewBox"
        , "viewTarget", "width", "widths", "x", "x-height", "x1", "x2"
        , "xChannelSelector", "xlink:actuate", "xlink:arcrole", "xlink:href"
        , "xlink:role", "xlink:show", "xlink:title", "xlink:type", "xml:base"
        , "xml:lang", "xml:space", "y", "y1", "y2", "yChannelSelector", "z", "zoomAndPan"
        , "alignment-baseline", "baseline-shift", "clip-path", "clip-rule"
        , "clip", "color-interpolation-filters", "color-interpolation"
        , "color-profile", "color-rendering", "color", "cursor", "direction"
        , "display", "dominant-baseline", "enable-background", "fill-opacity"
        , "fill-rule", "fill", "filter", "flood-color", "flood-opacity"
        , "font-size-adjust", "glyph-orientation-horizontal"
        , "glyph-orientation-vertical", "image-rendering", "kerning", "letter-spacing"
        , "lighting-color", "marker-end", "marker-mid", "marker-start", "mask"
        , "opacity", "overflow", "pointer-events", "shape-rendering", "stop-color"
        , "stop-opacity", "stroke-dasharray", "stroke-dashoffset", "stroke-linecap"
        , "stroke-linejoin", "stroke-miterlimit", "stroke-opacity", "stroke-width"
        , "stroke", "text-anchor", "text-decoration", "text-rendering", "unicode-bidi"
        , "visibility", "word-spacing", "writing-mode", "is"
        , "cellspacing", "cellpadding", "bgcolor", "classes"
        , "loading"
        ]

parents :: Set Text
parents = Set.fromList
        [ "a", "abbr", "address", "article", "aside", "audio", "b"
        , "bdo", "blockquote", "body", "button", "canvas", "caption", "cite"
        , "code", "colgroup", "command", "datalist", "dd", "del", "details"
        , "dfn", "div", "dl", "dt", "em", "fieldset", "figcaption", "figure"
        , "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header"
        , "hgroup", "html", "i", "iframe", "ins", "ion-icon", "kbd", "label"
        , "legend", "li", "main", "map", "mark", "menu", "meter", "nav"
        , "noscript", "object", "ol", "optgroup", "option", "output", "p"
        , "pre", "progress", "q", "rp", "rt", "ruby", "samp", "script"
        , "section", "select", "small", "span", "strong", "style", "sub"
        , "summary", "sup", "table", "tbody", "td", "textarea", "tfoot", "th"
        , "thead", "time", "title", "tr", "u", "ul", "var", "video"
        , "svg", "path", "text", "circle", "marquee", "blink"
        , "loading", "animate", "animateMotion", "animateTransform"
        , "clipPath", "defs", "desc", "discard", "ellipse", "feBlend"
        , "feColorMatrix", "feComponentTransfer", "feComposite"
        , "feConvolveMatrix", "feDiffuseLighting", "feDisplacementMap"
        , "feDistantLight", "feDropShadow", "feFlood", "feFuncA", "feFuncB"
        , "feFuncG", "feFuncR", "feGaussianBlur", "feImage", "feMerge"
        , "feMergeNode", "feMorphology", "feOffset", "fePointLight"
        , "feSpecularLighting", "feSpotLight", "feTile", "feTurbulence"
        , "filter", "foreignObject", "g", "line", "linearGradient", "marker"
        , "mask", "metadata", "mpath", "path", "pattern", "polygon", "polyline"
        , "radialGradient", "rect", "set", "stop", "switch", "symbol"
        , "textPath", "tspan", "unknown", "use", "view", "template"
        ]

leafs :: Set Text
leafs = Set.fromList
        [ "area", "br", "col", "hr", "link", "img", "input",  "meta", "param"
        ]

stripTextNodeWhitespaces nodes = stripLastTextNodeWhitespaces (stripFirstTextNodeWhitespaces nodes)

stripLastTextNodeWhitespaces nodes =
    let strippedLastElement = if List.length nodes > 0
            then case List.last nodes of
                TextNode text -> Just $ TextNode (Text.stripEnd text)
                otherwise -> Nothing
            else Nothing
    in case strippedLastElement of
        Just last -> (fst $ List.splitAt ((List.length nodes) - 1) nodes) <> [last]
        Nothing -> nodes

stripFirstTextNodeWhitespaces nodes =
    let strippedFirstElement = if List.length nodes > 0
            then case List.head nodes of
                TextNode text -> Just $ TextNode (Text.stripStart text)
                otherwise -> Nothing
            else Nothing
    in case strippedFirstElement of
        Just first -> first:(List.tail nodes)
        Nothing -> nodes

-- | Replaces multiple space characters with a single one
collapseSpace :: Text -> Text
collapseSpace text = cs $ filterDuplicateSpaces (cs text)
    where
        filterDuplicateSpaces :: String -> String
        filterDuplicateSpaces string = filterDuplicateSpaces' string False

        filterDuplicateSpaces' :: String -> Bool -> String
        filterDuplicateSpaces' (char:rest) True | Char.isSpace char = filterDuplicateSpaces' rest True
        filterDuplicateSpaces' (char:rest) False | Char.isSpace char = ' ':(filterDuplicateSpaces' rest True)
        filterDuplicateSpaces' (char:rest) isRemovingSpaces = char:(filterDuplicateSpaces' rest False)
        filterDuplicateSpaces' [] isRemovingSpaces = []


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
        a' = List.map patchDec a
        patchDec (TH.ValD a (TH.NormalB b) c) = (TH.ValD a (TH.NormalB (patchExpr b)) c)
        patchDec a = a
patchExpr (TH.CondE a b c) = TH.CondE (patchExpr a) (patchExpr b) (patchExpr c)
patchExpr (TH.SigE a b) = TH.SigE (patchExpr a) b
patchExpr e = e
