{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies  #-}
{-|
Module: IHP.HSX.Parser
Description: Parser for HSX code
Copyright: (c) digitally induced GmbH, 2022
-}
module IHP.HSX.Parser
( parseHsx
, Node (..)
, Attribute (..)
, AttributeValue (..)
, collapseSpace
) where

import Prelude
import Data.Text
import Data.Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Char as Char
import qualified Data.Text as Text
import Data.String.Conversions
import qualified Data.List as List
import Control.Monad (unless)
import qualified "template-haskell" Language.Haskell.TH.Syntax as Haskell
import qualified "template-haskell" Language.Haskell.TH as TH
import qualified Data.Set as Set
import qualified Data.Containers.ListUtils as List
import qualified IHP.HSX.HaskellParser as HaskellParser

data AttributeValue = TextValue !Text | ExpressionValue !Haskell.Exp deriving (Eq, Show)

data Attribute = StaticAttribute !Text !AttributeValue | SpreadAttributes !Haskell.Exp deriving (Eq, Show)

data Node = Node !Text ![Attribute] ![Node] !Bool
    | TextNode !Text
    | PreEscapedTextNode !Text -- ^ Used in @script@ or @style@ bodies
    | SplicedNode !Haskell.Exp -- ^ Inline haskell expressions like @{myVar}@ or @{f "hello"}@
    | Children ![Node]
    | CommentNode !Text -- ^ A Comment that is rendered in the final HTML
    | NoRenderCommentNode -- ^ A comment that is not rendered in the final HTML
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
-- > let (Right node) = parseHsx position [] hsxText
parseHsx :: SourcePos -> [TH.Extension] -> Text -> Either (ParseErrorBundle Text Void) Node
parseHsx position extensions code =
    let
        ?extensions = extensions
    in
        runParser (setPosition position *> parser) "" code

type Parser a = (?extensions :: [TH.Extension]) => Parsec Void Text a

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

hsxElement :: Parser Node
hsxElement = try hsxNoRenderComment <|> try hsxComment <|> try hsxSelfClosingElement <|> hsxNormalElement

manyHsxElement :: Parser Node
manyHsxElement = do
    children <- many hsxChild
    pure (Children (stripTextNodeWhitespaces children))

hsxSelfClosingElement :: Parser Node
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

hsxNormalElement :: Parser Node
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

hsxOpeningElement :: Parser (Text, [Attribute])
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

hsxNoRenderComment :: Parser Node
hsxNoRenderComment = do
    string "{-"
    manyTill (satisfy (const True)) (string "-}")
    space
    pure NoRenderCommentNode


hsxNodeAttributes :: Parser a -> Parser [Attribute]
hsxNodeAttributes end = staticAttributes
    where
        staticAttributes = do
            attributes <- manyTill (hsxNodeAttribute <|> hsxSplicedAttributes) end
            let staticAttributes = List.filter isStaticAttribute attributes
            let keys = List.map (\(StaticAttribute name _) -> name) staticAttributes
            let uniqueKeys = List.nubOrd keys
            unless (keys == uniqueKeys) (fail $ "Duplicate attribute found in tag: " <> show (keys List.\\ uniqueKeys))
            pure attributes

isStaticAttribute (StaticAttribute _ _) = True
isStaticAttribute _ = False

hsxSplicedAttributes :: Parser Attribute
hsxSplicedAttributes = do
    (pos, name) <- between (do char '{'; optional space; string "...") (string "}") do
            pos <- getSourcePos
            code <- takeWhile1P Nothing (\c -> c /= '}')
            pure (pos, code)
    space
    haskellExpression <- parseHaskellExpression pos (cs name)
    pure (SpreadAttributes haskellExpression)

parseHaskellExpression :: SourcePos -> Text -> Parser Haskell.Exp
parseHaskellExpression sourcePos input = do
    case HaskellParser.parseHaskellExpression sourcePos ?extensions (cs input) of
        Right expression -> pure expression
        Left (line, col, error) -> do
            pos <- getSourcePos
            setPosition pos { sourceLine = mkPos line, sourceColumn = mkPos col }
            fail (show error)

hsxNodeAttribute :: Parser Attribute
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

        rawAttribute = takeWhile1P Nothing (\c -> Char.isAlphaNum c || c == '-' || c == '_')


hsxQuotedValue :: Parser AttributeValue
hsxQuotedValue = do
    value <- between (char '"') (char '"') (takeWhileP Nothing (\c -> c /= '\"'))
    pure (TextValue value)

hsxSplicedValue :: Parser AttributeValue
hsxSplicedValue = do
    (pos, value) <- between (char '{') (char '}') do
        pos <- getSourcePos
        code <- takeWhile1P Nothing (\c -> c /= '}')
        pure (pos, code)
    haskellExpression <- parseHaskellExpression pos (cs value)
    pure (ExpressionValue haskellExpression)

hsxClosingElement name = (hsxClosingElement' name) <?> friendlyErrorMessage
    where
        friendlyErrorMessage = show (Text.unpack ("</" <> name <> ">"))
        hsxClosingElement' name = do
            _ <- string ("</" <> name)
            space
            char ('>')
            pure ()

hsxChild :: Parser Node
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
        (pos, expression) <- doParse
        haskellExpression <- parseHaskellExpression pos (cs expression)
        pure (SplicedNode haskellExpression)
    where
        doParse = do
            (pos, tree) <- node
            let value = (treeToString "" tree)
            pure (pos, Text.init $ Text.tail value)

        parseTree = (snd <$> node) <|> leaf
        node = between (char '{') (char '}') do
                pos <- getSourcePos
                tree <- many parseTree
                pure (pos, TokenNode tree)
        leaf = TokenLeaf <$> takeWhile1P Nothing (\c -> c /= '{' && c /= '}')
        treeToString :: Text -> TokenTree -> Text
        treeToString acc (TokenLeaf value)  = acc <> value
        treeToString acc (TokenNode [])     = acc
        treeToString acc (TokenNode (x:xs)) = ((treeToString (acc <> "{") x) <> (Text.concat $ fmap (treeToString "") xs)) <> "}"


hsxElementName :: Parser Text
hsxElementName = do
    name <- takeWhile1P (Just "identifier") (\c -> Char.isAlphaNum c || c == '_' || c == '-' || c == '!')
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
        , "spellcheck", "src", "srcdoc", "srcset", "start", "step", "style", "subject"
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
        , "frameborder", "allow", "allowfullscreen", "nonce", "referrerpolicy", "slot"
        , "kind"
        , "html"
        , "sse-connect", "sse-swap"
        ]

parents :: Set Text
parents = Set.fromList
          [ "a"
          , "abbr"
          , "address"
          , "animate"
          , "animateMotion"
          , "animateTransform"
          , "article"
          , "aside"
          , "audio"
          , "b"
          , "bdi"
          , "bdo"
          , "blink"
          , "blockquote"
          , "body"
          , "button"
          , "canvas"
          , "caption"
          , "circle"
          , "cite"
          , "clipPath"
          , "code"
          , "colgroup"
          , "command"
          , "data"
          , "datalist"
          , "dd"
          , "defs"
          , "del"
          , "desc"
          , "details"
          , "dfn"
          , "dialog"
          , "discard"
          , "div"
          , "dl"
          , "dt"
          , "ellipse"
          , "em"
          , "feBlend"
          , "feColorMatrix"
          , "feComponentTransfer"
          , "feComposite"
          , "feConvolveMatrix"
          , "feDiffuseLighting"
          , "feDisplacementMap"
          , "feDistantLight"
          , "feDropShadow"
          , "feFlood"
          , "feFuncA"
          , "feFuncB"
          , "feFuncG"
          , "feFuncR"
          , "feGaussianBlur"
          , "feImage"
          , "feMerge"
          , "feMergeNode"
          , "feMorphology"
          , "feOffset"
          , "fePointLight"
          , "feSpecularLighting"
          , "feSpotLight"
          , "feTile"
          , "feTurbulence"
          , "fieldset"
          , "figcaption"
          , "figure"
          , "filter"
          , "footer"
          , "foreignObject"
          , "form"
          , "g"
          , "h1"
          , "h2"
          , "h3"
          , "h4"
          , "h5"
          , "h6"
          , "hatch"
          , "hatchpath"
          , "head"
          , "header"
          , "hgroup"
          , "html"
          , "i"
          , "iframe"
          , "ins"
          , "ion-icon"
          , "kbd"
          , "label"
          , "legend"
          , "li"
          , "line"
          , "linearGradient"
          , "loading"
          , "main"
          , "map"
          , "mark"
          , "marker"
          , "marquee"
          , "mask"
          , "menu"
          , "mesh"
          , "meshgradient"
          , "meshpatch"
          , "meshrow"
          , "metadata"
          , "meter"
          , "mpath"
          , "nav"
          , "noscript"
          , "object"
          , "ol"
          , "optgroup"
          , "option"
          , "output"
          , "p"
          , "path"
          , "pattern"
          , "picture"
          , "polygon"
          , "polyline"
          , "pre"
          , "progress"
          , "q"
          , "radialGradient"
          , "rect"
          , "rp"
          , "rt"
          , "ruby"
          , "s"
          , "samp"
          , "script"
          , "section"
          , "select"
          , "set"
          , "slot"
          , "small"
          , "source"
          , "span"
          , "stop"
          , "strong"
          , "style"
          , "sub"
          , "summary"
          , "sup"
          , "svg"
          , "switch"
          , "symbol"
          , "table"
          , "tbody"
          , "td"
          , "template"
          , "text"
          , "textPath"
          , "textarea"
          , "tfoot"
          , "th"
          , "thead"
          , "time"
          , "title"
          , "tr"
          , "track"
          , "tspan"
          , "u"
          , "ul"
          , "unknown"
          , "use"
          , "var"
          , "video"
          , "view"
          , "wbr"
          ]

leafs :: Set Text
leafs = Set.fromList
        [ "area"
        , "base"
        , "br"
        , "col"
        , "embed"
        , "hr"
        , "img"
        , "input"
        , "link"
        , "meta"
        , "param"
        , "!DOCTYPE"
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
