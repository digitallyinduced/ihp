module IHP.HtmlSupport.Parser
( parseHsx
, Node (..)
, Attribute (..)
, AttributeValue (..)
, attributes
, parents
, leafs
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

data AttributeValue = TextValue !Text | ExpressionValue !Text deriving (Show)

data Attribute = StaticAttribute !Text !AttributeValue | SpreadAttributes Text deriving (Show)

data Node = Node !Text ![Attribute] ![Node]
    | TextNode !Text
    | PreEscapedTextNode !Text -- ^ Used in @script@ or @style@ bodies
    | SplicedNode !Text -- ^ Inline haskell expressions like @{myVar}@ or @{f "hello"}@
    | Children ![Node]
    deriving (Show)

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

hsxElement = try hsxSelfClosingElement <|> hsxNormalElement

manyHsxElement = do
    children <- many hsxChild
    pure (Children (stripTextNodeWhitespaces children))

hsxSelfClosingElement = do
    _ <- char '<'
    name <- hsxElementName
    attributes <-
      if name `List.elem` leafs
        then hsxNodeAttributes (string ">" <|> string "/>")
        else hsxNodeAttributes (string "/>")
    space
    pure (Node name attributes [])

hsxNormalElement = do
    (name, attributes) <- hsxOpeningElement
    let parsePreEscapedTextChildren transformText = do
                    let closingElement = "</" <> name <> ">"
                    text <- cs <$> manyTill anySingle (string closingElement)
                    pure [PreEscapedTextNode (transformText text)]
    let parseNormalHSXChildren = stripTextNodeWhitespaces <$> manyTill (try hsxChild) (hsxClosingElement name)

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
    pure (Node name attributes children)

hsxOpeningElement = do
    char '<'
    name <- hsxElementName
    space
    attributes <- hsxNodeAttributes (char '>')
    pure (name, attributes)

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
    pure (SpreadAttributes name)

hsxNodeAttribute = do
    key <- hsxAttributeName
    space
    _ <- char '='
    space
    value <- hsxQuotedValue <|> hsxSplicedValue
    space
    pure (StaticAttribute key value)

hsxAttributeName :: Parser Text
hsxAttributeName = choice ([dataAttribute, ariaAttribute, htmxAttribute] <> (List.map string attributes))
    where
        prefixedAttribute prefix = do
            string prefix
            d <- takeWhile1P Nothing (\c -> Char.isAlphaNum c || c == '-')
            pure (prefix <> d)
        dataAttribute = prefixedAttribute "data-"
        ariaAttribute = prefixedAttribute "aria-"
        htmxAttribute = prefixedAttribute "hx-"

hsxQuotedValue :: Parser AttributeValue
hsxQuotedValue = do
    value <- between (char '"') (char '"') (takeWhileP Nothing (\c -> c /= '\"'))
    pure (TextValue value)

hsxSplicedValue :: Parser AttributeValue
hsxSplicedValue = do
    value <- between (char '{') (char '}') (takeWhile1P Nothing (\c -> c /= '}'))
    pure (ExpressionValue value)

hsxClosingElement name = do
    _ <- string ("</" <> name <> ">")
    pure ()

hsxChild = hsxElement <|> hsxSplicedNode <|> hsxText

-- | Parses a hsx text node
--
-- Stops parsing when hitting a variable, like `{myVar}`
hsxText :: Parser Node
hsxText = buildTextNode <$> takeWhile1P (Just "text") (\c -> c /= '{' && c /= '}' && c /= '<' && c /= '>')

-- | Builds a TextNode and strips all surround whitespace from the input string
buildTextNode :: Text -> Node
buildTextNode value 
    | Text.null (Text.strip value) = TextNode ""
    | otherwise = TextNode (collapseSpace value)

data TokenTree = TokenLeaf Text | TokenNode [TokenTree] deriving (Show)

hsxSplicedNode :: Parser Node
hsxSplicedNode = do
        expression <- doParse
        space
        pure (SplicedNode expression)
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
    unless (name `List.elem` parents || name `List.elem` leafs) (fail $ "Invalid tag name: " <> cs name)
    space
    pure name

hsxIdentifier :: Parser Text
hsxIdentifier = do
    name <- takeWhile1P (Just "identifier") (\c -> Char.isAlphaNum c || c == '_')
    space
    pure name


attributes :: [Text]
attributes =
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
        , "d", "viewBox", "fill", "cx", "cy", "r", "x", "y", "text-anchor", "alignment-baseline"
        , "line-spacing", "letter-spacing"
        , "integrity", "crossorigin", "poster"
        ]

parents :: [Text]
parents =
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
        ]

leafs :: [Text]
leafs =
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
collapseSpace text = Text.intercalate " " (filterDuplicateSpaces $ Text.split Char.isSpace text)
    where
        filterDuplicateSpaces ("":"":rest) = (filterDuplicateSpaces ("":rest))
        filterDuplicateSpaces (a:rest) = a:(filterDuplicateSpaces rest)
        filterDuplicateSpaces [] = []
