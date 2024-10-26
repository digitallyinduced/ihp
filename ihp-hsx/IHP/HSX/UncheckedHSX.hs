{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module IHP.HSX.UncheckedHSX (uncheckedHsx) where

import Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.String.Conversions (cs)
import qualified IHP.HSX.HaskellParser as HaskellParser
import Text.Blaze.Internal (MarkupM(Parent, Leaf), attribute)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Control.Monad (when)
import Data.Char (isLower)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor (void)
import qualified Text.Megaparsec.Char as C

type Parser = Parsec Void Text

data UNode = UNode Text [(Text, AttributeValue)] [UNode] Bool
           | UTextNode Text
           | USplicedNode Exp
           | UCommentNode Text
           | UNoRenderCommentNode
           deriving (Show)

data AttributeValue = TextValue Text | ExpressionValue Exp deriving (Show)

uncheckedHsx :: QuasiQuoter
uncheckedHsx = QuasiQuoter
    { quoteExp = quoteUncheckedHsxExpression
    , quotePat = error "quotePat not implemented for uncheckedHsx"
    , quoteType = error "quoteType not implemented for uncheckedHsx"
    , quoteDec = error "quoteDec not implemented for uncheckedHsx"
    }

quoteUncheckedHsxExpression :: String -> Q Exp
quoteUncheckedHsxExpression code = do
    loc <- location
    let position = SourcePos (loc_filename loc) (mkPos (fst (loc_start loc))) (mkPos (snd (loc_start loc)))
    extensions <- extsEnabled
    case runParser (uncheckedParser extensions) "" (cs code) of
        Left err -> fail $ errorBundlePretty err
        Right result -> compileToHaskell result

uncheckedParser :: [Extension] -> Parser UNode
uncheckedParser extensions = space *> manyUncheckedElement <* eof

manyUncheckedElement :: Parser UNode
manyUncheckedElement = do
    children <- many (try uncheckedElement <|> uncheckedTextNode <|> uncheckedSplicedNode)
    case children of
        [node] -> return node
        _      -> return $ UNode "div" [] children False

uncheckedElement :: Parser UNode
uncheckedElement = do
    void $ char '<'
    tagName <- T.pack <$> some (alphaNumChar <|> char '-' <|> char '_' <|> char ':')
    attrs <- many uncheckedAttribute
    space
    if Set.member tagName selfClosingTags
        then (void (string "/>" <|> string ">")) >> return (UNode tagName attrs [] True)
        else do
            void $ char '>'
            children <- many (try uncheckedElement <|> uncheckedTextNode <|> uncheckedSplicedNode)
            closing <- optional (try $ string "</" *> chunk tagName *> char '>')
            case closing of
                Just _  -> return $ UNode tagName attrs children False
                Nothing -> fail $ "Unclosed tag: <" ++ T.unpack tagName ++ ">"

uncheckedAttribute :: Parser (Text, AttributeValue)
uncheckedAttribute = do
    space
    name <- T.pack <$> some (alphaNumChar <|> char '-' <|> char '_' <|> char ':')
    value <- option (TextValue "") (char '=' *> (quotedValue <|> unquotedValue <|> expressionValue))
    return (name, value)
  where
    quotedValue = TextValue . T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))
    unquotedValue = TextValue . T.pack <$> some (alphaNumChar <|> char '-' <|> char '_')
    expressionValue = ExpressionValue <$> (char '{' *> parseHaskellExpression <* char '}')

uncheckedTextNode :: Parser UNode
uncheckedTextNode = UTextNode . T.pack <$> some (satisfy (\c -> c /= '<' && c /= '{' && c /= '}'))

uncheckedSplicedNode :: Parser UNode
uncheckedSplicedNode = USplicedNode <$> (char '{' *> parseHaskellExpression <* char '}')

parseHaskellExpression :: Parser Exp
parseHaskellExpression = do
    pos <- getSourcePos
    code <- takeWhileP Nothing (\c -> c /= '}')
    case HaskellParser.parseHaskellExpression pos [] (T.unpack code) of
        Right exp -> return exp
        Left err -> fail $ show err

compileToHaskell :: UNode -> Q Exp
compileToHaskell (UNode name attrs children isLeaf) = do
    when (T.toLower name `notElem` knownElements && T.toLower name `notElem` knownLeafs) $
        case validateCustomElement name of
            Left err -> fail err
            Right () -> pure ()
    when (not isLeaf && null children && name `notElem` voidElements) $
        fail $ "Empty non-void element: <" ++ T.unpack name ++ ">"
    let element = if isLeaf || name `elem` voidElements
                  then nodeToBlazeLeaf name
                  else nodeToBlazeElement name
        applyAttrs = foldr (\(k, v) e -> [| $e <> $(compileAttribute k v) |]) element attrs
        closeTag = if isLeaf
                   then [| mempty |]
                   else [| H.preEscapedText $(litE $ stringL $ "</" ++ T.unpack name ++ ">") |]
        applyChildren = if null children
                        then [| $applyAttrs |]
                        else [| $applyAttrs <> mconcat $(listE (map compileToHaskell children)) |]
    [| $applyChildren <> $closeTag |]
compileToHaskell (UTextNode value) = [| H.text $(litE $ stringL $ T.unpack value) |]
compileToHaskell (USplicedNode exp) = [| H.preEscapedToHtml $(return exp) |]
compileToHaskell (UCommentNode value) = [| H.textComment $(litE $ stringL $ T.unpack value) |]
compileToHaskell UNoRenderCommentNode = [| mempty |]

selfClosingTags :: Set.Set Text
selfClosingTags = Set.fromList
    [ "area", "base", "br", "col", "embed", "hr", "img", "input"
    , "link", "meta", "param", "source", "track", "wbr"
    ]

voidElements :: [Text]
voidElements = ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"]

validateCustomElement :: Text -> Either String ()
validateCustomElement name 
    | T.any (== '-') name && not (isLower (T.head name)) = Left $ "Custom element '" ++ T.unpack name ++ "' must start with a lowercase letter"
    | T.any (== '-') name = Right ()
    | otherwise = Left $ "Custom element '" ++ T.unpack name ++ "' must contain a hyphen (-) and start with a lowercase letter"

knownElements :: [Text]
knownElements = ["div", "span", "p", "a", "h1", "h2", "h3", "h4", "h5", "h6", "ul", "ol", "li", "table", "tr", "td", "th", "form", "input", "button", "select", "option", "textarea", "label", "header", "footer", "nav", "main", "section", "article", "aside"]

knownLeafs :: [Text]
knownLeafs = ["br", "hr", "img", "input", "meta", "link"]

nodeToBlazeLeaf :: Text -> Q Exp
nodeToBlazeLeaf name = [| H.preEscapedText $(litE $ stringL $ "<" ++ T.unpack name ++ "/>") |]

nodeToBlazeElement :: Text -> Q Exp
nodeToBlazeElement name = [| H.preEscapedText $(litE $ stringL $ "<" ++ T.unpack name ++ ">") |]

compileAttribute :: Text -> AttributeValue -> Q Exp
compileAttribute name (TextValue value) = [| H.preEscapedText $(litE $ stringL $ " " ++ T.unpack name ++ "=\"" ++ T.unpack value ++ "\"") |]
compileAttribute name (ExpressionValue exp) = [| H.preEscapedText (T.pack $ " " ++ T.unpack name ++ "=\"") <> H.toHtml $(return exp) <> H.preEscapedText "\"" |]
