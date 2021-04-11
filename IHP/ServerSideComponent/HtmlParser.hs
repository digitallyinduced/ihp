{-|
Module: IHP.ServerSideComponent.HtmlParser
Copyright: (c) digitally induced GmbH, 2021
Description: Used by serverside DOM diff
-}
module IHP.ServerSideComponent.HtmlParser
( parseHtml
, Node (..)
, Attribute (..)
) where

import CorePrelude
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Char as Char
import Data.String.Conversions

data Attribute = Attribute
    { attributeName :: !Text
    , attributeValue :: !Text
    } deriving (Eq, Show)

data Node = Node { tagName :: !Text, attributes :: ![Attribute], children :: ![Node], startOffset :: Int, endOffset :: Int }
    | TextNode { textContent :: !Text }
    | PreEscapedTextNode { textContent :: !Text } -- ^ Used in @script@ or @style@ bodies
    | Children { children :: ![Node] }
    | CommentNode { comment :: !Text }
    deriving (Eq, Show)

parseHtml :: Text -> Either (ParseErrorBundle Text Void) Node
parseHtml code = runParser (parser) "" code

type Parser = Parsec Void Text

parser :: Parser Node
parser = do
    space
    node <- parseChildren <|> parseElement
    space
    eof
    pure node

parseElement = try parseComment <|> try parseNormalElement <|> try parseSelfClosingElement

parseChildren = Children <$> many parseChild

parseSelfClosingElement = do
    startOffset <- getOffset
    _ <- char '<'
    name <- parseElementName
    attributes <- parseNodeAttributes (string ">" <|> string "/>")
    endOffset <- getOffset
    space
    pure (Node name attributes [] startOffset endOffset)

parseNormalElement = do
    startOffset <- getOffset
    (name, attributes) <- parseOpeningElement
    let parsePreEscapedTextChildren = do
                    let closingElement = "</" <> name <> ">"
                    text <- cs <$> manyTill anySingle (string closingElement)
                    pure [PreEscapedTextNode text]
    let parseNormalChildren = (space >> (manyTill (try parseChild) (try (space >> parseClosingElement name))))

    children <- case name of
            "script" -> parsePreEscapedTextChildren
            "style" -> parsePreEscapedTextChildren
            otherwise -> parseNormalChildren
    endOffset <- getOffset
    pure (Node name attributes children startOffset endOffset)

parseOpeningElement = do
    char '<'
    name <- parseElementName
    space
    attributes <- parseNodeAttributes (char '>')
    pure (name, attributes)

parseComment :: Parser Node
parseComment = do
    string "<!--"
    body :: String <- manyTill (satisfy (const True)) (string "-->")
    space
    pure (CommentNode (cs body))


parseNodeAttributes :: Parser a -> Parser [Attribute]
parseNodeAttributes end = manyTill parseNodeAttribute end 

parseNodeAttribute = do
    attributeName <- parseAttributeName
    space

    let parseAttributeValue = do
            _ <- char '='
            space
            attributeValue <- parseQuotedValue
            space
            pure attributeValue

    attributeValue <- fromMaybe "" <$> optional parseAttributeValue
    pure Attribute { attributeName, attributeValue }

parseAttributeName :: Parser Text
parseAttributeName = takeWhile1P Nothing (\c -> Char.isAlphaNum c || c == '-')

parseQuotedValue :: Parser Text
parseQuotedValue = between (char '"') (char '"') (takeWhileP Nothing (\c -> c /= '\"'))

parseClosingElement name = do
    _ <- string ("</" <> name)
    space
    char ('>')
    pure ()

parseChild = parseElement <|> try (space >> parseElement) <|> parseText

parseText :: Parser Node
parseText = TextNode <$> takeWhile1P (Just "text") (\c -> c /= '<' && c /= '>')

parseElementName :: Parser Text
parseElementName = do
    name <- takeWhile1P (Just "identifier") (\c -> Char.isAlphaNum c || c == '_' || c == '-')
    space
    pure name

