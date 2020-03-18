module TurboHaskell.HtmlSupport.Parser (parseHsx, Node (..), Attributes (..), AttributeValue (..)) where

import           Prelude       hiding (many, try)
import ClassyPrelude ((<>), unsafeInit, unsafeHead, intercalate, unsafeTail)
import           Control.Applicative ((<|>))
import           Text.Parsec         (Parsec, between, eof, many, many1, runParser, try)
import           Text.Parsec.Char    (alphaNum, char, letter, noneOf, spaces, string)
import           Text.Parsec.Error
import Data.Char (isSpace)

data AttributeValue = TextValue !String | ExpressionValue !String deriving (Show)
data Attributes = SplicedAttributes !String | StaticAttributes ![(String, AttributeValue)] deriving (Show)

data Node = Node !String !Attributes ![Node]
    | TextNode !String
    | SplicedNode !String
    | Children ![Node]
    deriving (Show)

parseHsx :: String -> Either ParseError Node
parseHsx code = runParser parser () "" code


parser = do
    spaces
    node <- manyHsxElement <|> hsxElement
    spaces
    eof
    pure node

hsxElement = try hsxSelfClosingElement <|> hsxNormalElement

manyHsxElement = do
    values <- many (do a <- hsxChild; spaces; pure a)
    pure $ Children values

hsxSelfClosingElement = do
    _ <- char '<'
    name <- hsxElementName
    attributes <- hsxNodeAttributes
    _ <- string "/>"
    pure (Node name attributes [])

hsxNormalElement = do
    (name, attributes) <- hsxOpeningElement
    children <- many hsxChild
    hsxClosingElement name
    pure (Node name attributes children)

hsxOpeningElement = do
    _ <- char '<'
    name <- hsxElementName
    attributes <- hsxNodeAttributes
    _ <- char '>'
    pure (name, attributes)

hsxNodeAttributes = try hsxSplicedAttributes <|> (StaticAttributes <$> many hsxNodeAttribute)

hsxSplicedAttributes = do
    name <- between (string "{...") (string "}") (many (noneOf "}"))
    pure (SplicedAttributes name)

hsxNodeAttribute = do
    key <- hsxAttributeName
    spaces
    _ <- char '='
    spaces
    value <- hsxQuotedValue <|> hsxSplicedValue
    spaces
    pure (key, value)

hsxAttributeName = many (letter <|> char '-')

hsxQuotedValue = do
    value <- between (char '"') (char '"') (many (noneOf "\""))
    pure (TextValue value)

hsxSplicedValue = do
    value <- between (char '{') (char '}') (many (noneOf "}"))
    pure (ExpressionValue value)

hsxClosingElement name = do
    _ <- string ("</" <> name <> ">")
    pure ()

hsxChild = try hsxText <|> try hsxSplicedNode <|> try hsxElement

hsxText = do
    value <- many1 (noneOf "{}<>")
    pure (TextNode (trim value))


data TokenTree = TokenLeaf String | TokenNode [TokenTree] deriving (Show)

hsxSplicedNode = do
        expression <- doParse
        pure (SplicedNode expression)
    where
        doParse = do
            tree <- node
            let value = (treeToString "" tree)
            pure $ unsafeInit $ unsafeTail value

        parseTree = node <|> leaf
        node = TokenNode <$> between (char '{') (char '}') (many parseTree)
        leaf = TokenLeaf <$> many1 (noneOf "{}")
        treeToString :: String -> TokenTree -> String
        treeToString acc (TokenLeaf value)  = acc <> value
        treeToString acc (TokenNode [])     = acc
        treeToString acc (TokenNode (x:xs)) = ((treeToString (acc <> "{") x) <> (intercalate "" $ map (treeToString "") xs)) <> "}"


hsxElementName = hsxIdentifier

hsxIdentifier = do
    name <- many1 alphaNum
    spaces
    pure name

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace