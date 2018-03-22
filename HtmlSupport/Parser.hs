module Foundation.HtmlSupport.Parser (parseHsx, Node (..), Attributes (..), AttributeValue (..)) where

import           ClassyPrelude                hiding (many, try)
import           Control.Applicative          ((<|>))
import           Text.Parsec                  (Parsec, between, eof, many, many1, runParser, try)
import           Text.Parsec.Char             (alphaNum, char, letter, noneOf, spaces, string)
import           Text.Parsec.Error

data AttributeValue = TextValue String | ExpressionValue String deriving (Show)
data Attributes = SplicedAttributes String | StaticAttributes [(String, AttributeValue)] deriving (Show)

data Node = Node String Attributes [Node]
    | TextNode String
    | SplicedNode String
    | Children [Node]
    deriving (Show)

parseHsx :: String -> Either ParseError Node
parseHsx code = runParser parser () "" code


parser = do
    spaces
    node <- manyHsxElement <|> hsxElement
    spaces
    eof
    return node

hsxElement = try hsxSelfClosingElement <|> hsxNormalElement

manyHsxElement = do
    values <- many (do a <- hsxElement; spaces; return a)
    return $ Children values

hsxSelfClosingElement = do
    _ <- char '<'
    name <- hsxElementName
    attributes <- hsxNodeAttributes
    _ <- string "/>"
    return (Node name attributes [])

hsxNormalElement = do
    (name, attributes) <- hsxOpeningElement
    children <- many hsxChild
    hsxClosingElement name
    return (Node name attributes children)

hsxOpeningElement = do
    _ <- char '<'
    name <- hsxElementName
    attributes <- hsxNodeAttributes
    _ <- char '>'
    return (name, attributes)

hsxNodeAttributes = try hsxSplicedAttributes <|> (StaticAttributes <$> many hsxNodeAttribute)

hsxSplicedAttributes = do
    name <- between (string "{...") (string "}") (many (noneOf "}"))
    return (SplicedAttributes name)

hsxNodeAttribute = do
    key <- hsxAttributeName
    spaces
    _ <- char '='
    spaces
    value <- hsxQuotedValue <|> hsxSplicedValue
    spaces
    return (key, value)

hsxAttributeName = many (letter <|> char '-')

hsxQuotedValue = do
    value <- between (char '"') (char '"') (many (noneOf "\""))
    return (TextValue value)

hsxSplicedValue = do
    value <- between (char '{') (char '}') (many (noneOf "}"))
    return (ExpressionValue value)

hsxClosingElement name = do
    _ <- string ("</" <> name <> ">")
    return ()

hsxChild = try hsxText <|> try hsxSplicedNode <|> try hsxElement

hsxText = do
    value <- many1 (noneOf "{}<>")
    return (TextNode value)


data TokenTree = TokenLeaf String | TokenNode [TokenTree] deriving (Show)

hsxSplicedNode = do
        expression <- doParse
        return (SplicedNode expression)
    where
        doParse = do
            tree <- node
            let value = (treeToString "" tree)
            return $ unsafeInit $ unsafeTail value

        parseTree = node <|> leaf
        node = TokenNode <$> between (char '{') (char '}') (many parseTree)
        leaf = TokenLeaf <$> many1 (noneOf "{}")
        treeToString :: String -> TokenTree -> String
        treeToString acc (TokenLeaf value) = acc <> value
        treeToString acc (TokenNode []) = acc
        treeToString acc (TokenNode (x:xs)) = ((treeToString (acc <> "{") x) <> (intercalate "" $ map (treeToString "") xs)) <> "}"


hsxElementName = hsxIdentifier

hsxIdentifier = do
    name <- many1 alphaNum
    spaces
    return name
