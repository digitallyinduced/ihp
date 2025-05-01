module IHP.Test.CssQuery where

import IHP.Prelude hiding (takeWhile)
import Data.Text as Text (Text)
import Data.Attoparsec.Text
import Control.Applicative
import Data.Char

data SelectorGroup
  = DirectChildren [Selector]
  | DeepChildren [Selector]
  deriving (Show, Eq)

data Selector
  = ById Text
  | ByClass Text
  | ByTagName Text
  | ByAttrExists Text
  | ByAttrEquals Text Text
  | ByAttrContains Text Text
  | ByAttrStarts Text Text
  | ByAttrEnds Text Text
  deriving (Show, Eq)


-- The official syntax specification for CSS2 can be found here:
--      http://www.w3.org/TR/CSS2/syndata.html
-- but that spec is tricky to fully support. Instead we do the minimal and we
-- can extend it as needed.


-- | Parses a query into an intermediate format which is easy to feed to HXT
--
-- * The top-level lists represent the top level comma separated queries.
--
-- * SelectorGroup is a group of qualifiers which are separated
--   with spaces or > like these three: /table.main.odd tr.even > td.big/
--
-- * A SelectorGroup as a list of Selector items, following the above example
--   the selectors in the group are: /table/, /.main/ and /.odd/
parseQuery :: Text -> Either String [[SelectorGroup]]
parseQuery = parseOnly cssQuery

-- Below this line is the Parsec parser for css queries.
cssQuery :: Parser [[SelectorGroup]]
cssQuery = many (char ' ') >> sepBy rules (char ',' >> many (char ' '))

rules :: Parser [SelectorGroup]
rules = many $ directChildren <|> deepChildren

directChildren :: Parser SelectorGroup
directChildren =
    string "> " >> (many (char ' ')) >> DirectChildren <$> pOptionalTrailingSpace parseSelectors

deepChildren :: Parser SelectorGroup
deepChildren = pOptionalTrailingSpace $ DeepChildren <$> parseSelectors

parseSelectors :: Parser [Selector]
parseSelectors = many1 $
    parseId <|> parseClass <|> parseTag <|> parseAttr

parseId :: Parser Selector
parseId = char '#' >> ById <$> pIdent

parseClass :: Parser Selector
parseClass = char '.' >> ByClass <$> pIdent

parseTag :: Parser Selector
parseTag = ByTagName <$> pIdent

parseAttr :: Parser Selector
parseAttr = pSquare $ choice
    [ ByAttrEquals <$> pIdent <*> (string "=" *> pAttrValue)
    , ByAttrContains <$> pIdent <*> (string "*=" *> pAttrValue)
    , ByAttrStarts <$> pIdent <*> (string "^=" *> pAttrValue)
    , ByAttrEnds <$> pIdent <*> (string "$=" *> pAttrValue)
    , ByAttrExists <$> pIdent
    ]

-- | pIdent : Parse an identifier (not yet supporting escapes and unicode as
-- part of the identifier). Basically the regex: [-]?[_a-zA-Z][_a-zA-Z0-9]*
pIdent :: Parser Text
pIdent = do
    leadingMinus <- string "-" <|> pure ""
    nmstart <- singleton <$> satisfy (\c -> isAlpha c || c == '_')
    nmchar <- takeWhile (\c -> isAlphaNum c || c == '_' || c == '-')
    return $ mconcat [ leadingMinus, cs nmstart, nmchar ]


pAttrValue :: Parser Text
pAttrValue = takeWhile (/= ']')

pSquare :: Parser a -> Parser a
pSquare p = char '[' *> p <* char ']'

pOptionalTrailingSpace :: Parser a -> Parser a
pOptionalTrailingSpace p = p <* many (char ' ')