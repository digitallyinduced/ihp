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

type Parser = Parsec Void Text

data UNode = UNode Text [(Text, AttributeValue)] [UNode] Bool
           | UTextNode Text
           | USplicedNode Exp
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
    return $ UNode "div" [] children False

uncheckedElement :: Parser UNode
uncheckedElement = do
    char '<'
    tagName <- some (alphaNumChar <|> char '-' <|> char '_' <|> char ':')
    attrs <- many uncheckedAttribute
    space
    (do  string "/>"
         return $ UNode (T.pack tagName) attrs [] True)
     <|>
     (do  char '>'
          children <- many (try uncheckedElement <|> uncheckedTextNode <|> uncheckedSplicedNode)
          closing <- optional (try $ string "</" >> string (T.pack tagName) >> char '>')
          case closing of
            Just _  -> return $ UNode (T.pack tagName) attrs children False
            Nothing -> return $ UNode (T.pack tagName) attrs children True)

uncheckedAttribute :: Parser (Text, AttributeValue)
uncheckedAttribute = do
    space
    name <- some (alphaNumChar <|> char '-' <|> char '_' <|> char ':')
    value <- option (TextValue "") (char '=' *> (quotedValue <|> unquotedValue <|> expressionValue))
    return (T.pack name, value)
  where
    quotedValue = TextValue . T.pack <$> (char '"' *> manyTill anySingle (char '"'))
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
    case HaskellParser.parseHaskellExpression pos [] (cs code) of
        Right exp -> return exp
        Left err -> fail $ show err

compileToHaskell :: UNode -> Q Exp
compileToHaskell (UNode name attrs children isLeaf) =
    let element = [| H.preEscapedText $(litE $ stringL $ "<" ++ T.unpack name) |]
        applyAttrs = foldr (\(k, v) e -> [| $e <> $(compileAttribute k v) |]) element attrs
        closeTag = if isLeaf
                   then [| H.preEscapedText $(litE $ stringL "/>") |]
                   else [| H.preEscapedText $(litE $ stringL ">") |]
        applyChildren = if null children
                        then [| $applyAttrs <> $closeTag |]
                        else [| $applyAttrs <> H.preEscapedText $(litE $ stringL ">") <> 
                                mconcat $(listE (map compileToHaskell children)) <> 
                                H.preEscapedText $(litE $ stringL $ "</" ++ T.unpack name ++ ">") |]
    in applyChildren
compileToHaskell (UTextNode text) = [| H.text $(litE $ stringL $ T.unpack text) |]
compileToHaskell (USplicedNode exp) = [| H.preEscapedToHtml $(return exp) |]

compileAttribute :: Text -> AttributeValue -> Q Exp
compileAttribute name (TextValue value) = [| H.preEscapedText $(litE $ stringL $ " " ++ T.unpack name ++ "=\"" ++ T.unpack value ++ "\"") |]
compileAttribute name (ExpressionValue exp) = [| H.preEscapedText (T.pack $ " " ++ T.unpack name ++ "=\"") <> H.toHtml $(return exp) <> H.preEscapedText "\"" |]
