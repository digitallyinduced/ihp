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
import qualified Data.Set as Set

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
    case runParser uncheckedParser "" (cs code) of
        Left err -> fail $ errorBundlePretty err
        Right result -> compileToHaskell result

type Parser = Parsec Void Text

data UNode = UNode Text [(Text, Text)] [UNode] Bool
           | UTextNode Text
           | USplicedNode Exp
           deriving (Show)

uncheckedParser :: Parser UNode
uncheckedParser = space *> (manyUncheckedElement <|> uncheckedElement) <* space <* eof

manyUncheckedElement :: Parser UNode
manyUncheckedElement = do
    children <- many uncheckedChild
    return $ UNode "div" [] children False

uncheckedElement :: Parser UNode
uncheckedElement = do
    char '<'
    tagName <- some (alphaNumChar <|> char '-' <|> char '_')
    attrs <- many uncheckedAttribute
    space
    (do  string "/>"
         return $ UNode (T.pack tagName) attrs [] True)
     <|>
     (do  char '>'
          children <- many uncheckedChild
          string "</"
          string (T.pack tagName)
          char '>'
          return $ UNode (T.pack tagName) attrs children False)
     <|>
     (do  char '>'
          if tagName `Set.member` selfClosingTags
              then return $ UNode (T.pack tagName) attrs [] True
              else do
                  children <- many uncheckedChild
                  string "</"
                  string (T.pack tagName)
                  char '>'
                  return $ UNode (T.pack tagName) attrs children False)

uncheckedAttribute :: Parser (Text, Text)
uncheckedAttribute = do
    space
    name <- some (alphaNumChar <|> char '-' <|> char '_')
    value <- option "" (char '=' *> (quoted <|> unquoted))
    return (T.pack name, T.pack value)
  where
    quoted = char '"' *> manyTill anySingle (char '"')
    unquoted = some (alphaNumChar <|> char '-' <|> char '_')

uncheckedChild :: Parser UNode
uncheckedChild = uncheckedElement <|> uncheckedTextNode <|> uncheckedSplicedNode

uncheckedTextNode :: Parser UNode
uncheckedTextNode = UTextNode . T.pack <$> some (anySingleBut '<')

uncheckedSplicedNode :: Parser UNode
uncheckedSplicedNode = between (string "{") (string "}") $ do
    expr <- parseHaskellExpression
    return $ USplicedNode expr

parseHaskellExpression :: Parser Exp
parseHaskellExpression = error "Implement Haskell expression parsing here"

compileToHaskell :: UNode -> Q Exp
compileToHaskell (UNode name attrs children isLeaf) =
    let element = if isLeaf
                  then [| H.preEscapedText $(litE $ stringL $ T.unpack $ "<" <> name <> "/>") |]
                  else [| H.preEscapedText $(litE $ stringL $ T.unpack $ "<" <> name <> ">") |]
        applyAttrs = foldr (\(k, v) e -> [| $e ! H.customAttribute (H.stringTag $(litE $ stringL $ T.unpack k)) $(litE $ stringL $ T.unpack v) |]) element attrs
        applyChildren = if null children
                        then applyAttrs
                        else [| $applyAttrs <> mconcat $(listE (map compileToHaskell children)) <> H.preEscapedText $(litE $ stringL $ T.unpack $ "</" <> name <> ">") |]
    in applyChildren
compileToHaskell (UTextNode text) = [| H.text $(litE $ stringL $ T.unpack text) |]
compileToHaskell (USplicedNode exp) = [| H.preEscapedToHtml $(return exp) |]

selfClosingTags :: Set.Set String
selfClosingTags = Set.fromList ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"]

-- You can add more helper functions here as needed
