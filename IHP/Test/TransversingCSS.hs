module IHP.Test.TransversingCSS where

import IHP.Test.CssQuery
import qualified Data.Text as Text
import qualified Control.Applicative
import Text.XML
import Text.XML.Cursor
import qualified Data.ByteString.Lazy as LBS
import qualified Text.HTML.DOM as HD
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)

type Query = Text.Text
type HtmlLBS = LBS.ByteString

-- | Perform a css 'Query' on 'Html'. Returns Either
--
-- * Left: Query parse error.
--
-- * Right: List of matching Html fragments.
findBySelector :: HtmlLBS -> Query -> Either String [String]
findBySelector html query =
  map (renderHtml . toHtml . node) Control.Applicative.<$> findCursorsBySelector html query

-- | Perform a css 'Query' on 'Html'. Returns Either
--
-- * Left: Query parse error.
--
-- * Right: List of matching Cursors
findCursorsBySelector :: HtmlLBS -> Query -> Either String [Cursor]
findCursorsBySelector html query =
  runQuery (fromDocument $ HD.parseLBS html)
       Control.Applicative.<$> parseQuery query

-- | Perform a css 'Query' on 'Html'. Returns Either
--
-- * Left: Query parse error.
--
-- * Right: List of matching Cursors
findAttributeBySelector :: HtmlLBS -> Query -> Text.Text -> Either String [[Text.Text]]
findAttributeBySelector html query attr =
  map (laxAttribute attr) Control.Applicative.<$> findCursorsBySelector html query


-- Run a compiled query on Html, returning a list of matching Html fragments.
runQuery :: Cursor -> [[SelectorGroup]] -> [Cursor]
runQuery html query = concatMap (runGroup html) query

runGroup :: Cursor -> [SelectorGroup] -> [Cursor]
runGroup c [] = [c]
runGroup c (DirectChildren s:gs) = concatMap (flip runGroup gs) $ c $/ selectors s
runGroup c (DeepChildren s:gs) = concatMap (flip runGroup gs) $ c $// selectors s

selectors :: [Selector] -> Cursor -> [Cursor]
selectors ss c
    | all (selector c) ss = [c]
    | otherwise = []

selector :: Cursor -> Selector -> Bool
selector c (ById x) = not $ null $ attributeIs "id" x c
selector c (ByClass x) =
    case attribute "class" c of
        t:_ -> x `elem` Text.words t
        [] -> False
selector c (ByTagName t) = not $ null $ element (Name t Nothing Nothing) c
selector c (ByAttrExists t) = not $ null $ hasAttribute (Name t Nothing Nothing) c
selector c (ByAttrEquals t v) = not $ null $ attributeIs (Name t Nothing Nothing) v c
selector c (ByAttrContains n v) =
    case attribute (Name n Nothing Nothing) c of
        t:_ -> v `Text.isInfixOf` t
        [] -> False
selector c (ByAttrStarts n v) =
    case attribute (Name n Nothing Nothing) c of
        t:_ -> v `Text.isPrefixOf` t
        [] -> False
selector c (ByAttrEnds n v) =
    case attribute (Name n Nothing Nothing) c of
        t:_ -> v `Text.isSuffixOf` t
        [] -> False