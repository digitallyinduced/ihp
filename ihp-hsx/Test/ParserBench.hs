{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Main where

import Prelude
import IHP.HSX.Parser
import qualified Text.Megaparsec as Megaparsec
import qualified Data.Text as Text
import Data.Text (Text)
import Control.DeepSeq (NFData(..), rnf)
import Test.Tasty.Bench

instance NFData Node where
    rnf (Node t as cs b) = rnf t `seq` rnf as `seq` rnf cs `seq` rnf b
    rnf (TextNode t) = rnf t
    rnf (PreEscapedTextNode t) = rnf t
    rnf (SplicedNode _) = () -- TH.Exp has no NFData
    rnf (Children cs) = rnf cs
    rnf (CommentNode t) = rnf t
    rnf NoRenderCommentNode = ()

instance NFData Attribute where
    rnf (StaticAttribute t v) = rnf t `seq` rnf v
    rnf (SpreadAttributes _) = ()

instance NFData AttributeValue where
    rnf (TextValue t) = rnf t
    rnf (ExpressionValue _) = ()

position :: Megaparsec.SourcePos
position = Megaparsec.SourcePos "" (Megaparsec.mkPos 1) (Megaparsec.mkPos 1)

settings :: HsxSettings
settings = HsxSettings True mempty mempty

-- Small template: single element with text
small :: Text
small = "<div>Hello World</div>"

-- Medium template: nested elements with attributes
medium :: Text
medium = Text.unlines
    [ "<div class=\"container mt-4\">"
    , "    <nav class=\"navbar navbar-expand-lg navbar-light mb-4\">"
    , "        <a class=\"navbar-brand\" href=\"/\">IHP Forum</a>"
    , "    </nav>"
    , "    <h1>Welcome</h1>"
    , "    <p>This is a page with some content.</p>"
    , "</div>"
    ]

-- Large template: many children (simulates forEach output)
large :: Text
large = "<ul>" <> mconcat (replicate 100 "<li><a href=\"/posts\">Post title here</a></li>") <> "</ul>"

-- Template with splices
splices :: Text
splices = "<div>" <> mconcat (replicate 50 "<span>{\"hello\"}</span>") <> "</div>"

-- Template with nested braces in splices
nestedSplices :: Text
nestedSplices = "<div>" <> mconcat (replicate 50 "<span>{show (1 + 2)}</span>") <> "</div>"

-- Deep nesting
deep :: Text
deep = Text.replicate 20 "<div>" <> "content" <> Text.replicate 20 "</div>"

-- Many attributes
manyAttrs :: Text
manyAttrs = "<div class=\"a\" id=\"b\" style=\"c\" data-x=\"1\" data-y=\"2\" data-z=\"3\" role=\"main\" tabindex=\"0\" title=\"t\" hidden>text</div>"

parse :: Text -> Node
parse input = case parseHsx settings position [] input of
    Right node -> node
    Left err -> error (show err)

main :: IO ()
main = defaultMain
    [ bench "small"         $ nf parse small
    , bench "medium"        $ nf parse medium
    , bench "large"         $ nf parse large
    , bench "splices"       $ nf parse splices
    , bench "nestedSplices" $ nf parse nestedSplices
    , bench "deep"          $ nf parse deep
    , bench "manyAttrs"     $ nf parse manyAttrs
    ]
