{-# LANGUAGE TemplateHaskell, UndecidableInstances, BangPatterns, PackageImports, FlexibleInstances, OverloadedStrings #-}

{-|
Module: IHP.HSX.TextMarkupQQ
Description: Quasiquoter for Text.Lazy.Builder based HSX backend.
Copyright: (c) digitally induced GmbH, 2024
-}
module IHP.HSX.TextMarkupQQ
  ( hsx
  , uncheckedHsx
  , customHsx
  ) where

import           Prelude
import Data.Text (Text)
import           IHP.HSX.Parser
import qualified "template-haskell" Language.Haskell.TH           as TH
import qualified "template-haskell" Language.Haskell.TH.Syntax           as TH
import           Language.Haskell.TH.Quote
import Data.String.Conversions (cs)
import IHP.HSX.TextMarkup
import qualified Text.Megaparsec as Megaparsec
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Set as Set

hsx :: QuasiQuoter
hsx = customHsx
        (HsxSettings
            { checkMarkup = True
            , additionalTagNames = Set.empty
            , additionalAttributeNames = Set.empty
            }
        )

uncheckedHsx :: QuasiQuoter
uncheckedHsx = customHsx
        (HsxSettings
            { checkMarkup = False
            , additionalTagNames = Set.empty
            , additionalAttributeNames = Set.empty
            }
        )

customHsx :: HsxSettings -> QuasiQuoter
customHsx settings =
    QuasiQuoter
        { quoteExp = quoteHsxExpression settings
        , quotePat = error "quotePat: not defined"
        , quoteDec = error "quoteDec: not defined"
        , quoteType = error "quoteType: not defined"
        }

quoteHsxExpression :: HsxSettings -> String -> TH.ExpQ
quoteHsxExpression settings code = do
        hsxPosition <- findHSXPosition
        extensions <- TH.extsEnabled
        expression <- case parseHsx settings hsxPosition extensions (cs code) of
                Left error   -> fail (Megaparsec.errorBundlePretty error)
                Right result -> pure result
        compileToMarkup expression
    where
        findHSXPosition = do
            loc <- TH.location
            let (line, col) = TH.loc_start loc
            pure $ Megaparsec.SourcePos (TH.loc_filename loc) (Megaparsec.mkPos line) (Megaparsec.mkPos col)

compileToMarkup :: Node -> TH.ExpQ
compileToMarkup node
    | isStaticTree node, isNonTrivialStaticNode node =
        let bs = Text.encodeUtf8 (renderStaticHtml node)
        in [| rawByteString $(TH.lift bs) |]
compileToMarkup (Node name attributes children isLeaf) =
    emitParts (flattenNodeAll name attributes children isLeaf)
compileToMarkup (Children children) = compileChildList children
compileToMarkup (TextNode value) =
    let bs = Text.encodeUtf8 value
    in [| rawByteString $(TH.lift bs) |]
compileToMarkup (PreEscapedTextNode value) =
    let bs = Text.encodeUtf8 value
    in [| rawByteString $(TH.lift bs) |]
compileToMarkup (SplicedNode expression) = [| toHtml $(pure expression) |]
compileToMarkup (CommentNode value) = [| textComment value |]
compileToMarkup NoRenderCommentNode = [| mempty |]

data Part = S !Text | D TH.ExpQ

flattenNodeAll :: Text -> [Attribute] -> [Node] -> Bool -> [Part]
flattenNodeAll "!DOCTYPE" _ _ _ = [S "<!DOCTYPE HTML>\n"]
flattenNodeAll name attributes children isLeaf =
    S ("<" <> name) :
    concatMap flattenAttr attributes ++
    [S ">"] ++
    if isLeaf then [] else concatMap flattenChild children ++ [S ("</" <> name <> ">")]

flattenAttr :: Attribute -> [Part]
flattenAttr (StaticAttribute attrName (TextValue value)) =
    [S (" " <> attrName <> "=\"" <> value <> "\"")]
flattenAttr (StaticAttribute attrName (ExpressionValue expr)) =
    let prefix = " " <> attrName <> "=\""
    in [D [| applyAttribute $(TH.lift attrName) $(TH.lift prefix) $(pure expr) |]]
flattenAttr (SpreadAttributes expr) =
    [D [| spreadAttributes $(pure expr) |]]

flattenChild :: Node -> [Part]
flattenChild node | isStaticTree node = [S (renderStaticHtml node)]
flattenChild (SplicedNode expression) = [D [| toHtml $(pure expression) |]]
flattenChild (CommentNode value) = [D [| textComment value |]]
flattenChild NoRenderCommentNode = []
flattenChild (Children children) = concatMap flattenChild children
flattenChild (Node name attributes children isLeaf) =
    flattenNodeAll name attributes children isLeaf
flattenChild node = [S (renderStaticHtml node)]

emitParts :: [Part] -> TH.ExpQ
emitParts parts = case mergeParts parts of
    []      -> [| mempty |]
    [single] -> single
    exprs   -> foldl1 (\a b -> [| $a <> $b |]) exprs
  where
    mergeParts [] = []
    mergeParts (S a : S b : rest) = mergeParts (S (a <> b) : rest)
    mergeParts (S t : rest) = let bs = Text.encodeUtf8 t in [| rawByteString $(TH.lift bs) |] : mergeParts rest
    mergeParts (D e : rest) = e : mergeParts rest

compileChildList :: [Node] -> TH.ExpQ
compileChildList children = case compileChildren children of
    []      -> [| mempty |]
    [single] -> single
    compiled -> let xs = TH.listE compiled in [| mconcat $xs |]

compileChildren :: [Node] -> [TH.ExpQ]
compileChildren [] = []
compileChildren nodes =
    case span isStaticTree nodes of
        ([], x:xs) -> compileToMarkup x : compileChildren xs
        (statics, rest)
            | any isNonTrivialStaticNode statics ->
                let bs = Text.encodeUtf8 (foldMap renderStaticHtml statics)
                in [| rawByteString $(TH.lift bs) |] : compileChildren rest
            | otherwise ->
                map compileToMarkup statics ++ compileChildren rest

-- Helpers (same as other QQ modules)
isStaticTree :: Node -> Bool
isStaticTree (Node _ attributes children _) = all isStaticAttribute attributes && all isStaticTree children
isStaticTree (TextNode _) = True
isStaticTree (PreEscapedTextNode _) = True
isStaticTree (SplicedNode _) = False
isStaticTree (Children children) = all isStaticTree children
isStaticTree (CommentNode _) = True
isStaticTree NoRenderCommentNode = True

isStaticAttribute :: Attribute -> Bool
isStaticAttribute (StaticAttribute _ (TextValue _)) = True
isStaticAttribute (StaticAttribute _ (ExpressionValue _)) = False
isStaticAttribute (SpreadAttributes _) = False

isNonTrivialStaticNode :: Node -> Bool
isNonTrivialStaticNode (TextNode _) = False
isNonTrivialStaticNode (PreEscapedTextNode _) = False
isNonTrivialStaticNode NoRenderCommentNode = False
isNonTrivialStaticNode _ = True

renderStaticHtml :: Node -> Text
renderStaticHtml (Node "!DOCTYPE" _ _ _) = "<!DOCTYPE HTML>\n"
renderStaticHtml (Node name attributes children isLeaf) =
    let openTag = "<" <> name <> foldMap renderStaticAttribute attributes <> ">"
    in if isLeaf then openTag else openTag <> foldMap renderStaticHtml children <> "</" <> name <> ">"
renderStaticHtml (TextNode value) = value
renderStaticHtml (PreEscapedTextNode value) = value
renderStaticHtml (SplicedNode _) = error "renderStaticHtml: unexpected SplicedNode"
renderStaticHtml (Children children) = foldMap renderStaticHtml children
renderStaticHtml (CommentNode value) = "<!-- " <> value <> " -->"
renderStaticHtml NoRenderCommentNode = ""

renderStaticAttribute :: Attribute -> Text
renderStaticAttribute (StaticAttribute name (TextValue value)) = " " <> name <> "=\"" <> value <> "\""
renderStaticAttribute _ = error "renderStaticAttribute: unexpected dynamic attribute"
