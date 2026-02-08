{-# LANGUAGE TemplateHaskell, UndecidableInstances, BangPatterns, PackageImports, FlexibleInstances, OverloadedStrings #-}

{-|
Module: IHP.HSX.QQ
Description: Defines the @[hsx||]@ syntax
Copyright: (c) digitally induced GmbH, 2022
-}
module IHP.HSX.QQ
  ( hsx
  , uncheckedHsx
  , customHsx
  , quoteHsxExpression
  ) where

import           Prelude
import Data.Text (Text)
import           IHP.HSX.Parser
import qualified "template-haskell" Language.Haskell.TH           as TH
import qualified "template-haskell" Language.Haskell.TH.Syntax           as TH
import           Language.Haskell.TH.Quote
import qualified Text.Blaze.Html5              as Html5
import Text.Blaze.Html (Html)
import Text.Blaze.Internal (MarkupM (Parent, Leaf), StaticString (..), unsafeByteString, textComment, attribute, (!))
import Data.String.Conversions
import IHP.HSX.ToHtml
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Blaze.Html.Renderer.String as BlazeString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import IHP.HSX.Attribute
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
        compileToHaskell expression
    where

        findHSXPosition = do
            loc <- TH.location
            let (line, col) = TH.loc_start loc
            pure $ Megaparsec.SourcePos (TH.loc_filename loc) (Megaparsec.mkPos line) (Megaparsec.mkPos col)

compileToHaskell :: Node -> TH.ExpQ
-- Pre-render fully static subtrees to a single unsafeByteString at compile time
compileToHaskell node
    | isStaticTree node, isNonTrivialStaticNode node =
        let bs = Text.encodeUtf8 (renderStaticHtml node)
        in [| unsafeByteString $(TH.lift bs) |]
-- Elements with only static attributes: flatten to Parts for merging
compileToHaskell (Node name attributes children isLeaf)
    | all isStaticAttribute attributes =
        emitParts (flattenNode name attributes children isLeaf)
    | otherwise =
        -- Has dynamic/spread attributes: use Parent/Leaf + AddAttribute
        let element = if isLeaf then makeLeaf name else makeParentEl name
        in if isLeaf
            then applyDynAttrs element attributes
            else applyDynAttrs [| $element $(compileChildList children) |] attributes
compileToHaskell (Children children) = compileChildList children
compileToHaskell (TextNode value) =
    let bs = Text.encodeUtf8 value
    in [| unsafeByteString $(TH.lift bs) |]
compileToHaskell (PreEscapedTextNode value) =
    let bs = Text.encodeUtf8 value
    in [| unsafeByteString $(TH.lift bs) |]
compileToHaskell (SplicedNode expression) = [| toHtml $(pure expression) |]
compileToHaskell (CommentNode value) = [| textComment value |]
compileToHaskell (NoRenderCommentNode) = [| mempty |]

-- | A part is either static text (merged at compile time) or a dynamic expression.
data Part = S !Text | D TH.ExpQ

-- | Flatten a static-attribute node into parts, recursing into children.
-- Adjacent static parts are merged into single ByteString literals.
flattenNode :: Text -> [Attribute] -> [Node] -> Bool -> [Part]
flattenNode "!DOCTYPE" _ _ _ = [S "<!DOCTYPE HTML>\n"]
flattenNode name attributes children isLeaf
    | isLeaf    = [S ("<" <> name <> attrs <> ">")]
    | otherwise = S ("<" <> name <> attrs <> ">") : concatMap flattenChild children ++ [S ("</" <> name <> ">")]
    where attrs = foldMap renderStaticAttribute attributes

-- | Flatten a child node into parts.
flattenChild :: Node -> [Part]
flattenChild node | isStaticTree node = [S (renderStaticHtml node)]
flattenChild (SplicedNode expression) = [D [| toHtml $(pure expression) |]]
flattenChild (CommentNode value) = [D [| textComment value |]]
flattenChild (NoRenderCommentNode) = []
flattenChild (Children children) = concatMap flattenChild children
flattenChild (Node name attributes children isLeaf)
    | all isStaticAttribute attributes = flattenNode name attributes children isLeaf
    | otherwise = [D (compileToHaskell (Node name attributes children isLeaf))]
flattenChild node = [S (renderStaticHtml node)]

-- | Merge adjacent S parts and emit as a chain of @<>@.
emitParts :: [Part] -> TH.ExpQ
emitParts parts = case mergeParts parts of
    []      -> [| mempty |]
    [single] -> single
    exprs   -> foldl1 (\a b -> [| $a <> $b |]) exprs
  where
    mergeParts [] = []
    mergeParts (S a : S b : rest) = mergeParts (S (a <> b) : rest)
    mergeParts (S t : rest) = let bs = Text.encodeUtf8 t in [| unsafeByteString $(TH.lift bs) |] : mergeParts rest
    mergeParts (D e : rest) = e : mergeParts rest

-- | Apply dynamic attributes to an element using the AddAttribute approach.
applyDynAttrs :: TH.ExpQ -> [Attribute] -> TH.ExpQ
applyDynAttrs element [] = element
applyDynAttrs element attributes =
    let stringAttributes = TH.listE $ map toStringAttribute attributes
    in [| applyAttributes $element $stringAttributes |]

-- | Compile a child list to a single expression.
compileChildList :: [Node] -> TH.ExpQ
compileChildList children = case compileChildren children of
    []      -> [| mempty |]
    [single] -> single
    compiled -> let xs = TH.listE compiled in [| mconcat $xs |]

-- | Returns True if the entire subtree is static (no dynamic content).
isStaticTree :: Node -> Bool
isStaticTree (Node _ attributes children _) =
    all isStaticAttribute attributes && all isStaticTree children
isStaticTree (TextNode _)          = True
isStaticTree (PreEscapedTextNode _) = True
isStaticTree (SplicedNode _)       = False
isStaticTree (Children children)   = all isStaticTree children
isStaticTree (CommentNode _)       = True
isStaticTree (NoRenderCommentNode) = True

isStaticAttribute :: Attribute -> Bool
isStaticAttribute (StaticAttribute _ (TextValue _))      = True
isStaticAttribute (StaticAttribute _ (ExpressionValue _)) = False
isStaticAttribute (SpreadAttributes _)                    = False

-- | Returns True if a node is worth pre-rendering.
-- Bare TextNode/PreEscapedTextNode already compile to unsafeByteString.
isNonTrivialStaticNode :: Node -> Bool
isNonTrivialStaticNode (TextNode _)          = False
isNonTrivialStaticNode (PreEscapedTextNode _) = False
isNonTrivialStaticNode (NoRenderCommentNode) = False
isNonTrivialStaticNode _                     = True

-- | Render a static Node tree to HTML Text at compile time.
renderStaticHtml :: Node -> Text
renderStaticHtml (Node "!DOCTYPE" _ _ _) = "<!DOCTYPE HTML>\n"
renderStaticHtml (Node name attributes children isLeaf) =
    let openTag = "<" <> name <> foldMap renderStaticAttribute attributes <> ">"
    in if isLeaf
        then openTag
        else openTag <> foldMap renderStaticHtml children <> "</" <> name <> ">"
renderStaticHtml (TextNode value)          = value
renderStaticHtml (PreEscapedTextNode value) = value
renderStaticHtml (SplicedNode _)           = error "renderStaticHtml: unexpected SplicedNode"
renderStaticHtml (Children children)       = foldMap renderStaticHtml children
renderStaticHtml (CommentNode value)       = "<!-- " <> value <> " -->"
renderStaticHtml (NoRenderCommentNode)     = ""

renderStaticAttribute :: Attribute -> Text
renderStaticAttribute (StaticAttribute name (TextValue value)) =
    " " <> name <> "=\"" <> value <> "\""
renderStaticAttribute _ = error "renderStaticAttribute: unexpected dynamic attribute"

-- | Compile a list of children, coalescing adjacent static siblings
-- into single unsafeByteString chunks.
compileChildren :: [Node] -> [TH.ExpQ]
compileChildren [] = []
compileChildren nodes =
    case span isStaticTree nodes of
        -- Leading dynamic node: compile it, continue
        ([], x:xs) -> compileToHaskell x : compileChildren xs
        -- Leading static batch worth coalescing
        (statics, rest)
            | any isNonTrivialStaticNode statics ->
                let bs = Text.encodeUtf8 (foldMap renderStaticHtml statics)
                in [| unsafeByteString $(TH.lift bs) |] : compileChildren rest
            -- Trivial statics (bare text nodes): compile individually
            | otherwise ->
                map compileToHaskell statics ++ compileChildren rest

-- | Create a Parent element for the AddAttribute approach.
makeParentEl :: Text -> TH.ExpQ
makeParentEl name =
    [| makeParent (textToStaticString $(TH.lift name)) (textToStaticString $(TH.lift ("<" <> name))) (textToStaticString $(TH.lift ("</" <> name <> ">"))) |]

-- | Create a Leaf element for the AddAttribute approach.
makeLeaf :: Text -> TH.ExpQ
makeLeaf name =
    [| (Leaf (textToStaticString $(TH.lift name)) (textToStaticString $(TH.lift ("<" <> name))) (textToStaticString $(TH.lift (">" :: Text))) ()) |]

toStringAttribute :: Attribute -> TH.ExpQ
toStringAttribute (StaticAttribute name (TextValue value)) =
    let nameWithSuffix = " " <> name <> "=\""
    in if Text.null value
        then [| (! attribute (Html5.textTag name) (Html5.textTag nameWithSuffix) mempty) |]
        else [| (! attribute (Html5.textTag name) (Html5.textTag nameWithSuffix) (Html5.preEscapedTextValue value)) |]
toStringAttribute (StaticAttribute name (ExpressionValue expression)) = let nameWithSuffix = " " <> name <> "=\"" in [| applyAttribute name nameWithSuffix $(pure expression) |]
toStringAttribute (SpreadAttributes expression) = [| spreadAttributes $(pure expression) |]

spreadAttributes :: ApplyAttribute value => [(Text, value)] -> Html5.Html -> Html5.Html
spreadAttributes attributes html = applyAttributes html $ map (\(name, value) -> applyAttribute name (" " <> name <> "=\"") value) attributes
{-# INLINE spreadAttributes #-}

applyAttributes :: Html5.Html -> [Html5.Html -> Html5.Html] -> Html5.Html
applyAttributes element (attribute:rest) = applyAttributes (attribute element) rest
applyAttributes element [] = element
{-# INLINE applyAttributes #-}

makeParent :: StaticString -> StaticString -> StaticString -> Html -> Html
makeParent tag openTag closeTag children = Parent tag openTag closeTag children
{-# INLINE makeParent #-}

textToStaticString :: Text -> StaticString
textToStaticString text = StaticString (Text.unpack text ++) (Text.encodeUtf8 text) text
{-# INLINE textToStaticString #-}

instance Show (MarkupM ()) where
    show html = BlazeString.renderHtml html
