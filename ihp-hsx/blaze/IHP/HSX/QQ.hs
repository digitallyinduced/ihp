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
import           IHP.HSX.QQCompiler (Part(..))
import qualified IHP.HSX.QQCompiler as QQ
import qualified "template-haskell" Language.Haskell.TH           as TH
import qualified "template-haskell" Language.Haskell.TH.Syntax           as TH
import           Language.Haskell.TH.Quote
import qualified Text.Blaze.Html5              as Html5
import Text.Blaze.Html (Html)
import Text.Blaze.Internal (MarkupM (Parent, Leaf), StaticString (..), unsafeByteString, textComment, attribute, (!))
import Data.String.Conversions
import IHP.HSX.ToHtml
import qualified Text.Blaze.Html.Renderer.String as BlazeString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.ByteString (ByteString)
import IHP.HSX.Attribute
import qualified Data.Set as Set

hsx :: QuasiQuoter
hsx = QQ.hsxQuasiQuoter
        (HsxSettings
            { checkMarkup = True
            , additionalTagNames = Set.empty
            , additionalAttributeNames = Set.empty
            }
        )
        compileToHaskell

uncheckedHsx :: QuasiQuoter
uncheckedHsx = QQ.hsxQuasiQuoter
        (HsxSettings
            { checkMarkup = False
            , additionalTagNames = Set.empty
            , additionalAttributeNames = Set.empty
            }
        )
        compileToHaskell

customHsx :: HsxSettings -> QuasiQuoter
customHsx settings = QQ.hsxQuasiQuoter settings compileToHaskell

quoteHsxExpression :: HsxSettings -> String -> TH.ExpQ
quoteHsxExpression settings = QQ.quoteHsxExpression settings compileToHaskell

compileToHaskell :: Node -> TH.ExpQ
-- Pre-render fully static subtrees to a single unsafeByteString at compile time
compileToHaskell node
    | isStaticTree node, isNonTrivialStaticNode node =
        let bs = Text.encodeUtf8 (renderStaticHtml node)
        in [| unsafeByteString $(TH.lift bs) |]
-- Elements with only static attributes: flatten to Parts for merging
compileToHaskell (Node name attributes children isLeaf)
    | all isStaticAttribute attributes =
        QQ.emitParts rawEmit (flattenNode name attributes children isLeaf)
    | otherwise =
        -- Has dynamic/spread attributes: use Parent/Leaf + AddAttribute
        let element = if isLeaf then makeLeaf name else makeParentEl name
        in if isLeaf
            then applyDynAttrs element attributes
            else applyDynAttrs [| $element $(QQ.compileChildList compileToHaskell rawEmit children) |] attributes
compileToHaskell (Children children) = QQ.compileChildList compileToHaskell rawEmit children
compileToHaskell (TextNode value) =
    let bs = Text.encodeUtf8 value
    in [| unsafeByteString $(TH.lift bs) |]
compileToHaskell (PreEscapedTextNode value) =
    let bs = Text.encodeUtf8 value
    in [| unsafeByteString $(TH.lift bs) |]
compileToHaskell (SplicedNode expression) = [| toHtml $(pure expression) |]
compileToHaskell (CommentNode value) = [| textComment value |]
compileToHaskell (NoRenderCommentNode) = [| mempty |]

rawEmit :: ByteString -> TH.ExpQ
rawEmit bs = [| unsafeByteString $(TH.lift bs) |]

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

-- | Apply dynamic attributes to an element using the AddAttribute approach.
applyDynAttrs :: TH.ExpQ -> [Attribute] -> TH.ExpQ
applyDynAttrs element [] = element
applyDynAttrs element attributes =
    let stringAttributes = TH.listE $ map toStringAttribute attributes
    in [| applyAttributes $element $stringAttributes |]

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
