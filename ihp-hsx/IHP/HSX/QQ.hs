{-# LANGUAGE TemplateHaskell, UndecidableInstances, BangPatterns, PackageImports, FlexibleInstances, OverloadedStrings #-}

{-|
Module: IHP.HSX.QQ
Description: Defines the @[hsx||]@ syntax
Copyright: (c) digitally induced GmbH, 2022
-}
module IHP.HSX.QQ (hsx) where

import           Prelude
import qualified Data.Text as Text
import Data.Text (Text)
import           IHP.HSX.Parser
import qualified "template-haskell" Language.Haskell.TH           as TH
import qualified "template-haskell" Language.Haskell.TH.Syntax           as TH
import           Language.Haskell.TH.Quote
import           Text.Blaze.Html5              ((!))
import qualified Text.Blaze.Html5              as Html5
import Text.Blaze.Html (Html)
import Text.Blaze.Internal (attribute, MarkupM (Parent, Leaf), StaticString (..))
import Data.String.Conversions
import IHP.HSX.ToHtml
import Control.Monad.Fail
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Blaze.Html.Renderer.String as BlazeString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.List (foldl')

hsx :: QuasiQuoter
hsx = QuasiQuoter {
        quoteExp = quoteHsxExpression,
        quotePat = error "quotePat: not defined",
        quoteDec = error "quoteDec: not defined",
        quoteType = error "quoteType: not defined"
    }

quoteHsxExpression :: String -> TH.ExpQ
quoteHsxExpression code = do
        hsxPosition <- findHSXPosition
        expression <- case parseHsx hsxPosition (cs code) of
                Left error   -> fail (Megaparsec.errorBundlePretty error)
                Right result -> pure result
        compileToHaskell expression
    where

        findHSXPosition = do
            loc <- TH.location
            let (line, col) = TH.loc_start loc
            pure $ Megaparsec.SourcePos (TH.loc_filename loc) (Megaparsec.mkPos line) (Megaparsec.mkPos col)

compileToHaskell :: Node -> TH.ExpQ
compileToHaskell (Node name attributes children isLeaf) =
    let
        renderedChildren = TH.listE $ map compileToHaskell children
        stringAttributes = TH.listE $ map toStringAttribute attributes
        openTag :: Text
        openTag = "<" <> tag
        tag :: Text
        tag = cs name
    in
        if isLeaf
            then
                let
                    closeTag :: Text
                    closeTag = ">"
                in [| (applyAttributes (Leaf (textToStaticString $(TH.lift tag)) (textToStaticString $(TH.lift openTag)) (textToStaticString $(TH.lift closeTag)) ()) $(stringAttributes)) |]
            else
                let
                    closeTag :: Text
                    closeTag = "</" <> tag <> ">"
                in [| (applyAttributes (makeParent (textToStaticString $(TH.lift name)) (textToStaticString $(TH.lift openTag)) (textToStaticString $(TH.lift closeTag)) $renderedChildren) $(stringAttributes)) |]

compileToHaskell (Children children) =
    let
        renderedChildren = TH.listE $ map compileToHaskell children
    in [| mconcat $(renderedChildren) |]

compileToHaskell (TextNode value) = [| Html5.preEscapedText value |]
compileToHaskell (PreEscapedTextNode value) = [| Html5.preEscapedText value |]
compileToHaskell (SplicedNode expression) = [| toHtml $(pure expression) |]
compileToHaskell (CommentNode value) = [| Html5.textComment value |]


toStringAttribute :: Attribute -> TH.ExpQ
toStringAttribute (StaticAttribute name (TextValue value)) = do
    let nameWithSuffix = " " <> name <> "=\""
    if Text.null value
        then [| \h -> h ! ((attribute (Html5.textTag name) (Html5.textTag nameWithSuffix)) mempty) |]
        else [| \h -> h ! ((attribute (Html5.textTag name) (Html5.textTag nameWithSuffix)) (Html5.preEscapedTextValue value)) |]

toStringAttribute (StaticAttribute name (ExpressionValue expression)) = let nameWithSuffix = " " <> name <> "=\"" in [| applyAttribute name nameWithSuffix $(pure expression) |]
toStringAttribute (SpreadAttributes expression) = [| spreadAttributes $(pure expression) |]

spreadAttributes :: ApplyAttribute value => [(Text, value)] -> Html5.Html -> Html5.Html
spreadAttributes attributes html = applyAttributes html $ map (\(name, value) -> applyAttribute name (" " <> name <> "=\"") value) attributes

applyAttributes :: Html5.Html -> [Html5.Html -> Html5.Html] -> Html5.Html
applyAttributes element attributes = foldl' (\element attribute -> attribute element) element attributes
{-# INLINE applyAttributes #-}

makeParent :: StaticString -> StaticString -> StaticString -> [Html] -> Html
makeParent tag openTag closeTag children = Parent tag openTag closeTag (mconcat children)
{-# INLINE makeParent #-}

textToStaticString :: Text -> StaticString
textToStaticString text = StaticString (Text.unpack text ++) (Text.encodeUtf8 text) text
{-# INLINE textToStaticString #-}

class ApplyAttribute value where
    applyAttribute :: Text -> Text -> value -> (Html5.Html -> Html5.Html)

instance ApplyAttribute Bool where
    applyAttribute attr attr' True h = h ! (attribute (Html5.textTag attr) (Html5.textTag attr') (Html5.textValue value))
        where
            value = if "data-" `Text.isPrefixOf` attr
                    then "true" -- "true" for data attributes
                    else attr -- normal html boolean attriubtes, like <input disabled="disabled"/>, see https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#boolean-attributes
    applyAttribute attr attr' false h | "data-" `Text.isPrefixOf` attr = h ! (attribute (Html5.textTag attr) (Html5.textTag attr') "false") -- data attribute set to "false"
    applyAttribute attr attr' false h = h -- html boolean attribute, like <input disabled/> will be dropped as there is no other way to specify that it's set to false
    {-# INLINE applyAttribute #-}

instance ApplyAttribute attribute => ApplyAttribute (Maybe attribute) where
    applyAttribute attr attr' (Just value) h = applyAttribute attr attr' value h
    applyAttribute attr attr' Nothing h = h

instance {-# OVERLAPPABLE #-} ConvertibleStrings value Html5.AttributeValue => ApplyAttribute value where
    applyAttribute attr attr' value h = h ! (attribute (Html5.textTag attr) (Html5.textTag attr') (cs value))
    {-# INLINE applyAttribute #-}

instance Show (MarkupM ()) where
    show html = BlazeString.renderHtml html
