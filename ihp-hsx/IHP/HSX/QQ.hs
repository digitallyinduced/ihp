{-# LANGUAGE TemplateHaskell, UndecidableInstances, BangPatterns, PackageImports, FlexibleInstances, OverloadedStrings #-}

{-|
Module: IHP.HSX.QQ
Description: Defines the @[hsx||]@ syntax
Copyright: (c) digitally induced GmbH, 2022
-}
module IHP.HSX.QQ (hsx) where

import           Prelude
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
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Blaze.Html.Renderer.String as BlazeString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.List (foldl')
import IHP.HSX.Attribute
import qualified Text.Blaze.Html5.Attributes as Attributes
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import IHP.HSX.Html
import qualified IHP.HSX.Html as Html
import qualified Data.Maybe as Maybe

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
        extensions <- TH.extsEnabled
        expression <- case parseHsx hsxPosition extensions (cs code) of
                Left error   -> fail (Megaparsec.errorBundlePretty error)
                Right result -> pure result
        compileToHaskell expression
    where

        findHSXPosition = do
            loc <- TH.location
            let (line, col) = TH.loc_start loc
            pure $ Megaparsec.SourcePos (TH.loc_filename loc) (Megaparsec.mkPos line) (Megaparsec.mkPos col)

compileToHaskell :: Node -> TH.ExpQ
compileToHaskell (Node "!DOCTYPE" [StaticAttribute "html" (TextValue "html")] [] True) = [| Html.preEscapedToHtml "<!DOCTYPE HTML>\n" |]
compileToHaskell (Node name attributes children isLeaf) =
    let
        renderedChildren = TH.listE $ map compileToHaskell children
        stringAttributes = TH.listE $ map toStringAttribute attributes
        startTag = preEscapedToHtml ("<" <> name)
    in
        if isLeaf
            then
                if List.null attributes
                    then let tag = preEscapedToHtml ("<" <> name <> ">") in [| tag |]
                    else [| startTag <> Html.spaceSepWithLeadingSpace $stringAttributes <> ">" |]
            else
                let
                    endTag = preEscapedToHtml ("</" <> name <> ">")
                in
                    if List.null attributes
                        then let startTag = preEscapedToHtml ("<" <> name <> ">") in [| startTag <> Html.concat $renderedChildren <> endTag |]
                        else [| startTag <> Html.spaceSepWithLeadingSpace $stringAttributes <> ">" <> Html.concat $renderedChildren <> endTag |]
compileToHaskell (Children children) =
    let
        renderedChildren = TH.listE $ map compileToHaskell children
    in [| Html.concat $(renderedChildren) |]

compileToHaskell (TextNode value) = let value' = Html.preEscapedToHtml value in [| value' |]
compileToHaskell (PreEscapedTextNode value) = let value' = Html.preEscapedToHtml value in [| value' |]
compileToHaskell (SplicedNode expression) = [| toHtml $(pure expression) |]
compileToHaskell (CommentNode value) = let value' = Html.preEscapedToHtml ("<!-- " <> value <> " -->") in [| value' |]
compileToHaskell (NoRenderCommentNode) = [| "" |]

toStringAttribute :: Attribute -> TH.ExpQ
toStringAttribute (StaticAttribute name (TextValue value)) =
    attributeFromName name value

toStringAttribute (StaticAttribute name (ExpressionValue expression)) =
    [| case attributeValueToText name $(pure expression) of
        Just value -> preEscapedToHtml name <> "=" <> value
        Nothing -> ""
    |]
toStringAttribute (SpreadAttributes expression) = [| spreadAttributes $(pure expression) |]

attributeFromName :: Text -> Text -> TH.ExpQ
attributeFromName name value =
    let
        staticAttribute = preEscapedToHtml (name <> "=\"" <> value <> "\"")
    in
        [| staticAttribute |]


spreadAttributes :: AttributeConverter value => [(Text, value)] -> IHP.HSX.Html.Html
spreadAttributes attributes = Html.spaceSep $ Maybe.mapMaybe (\(name, value) -> attributeValueToText name value) attributes
{-# INLINE spreadAttributes #-}