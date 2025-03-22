{-# LANGUAGE TemplateHaskell, UndecidableInstances, BangPatterns, PackageImports, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

{-|
Module: IHP.HSX.Lucid2.QQ
Description: Defines the @[hsx||]@ syntax
Copyright: (c) digitally induced GmbH, 2022
-}
module IHP.HSX.Lucid2.QQ
  ( hsx
  , uncheckedHsx
  , customHsx
  , quoteHsxExpression
  ) where

import           Prelude
import Data.Foldable (Foldable(..))
import Data.Text (Text)
import           IHP.HSX.Parser
import           IHP.HSX.Lucid2.Attribute
import qualified "template-haskell" Language.Haskell.TH           as TH
import qualified "template-haskell" Language.Haskell.TH.Syntax           as TH
import           Language.Haskell.TH.Quote
import Data.String.Conversions
import qualified Text.Megaparsec as Megaparsec
import qualified Data.Set as Set
import Lucid.Html5 (doctype_)
import Lucid.Base
  ( Attributes
  , ToHtml (..)
  , makeElement
  , makeElementNoEnd
  )

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
compileToHaskell (Node "!DOCTYPE" [StaticAttribute "html" (TextValue "html")] [] True) = [| doctype_ |]
compileToHaskell (Node name attributes children isLeaf) =
    let
        renderedChildren = TH.listE $ map compileToHaskell children
        listAttributes = TH.listE $ map toLucidAttributes attributes
    in
        if isLeaf
            then
                let
                    element = nodeToLucidLeaf name
                in
                    [| $element $listAttributes |]
            else
                let
                    element = nodeToLucidElement name
                in [| $element $listAttributes (sequence_ @[] $renderedChildren) |]
compileToHaskell (Children children) =
    let
        renderedChildren = TH.listE $ map compileToHaskell children
    in [| (sequence_ @[] $(renderedChildren)) |]

compileToHaskell (TextNode value) = [| toHtmlRaw value |]
compileToHaskell (PreEscapedTextNode value) = [| toHtmlRaw value |]
compileToHaskell (SplicedNode expression) = [| toHtml $(pure expression) |]
compileToHaskell (CommentNode value) = [| toHtmlRaw @Text "<!--" >> toHtmlRaw value >> toHtmlRaw @Text "-->" |]
compileToHaskell NoRenderCommentNode = [| pure () |]

nodeToLucidElement :: Text -> TH.Q TH.Exp
nodeToLucidElement name =
    [| makeElement $(TH.lift name) |]

nodeToLucidLeaf :: Text -> TH.Q TH.Exp
nodeToLucidLeaf name =
    [| makeElementNoEnd $(TH.lift name) |]

toLucidAttributes :: Attribute -> TH.ExpQ
toLucidAttributes (StaticAttribute name (TextValue value)) =
    [| buildAttribute name value |]
toLucidAttributes (StaticAttribute name (ExpressionValue expression)) =
    [| buildAttribute name $(pure expression) |]
toLucidAttributes (SpreadAttributes expression) =
    [| spreadAttributes $(pure expression) |]

spreadAttributes :: (LucidAttributeValue lav) => [(Text, lav)] -> Attributes
spreadAttributes = foldMap' (uncurry buildAttribute)
{-# INLINE spreadAttributes #-}
