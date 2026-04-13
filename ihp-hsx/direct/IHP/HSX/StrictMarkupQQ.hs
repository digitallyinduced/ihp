{-# LANGUAGE TemplateHaskell, UndecidableInstances, BangPatterns, PackageImports, FlexibleInstances, OverloadedStrings #-}

{-|
Module: IHP.HSX.StrictMarkupQQ
Description: Quasiquoter for bytestring-strict-builder based HSX backend.
Copyright: (c) digitally induced GmbH, 2024
-}
module IHP.HSX.StrictMarkupQQ
  ( hsx
  , uncheckedHsx
  , customHsx
  ) where

import           Prelude
import Data.Text (Text)
import           IHP.HSX.Parser
import           IHP.HSX.QQCompiler (Part(..))
import qualified IHP.HSX.QQCompiler as QQ
import qualified "template-haskell" Language.Haskell.TH           as TH
import qualified "template-haskell" Language.Haskell.TH.Syntax    as TH
import           Language.Haskell.TH.Quote
import IHP.HSX.StrictMarkup
import qualified Data.Text.Encoding as Text
import Data.ByteString (ByteString)
import qualified Data.Set as Set

hsx :: QuasiQuoter
hsx = QQ.hsxQuasiQuoter
        (HsxSettings
            { checkMarkup = True
            , additionalTagNames = Set.empty
            , additionalAttributeNames = Set.empty
            }
        )
        compileToMarkup

uncheckedHsx :: QuasiQuoter
uncheckedHsx = QQ.hsxQuasiQuoter
        (HsxSettings
            { checkMarkup = False
            , additionalTagNames = Set.empty
            , additionalAttributeNames = Set.empty
            }
        )
        compileToMarkup

customHsx :: HsxSettings -> QuasiQuoter
customHsx settings = QQ.hsxQuasiQuoter settings compileToMarkup

compileToMarkup :: Node -> TH.ExpQ
compileToMarkup node
    | isStaticTree node, isNonTrivialStaticNode node =
        let bs = Text.encodeUtf8 (renderStaticHtml node)
        in [| rawByteString $(TH.lift bs) |]
compileToMarkup (Node name attributes children isLeaf) =
    QQ.emitParts rawEmit (QQ.flattenNodeAll myFlattenAttr myFlattenChild name attributes children isLeaf)
compileToMarkup (Children children) = QQ.compileChildList compileToMarkup rawEmit children
compileToMarkup (TextNode value) =
    let bs = Text.encodeUtf8 value
    in [| rawByteString $(TH.lift bs) |]
compileToMarkup (PreEscapedTextNode value) =
    let bs = Text.encodeUtf8 value
    in [| rawByteString $(TH.lift bs) |]
compileToMarkup (SplicedNode expression) = [| toHtml $(pure expression) |]
compileToMarkup (CommentNode value) = [| textComment value |]
compileToMarkup NoRenderCommentNode = [| mempty |]

rawEmit :: ByteString -> TH.ExpQ
rawEmit bs = [| rawByteString $(TH.lift bs) |]

myFlattenAttr :: Attribute -> [Part]
myFlattenAttr = QQ.flattenAttr
    (\name prefix expr -> [| applyAttribute $(TH.lift name) $(TH.lift prefix) $(pure expr) |])
    (\expr -> [| spreadAttributes $(pure expr) |])

myFlattenChild :: Node -> [Part]
myFlattenChild = QQ.flattenChild
    (\name attrs children isLeaf -> QQ.flattenNodeAll myFlattenAttr myFlattenChild name attrs children isLeaf)
    (\expr -> [| toHtml $(pure expr) |])
    (\val -> [| textComment val |])
