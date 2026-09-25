{-# LANGUAGE TemplateHaskell, BangPatterns, PackageImports, OverloadedStrings #-}

{-|
Module: IHP.HSX.QQCompiler
Description: Shared quasiquoter compilation helpers for HSX backends.
Copyright: (c) digitally induced GmbH, 2024

This module provides the common infrastructure used by all HSX quasiquoter
backends (Blaze, Direct, Strict). It includes:

- The 'Part' type for compile-time static\/dynamic merging
- 'emitParts' for merging adjacent static parts into single ByteString literals
- 'compileChildList' and 'compileChildren' for coalescing static siblings
- 'flattenNodeAll', 'flattenAttr', 'flattenChild' for the flatten-based compilation
  used by the Direct and Strict backends
- 'hsxQuasiQuoter' and 'quoteHsxExpression' for QQ boilerplate
-}
module IHP.HSX.QQCompiler
  ( Part(..)
  , emitParts
  , compileChildList
  , compileChildren
  , flattenNodeAll
  , flattenAttr
  , flattenChild
  , hsxQuasiQuoter
  , quoteHsxExpression
  ) where

import           Prelude
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.ByteString (ByteString)
import           IHP.HSX.Parser
import qualified "template-haskell" Language.Haskell.TH           as TH
import qualified "template-haskell" Language.Haskell.TH.Syntax    as TH
import           Language.Haskell.TH.Quote
import qualified Text.Megaparsec as Megaparsec
import Data.String.Conversions (cs)

-- | A part is either static text (merged at compile time) or a dynamic expression.
data Part = S !Text | D TH.ExpQ

-- | Merge adjacent S parts and emit as a chain of @<>@.
-- The @rawEmit@ function determines how ByteString literals are emitted
-- (e.g. @unsafeByteString@ for Blaze, @rawByteString@ for Direct\/Strict).
emitParts :: (ByteString -> TH.ExpQ) -> [Part] -> TH.ExpQ
emitParts rawEmit parts = case mergeParts parts of
    []       -> [| mempty |]
    [single] -> single
    exprs    -> foldl1 (\a b -> [| $a <> $b |]) exprs
  where
    mergeParts [] = []
    mergeParts (S a : S b : rest) = mergeParts (S (a <> b) : rest)
    mergeParts (S t : rest) = rawEmit (Text.encodeUtf8 t) : mergeParts rest
    mergeParts (D e : rest) = e : mergeParts rest

-- | Compile a child list to a single expression.
compileChildList :: (Node -> TH.ExpQ) -> (ByteString -> TH.ExpQ) -> [Node] -> TH.ExpQ
compileChildList compileFn rawEmit children = case compileChildren compileFn rawEmit children of
    []       -> [| mempty |]
    [single] -> single
    compiled -> let xs = TH.listE compiled in [| mconcat $xs |]

-- | Compile a list of children, coalescing adjacent static siblings
-- into single ByteString chunks.
compileChildren :: (Node -> TH.ExpQ) -> (ByteString -> TH.ExpQ) -> [Node] -> [TH.ExpQ]
compileChildren _ _ [] = []
compileChildren compileFn rawEmit nodes =
    case span isStaticTree nodes of
        -- Leading dynamic node: compile it, continue
        ([], x:xs) -> compileFn x : compileChildren compileFn rawEmit xs
        -- Leading static batch worth coalescing
        (statics, rest)
            | any isNonTrivialStaticNode statics ->
                let bs = Text.encodeUtf8 (foldMap renderStaticHtml statics)
                in rawEmit bs : compileChildren compileFn rawEmit rest
            -- Trivial statics (bare text nodes): compile individually
            | otherwise ->
                map compileFn statics ++ compileChildren compileFn rawEmit rest

-- | Flatten ANY node into parts, handling both static and dynamic attributes uniformly.
-- Used by the Direct and Strict backends (not Blaze, which needs Parent\/Leaf\/AddAttribute).
--
-- Parameters:
--   @attrFn@: how to flatten an individual attribute into parts
--   @childFn@: how to flatten a child node into parts
flattenNodeAll
    :: (Attribute -> [Part])    -- ^ attribute flattener
    -> (Node -> [Part])         -- ^ child flattener
    -> Text -> [Attribute] -> [Node] -> Bool -> [Part]
flattenNodeAll _ _ "!DOCTYPE" _ _ _ = [S "<!DOCTYPE HTML>\n"]
flattenNodeAll attrFn childFn name attributes children isLeaf =
    S ("<" <> name) :
    concatMap attrFn attributes ++
    [S ">"] ++
    if isLeaf then [] else concatMap childFn children ++ [S ("</" <> name <> ">")]

-- | Flatten an attribute into parts.
--
-- Parameters:
--   @dynAttrFn@: generates a TH expression for a dynamic attribute (receives name, prefix, expression)
--   @spreadFn@: generates a TH expression for spread attributes (receives expression)
flattenAttr
    :: (Text -> Text -> TH.Exp -> TH.ExpQ)   -- ^ dynamic attribute handler
    -> (TH.Exp -> TH.ExpQ)                    -- ^ spread handler
    -> Attribute -> [Part]
flattenAttr _ _ (StaticAttribute attrName (TextValue value)) =
    [S (" " <> attrName <> "=\"" <> value <> "\"")]
flattenAttr dynAttrFn _ (StaticAttribute attrName (ExpressionValue expr)) =
    let prefix = " " <> attrName <> "=\""
    in [D (dynAttrFn attrName prefix expr)]
flattenAttr _ spreadFn (SpreadAttributes expr) =
    [D (spreadFn expr)]

-- | Flatten a child node into parts.
--
-- Parameters:
--   @nodeFn@: how to flatten a complete Node (typically @flattenNodeAll@ partially applied)
--   @spliceFn@: generates a TH expression for a spliced node (receives expression)
--   @commentFn@: generates a TH expression for a comment node (receives comment text)
flattenChild
    :: (Text -> [Attribute] -> [Node] -> Bool -> [Part])  -- ^ node flattener
    -> (TH.Exp -> TH.ExpQ)                                -- ^ spliced node handler
    -> (Text -> TH.ExpQ)                                   -- ^ comment handler
    -> Node -> [Part]
flattenChild _ _ _ node | isStaticTree node = [S (renderStaticHtml node)]
flattenChild _ spliceFn _ (SplicedNode expression) = [D (spliceFn expression)]
flattenChild _ _ commentFn (CommentNode value) = [D (commentFn value)]
flattenChild _ _ _ NoRenderCommentNode = []
flattenChild nodeFn spliceFn commentFn (Children children) =
    concatMap (flattenChild nodeFn spliceFn commentFn) children
flattenChild nodeFn _ _ (Node name attributes children isLeaf) =
    nodeFn name attributes children isLeaf
flattenChild _ _ _ node = [S (renderStaticHtml node)]

-- | Create a QuasiQuoter from settings and a node compiler.
hsxQuasiQuoter :: HsxSettings -> (Node -> TH.ExpQ) -> QuasiQuoter
hsxQuasiQuoter settings compileFn =
    QuasiQuoter
        { quoteExp = quoteHsxExpression settings compileFn
        , quotePat = error "quotePat: not defined"
        , quoteDec = error "quoteDec: not defined"
        , quoteType = error "quoteType: not defined"
        }

-- | Parse HSX and compile using the provided compilation function.
quoteHsxExpression :: HsxSettings -> (Node -> TH.ExpQ) -> String -> TH.ExpQ
quoteHsxExpression settings compileFn code = do
    hsxPosition <- findHSXPosition
    extensions <- TH.extsEnabled
    expression <- case parseHsx settings hsxPosition extensions (cs code) of
        Left error   -> fail (Megaparsec.errorBundlePretty error)
        Right result -> pure result
    compileFn expression
  where
    findHSXPosition = do
        loc <- TH.location
        let (line, col) = TH.loc_start loc
        pure $ Megaparsec.SourcePos (TH.loc_filename loc) (Megaparsec.mkPos line) (Megaparsec.mkPos col)
