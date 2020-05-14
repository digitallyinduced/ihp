{-# LANGUAGE TemplateHaskell, TypeFamilies, BangPatterns #-}

module TurboHaskell.HtmlSupport.QQ (hsx) where

import           ClassyPrelude
import           TurboHaskell.HtmlSupport.Parser
import           Language.Haskell.Meta         (parseExp)
import qualified "template-haskell" Language.Haskell.TH           as TH
import           Language.Haskell.TH.Quote
import           Text.Blaze.Html5              ((!))
import qualified Text.Blaze.Html5              as Html5
import           Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html (Html)
import Text.Blaze.Internal (attribute, MarkupM (Parent, Leaf), StaticString)
import Data.String.Conversions (cs)
import TurboHaskell.HtmlSupport.ToHtml
import qualified Debug.Trace
import qualified Language.Haskell.Exts.Syntax as HS
import Control.Monad.Fail
import qualified Text.Megaparsec as Megaparsec

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
compileToHaskell (Node name attributes children) =
    let
        renderedChildren = TH.listE $ map compileToHaskell children
    in case attributes of
        StaticAttributes attributes ->
            let
                stringAttributes = TH.listE $ map toStringAttribute attributes
            in [| (applyAttributes (makeElement name $(renderedChildren)) $(stringAttributes)) |]

compileToHaskell (Children children) =
    let
        renderedChildren = TH.listE $ map compileToHaskell children
    in [| foldl' (>>) mempty $(renderedChildren) |]

compileToHaskell (TextNode value) = let value' :: String = cs value in [| Html5.string value' |]
compileToHaskell (SplicedNode code) =
    case parseExp (cs code) of
        Right expression -> let patched = patchExpr expression in [| toHtml $(pure patched) |]
        Left error -> fail ("compileToHaskell(" <> (cs code) <> "): " <> show error)

patchExpr :: TH.Exp -> TH.Exp
patchExpr (TH.UInfixE (TH.VarE varName) (TH.VarE hash) (TH.VarE labelValue)) | hash == TH.mkName "#" = TH.AppE (TH.VarE varName) fromLabel
    where
            fromLabel = TH.AppTypeE (TH.VarE (TH.mkName "fromLabel")) (TH.LitT (TH.StrTyLit (show labelValue)))
--- UInfixE (UInfixE a (VarE |>) (VarE get)) (VarE #) (VarE firstName)
patchExpr input@(TH.UInfixE (TH.UInfixE a (TH.VarE arrow) (TH.VarE get)) (TH.VarE hash) (TH.VarE labelValue)) | (hash == TH.mkName "#") && (arrow == TH.mkName "|>") && (get == TH.mkName "get") =
        (TH.UInfixE (patchExpr a) (TH.VarE arrow) (TH.AppE (TH.VarE get) fromLabel))
    where
            fromLabel = TH.AppTypeE (TH.VarE (TH.mkName "fromLabel")) (TH.LitT (TH.StrTyLit (show labelValue)))
-- UInfixE (UInfixE a (VarE $) (VarE get)) (VarE #) (AppE (VarE id) (VarE checklist))
patchExpr (TH.UInfixE (TH.UInfixE a b get) (TH.VarE hash) (TH.AppE (TH.VarE labelValue) (TH.VarE d))) | (hash == TH.mkName "#") =
        TH.UInfixE (patchExpr a) (patchExpr b) (TH.AppE (TH.AppE get fromLabel) (TH.VarE d))
    where
            fromLabel = TH.AppTypeE (TH.VarE (TH.mkName "fromLabel")) (TH.LitT (TH.StrTyLit (show labelValue)))
patchExpr (TH.UInfixE (TH.VarE varName) (TH.VarE hash) (TH.AppE (TH.VarE labelValue) arg)) | hash == TH.mkName "#" = TH.AppE (TH.AppE (TH.VarE varName) fromLabel) arg
    where
            fromLabel = TH.AppTypeE (TH.VarE (TH.mkName "fromLabel")) (TH.LitT (TH.StrTyLit (show labelValue)))
patchExpr (TH.UInfixE (TH.VarE a) (TH.VarE hash) (TH.AppE (TH.VarE labelValue) (TH.VarE b))) | hash == TH.mkName "#" =
        TH.AppE (TH.AppE (TH.VarE a) fromLabel) (TH.VarE b)
    where
            fromLabel = TH.AppTypeE (TH.VarE (TH.mkName "fromLabel")) (TH.LitT (TH.StrTyLit (show labelValue)))

patchExpr (TH.UInfixE a b c) = TH.UInfixE (patchExpr a) (patchExpr b) (patchExpr c)
patchExpr (TH.ParensE e) = TH.ParensE (patchExpr e)
patchExpr (TH.RecUpdE a b) = TH.RecUpdE (patchExpr a) b
patchExpr (TH.AppE a b) = TH.AppE (patchExpr a) (patchExpr b)
patchExpr (TH.LamE a b) = TH.LamE a (patchExpr b)
patchExpr (TH.LetE a b) = TH.LetE a' (patchExpr b)
    where
        a' = map patchDec a
        patchDec (TH.ValD a (TH.NormalB b) c) = (TH.ValD a (TH.NormalB (patchExpr b)) c)
        patchDec a = a
patchExpr (TH.CondE a b c) = TH.CondE (patchExpr a) (patchExpr b) (patchExpr c)
patchExpr (TH.SigE a b) = TH.SigE (patchExpr a) b
patchExpr e = e

-- UInfixE (VarE get) (VarE #) (AppE (VarE id) (VarE step))

-- UInfixE (UInfixE (UInfixE (UInfixE (UInfixE (UInfixE (VarE currentUser) (VarE |>) (VarE get)) (VarE #) (VarE firstName)) (VarE <>) (LitE (StringL " "))) (VarE <>) (VarE currentUser)) (VarE |>) (VarE get)) (VarE #) (VarE lastName)
-- UInfixE (UInfixE (VarE tshow) (VarE $) (VarE get)) (VarE #) (AppE (VarE id) (VarE checklist))


toStringAttribute :: (Text, AttributeValue) -> TH.ExpQ
toStringAttribute (name', TextValue value) = do
    let name :: String = cs name'
    let nameWithSuffix = " " <> name <> "=\""
    if null value
        then [| (attribute name nameWithSuffix) mempty |]
        else [| (attribute name nameWithSuffix) (cs value :: Html5.AttributeValue) |]

toStringAttribute (name', ExpressionValue code) = do
    let name :: String = cs name'
    let nameWithSuffix = " " <> name <> "=\""
    case parseExp (cs code) of
        Right expression -> let patched = patchExpr expression in [| (attribute name nameWithSuffix) (cs $(pure patched)) |]
        Left error -> fail ("toStringAttribute.compileToHaskell(" <> cs code <> "): " <> show error)


{-# INLINE applyAttributes #-}
applyAttributes :: Html5.Html -> [Html5.Attribute] -> Html5.Html
applyAttributes !el [] = el
applyAttributes !el (x:xs) = applyAttributes (el ! x) xs

{-# INLINE makeElement #-}
makeElement :: Text -> [Html] -> Html
makeElement name' children =
    let
        name :: String
        name = cs name'
        {-# INLINE element #-}
        element :: Html -> Html
        element = (Parent (fromString name) (fromString $ "<" <> name) (fromString $ "</" <> name <> ">"))
        {-# INLINE leaf #-}
        leaf = (Leaf (fromString name) (fromString $ "<" <> name) (fromString $ ">"))
    in if name' `elem` parents then
            let !children' = case length children of
                    0 -> mempty
                    1 -> unsafeHead children
                    _ -> (foldl' (<>) (unsafeHead children) (unsafeTail children))
            in element children'
        else
            if name' `elem` leafs then
                leaf ()
            else
                error ("makeElement: Unknown tag "  <> show name)
