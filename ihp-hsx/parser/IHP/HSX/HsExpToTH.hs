{-# LANGUAGE ViewPatterns, CPP #-}
{-|
Module: IHP.HSX.HsExpToTH
Copyright: (c) digitally induced GmbH, 2022
Description: Converts Haskell AST to Template Haskell AST

Based on https://github.com/guibou/PyF/blob/b3aaee12d34380e55aa3909690041eccb8fcf001/src/PyF/Internal/Meta.hs
-}
module IHP.HSX.HsExpToTH (toExp) where

import Prelude

import GHC.Hs.Expr as Expr
import GHC.Hs.Extension as Ext
import GHC.Hs.Pat as Pat
import GHC.Hs.Lit
import qualified GHC.Hs.Utils as Utils
import qualified Data.ByteString as B
import qualified Language.Haskell.TH.Syntax as TH
import GHC.Types.SrcLoc
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Data.FastString
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)
import GHC.Types.Basic (Boxity(..))
import GHC.Types.SourceText (il_value, rationalFromFractionalLit)
import qualified GHC.Unit.Module as Module
import GHC.Stack
import qualified Data.List.NonEmpty as NonEmpty
import Language.Haskell.Syntax.Type
#if __GLASGOW_HASKELL__ >= 906
import Language.Haskell.Syntax.Basic
#endif


fl_value = rationalFromFractionalLit

toLit :: HsLit GhcPs -> TH.Lit
toLit (HsChar _ c) = TH.CharL c
toLit (HsCharPrim _ c) = TH.CharPrimL c
toLit (HsString _ s) = TH.StringL (unpackFS s)
toLit (HsStringPrim _ s) = TH.StringPrimL (B.unpack s)
toLit (HsInt _ i) = TH.IntegerL (il_value i)
toLit (HsIntPrim _ i) = TH.IntPrimL i
toLit (HsWordPrim _ i) = TH.WordPrimL i
toLit (HsInt64Prim _ i) = TH.IntegerL i
toLit (HsWord64Prim _ i) = TH.WordPrimL i
toLit (HsInteger _ i _) = TH.IntegerL i
toLit (HsRat _ f _) = TH.FloatPrimL (fl_value f)
toLit (HsFloatPrim _ f) = TH.FloatPrimL (fl_value f)
toLit (HsDoublePrim _ f) = TH.DoublePrimL (fl_value f)

toLit' :: OverLitVal -> TH.Lit
toLit' (HsIntegral i) = TH.IntegerL (il_value i)
toLit' (HsFractional f) = TH.RationalL (fl_value f)
toLit' (HsIsString _ fs) = TH.StringL (unpackFS fs)

toType :: HsType GhcPs -> TH.Type
toType (HsWildCardTy _) = TH.WildCardT
toType (HsTyVar _ _ n) =
  let n' = unLoc n
   in if isRdrTyVar n'
        then TH.VarT (toName n')
        else TH.ConT (toName n')
toType t = todo "toType" t

toName :: RdrName -> TH.Name
toName n = case n of
  (Unqual o) -> TH.mkName (occNameString o)
  (Qual m o) -> TH.mkName (Module.moduleNameString m <> "." <> occNameString o)
  (Exact name) -> TH.mkName ((occNameString . rdrNameOcc . getRdrName) name) --error "exact"
  (Orig _ _) -> error "orig"

toFieldExp :: a
toFieldExp = undefined

toPat :: Pat.Pat GhcPs -> TH.Pat
toPat (Pat.VarPat _ (unLoc -> name)) = TH.VarP (toName name)
toPat (TuplePat _ p _) = TH.TupP (map (toPat . unLoc) p)
#if __GLASGOW_HASKELL__ >= 910
toPat (ParPat xP lP)  = (toPat . unLoc) lP
#else
toPat (ParPat xP _ lP _) = (toPat . unLoc) lP
#endif
toPat (ConPat pat_con_ext ((unLoc -> name)) pat_args) = TH.ConP (toName name) (map toType []) (map (toPat . unLoc) (Pat.hsConPatArgs pat_args))
toPat (ViewPat pat_con pat_args pat_con_ext) = error "TH.ViewPattern not implemented"
toPat (SumPat _ _ _ _) = error "TH.SumPat not implemented"
toPat (WildPat _ ) = error "TH.WildPat not implemented"
toPat (NPat _ _ _ _ ) = error "TH.NPat not implemented"
toPat p = todo "toPat" p

toExp :: Expr.HsExpr GhcPs -> TH.Exp
toExp (Expr.HsVar _ n) =
  let n' = unLoc n
   in if isRdrDataCon n'
        then TH.ConE (toName n')
        else TH.VarE (toName n')

#if __GLASGOW_HASKELL__ >= 906
toExp (Expr.HsUnboundVar _ n)              = TH.UnboundVarE (TH.mkName . occNameString $ occName n)
#else
toExp (Expr.HsUnboundVar _ n)              = TH.UnboundVarE (TH.mkName . occNameString $ n)
#endif

toExp Expr.HsIPVar {}
  = noTH "toExp" "HsIPVar"

toExp (Expr.HsLit _ l)
  = TH.LitE (toLit l)

toExp (Expr.HsOverLit _ OverLit {ol_val})
  = TH.LitE (toLit' ol_val)

toExp (Expr.HsApp _ e1 e2)
  = TH.AppE (toExp . unLoc $ e1) (toExp . unLoc $ e2)

#if __GLASGOW_HASKELL__ >= 910
toExp (Expr.HsAppType _ e HsWC {hswc_body}) = TH.AppTypeE (toExp . unLoc $ e) (toType . unLoc $ hswc_body)
#elif __GLASGOW_HASKELL__ >= 906
toExp (Expr.HsAppType _ e _ HsWC {hswc_body}) = TH.AppTypeE (toExp . unLoc $ e) (toType . unLoc $ hswc_body)
#else
toExp (Expr.HsAppType _ e HsWC {hswc_body}) = TH.AppTypeE (toExp . unLoc $ e) (toType . unLoc $ hswc_body)
#endif
toExp (Expr.ExprWithTySig _ e HsWC{hswc_body=unLoc -> HsSig{sig_body}}) = TH.SigE (toExp . unLoc $ e) (toType . unLoc $ sig_body)

toExp (Expr.OpApp _ e1 o e2)
  = TH.UInfixE (toExp . unLoc $ e1) (toExp . unLoc $ o) (toExp . unLoc $ e2)

toExp (Expr.NegApp _ e _)
  = TH.AppE (TH.VarE 'negate) (toExp . unLoc $ e)

-- NOTE: for lambda, there is only one match
#if __GLASGOW_HASKELL__ >= 912
toExp (Expr.HsLam _ LamSingle (Expr.MG _ (unLoc -> (map unLoc -> [Expr.Match _ _ (map unLoc . unLoc -> ps) (Expr.GRHSs _ [unLoc -> Expr.GRHS _ _ (unLoc -> e)] _)]))))
#elif __GLASGOW_HASKELL__ >= 910
toExp (Expr.HsLam _ LamSingle (Expr.MG _ (unLoc -> (map unLoc -> [Expr.Match _ _ (map unLoc -> ps) (Expr.GRHSs _ [unLoc -> Expr.GRHS _ _ (unLoc -> e)] _)]))))
#elif __GLASGOW_HASKELL__ >= 906
toExp (Expr.HsLam _ (Expr.MG _ (unLoc -> (map unLoc -> [Expr.Match _ _ (map unLoc -> ps) (Expr.GRHSs _ [unLoc -> Expr.GRHS _ _ (unLoc -> e)] _)]))))
#else
toExp (Expr.HsLam _ (Expr.MG _ (unLoc -> (map unLoc -> [Expr.Match _ _ (map unLoc -> ps) (Expr.GRHSs _ [unLoc -> Expr.GRHS _ _ (unLoc -> e)] _)])) _))
#endif
  = TH.LamE (fmap toPat ps) (toExp e)

-- toExp (Expr.Let _ bs e)                       = TH.LetE (toDecs bs) (toExp e)
--
toExp (Expr.HsIf _ a b c)                   = TH.CondE (toExp (unLoc a)) (toExp (unLoc b)) (toExp (unLoc c))

-- toExp (Expr.MultiIf _ ifs)                    = TH.MultiIfE (map toGuard ifs)
-- toExp (Expr.Case _ e alts)                    = TH.CaseE (toExp e) (map toMatch alts)
-- toExp (Expr.Do _ ss)                          = TH.DoE (map toStmt ss)
-- toExp e@Expr.MDo{}                            = noTH "toExp" e
--
toExp (Expr.ExplicitTuple _ args boxity) = ctor tupArgs
  where
    toTupArg (Expr.Present _ e) = Just $ unLoc e
    toTupArg (Expr.Missing _) = Nothing
    toTupArg _ = error "impossible case"

    ctor = case boxity of
      Boxed -> TH.TupE
      Unboxed -> TH.UnboxedTupE

    tupArgs = fmap ((fmap toExp) . toTupArg) args

-- toExp (Expr.List _ xs)                        = TH.ListE (fmap toExp xs)
#if __GLASGOW_HASKELL__ >= 910
toExp (Expr.HsPar _ e) =
#else
toExp (Expr.HsPar _ _ e _) =
#endif
  TH.ParensE (toExp . unLoc $ e)

toExp (Expr.SectionL _ (unLoc -> a) (unLoc -> b))
  = TH.InfixE (Just . toExp $ a) (toExp b) Nothing

toExp (Expr.SectionR _ (unLoc -> a) (unLoc -> b))
  = TH.InfixE Nothing (toExp a) (Just . toExp $ b)

toExp (Expr.RecordCon _ name HsRecFields {rec_flds})
  = TH.RecConE (toName . unLoc $ name) (fmap toFieldExp rec_flds)

toExp (Expr.RecordUpd _ (unLoc -> e) xs)                 = TH.RecUpdE (toExp e) $ case xs of
#if __GLASGOW_HASKELL__ >= 908
    RegularRecUpdFields { recUpdFields = fields } ->
#else
    Left fields ->
#endif
        let
            f (unLoc -> x) = (name, value)
                where
                    value = toExp $ unLoc $ hfbRHS x
                    name =
                        case unLoc (hfbLHS x) of
#if __GLASGOW_HASKELL__ >= 912
                            FieldOcc _ (unLoc -> name) -> toName name
                            XFieldOcc _ -> error "todo"
#else
                            Unambiguous _ (unLoc -> name) -> toName name
                            Ambiguous _ (unLoc -> name) -> toName name
#endif
        in
            map f fields
    otherwise -> error "todo"
-- toExp (Expr.ListComp _ e ss)                  = TH.CompE $ map convert ss ++ [TH.NoBindS (toExp e)]
--  where
--   convert (Expr.QualStmt _ st)                = toStmt st
--   convert s                                   = noTH "toExp ListComp" s
-- toExp (Expr.ExpTypeSig _ e t)                 = TH.SigE (toExp e) (toType t)
--
toExp (Expr.ExplicitList _ (map unLoc -> args)) = TH.ListE (map toExp args)

toExp (Expr.ArithSeq _ _ e)
  = TH.ArithSeqE $ case e of
    (From a) -> TH.FromR (toExp $ unLoc a)
    (FromThen a b) -> TH.FromThenR (toExp $ unLoc a) (toExp $ unLoc b)
    (FromTo a b) -> TH.FromToR (toExp $ unLoc a) (toExp $ unLoc b)
    (FromThenTo a b c) -> TH.FromThenToR (toExp $ unLoc a) (toExp $ unLoc b) (toExp $ unLoc c)


toExp (Expr.HsProjection _ locatedFields) =
  let
    extractFieldLabel (DotFieldOcc _ locatedStr) = locatedStr
    extractFieldLabel _ = error "Don't know how to handle XDotFieldOcc constructor..."
  in
#if __GLASGOW_HASKELL__ >= 912
    TH.ProjectionE (NonEmpty.map (unpackFS . (.field_label) . unLoc . extractFieldLabel) locatedFields)
#elif __GLASGOW_HASKELL__ >= 906
    TH.ProjectionE (NonEmpty.map (unpackFS . (.field_label) . unLoc . extractFieldLabel . unLoc) locatedFields)
#else
    TH.ProjectionE (NonEmpty.map (unpackFS . unLoc . extractFieldLabel . unLoc) locatedFields)
#endif

toExp (Expr.HsGetField _ expr locatedField) =
  let
    extractFieldLabel (DotFieldOcc _ locatedStr) = locatedStr
    extractFieldLabel _ = error "Don't know how to handle XDotFieldOcc constructor..."
  in
#if __GLASGOW_HASKELL__ >= 906
    TH.GetFieldE (toExp (unLoc expr)) (unpackFS . (.field_label) . unLoc . extractFieldLabel . unLoc $ locatedField)
#else
    TH.GetFieldE (toExp (unLoc expr)) (unpackFS . unLoc . extractFieldLabel . unLoc $ locatedField)
#endif

#if __GLASGOW_HASKELL__ >= 912
toExp (Expr.HsOverLabel _ fastString) = TH.LabelE (unpackFS fastString)
#elif __GLASGOW_HASKELL__ >= 906
toExp (Expr.HsOverLabel _ _ fastString) = TH.LabelE (unpackFS fastString)
#else
toExp (Expr.HsOverLabel _ fastString) = TH.LabelE (unpackFS fastString)
#endif

toExp e = todo "toExp" e


todo :: Outputable e => String -> e -> a
todo fun thing = error . concat $ [moduleName, ".", fun, ": not implemented: ", (showSDocUnsafe $ ppr thing)]

noTH :: (HasCallStack, Show e) => String -> e -> a
noTH fun thing = error . concat $ [moduleName, ".", fun, ": no TemplateHaskell for: ", show thing]

moduleName :: String
moduleName = "IHP.HSX.HsExpToTH"
