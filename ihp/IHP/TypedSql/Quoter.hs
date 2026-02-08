{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module IHP.TypedSql.Quoter
    ( typedSql
    ) where

import           Control.Monad                  (when)
import           Data.Coerce                    (coerce)
import qualified Data.Char                      as Char
import qualified Data.Map.Strict                as Map
import qualified Data.String.Conversions        as CS
import qualified Hasql.DynamicStatements.Snippet as Snippet
import qualified Language.Haskell.TH            as TH
import qualified Language.Haskell.TH.Quote      as TH
import           System.Environment              (lookupEnv)
import           IHP.Prelude
import           IHP.Hasql.Encoders              ()

import           IHP.TypedSql.Bootstrap         (describeUsingBootstrap)
import           IHP.TypedSql.Decoders          (resultDecoderForColumns)
import           IHP.TypedSql.Metadata          (DescribeColumn (..), DescribeResult (..), PgTypeInfo (..),
                                                 describeStatement)
import           IHP.TypedSql.ParamHints        (buildAliasMap, collectParamHints,
                                                 resolveParamHintTypes, tokenizeSql)
import           IHP.TypedSql.Placeholders      (PlaceholderPlan (..), parseExpr,
                                                 planPlaceholders)
import           IHP.TypedSql.TypeMapping       (hsTypeForColumns, hsTypeForParam)
import           IHP.TypedSql.Types             (TypedQuery (..))

-- | QuasiQuoter entry point for typed SQL.
-- High-level: produces a TH expression that builds a TypedQuery at compile time.
typedSql :: TH.QuasiQuoter
typedSql =
    TH.QuasiQuoter
        { TH.quoteExp = typedSqlExp
        , TH.quotePat = \_ -> fail "typedSql: not supported in patterns"
        , TH.quoteType = \_ -> fail "typedSql: not supported in types"
        , TH.quoteDec = \_ -> fail "typedSql: not supported at top-level"
        }

-- | Build the TH expression for a typed SQL quasiquote.
-- This is the heart of typedSql: parse placeholders, describe SQL, and assemble a TypedQuery.
typedSqlExp :: String -> TH.ExpQ
typedSqlExp rawSql = do
    let PlaceholderPlan { ppDescribeSql, ppRuntimeSql, ppExprs } = planPlaceholders rawSql
    parsedExprs <- mapM parseExpr ppExprs

    bootstrapEnv <- TH.runIO (lookupEnv "IHP_TYPED_SQL_BOOTSTRAP")
    loc <- TH.location
    let useBootstrap = isBootstrapEnabled bootstrapEnv
    describeResult <- TH.runIO $
        if useBootstrap
            then describeUsingBootstrap (TH.loc_filename loc) ppDescribeSql
            else describeStatement (CS.cs ppDescribeSql)

    let DescribeResult { drParams, drColumns, drTables, drTypes } = describeResult
    when (length drParams /= length parsedExprs) $
        fail (CS.cs ("typedSql: placeholder count mismatch. SQL expects " <> show (length drParams) <> " parameters but found " <> show (length parsedExprs) <> " ${..} expressions."))

    paramTypes <- mapM (hsTypeForParam drTypes) drParams

    let sqlTokens = tokenizeSql ppDescribeSql
    let aliasMap = buildAliasMap sqlTokens
    let paramHints = collectParamHints sqlTokens aliasMap
    paramHintTypes <- resolveParamHintTypes drTables drTypes paramHints

    let annotatedParams =
            zipWith3
                (\index expr paramTy ->
                    let expectedType = fromMaybe paramTy (Map.lookup index paramHintTypes)
                    in TH.SigE (TH.AppE (TH.VarE 'coerce) expr) expectedType
                )
                [1..]
                parsedExprs
                paramTypes

    resultType <- hsTypeForColumns drTypes drTables drColumns

    let isCompositeColumn =
            case drColumns of
                [DescribeColumn { dcType }] ->
                    case Map.lookup dcType drTypes of
                        Just PgTypeInfo { ptiType = Just 'c' } -> True
                        _ -> False
                _ -> False
    when (length drColumns == 1 && isCompositeColumn) $
        fail
            ("typedSql: composite columns must be expanded (use SELECT table.* "
                <> "or list columns explicitly)")
    resultDecoder <- resultDecoderForColumns drTypes drTables drColumns
    snippetExpr <- buildSnippetExpression ppRuntimeSql annotatedParams
    let typedQueryExpr =
            TH.AppE
                (TH.AppE
                    (TH.ConE 'TypedQuery)
                    snippetExpr
                )
                resultDecoder

    pure (TH.SigE typedQueryExpr (TH.AppT (TH.ConT ''TypedQuery) resultType))

buildSnippetExpression :: String -> [TH.Exp] -> TH.ExpQ
buildSnippetExpression sql params = do
    let chunks = splitOnQuestion sql
    when (length chunks /= length params + 1) do
        fail "typedSql: internal error while building hasql snippet"
    let sqlSnippets = map (TH.AppE (TH.VarE 'Snippet.sql) . TH.LitE . TH.StringL) chunks
    let paramSnippets = map (TH.AppE (TH.VarE 'Snippet.param)) params
    let pieces = interleave sqlSnippets paramSnippets
    case pieces of
        [] -> pure (TH.AppE (TH.VarE 'Snippet.sql) (TH.LitE (TH.StringL "")))
        firstPiece:restPieces ->
            pure (foldl (\acc piece -> TH.InfixE (Just acc) (TH.VarE '(<>) ) (Just piece)) firstPiece restPieces)

splitOnQuestion :: String -> [String]
splitOnQuestion input = go "" [] input
  where
    go current acc [] = reverse (reverse current : acc)
    go current acc ('?':rest) = go "" (reverse current : acc) rest
    go current acc (char:rest) = go (char:current) acc rest

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

-- | Interpret IHP_TYPED_SQL_BOOTSTRAP to decide between live DB and bootstrap mode.
isBootstrapEnabled :: Maybe String -> Bool
isBootstrapEnabled = \case
    Nothing -> False
    Just raw ->
        let value = map Char.toLower raw
        in not (value `elem` ["", "0", "false", "no", "off"])
