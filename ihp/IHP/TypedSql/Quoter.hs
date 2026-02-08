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
import qualified Database.PostgreSQL.Simple.FromRow as PGFR
import qualified Database.PostgreSQL.Simple.ToField as PGTF
import qualified Database.PostgreSQL.Simple.Types   as PG
import qualified Language.Haskell.TH            as TH
import qualified Language.Haskell.TH.Quote      as TH
import           System.Environment              (lookupEnv)
import           IHP.Prelude

import           IHP.TypedSql.Bootstrap         (describeUsingBootstrap)
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

-- Build the TH expression for a typed SQL quasiquote.
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

    let sqlLiteral = TH.SigE (TH.LitE (TH.StringL ppRuntimeSql)) (TH.ConT ''String)
        queryExpr = TH.AppE (TH.ConE 'PG.Query) (TH.AppE (TH.VarE 'CS.cs) sqlLiteral)
        isCompositeColumn =
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
    let rowParserExpr =
            case length drColumns of
                0 -> TH.AppE (TH.VarE 'pure) (TH.ConE '())
                1 -> TH.VarE 'PGFR.field
                _ -> TH.VarE 'PGFR.fromRow
        typedQueryExpr =
            TH.AppE
                (TH.AppE
                    (TH.AppE
                        (TH.ConE 'TypedQuery)
                        queryExpr
                    )
                    (TH.ListE (map (TH.AppE (TH.VarE 'PGTF.toField)) annotatedParams))
                )
                rowParserExpr

    pure (TH.SigE typedQueryExpr (TH.AppT (TH.ConT ''TypedQuery) resultType))

isBootstrapEnabled :: Maybe String -> Bool
isBootstrapEnabled = \case
    Nothing -> False
    Just raw ->
        let value = map Char.toLower raw
        in not (value `elem` ["", "0", "false", "no", "off"])
