{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module IHP.TypedSql.Quoter
    ( typedSql
    , typedSqlStar
    ) where

import           Data.Coerce                    (coerce)
import qualified Data.List                      as List
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set
import qualified Data.String.Conversions        as CS
import qualified Hasql.DynamicStatements.Snippet as Snippet
import qualified Language.Haskell.TH            as TH
import qualified Language.Haskell.TH.Quote      as TH
import           IHP.Prelude
import           IHP.Hasql.Encoders              ()

import           IHP.TypedSql.Decoders          (resultDecoderForColumns)
import           IHP.TypedSql.Metadata          (DescribeColumn (..), DescribeResult (..), PgTypeInfo (..), TableMeta (..),
                                                 describeStatement)
import           IHP.TypedSql.ParamHints        (extractParamHintsFromAst, extractJoinNullableTablesFromAst,
                                                 extractNonNullableComputedColumnsFromAst,
                                                 parseSql, resolveParamHintTypes, detectStarSelects)
import           IHP.TypedSql.Placeholders      (PlaceholderPlan (..), parseExpr,
                                                 planPlaceholders)
import           IHP.TypedSql.RowType           (SqlRow (..), sanitizeColumnName, deduplicateNames, sqlRowType)
import           IHP.TypedSql.TypeMapping       (hsTypeForColumns, hsTypesForColumns, hsTypeForParam, detectFullTable)
import           IHP.TypedSql.Types             (TypedQuery (..))

-- | QuasiQuoter entry point for typed SQL.
-- Disallows SELECT * and SELECT table.* by default to prevent production errors
-- when the schema changes. Use 'typedSqlStar' to opt in to star selects.
typedSql :: TH.QuasiQuoter
typedSql =
    TH.QuasiQuoter
        { TH.quoteExp = typedSqlExp False
        , TH.quotePat = \_ -> fail "typedSql: not supported in patterns"
        , TH.quoteType = \_ -> fail "typedSql: not supported in types"
        , TH.quoteDec = \_ -> fail "typedSql: not supported at top-level"
        }

-- | Like 'typedSql' but allows SELECT * and SELECT table.* patterns.
-- Use this when you understand that star selects can break at runtime if the
-- schema changes between compilation and deployment.
typedSqlStar :: TH.QuasiQuoter
typedSqlStar =
    TH.QuasiQuoter
        { TH.quoteExp = typedSqlExp True
        , TH.quotePat = \_ -> fail "typedSqlStar: not supported in patterns"
        , TH.quoteType = \_ -> fail "typedSqlStar: not supported in types"
        , TH.quoteDec = \_ -> fail "typedSqlStar: not supported at top-level"
        }

-- | Build the TH expression for a typed SQL quasiquote.
-- This is the heart of typedSql: parse placeholders, describe SQL, and assemble a TypedQuery.
typedSqlExp :: Bool -> String -> TH.ExpQ
typedSqlExp allowStar rawSql = do
    let PlaceholderPlan { ppDescribeSql, ppRuntimeSql, ppExprs } = planPlaceholders rawSql
    parsedExprs <- mapM parseExpr ppExprs

    describeResult <- TH.runIO $ describeStatement (CS.cs ppDescribeSql)

    let DescribeResult { drParams, drColumns, drTables, drTypes } = describeResult
    when (length drParams /= length parsedExprs) $
        fail (CS.cs ("typedSql: placeholder count mismatch. SQL expects " <> show (length drParams) <> " parameters but found " <> show (length parsedExprs) <> " ${..} expressions."))

    paramTypes <- mapM (hsTypeForParam drTypes) drParams

    let parsedAst = parseSql ppDescribeSql
    when (isNothing parsedAst) do
        TH.reportWarning "typedSql: could not parse SQL for type refinement; parameter hints and LEFT/RIGHT JOIN nullability detection are disabled for this query."

    unless allowStar do
        case parsedAst of
            Just ast -> do
                let stars = detectStarSelects ast
                unless (null stars) do
                    let columnNames = map (CS.cs . dcName) drColumns
                    let suggestion = List.intercalate ", " columnNames
                    fail ("typedSql: SELECT " <> List.intercalate ", " stars
                        <> " is not allowed because it can break at runtime when the schema changes. "
                        <> "List columns explicitly:\n  SELECT " <> suggestion <> " FROM ...\n"
                        <> "Or use [typedSqlStar| ... |] if you understand the risk.")
            Nothing -> pure ()

    let paramHints = maybe Map.empty extractParamHintsFromAst parsedAst
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

    let nullableTableNames = maybe Set.empty extractJoinNullableTablesFromAst parsedAst
    let joinNullableOids = drTables
            |> Map.toList
            |> filter (\(_, TableMeta { tmName }) -> tmName `Set.member` nullableTableNames)
            |> map fst
            |> Set.fromList

    let nonNullableColumns = maybe Set.empty extractNonNullableComputedColumnsFromAst parsedAst

    let isCompositeColumn =
            case drColumns of
                [DescribeColumn { dcType }] ->
                    case Map.lookup dcType drTypes of
                        Just PgTypeInfo { ptiType = 'c' } -> True
                        _ -> False
                _ -> False
    when (length drColumns == 1 && isCompositeColumn) $
        fail
            ("typedSql: composite columns must be expanded (use SELECT table.* "
                <> "or list columns explicitly)")

    let isFullTable = isJust (detectFullTable drTables drColumns)
    let isMultiColumnAdhoc = not isFullTable && length drColumns > 1

    (resultType, resultDecoder) <-
        if isMultiColumnAdhoc
            then do
                -- Wrap the tuple in SqlRow for labeled field access
                columnTypes <- hsTypesForColumns drTypes drTables joinNullableOids nonNullableColumns drColumns
                let colNames = deduplicateNames (map (sanitizeColumnName . dcName) drColumns)
                let fields = zip colNames columnTypes
                let rowType = sqlRowType fields
                tupleDecoder <- resultDecoderForColumns drTypes drTables joinNullableOids nonNullableColumns drColumns
                -- fmap SqlRow tupleDecoder
                let wrappedDecoder = TH.AppE (TH.AppE (TH.VarE 'fmap) (TH.ConE 'SqlRow)) tupleDecoder
                pure (rowType, wrappedDecoder)
            else do
                rt <- hsTypeForColumns drTypes drTables joinNullableOids nonNullableColumns drColumns
                decoder <- resultDecoderForColumns drTypes drTables joinNullableOids nonNullableColumns drColumns
                pure (rt, decoder)

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
    let chunks = splitOnSentinel sql
    when (length chunks /= length params + 1) do
        fail "typedSql: internal error while building hasql snippet"
    let sqlSnippets = map (TH.AppE (TH.VarE 'Snippet.sql) . TH.LitE . TH.StringL) chunks
    let paramSnippets = map (TH.AppE (TH.VarE 'Snippet.param)) params
    let pieces = interleave sqlSnippets paramSnippets
    case pieces of
        [] -> pure (TH.AppE (TH.VarE 'Snippet.sql) (TH.LitE (TH.StringL "")))
        firstPiece:restPieces ->
            pure (foldl (\acc piece -> TH.InfixE (Just acc) (TH.VarE '(<>) ) (Just piece)) firstPiece restPieces)

splitOnSentinel :: String -> [String]
splitOnSentinel input = go "" [] input
  where
    go current acc [] = reverse (reverse current : acc)
    go current acc ('\0':rest) = go "" (reverse current : acc) rest
    go current acc (char:rest) = go (char:current) acc rest

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys
