{-# LANGUAGE BangPatterns, TypeFamilies, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, InstanceSigs, AllowAmbiguousTypes, DeriveAnyClass #-}

{-|
Module: IHP.QueryBuilder.HasqlCompiler
Description: Compile QueryBuilder to Hasql Snippet
Copyright: (c) digitally induced GmbH, 2025

This module compiles QueryBuilder queries to Hasql's Snippet type for execution
with prepared statements.
-}
module IHP.QueryBuilder.HasqlCompiler
( toSnippet
, buildSnippet
, snippetToSQL
, compileOperator
) where

import IHP.Prelude
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import IHP.QueryBuilder.Types
import IHP.QueryBuilder.Compiler (buildQuery)
import qualified Data.List as List

-- | Compile a QueryBuilder to a Hasql Snippet
toSnippet :: forall table queryBuilderProvider joinRegister. (KnownSymbol table, HasQueryBuilder queryBuilderProvider joinRegister) => queryBuilderProvider table -> Snippet
toSnippet queryBuilderProvider = buildSnippet (buildQuery queryBuilderProvider)
{-# INLINE toSnippet #-}

-- | Build a Snippet from a compiled SQLQuery
buildSnippet :: SQLQuery -> Snippet
buildSnippet sqlQuery@SQLQuery { queryIndex, selectFrom, distinctClause, distinctOnClause, orderByClause, limitClause, offsetClause, columns } =
    selectSnippet
    <> Snippet.sql " " <> selectorsSnippet
    <> Snippet.sql " FROM " <> Snippet.sql selectFrom
    <> joinSnippet (reverse (joins sqlQuery))
    <> whereSnippet (whereCondition sqlQuery)
    <> orderBySnippet orderByClause
    <> limitSnippet limitClause
    <> offsetSnippet offsetClause
    where
        -- Fold DISTINCT/DISTINCT ON into the SELECT keyword to avoid mempty <> appends
        selectSnippet = case (distinctClause, distinctOnClause) of
            (False, Nothing) -> Snippet.sql "SELECT"
            (True, Nothing)  -> Snippet.sql "SELECT DISTINCT"
            (_, Just col)    -> Snippet.sql "SELECT DISTINCT ON (" <> Snippet.sql col <> Snippet.sql ")"

        limitSnippet :: Maybe Int -> Snippet
        limitSnippet Nothing = mempty
        limitSnippet (Just n) = Snippet.sql " LIMIT " <> Snippet.param (fromIntegral n :: Int32)
        {-# INLINE limitSnippet #-}

        offsetSnippet :: Maybe Int -> Snippet
        offsetSnippet Nothing = mempty
        offsetSnippet (Just n) = Snippet.sql " OFFSET " <> Snippet.param (fromIntegral n :: Int32)
        {-# INLINE offsetSnippet #-}

        selectorsSnippet :: Snippet
        selectorsSnippet =
            let indexParts = case queryIndex of
                    Just idx -> [Snippet.sql idx]
                    Nothing -> []
                columnParts = map (\column -> Snippet.sql selectFrom <> Snippet.sql "." <> Snippet.sql column) columns
            in mconcat $ List.intersperse (Snippet.sql ", ") (indexParts <> columnParts)

        joinSnippet :: [Join] -> Snippet
        joinSnippet [] = mempty
        joinSnippet (j:js) = Snippet.sql " INNER JOIN " <> Snippet.sql (table j) <> Snippet.sql " ON " <> Snippet.sql (tableJoinColumn j) <> Snippet.sql " = " <> Snippet.sql (table j) <> Snippet.sql "." <> Snippet.sql (otherJoinColumn j) <> joinSnippet js
{-# INLINE buildSnippet #-}

-- | Convert a WHERE condition to a Snippet
whereSnippet :: Maybe Condition -> Snippet
whereSnippet Nothing = mempty
whereSnippet (Just condition) = Snippet.sql " WHERE " <> conditionToSnippet condition
{-# INLINE whereSnippet #-}

-- | Convert a Condition to a Snippet
--
-- Non-recursive wrapper that handles the common single-column case inline.
-- GHC refuses to inline recursive functions, so splitting lets the common path
-- (ColumnCondition) be fully inlined and specialized at call sites.
conditionToSnippet :: Condition -> Snippet
conditionToSnippet (ColumnCondition col op val al ar) = columnConditionSnippet col op val al ar
conditionToSnippet cond = conditionToSnippetWorker cond
{-# INLINE conditionToSnippet #-}

-- | Compile a single column condition to a Snippet
columnConditionSnippet :: Text -> FilterOperator -> Snippet -> Maybe Text -> Maybe Text -> Snippet
columnConditionSnippet column operator value applyLeft applyRight =
    let applyFn fn snippet = case fn of
            Just f  -> Snippet.sql f <> Snippet.sql "(" <> snippet <> Snippet.sql ")"
            Nothing -> snippet
        colSnippet = applyFn applyLeft (Snippet.sql column)
        valSnippet = case operator of
            InOp    -> Snippet.sql "(" <> value <> Snippet.sql ")"
            NotInOp -> Snippet.sql "(" <> value <> Snippet.sql ")"
            SqlOp   -> value
            _       -> applyFn applyRight value
    in case operator of
        SqlOp -> colSnippet <> Snippet.sql " " <> valSnippet
        _     -> colSnippet <> Snippet.sql (compileOperatorPadded operator) <> valSnippet
{-# INLINE columnConditionSnippet #-}

-- | Recursive worker for compound conditions (OR/AND).
-- Not marked INLINE — compound conditions are rare and recursion prevents inlining anyway.
conditionToSnippetWorker :: Condition -> Snippet
conditionToSnippetWorker (ColumnCondition col op val al ar) = columnConditionSnippet col op val al ar
conditionToSnippetWorker (OrCondition a b) =
    Snippet.sql "(" <> conditionToSnippetWorker a <> Snippet.sql ") OR (" <> conditionToSnippetWorker b <> Snippet.sql ")"
conditionToSnippetWorker (AndCondition a b) =
    Snippet.sql "(" <> conditionToSnippetWorker a <> Snippet.sql ") AND (" <> conditionToSnippetWorker b <> Snippet.sql ")"

-- | Compiles a 'FilterOperator' to its SQL representation
compileOperator :: FilterOperator -> Text
compileOperator EqOp = "="
compileOperator NotEqOp = "!="
compileOperator InOp = "= ANY"
compileOperator NotInOp = "<> ALL"
compileOperator IsOp = "IS"
compileOperator IsNotOp = "IS NOT"
compileOperator (LikeOp CaseSensitive) = "LIKE"
compileOperator (LikeOp CaseInsensitive) = "ILIKE"
compileOperator (NotLikeOp CaseSensitive) = "NOT LIKE"
compileOperator (NotLikeOp CaseInsensitive) = "NOT ILIKE"
compileOperator (MatchesOp CaseSensitive) = "~"
compileOperator (MatchesOp CaseInsensitive) = "~*"
compileOperator GreaterThanOp = ">"
compileOperator GreaterThanOrEqualToOp = ">="
compileOperator LessThanOp = "<"
compileOperator LessThanOrEqualToOp = "<="
compileOperator SqlOp = ""
{-# INLINE compileOperator #-}

-- | Like 'compileOperator' but with spaces pre-padded on both sides.
-- Saves 2 @Snippet.sql@ allocations + 2 @<>@ calls per condition vs padding at the call site.
compileOperatorPadded :: FilterOperator -> Text
compileOperatorPadded EqOp = " = "
compileOperatorPadded NotEqOp = " != "
compileOperatorPadded InOp = " = ANY "
compileOperatorPadded NotInOp = " <> ALL "
compileOperatorPadded IsOp = " IS "
compileOperatorPadded IsNotOp = " IS NOT "
compileOperatorPadded (LikeOp CaseSensitive) = " LIKE "
compileOperatorPadded (LikeOp CaseInsensitive) = " ILIKE "
compileOperatorPadded (NotLikeOp CaseSensitive) = " NOT LIKE "
compileOperatorPadded (NotLikeOp CaseInsensitive) = " NOT ILIKE "
compileOperatorPadded (MatchesOp CaseSensitive) = " ~ "
compileOperatorPadded (MatchesOp CaseInsensitive) = " ~* "
compileOperatorPadded GreaterThanOp = " > "
compileOperatorPadded GreaterThanOrEqualToOp = " >= "
compileOperatorPadded LessThanOp = " < "
compileOperatorPadded LessThanOrEqualToOp = " <= "
compileOperatorPadded SqlOp = " "
{-# INLINE compileOperatorPadded #-}

-- | Convert ORDER BY clause to Snippet
orderBySnippet :: [OrderByClause] -> Snippet
orderBySnippet [] = mempty
orderBySnippet clauses = Snippet.sql " ORDER BY " <> mconcat (List.intersperse (Snippet.sql ",") (map orderByClauseToSnippet clauses))
    where
        orderByClauseToSnippet OrderByClause { orderByColumn, orderByDirection } =
            Snippet.sql orderByColumn <> (if orderByDirection == Desc then Snippet.sql " DESC" else mempty)
{-# INLINE orderBySnippet #-}

-- | Extract the SQL ByteString from a Snippet (for testing purposes)
--
-- This converts a Snippet to a Statement and extracts the SQL text.
-- Useful for verifying the hasql compilation path in tests.
snippetToSQL :: Snippet -> Text
snippetToSQL snippet = Snippet.toSql snippet
