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
) where

import IHP.Prelude
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import IHP.QueryBuilder.Types
import IHP.QueryBuilder.Compiler (buildQuery)
import qualified Data.Text as Text
import qualified Data.List as List
import Data.Int (Int32)

-- | Compile a QueryBuilder to a Hasql Snippet
toSnippet :: forall table queryBuilderProvider joinRegister. (KnownSymbol table, HasQueryBuilder queryBuilderProvider joinRegister) => queryBuilderProvider table -> Snippet
toSnippet queryBuilderProvider = buildSnippet (buildQuery queryBuilderProvider)
{-# INLINE toSnippet #-}

-- | Build a Snippet from a compiled SQLQuery
buildSnippet :: SQLQuery -> Snippet
buildSnippet sqlQuery@SQLQuery { queryIndex, selectFrom, distinctClause, distinctOnClause, orderByClause, limitClause, offsetClause, columns } =
    Snippet.sql "SELECT"
    <> distinctSnippet distinctClause
    <> distinctOnSnippet distinctOnClause
    <> Snippet.sql " " <> selectorsSnippet
    <> Snippet.sql " FROM"
    <> Snippet.sql " " <> Snippet.sql selectFrom
    <> joinSnippet (reverse (joins sqlQuery))
    <> whereSnippet (whereCondition sqlQuery)
    <> orderBySnippet orderByClause
    <> limitSnippet limitClause
    <> offsetSnippet offsetClause
    where
        distinctSnippet :: Bool -> Snippet
        distinctSnippet False = mempty
        distinctSnippet True = Snippet.sql " DISTINCT"
        {-# INLINE distinctSnippet #-}

        distinctOnSnippet :: Maybe Text -> Snippet
        distinctOnSnippet Nothing = mempty
        distinctOnSnippet (Just col) = Snippet.sql " DISTINCT ON (" <> Snippet.sql col <> Snippet.sql ")"
        {-# INLINE distinctOnSnippet #-}

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
-- buildSnippet takes monomorphic SQLQuery — no specialization benefit from INLINE.
-- Removing INLINE prevents duplicating the snippet compilation logic at every call site.

-- | Convert a WHERE condition to a Snippet
whereSnippet :: Maybe Condition -> Snippet
whereSnippet Nothing = mempty
whereSnippet (Just condition) = Snippet.sql " WHERE " <> conditionToSnippet condition
{-# INLINE whereSnippet #-}

-- | Convert a Condition to a Snippet
--
-- Uses the hasql-specific template (with = ANY/<> ALL for IN/NOT IN).
conditionToSnippet :: Condition -> Snippet
conditionToSnippet (VarCondition template snippet) =
    -- VarCondition stores template (e.g., "id = ANY(?)") and a snippet parameter
    -- We substitute the ? with the actual parameter
    substituteSnippet template snippet
conditionToSnippet (OrCondition a b) =
    Snippet.sql "(" <> conditionToSnippet a <> Snippet.sql ") OR (" <> conditionToSnippet b <> Snippet.sql ")"
conditionToSnippet (AndCondition a b) =
    Snippet.sql "(" <> conditionToSnippet a <> Snippet.sql ") AND (" <> conditionToSnippet b <> Snippet.sql ")"
{-# INLINE conditionToSnippet #-}

-- | Substitute a ? placeholder in a template with the snippet parameter
substituteSnippet :: Text -> Snippet -> Snippet
substituteSnippet template snippet =
    let (before, after) = Text.break (== '?') template
    in if Text.null after
        then Snippet.sql template  -- No ? found, just use the template (e.g., for raw SQL conditions)
        else Snippet.sql before <> snippet <> Snippet.sql (Text.drop 1 after)
{-# INLINE substituteSnippet #-}

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
