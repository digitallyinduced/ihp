{-# LANGUAGE BangPatterns, TypeFamilies, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, InstanceSigs, AllowAmbiguousTypes, DeriveAnyClass #-}

{-|
Module: IHP.QueryBuilder.HasqlCompiler
Description: Compile QueryBuilder to Hasql Snippet
Copyright: (c) digitally induced GmbH, 2025

This module compiles QueryBuilder queries to Hasql's Snippet type for execution
with prepared statements. This provides better performance than the text-based
postgresql-simple approach.

The compilation is parallel to 'IHP.QueryBuilder.Compiler.toSQL' but produces
Snippet values instead of (ByteString, [Action]) tuples.
-}
module IHP.QueryBuilder.HasqlCompiler
( toSnippet
, toSnippetWithRLS
, buildSnippet
) where

import IHP.Prelude
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import IHP.QueryBuilder.Types
import IHP.ModelSupport.Types (RowLevelSecurityContext(..))
import IHP.QueryBuilder.Compiler (buildQuery)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.List as List
import qualified Database.PostgreSQL.Simple.ToField as PG

-- | Compile a QueryBuilder to a Hasql Snippet
--
-- This is the hasql equivalent of 'toSQL' from IHP.QueryBuilder.Compiler.
toSnippet :: forall table queryBuilderProvider joinRegister. (KnownSymbol table, HasQueryBuilder queryBuilderProvider joinRegister) => queryBuilderProvider table -> Snippet
toSnippet queryBuilderProvider = buildSnippet (buildQuery queryBuilderProvider)
{-# INLINE toSnippet #-}

-- | Compile a QueryBuilder to a Hasql Snippet with RLS context
--
-- When RLS is enabled, this wraps the query with SET LOCAL statements.
toSnippetWithRLS :: forall table queryBuilderProvider joinRegister. (KnownSymbol table, HasQueryBuilder queryBuilderProvider joinRegister) => Maybe RowLevelSecurityContext -> queryBuilderProvider table -> Snippet
toSnippetWithRLS Nothing queryBuilderProvider = toSnippet queryBuilderProvider
toSnippetWithRLS (Just rlsContext) queryBuilderProvider =
    rlsSetupSnippet rlsContext <> toSnippet queryBuilderProvider
{-# INLINE toSnippetWithRLS #-}

-- | Generate the RLS setup SQL as a Snippet
rlsSetupSnippet :: RowLevelSecurityContext -> Snippet
rlsSetupSnippet RowLevelSecurityContext { rlsAuthenticatedRole, rlsUserId } =
    Snippet.sql "SET LOCAL ROLE " <> Snippet.param rlsAuthenticatedRole <> Snippet.sql "; " <>
    Snippet.sql "SET LOCAL rls.ihp_user_id = " <> actionToSnippet rlsUserId <> Snippet.sql "; "
{-# INLINE rlsSetupSnippet #-}

-- | Convert a postgresql-simple Action to a Snippet
--
-- This is used to convert the RLS user ID action to a snippet.
actionToSnippet :: PG.Action -> Snippet
actionToSnippet (PG.Plain builder) = Snippet.sql (cs (toLazyByteString builder))
actionToSnippet (PG.Escape bs) = Snippet.param (cs bs :: Text)
actionToSnippet (PG.EscapeByteA bs) = Snippet.param bs
actionToSnippet (PG.EscapeIdentifier bs) = Snippet.sql ("\"" <> cs bs <> "\"")
actionToSnippet (PG.Many actions) = mconcat (map actionToSnippet actions)

-- | Build a Snippet from a compiled SQLQuery
buildSnippet :: SQLQuery -> Snippet
buildSnippet sqlQuery@SQLQuery { queryIndex, selectFrom, distinctClause, distinctOnClause, orderByClause, limitClause, offsetClause, columns } =
    Snippet.sql "SELECT"
    <> optionalSnippet distinctClause
    <> optionalSnippet distinctOnClause
    <> Snippet.sql " " <> selectorsSnippet
    <> Snippet.sql " FROM"
    <> Snippet.sql " " <> Snippet.sql selectFrom
    <> optionalSnippet joinClause
    <> whereSnippet (whereCondition sqlQuery)
    <> orderBySnippet orderByClause
    <> optionalSnippet limitClause
    <> optionalSnippet offsetClause
    where
        optionalSnippet :: Maybe ByteString -> Snippet
        optionalSnippet Nothing = mempty
        optionalSnippet (Just bs) = Snippet.sql " " <> Snippet.sql bs
        {-# INLINE optionalSnippet #-}

        selectorsSnippet :: Snippet
        selectorsSnippet =
            let indexParts = case queryIndex of
                    Just idx -> [Snippet.sql idx]
                    Nothing -> []
                columnParts = map (\column -> Snippet.sql selectFrom <> Snippet.sql "." <> Snippet.sql column) columns
            in mconcat $ List.intersperse (Snippet.sql ", ") (indexParts <> columnParts)

        joinClause :: Maybe ByteString
        joinClause = buildJoinClause $ reverse $ joins sqlQuery

        buildJoinClause :: [Join] -> Maybe ByteString
        buildJoinClause [] = Nothing
        buildJoinClause (joinClause:joinClauses) = Just $
            "INNER JOIN " <> table joinClause <> " ON " <> tableJoinColumn joinClause <>
            " = " <> table joinClause <> "." <> otherJoinColumn joinClause <>
            maybe "" (" " <>) (buildJoinClause joinClauses)
{-# INLINE buildSnippet #-}

-- | Convert a WHERE condition to a Snippet
whereSnippet :: Maybe Condition -> Snippet
whereSnippet Nothing = mempty
whereSnippet (Just condition) = Snippet.sql " WHERE " <> conditionToSnippet condition
{-# INLINE whereSnippet #-}

-- | Convert a Condition to a Snippet
conditionToSnippet :: Condition -> Snippet
conditionToSnippet (VarCondition template action) =
    -- VarCondition contains a template like "id = ?" and an action
    -- We need to substitute the ? with the actual parameter
    substituteAction template action
conditionToSnippet (OrCondition a b) =
    Snippet.sql "(" <> conditionToSnippet a <> Snippet.sql ") OR (" <> conditionToSnippet b <> Snippet.sql ")"
conditionToSnippet (AndCondition a b) =
    Snippet.sql "(" <> conditionToSnippet a <> Snippet.sql ") AND (" <> conditionToSnippet b <> Snippet.sql ")"
{-# INLINE conditionToSnippet #-}

-- | Substitute a ? placeholder in a template with the action value
substituteAction :: ByteString -> PG.Action -> Snippet
substituteAction template action =
    let (before, after) = BS8.break (== '?') template
    in if BS.null after
        then Snippet.sql template  -- No ? found, just use the template
        else Snippet.sql before <> actionToSnippet action <> Snippet.sql (BS.drop 1 after)
{-# INLINE substituteAction #-}

-- | Convert ORDER BY clause to Snippet
orderBySnippet :: [OrderByClause] -> Snippet
orderBySnippet [] = mempty
orderBySnippet clauses = Snippet.sql " ORDER BY " <> mconcat (List.intersperse (Snippet.sql ",") (map orderByClauseToSnippet clauses))
    where
        orderByClauseToSnippet OrderByClause { orderByColumn, orderByDirection } =
            Snippet.sql orderByColumn <> (if orderByDirection == Desc then Snippet.sql " DESC" else mempty)
{-# INLINE orderBySnippet #-}
