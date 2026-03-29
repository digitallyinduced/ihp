{-# LANGUAGE BangPatterns, TypeFamilies, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, InstanceSigs, AllowAmbiguousTypes, DeriveAnyClass #-}
{-|
Module: IHP.QueryBuilder.Union
Description: UNION and OR operations for QueryBuilder
Copyright: (c) digitally induced GmbH, 2020

This module provides functions for combining queries with UNION and OR operations.
-}
module IHP.QueryBuilder.Union
( queryUnion
, queryUnionList
, queryOr
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder.Types
import IHP.QueryBuilder.Compiler (query)

-- | Merges the results of two query builders by ORing their WHERE conditions.
--
-- Take a look at 'queryOr'  as well, as this might be a bit shorter.
--
-- __Example:__ Return all pages owned by the user or owned by the users team.
--
-- > let userPages = query @Page |> filterWhere (#ownerId, currentUserId)
-- > let teamPages = query @Page |> filterWhere (#teamId, currentTeamId)
-- > pages <- queryUnion userPages teamPages |> fetch
-- > -- SELECT * FROM pages WHERE (owner_id = '..') OR (team_id = '..')
queryUnion :: (HasQueryBuilder queryBuilderProvider joinRegister, HasQueryBuilder r joinRegister') => queryBuilderProvider model -> r model -> NoJoinQueryBuilderWrapper model
queryUnion firstQueryBuilderProvider secondQueryBuilderProvider =
    let QueryBuilder first = getQueryBuilder firstQueryBuilderProvider
        QueryBuilder second = getQueryBuilder secondQueryBuilderProvider
        isSimple q = null (orderByClause q) && isNothing (limitClause q) && isNothing (offsetClause q) && null (joins q)
        unionWhere = case (whereCondition first, whereCondition second) of
            (Nothing, wc) -> wc
            (wc, Nothing) -> wc
            (Just a, Just b) -> Just (OrCondition a b)
    in if isSimple first && isSimple second
        then NoJoinQueryBuilderWrapper $ QueryBuilder first { whereCondition = unionWhere }
        else error "queryUnion: Union of complex queries (with ORDER BY, LIMIT, OFFSET, or JOINs) not supported"
{-# INLINE queryUnion #-}

-- | Like 'queryUnion', but applied on all the elements on the list
--
-- >  action ProjectsAction = do
-- >      let values :: [(ProjectType, Int)] = [(ProjectTypeOngoing, 3), (ProjectTypeNotStarted, 2)]
-- >
-- >          valuePairToCondition :: (ProjectType, Int) -> QueryBuilder "projects"
-- >          valuePairToCondition (projectType, participants) =
-- >              query @Project
-- >                  |> filterWhere (#projectType, projectType)
-- >                  |> filterWhere (#participants, participants)
-- >
-- >          theQuery = queryUnionList (map valuePairToCondition values)
-- >
-- >      projects <- fetch theQuery
-- >      render IndexView { .. }
queryUnionList :: forall table. (Table (GetModelByTableName table), KnownSymbol table, GetTableName (GetModelByTableName table) ~ table) => [QueryBuilder table] -> QueryBuilder table
-- For empty list, create a condition that is always false: id <> id (which is always false for non-null)
queryUnionList [] = addCondition (ColumnCondition "id" NotEqOp (Literal "id") Nothing Nothing) (query @(GetModelByTableName table) @table)
queryUnionList [single] = single
queryUnionList (first:rest) =
    let QueryBuilder firstSq = first
        QueryBuilder restSq = queryUnionList @table rest
        isSimple q = null (orderByClause q) && isNothing (limitClause q) && isNothing (offsetClause q) && null (joins q)
        unionWhere = case (whereCondition firstSq, whereCondition restSq) of
            (Nothing, wc) -> wc
            (wc, Nothing) -> wc
            (Just a, Just b) -> Just (OrCondition a b)
    in if isSimple firstSq && isSimple restSq
        then QueryBuilder firstSq { whereCondition = unionWhere }
        else error "queryUnionList: Union of complex queries (with ORDER BY, LIMIT, OFFSET, or JOINs) not supported"


-- | Adds an @a OR b@ condition
--
-- __Example:__ Return all pages owned by the user or public.
--
-- > query @Page
-- >     |> queryOr
-- >         (filterWhere (#createdBy, currentUserId))
-- >         (filterWhere (#public, True))
-- >     |> fetch
-- > -- SELECT * FROM pages WHERE created_by = '..' OR public = True
queryOr :: (HasQueryBuilder queryBuilderProvider joinRegister, HasQueryBuilder queryBuilderProvider'' joinRegister'', HasQueryBuilder queryBuilderProvider''' joinRegister''') => (queryBuilderProvider model -> queryBuilderProvider''' model) -> (queryBuilderProvider model -> queryBuilderProvider'' model) -> queryBuilderProvider model -> queryBuilderProvider model
queryOr firstQuery secondQuery queryBuilderProvider =
    let QueryBuilder firstSq = getQueryBuilder $ firstQuery queryBuilderProvider
        QueryBuilder secondSq = getQueryBuilder $ secondQuery queryBuilderProvider
        unionWhere = case (whereCondition firstSq, whereCondition secondSq) of
            (Nothing, wc) -> wc
            (wc, Nothing) -> wc
            (Just a, Just b) -> Just (OrCondition a b)
        QueryBuilder baseSq = getQueryBuilder queryBuilderProvider
    in injectQueryBuilder $ QueryBuilder baseSq { whereCondition = unionWhere }
{-# INLINE queryOr #-}
