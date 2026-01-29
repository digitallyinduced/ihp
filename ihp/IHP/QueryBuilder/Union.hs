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
import qualified Hasql.DynamicStatements.Snippet as Snippet

-- | Merges the results of two query builders.
--
-- Take a look at 'queryOr'  as well, as this might be a bit shorter.
--
-- __Example:__ Return all pages owned by the user or owned by the users team.
--
-- > let userPages = query @Page |> filterWhere (#ownerId, currentUserId)
-- > let teamPages = query @Page |> filterWhere (#teamId, currentTeamId)
-- > pages <- queryUnion userPages teamPages |> fetch
-- > -- (SELECT * FROM pages WHERE owner_id = '..') UNION (SELECT * FROM pages WHERE team_id = '..')
queryUnion :: (HasQueryBuilder queryBuilderProvider joinRegister, HasQueryBuilder r joinRegister') => queryBuilderProvider model -> r model -> NoJoinQueryBuilderWrapper model
queryUnion firstQueryBuilderProvider secondQueryBuilderProvider = NoJoinQueryBuilderWrapper (UnionQueryBuilder { firstQueryBuilder, secondQueryBuilder })
    where
        firstQueryBuilder = getQueryBuilder firstQueryBuilderProvider
        secondQueryBuilder = getQueryBuilder secondQueryBuilderProvider


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
queryUnionList [] = FilterByQueryBuilder { queryBuilder = query @(GetModelByTableName table) @table, queryFilter = ("id", NotEqOp, Snippet.sql "id"), applyLeft = Nothing, applyRight = Nothing }
queryUnionList (firstQueryBuilder:secondQueryBuilder:[]) = UnionQueryBuilder { firstQueryBuilder, secondQueryBuilder }
queryUnionList (firstQueryBuilder:rest) = UnionQueryBuilder { firstQueryBuilder, secondQueryBuilder = queryUnionList @table rest }


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
queryOr firstQuery secondQuery queryBuilder = injectQueryBuilder
    (UnionQueryBuilder {
        firstQueryBuilder = getQueryBuilder $ firstQuery queryBuilder,
        secondQueryBuilder = getQueryBuilder $ secondQuery queryBuilder}
    )
{-# INLINE queryOr #-}
