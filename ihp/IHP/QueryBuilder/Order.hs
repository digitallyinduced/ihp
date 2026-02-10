{-# LANGUAGE BangPatterns, TypeFamilies, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, InstanceSigs, AllowAmbiguousTypes, DeriveAnyClass #-}
{-|
Module: IHP.QueryBuilder.Order
Description: ORDER BY, LIMIT, OFFSET, and DISTINCT operations for QueryBuilder
Copyright: (c) digitally induced GmbH, 2020

This module provides functions for ordering, limiting, and deduplicating query results.
-}
module IHP.QueryBuilder.Order
( orderBy
, orderByAsc
, orderByDesc
, orderByJoinedTable
, orderByAscJoinedTable
, orderByDescJoinedTable
, limit
, offset
, distinct
, distinctOn
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder.Types
import IHP.QueryBuilder.Compiler (qualifiedColumnName)

-- | Adds an @ORDER BY .. ASC@ to your query.
--
-- Use 'orderByDesc' for descending order.
--
-- __Example:__ Fetch the 10 oldest books.
--
-- > query @Book
-- >     |> orderBy #createdAt -- >     |> limit 10
-- >     |> fetch
-- > -- SELECT * FROM books LIMIT 10 ORDER BY created_at ASC
orderByAsc :: forall name model table value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => Proxy name -> queryBuilderProvider table -> queryBuilderProvider table
orderByAsc !name queryBuilderProvider = injectQueryBuilder OrderByQueryBuilder { queryBuilder, queryOrderByClause = OrderByClause { orderByColumn = columnName, orderByDirection = Asc } }
    where
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE orderByAsc #-}

-- | Adds an @ORDER BY .. ASC@ on a joined table column to your query.
--
-- Use 'orderByDescJoinedTable' for descending order.
--
-- __Example:__ Order joined `User` records by `username` ascending.
--
-- > query @Project
-- >     |> innerJoin @User (#id, #projectId)
-- >     |> orderByAscJoinedTable #username
-- >     |> fetch
-- > -- SELECT ... FROM projects
-- > -- INNER JOIN users ON projects.id = users.project_id
-- > -- ORDER BY users.username ASC
orderByAscJoinedTable :: forall model name table value queryBuilderProvider joinRegister table'. ( KnownSymbol table, KnownSymbol name, HasField name model value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => Proxy name -> queryBuilderProvider table' -> queryBuilderProvider table'
orderByAscJoinedTable !name queryBuilderProvider = injectQueryBuilder OrderByQueryBuilder { queryBuilder = queryBuilder, queryOrderByClause = OrderByClause { orderByColumn = columnName, orderByDirection = Asc } }
    where
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE orderByAscJoinedTable #-}

-- | Adds an @ORDER BY .. DESC@ to your query.
--
-- Use 'orderBy' for ascending order.
--
-- __Example:__ Fetch the 10 newest projects (ordered by creation time).
--
-- > query @Project
-- >     |> orderByDesc #createdAt
-- >     |> limit 10
-- >     |> fetch
-- > -- SELECT * FROM projects LIMIT 10 ORDER BY created_at DESC
orderByDesc :: forall name model table value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => Proxy name -> queryBuilderProvider table -> queryBuilderProvider table
orderByDesc !name queryBuilderProvider = injectQueryBuilder OrderByQueryBuilder { queryBuilder, queryOrderByClause = OrderByClause { orderByColumn = columnName, orderByDirection = Desc } }
    where
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE orderByDesc #-}

-- | Adds an @ORDER BY .. DESC@ on a joined table column to your query.
--
-- Use 'orderByAscJoinedTable' for ascending order.
--
-- __Example:__ Order joined `User` records by `username` descending.
--
-- > query @Project
-- >     |> innerJoin @User (#id, #projectId)
-- >     |> orderByDescJoinedTable #username
-- >     |> fetch
-- > -- SELECT ... FROM projects
-- > -- INNER JOIN users ON projects.id = users.project_id
-- > -- ORDER BY users.username DESC
orderByDescJoinedTable :: forall model name table value queryBuilderProvider joinRegister table'. ( KnownSymbol table, KnownSymbol name, HasField name model value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => Proxy name -> queryBuilderProvider table' -> queryBuilderProvider table'
orderByDescJoinedTable !name queryBuilderProvider = injectQueryBuilder OrderByQueryBuilder { queryBuilder = queryBuilder, queryOrderByClause = OrderByClause { orderByColumn = columnName, orderByDirection = Desc } }
    where
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE orderByDescJoinedTable #-}

-- | Alias for 'orderByAsc'
orderBy :: (KnownSymbol table, KnownSymbol name, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => Proxy name -> queryBuilderProvider table -> queryBuilderProvider table
orderBy !name = orderByAsc name
{-# INLINE orderBy #-}

-- | Alias for 'orderByAscJoinedTable'
orderByJoinedTable :: forall model name table value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, HasField name model value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => Proxy name -> queryBuilderProvider table' -> queryBuilderProvider table'
orderByJoinedTable !name = orderByAscJoinedTable @model @name @table @value @queryBuilderProvider @joinRegister @table' name
{-# INLINE orderByJoinedTable #-}

-- | Adds an @LIMIT ..@ to your query.
--
--
-- __Example:__ Fetch 10 posts
--
-- > query @Post
-- >     |> limit 10
-- >     |> fetch
-- > -- SELECT * FROM posts LIMIT 10
limit :: (HasQueryBuilder queryBuilderProvider joinRegister) => Int -> queryBuilderProvider model -> queryBuilderProvider model
limit !queryLimit queryBuilderProvider = injectQueryBuilder LimitQueryBuilder { queryBuilder, queryLimit }
    where
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE limit #-}

-- | Adds an @OFFSET ..@ to your query. Most often used together with @LIMIT...@
--
--
-- __Example:__ Fetch posts 10-20
--
-- > query @Post
-- >     |> limit 10
-- >     |> offset 10
-- >     |> fetch
-- > -- SELECT * FROM posts LIMIT 10 OFFSET 10
offset :: (HasQueryBuilder queryBuilderProvider joinRegister) => Int -> queryBuilderProvider model -> queryBuilderProvider model
offset !queryOffset queryBuilderProvider = injectQueryBuilder OffsetQueryBuilder { queryBuilder, queryOffset }
    where
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE offset #-}

-- | Adds a @DISTINCT@ to your query.
--
-- Use 'distinct' to remove all duplicate rows from the result
--
-- __Example:__ Fetch distinct books
--
-- > query @Book
-- >     |> distinct
-- >     |> fetch
-- > -- SELECT DISTINCT * FROM books
distinct :: (HasQueryBuilder queryBuilderProvider joinRegister) => queryBuilderProvider table -> queryBuilderProvider table
distinct = injectQueryBuilder . DistinctQueryBuilder . getQueryBuilder
{-# INLINE distinct #-}

-- | Adds an @DISTINCT ON .. to your query.
--
-- Use 'distinctOn' to return a single row for each distinct value provided.
--
-- __Example:__ Fetch one book for each categoryId field
--
-- > query @Book
-- >     |> distinctOn #categoryId
-- >     |> fetch
-- > -- SELECT DISTINCT ON (category_id) * FROM books
distinctOn :: forall name model value table queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => Proxy name -> queryBuilderProvider table -> queryBuilderProvider table
distinctOn !name queryBuilderProvider = injectQueryBuilder DistinctOnQueryBuilder { distinctOnColumn = columnName, queryBuilder = getQueryBuilder queryBuilderProvider}
    where
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
{-# INLINE distinctOn #-}
