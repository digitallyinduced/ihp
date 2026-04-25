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
orderByAsc :: forall name model table value. (KnownSymbol table, KnownSymbol name, HasField name model value, model ~ GetModelByTableName table, Table model) => Proxy name -> QueryBuilder table -> QueryBuilder table
orderByAsc !name (QueryBuilder sq) =
    QueryBuilder sq { orderByClause = orderByClause sq <> [OrderByClause { orderByColumn = columnName, orderByDirection = Asc }] }
    where
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
{-# INLINE orderByAsc #-}

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
orderByDesc :: forall name model table value. (KnownSymbol table, KnownSymbol name, HasField name model value, model ~ GetModelByTableName table, Table model) => Proxy name -> QueryBuilder table -> QueryBuilder table
orderByDesc !name (QueryBuilder sq) =
    QueryBuilder sq { orderByClause = orderByClause sq <> [OrderByClause { orderByColumn = columnName, orderByDirection = Desc }] }
    where
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
{-# INLINE orderByDesc #-}

-- | Alias for 'orderByAsc'
orderBy :: (KnownSymbol table, KnownSymbol name, HasField name model value, model ~ GetModelByTableName table, Table model) => Proxy name -> QueryBuilder table -> QueryBuilder table
orderBy !name = orderByAsc name
{-# INLINE orderBy #-}

-- | Adds an @LIMIT ..@ to your query.
--
--
-- __Example:__ Fetch 10 posts
--
-- > query @Post
-- >     |> limit 10
-- >     |> fetch
-- > -- SELECT * FROM posts LIMIT 10
limit :: Int64 -> QueryBuilder model -> QueryBuilder model
limit !queryLimit (QueryBuilder sq) =
    QueryBuilder sq { limitClause = Just queryLimit }
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
offset :: Int64 -> QueryBuilder model -> QueryBuilder model
offset !queryOffset (QueryBuilder sq) =
    QueryBuilder sq { offsetClause = Just queryOffset }
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
distinct :: QueryBuilder table -> QueryBuilder table
distinct (QueryBuilder sq) =
    QueryBuilder sq { distinctClause = True }
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
distinctOn :: forall name model value table. (KnownSymbol table, KnownSymbol name, HasField name model value, model ~ GetModelByTableName table, Table model) => Proxy name -> QueryBuilder table -> QueryBuilder table
distinctOn !name (QueryBuilder sq) =
    QueryBuilder sq { distinctOnClause = Just columnName }
    where
        columnName = qualifiedColumnName (tableName @model) (symbolToText @name)
{-# INLINE distinctOn #-}
