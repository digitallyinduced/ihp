{-|
Module: IHP.QueryBuilder
Description: Tool to build simple sql queries
Copyright: (c) digitally induced GmbH, 2020

QueryBuilder is mainly used for doing simple @SELECT@ sql queries. It allows dynamic
creation of sql queries in a type safe way.

For more complex sql queries, use 'IHP.ModelSupport.sqlQuery'.

This module re-exports all QueryBuilder submodules for backward compatibility.
-}
module IHP.QueryBuilder
( -- * Starting a Query
  query
  -- * Core Types
, QueryBuilder (..)
, Condition (..)
, ConditionValue (..)
, OrderByClause (..)
, OrderByDirection (..)
, FilterOperator (..)
, MatchSensitivity (..)
  -- * Type Classes
, DefaultScope (..)
, EqOrIsOperator
, FilterPrimaryKey (..)
  -- * SQL Compilation
, buildQuery
  -- * Hasql Compilation
, toSQL
  -- * Filtering
, filterWhere
, filterWhereCaseInsensitive
, filterWhereNot
, filterWhereIn
, filterWhereInCaseInsensitive
, filterWhereIdIn
, filterWhereNotIn
, filterWhereLike
, filterWhereILike
, filterWhereMatches
, filterWhereIMatches
, filterWherePast
, filterWhereFuture
, filterWhereGreaterThan
, filterWhereLarger
, filterWhereGreaterThanOrEqualTo
, filterWhereAtLeast
, filterWhereLessThan
, filterWhereSmaller
, filterWhereLessThanOrEqualTo
, filterWhereAtMost
, filterWhereSql
  -- * Ordering
, orderBy
, orderByAsc
, orderByDesc
  -- * Pagination
, limit
, offset
  -- * Deduplication
, distinct
, distinctOn
  -- * Unions
, queryUnion
, queryUnionList
, queryOr
  -- * Operators
, toEqOrIsOperator
, compileOperator
, negateFilterOperator
) where

import IHP.QueryBuilder.Types
import IHP.QueryBuilder.Compiler
import IHP.QueryBuilder.HasqlCompiler (toSQL, compileOperator)
import IHP.QueryBuilder.Filter
import IHP.QueryBuilder.Order
import IHP.QueryBuilder.Union
