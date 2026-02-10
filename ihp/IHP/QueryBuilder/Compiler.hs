{-# LANGUAGE BangPatterns, TypeFamilies, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, InstanceSigs, AllowAmbiguousTypes, DeriveAnyClass #-}
{-|
Module: IHP.QueryBuilder.Compiler
Description: SQL compilation for QueryBuilder
Copyright: (c) digitally induced GmbH, 2020

This module provides functions to compile a QueryBuilder into SQL.
-}
module IHP.QueryBuilder.Compiler
( query
, buildQuery
, negateFilterOperator
, compileSQLQuery
, qualifiedColumnName
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder.Types
import IHP.NameSupport (fieldNameToColumnName)


-- | Returns the "NOT" version of an operator
--
-- >>> negateFilterOperator EqOp
-- NotEqOp
--
negateFilterOperator :: FilterOperator -> FilterOperator
negateFilterOperator EqOp = NotEqOp
negateFilterOperator NotEqOp = EqOp
negateFilterOperator InOp = NotInOp
negateFilterOperator NotInOp = InOp
negateFilterOperator IsOp = IsNotOp
negateFilterOperator IsNotOp = IsOp
negateFilterOperator (LikeOp matchSensitivity) = (NotLikeOp matchSensitivity)
negateFilterOperator (NotLikeOp matchSensitivity) = (LikeOp matchSensitivity)
negateFilterOperator (MatchesOp matchSensitivity) = error "not supported"
negateFilterOperator GreaterThanOp = LessThanOrEqualToOp
negateFilterOperator GreaterThanOrEqualToOp = LessThanOp
negateFilterOperator LessThanOp = GreaterThanOrEqualToOp
negateFilterOperator LessThanOrEqualToOp = GreaterThanOp
negateFilterOperator SqlOp = SqlOp

-- | Represent's a @SELECT * FROM ..@ query. It's the starting point to build a query.
-- Used together with the other functions to compose a sql query.
--
-- Example: Fetching all users
--
-- > allUsers <- query @User |> fetch
-- > -- Runs a 'SELECT * FROM users' query
--
-- You can use it together with 'filterWhere':
--
-- > activeUsers :: [User] <-
-- >    query @User
-- >     |> filterWhere (#active, True)
-- >     |> fetch
query :: forall model table. (table ~ GetTableName model, Table model) => DefaultScope table => QueryBuilder table
query = (defaultScope @table) NewQueryBuilder { selectFrom = tableName @model, columns = columnNames @model }
{-# INLINE query #-}

{-# INLINE buildQuery #-}
buildQuery :: forall table queryBuilderProvider joinRegister. (KnownSymbol table, HasQueryBuilder queryBuilderProvider joinRegister) => queryBuilderProvider table -> SQLQuery
buildQuery queryBuilderProvider = compileSQLQuery (getQueryIndex queryBuilderProvider) (getQueryBuilder queryBuilderProvider)

-- | Traverse a 'QueryBuilder' tree and produce an 'SQLQuery'.
--
-- Defined at the top level so the recursive case analysis is shared across
-- all inlined call sites of 'buildQuery'. The tiny 'buildQuery' wrapper
-- stays INLINE to resolve the 'HasQueryBuilder' dictionary, then hands off
-- to this shared worker.
compileSQLQuery :: Maybe Text -> QueryBuilder table -> SQLQuery
compileSQLQuery qIndex NewQueryBuilder { selectFrom, columns } =
    SQLQuery
        {     queryIndex = qIndex
            , selectFrom = selectFrom
            , distinctClause = False
            , distinctOnClause = Nothing
            , whereCondition = Nothing
            , joins = []
            , orderByClause = []
            , limitClause = Nothing
            , offsetClause = Nothing
            , columns
            }
compileSQLQuery qIndex (DistinctQueryBuilder { queryBuilder }) = queryBuilder
        |> compileSQLQuery qIndex
        |> set #distinctClause True
compileSQLQuery qIndex (DistinctOnQueryBuilder { queryBuilder, distinctOnColumn }) = queryBuilder
        |> compileSQLQuery qIndex
        |> setJust #distinctOnClause distinctOnColumn
compileSQLQuery qIndex (FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, operator, snippet), applyLeft, applyRight }) =
            let condition = ColumnCondition columnName operator snippet applyLeft applyRight
            in
                queryBuilder
                    |> compileSQLQuery qIndex
                    |> modify #whereCondition \case
                            Just c -> Just (AndCondition c condition)
                            Nothing -> Just condition
compileSQLQuery qIndex (OrderByQueryBuilder { queryBuilder, queryOrderByClause }) = queryBuilder
        |> compileSQLQuery qIndex
        |> modify #orderByClause (\value -> value <> [queryOrderByClause] ) -- although adding to the end of a list is bad form, these lists are very short
compileSQLQuery qIndex (LimitQueryBuilder { queryBuilder, queryLimit }) =
                queryBuilder
                |> compileSQLQuery qIndex
                |> setJust #limitClause queryLimit
compileSQLQuery qIndex (OffsetQueryBuilder { queryBuilder, queryOffset }) = queryBuilder
        |> compileSQLQuery qIndex
        |> setJust #offsetClause queryOffset
compileSQLQuery qIndex (UnionQueryBuilder { firstQueryBuilder, secondQueryBuilder }) =
            let
                firstQuery = compileSQLQuery qIndex firstQueryBuilder
                secondQuery = compileSQLQuery qIndex secondQueryBuilder
                isSimpleQuery q = null (orderByClause q) && isNothing (limitClause q) && isNothing (offsetClause q) && null (joins q)
                isSimpleUnion = isSimpleQuery firstQuery && isSimpleQuery secondQuery
                unionWhere =
                    case (whereCondition firstQuery, whereCondition secondQuery) of
                        (Nothing, wc) -> wc
                        (wc, Nothing) -> wc
                        (Just firstWhere, Just secondWhere) -> Just $ OrCondition firstWhere secondWhere
            in
                if isSimpleUnion then
                    firstQuery { whereCondition = unionWhere }
                else
                    error "buildQuery: Union of complex queries not supported yet"
compileSQLQuery qIndex (JoinQueryBuilder { queryBuilder, joinData }) =
    let
        firstQuery = compileSQLQuery qIndex queryBuilder
     in firstQuery { joins = joinData:joins firstQuery }

-- | Build a qualified column name like @tablename.column_name@ from a table name
-- and a camelCase field name. The field name is converted to snake_case via
-- 'fieldNameToColumnName'.
--
-- This is intentionally NOINLINE: the call sites in filterWhere, orderBy, etc.
-- are always lifted to CAFs (evaluated once at program start), so inlining
-- only duplicates the Text.Inflections parsing logic and Text concatenation
-- without any runtime benefit.
qualifiedColumnName :: Text -> Text -> Text
qualifiedColumnName tableName fieldName = tableName <> "." <> fieldNameToColumnName fieldName
{-# NOINLINE qualifiedColumnName #-}
