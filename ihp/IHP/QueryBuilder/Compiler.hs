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
, qualifiedColumnName
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder.Types


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
query = let tn = tableName @model
            cols = columnNames @model
        in (defaultScope @table) QueryBuilder { unQueryBuilder = SQLQuery
    { queryIndex = Nothing
    , selectFrom = tn
    , columns = cols
    , columnsSql = qualifyAndJoinColumns tn cols
    , distinctClause = False
    , distinctOnClause = Nothing
    , whereCondition = Nothing
    , joins = []
    , orderByClause = []
    , limitClause = Nothing
    , offsetClause = Nothing
    } }
{-# INLINE query #-}

-- | Extract the SQLQuery from a QueryBuilder provider, setting the queryIndex.
{-# INLINE buildQuery #-}
buildQuery :: forall table queryBuilderProvider joinRegister. (KnownSymbol table, HasQueryBuilder queryBuilderProvider joinRegister) => queryBuilderProvider table -> SQLQuery
buildQuery queryBuilderProvider = (unQueryBuilder (getQueryBuilder queryBuilderProvider)) { queryIndex = getQueryIndex queryBuilderProvider }

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
