{-# LANGUAGE BangPatterns, TypeFamilies, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, InstanceSigs, AllowAmbiguousTypes, DeriveAnyClass #-}
{-|
Module: IHP.QueryBuilder.Filter
Description: WHERE clause filtering functions for QueryBuilder
Copyright: (c) digitally induced GmbH, 2020

This module provides all the filterWhere* functions for building WHERE clauses.
-}
module IHP.QueryBuilder.Filter
( filterWhere
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
  -- * Joined table variants
, filterWhereJoinedTable
, filterWhereCaseInsensitiveJoinedTable
, filterWhereNotJoinedTable
, filterWhereInJoinedTable
, filterWhereNotInJoinedTable
, filterWhereLikeJoinedTable
, filterWhereILikeJoinedTable
, filterWhereMatchesJoinedTable
, filterWhereIMatchesJoinedTable
  -- * Helpers
, snippetParamList
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder.Types
import IHP.QueryBuilder.Compiler (negateFilterOperator)
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text.Encoding as Text
import Data.Text (toLower)
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.Implicits.Encoders (DefaultParamEncoder(..))
import qualified Data.List

-- | Adds a simple @WHERE x = y@ condition to the query.
filterWhere :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, EqOrIsOperator value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhere (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, toEqOrIsOperator value, Snippet.param value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhere #-}

filterWhereJoinedTable :: forall model name table value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, EqOrIsOperator value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, toEqOrIsOperator value, Snippet.param value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereJoinedTable #-}

filterWhereCaseInsensitiveJoinedTable :: forall model name table value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, EqOrIsOperator value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereCaseInsensitiveJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, toEqOrIsOperator value, Snippet.param value), applyLeft = Just "LOWER", applyRight = Just "LOWER" }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereCaseInsensitiveJoinedTable #-}

filterWhereNot :: forall name table model value. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, EqOrIsOperator value, model ~ GetModelByTableName table, Table model) => (Proxy name, value) -> QueryBuilder table -> QueryBuilder table
filterWhereNot (name, value) queryBuilder = FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, negateFilterOperator (toEqOrIsOperator value), Snippet.param value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
{-# INLINE filterWhereNot #-}

filterWhereNotJoinedTable :: forall model name table value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, EqOrIsOperator value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereNotJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, negateFilterOperator (toEqOrIsOperator value), Snippet.param value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereNotJoinedTable #-}

-- | Adds a @WHERE x IN (y)@ condition to the query.
-- For hasql, we build an IN expression with individual parameters
filterWhereIn :: forall name table model value queryBuilderProvider (joinRegister :: Type). (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, EqOrIsOperator value, Table model) => (Proxy name, [value]) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereIn (name, values) queryBuilderProvider =
    case nullValues of
        [] -> injectQueryBuilder whereInQuery -- All values non null
        (nullValue:_) ->
            case nonNullValues of
                (_:_) -> -- Some non null values, some null values
                    injectQueryBuilder $ UnionQueryBuilder
                        (injectQueryBuilder whereInQuery)
                        (injectQueryBuilder isNullValueExpr)
                [] -> injectQueryBuilder isNullValueExpr -- All values null
    where
        (nonNullValues, nullValues) = values |> partition (\v -> toEqOrIsOperator v == EqOp)

        -- Build IN clause with individual params: col IN ($1, $2, $3, ...)
        inSnippet = snippetParamList (map Snippet.param nonNullValues)

        whereInQuery = FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, InOp, inSnippet), applyLeft = Nothing, applyRight = Nothing }

        isNullValueExpr = case nullValues of
            (nv:_) -> FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, IsOp, Snippet.param nv), applyLeft = Nothing, applyRight = Nothing }
            [] -> error "filterWhereIn: impossible"

        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereIn #-}

-- | Like 'filterWhereIn', but case insensitive.
filterWhereInCaseInsensitive :: forall name table model value queryBuilderProvider (joinRegister :: Type). ( KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, EqOrIsOperator value, Table model) => (Proxy name, [Text]) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereInCaseInsensitive (name, values) queryBuilderProvider =
    case nullValues of
        [] -> injectQueryBuilder whereInQuery
        (nullValue:_) ->
            case nonNullValues of
                (_:_) ->
                    injectQueryBuilder $ UnionQueryBuilder
                        (injectQueryBuilder whereInQuery)
                        (injectQueryBuilder isNullValueExpr)
                [] -> injectQueryBuilder isNullValueExpr
    where
        (nonNullValues, nullValues) = values |> partition (\v -> toEqOrIsOperator v == EqOp)
        lowerValues = map toLower nonNullValues

        inSnippet = snippetParamList (map Snippet.param lowerValues)

        whereInQuery = FilterByQueryBuilder { queryBuilder, queryFilter = (lowerColumnName, InOp, inSnippet), applyLeft = Nothing, applyRight = Nothing }

        isNullValueExpr = case nullValues of
            (nv:_) -> FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, IsOp, Snippet.param nv), applyLeft = Nothing, applyRight = Nothing }
            [] -> error "filterWhereInCaseInsensitive: impossible"

        lowerColumnName = "LOWER(" <> columnName <> ")"
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereInCaseInsensitive #-}

filterWhereInJoinedTable :: forall model name table value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => (Proxy name, [value]) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereInJoinedTable (name, values) queryBuilderProvider =
    let inSnippet = snippetParamList (map Snippet.param values)
    in injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, InOp, inSnippet), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereInJoinedTable #-}

filterWhereNotIn :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, EqOrIsOperator value) => (Proxy name, [value]) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereNotIn (_, []) queryBuilder = queryBuilder
filterWhereNotIn (name, values) queryBuilderProvider =
    case nullValues of
        [] -> injectQueryBuilder whereNotInQuery
        (nullValue:_) ->
            case nonNullValues of
                (_:_) -> injectQueryBuilder FilterByQueryBuilder { queryBuilder = whereNotInQuery, queryFilter = (columnName, IsNotOp, Snippet.param nullValue), applyLeft = Nothing, applyRight = Nothing }
                [] -> injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, IsNotOp, Snippet.param nullValue), applyLeft = Nothing, applyRight = Nothing }
    where
        (nonNullValues, nullValues) = values |> partition (\v -> toEqOrIsOperator v == EqOp)

        inSnippet = snippetParamList (map Snippet.param nonNullValues)

        whereNotInQuery = FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, NotInOp, inSnippet), applyLeft = Nothing, applyRight = Nothing }

        columnName = Text.encodeUtf8 (symbolToText @table) <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereNotIn #-}

filterWhereNotInJoinedTable :: forall model name table value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => (Proxy name, [value]) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereNotInJoinedTable (_, []) queryBuilderProvider = queryBuilderProvider
filterWhereNotInJoinedTable (name, values) queryBuilderProvider =
    let inSnippet = snippetParamList (map Snippet.param values)
    in injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, NotInOp, inSnippet), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereNotInJoinedTable #-}

filterWhereLike :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereLike (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, LikeOp CaseSensitive, Snippet.param value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereLike #-}

filterWhereLikeJoinedTable :: forall model name table value queryBuilderProvider joinRegister table'. (KnownSymbol name, KnownSymbol table, table ~ GetTableName model, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereLikeJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, LikeOp CaseSensitive, Snippet.param value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereLikeJoinedTable #-}

filterWhereILike :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereILike (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, LikeOp CaseInsensitive, Snippet.param value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereILike #-}

filterWhereILikeJoinedTable :: forall model table name table' model' value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, table ~ GetTableName model, model' ~ GetModelByTableName table', HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereILikeJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, LikeOp CaseInsensitive, Snippet.param value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereILikeJoinedTable #-}

filterWhereMatches :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereMatches (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, MatchesOp CaseSensitive, Snippet.param value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereMatches #-}

filterWhereMatchesJoinedTable :: forall model table name value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereMatchesJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, MatchesOp CaseSensitive, Snippet.param value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereMatchesJoinedTable #-}

filterWhereIMatches :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereIMatches (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, MatchesOp CaseInsensitive, Snippet.param value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereIMatches #-}

filterWhereIMatchesJoinedTable :: forall model table name value queryBuilderProvider joinRegister table'. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, table ~ GetTableName model, HasQueryBuilder queryBuilderProvider joinRegister, IsJoined model joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table' -> queryBuilderProvider table'
filterWhereIMatchesJoinedTable (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, MatchesOp CaseInsensitive, Snippet.param value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereIMatchesJoinedTable #-}

filterWherePast
    :: ( KnownSymbol table
       , KnownSymbol name
       , DefaultParamEncoder value
       , HasField name (GetModelByTableName table) value
       , HasQueryBuilder queryBuilderProvider joinRegister
       , Table (GetModelByTableName table)
       )
    => Proxy name -> queryBuilderProvider table -> queryBuilderProvider table
filterWherePast name = filterWhereSql (name, "<= NOW()")
{-# INLINE filterWherePast #-}

filterWhereFuture
    :: ( KnownSymbol table
       , KnownSymbol name
       , DefaultParamEncoder value
       , HasField name (GetModelByTableName table) value
       , HasQueryBuilder queryBuilderProvider joinRegister
       , Table (GetModelByTableName table)
       )
    => Proxy name -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereFuture name = filterWhereSql (name, "> NOW()")
{-# INLINE filterWhereFuture #-}

filterWhereGreaterThan :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereGreaterThan (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, GreaterThanOp, Snippet.param value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereGreaterThan #-}

filterWhereLarger :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereLarger = filterWhereGreaterThan
{-# INLINE filterWhereLarger #-}

filterWhereGreaterThanOrEqualTo :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereGreaterThanOrEqualTo (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, GreaterThanOrEqualToOp, Snippet.param value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereGreaterThanOrEqualTo #-}

filterWhereAtLeast :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereAtLeast = filterWhereGreaterThanOrEqualTo
{-# INLINE filterWhereAtLeast #-}

filterWhereLessThan :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereLessThan (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, LessThanOp, Snippet.param value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereLessThan #-}

filterWhereSmaller :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereSmaller = filterWhereLessThan
{-# INLINE filterWhereSmaller #-}

filterWhereLessThanOrEqualTo :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereLessThanOrEqualTo (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, LessThanOrEqualToOp, Snippet.param value), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereLessThanOrEqualTo #-}

filterWhereAtMost :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereAtMost = filterWhereLessThanOrEqualTo
{-# INLINE filterWhereAtMost #-}

-- | Allows to add a custom raw sql where condition
filterWhereSql :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, ByteString) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereSql (name, sqlCondition) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, SqlOp, Snippet.sql (cs sqlCondition)), applyLeft = Nothing, applyRight = Nothing }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereSql #-}

filterWhereCaseInsensitive :: forall name table model value queryBuilderProvider joinRegister. (KnownSymbol table, KnownSymbol name, DefaultParamEncoder value, HasField name model value, EqOrIsOperator value, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister, Table model) => (Proxy name, value) -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereCaseInsensitive (name, value) queryBuilderProvider = injectQueryBuilder FilterByQueryBuilder { queryBuilder, queryFilter = (columnName, toEqOrIsOperator value, Snippet.param value), applyLeft = Just "LOWER", applyRight = Just "LOWER" }
    where
        columnName = tableNameByteString @model <> "." <> Text.encodeUtf8 (fieldNameToColumnName (symbolToText @name))
        queryBuilder = getQueryBuilder queryBuilderProvider
{-# INLINE filterWhereCaseInsensitive #-}


filterWhereIdIn :: forall table model queryBuilderProvider (joinRegister :: Type). (KnownSymbol table, Table model, model ~ GetModelByTableName table, HasQueryBuilder queryBuilderProvider joinRegister) => [Id model] -> queryBuilderProvider table -> queryBuilderProvider table
filterWhereIdIn values queryBuilderProvider =
    let
        pkSnippets = map (primaryKeyConditionForId @model) values
        inSnippet = snippetParamList pkSnippets
        queryBuilder = getQueryBuilder queryBuilderProvider
        whereInQuery = FilterByQueryBuilder {queryBuilder, queryFilter = (primaryKeyConditionColumnSelector @model, InOp, inSnippet), applyLeft = Nothing, applyRight = Nothing}
     in
        injectQueryBuilder whereInQuery
{-# INLINE filterWhereIdIn #-}

-- | Build a parenthesized, comma-separated SQL list from Snippet values.
-- Returns @(NULL)@ for an empty list (which never matches in @IN@ expressions).
snippetParamList :: [Snippet] -> Snippet
snippetParamList [] = Snippet.sql "(NULL)"
snippetParamList vs = mconcat
    [ Snippet.sql "("
    , mconcat $ Data.List.intersperse (Snippet.sql ", ") vs
    , Snippet.sql ")"
    ]
