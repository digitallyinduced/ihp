{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications, DataKinds, TypeFamilies, PolyKinds, ConstraintKinds, TypeOperators, BangPatterns, FlexibleContexts #-}
{-|
Module: IHP.Fetch.Statement
Description: Prepared statements for fetch operations
Copyright: (c) digitally induced GmbH, 2025

Internal module containing hasql 'Statement' definitions shared by
'IHP.Fetch' and 'IHP.FetchPipelined'.
-}
module IHP.Fetch.Statement
( buildQueryListStatement
, buildQueryVectorStatement
, buildQueryMaybeStatement
, buildTrackedQueryListStatement
, buildTrackedQueryVectorStatement
, buildTrackedQueryMaybeStatement
, buildCountStatement
, buildExistsStatement
) where

import Prelude
import IHP.ModelSupport (Table(..), GetModelByTableName)
import IHP.Hasql.FromRow (FromRowHasql(..))
import qualified Hasql.Statement as Hasql
import qualified Hasql.Decoders as Decoders
import IHP.QueryBuilder.Types (QueryBuilder(..), QueryBuilderPrimaryKey (..), SQLQuery(..))
import IHP.QueryBuilder.Compiler (buildQuery)
import IHP.QueryBuilder.HasqlCompiler (buildStatement, buildWrappedStatement)
import GHC.TypeLits (KnownSymbol)
import Data.Int (Int64)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson

-- | Build a statement that fetches all rows matching a query builder.
buildQueryListStatement :: forall model table.
    ( Table model
    , model ~ GetModelByTableName table, KnownSymbol table, FromRowHasql model
    ) => QueryBuilder table -> Hasql.Statement () [model]
buildQueryListStatement !queryBuilder =
    buildStatement (buildQuery queryBuilder) (Decoders.rowList (hasqlRowDecoder @model))

-- | Like 'buildQueryListStatement', but returns a 'Vector' instead of a list.
buildQueryVectorStatement :: forall model table.
    ( Table model
    , model ~ GetModelByTableName table, KnownSymbol table, FromRowHasql model
    ) => QueryBuilder table -> Hasql.Statement () (Vector model)
buildQueryVectorStatement !queryBuilder =
    buildStatement (buildQuery queryBuilder) (Decoders.rowVector (hasqlRowDecoder @model))

-- | Build a statement that fetches at most one row (adds LIMIT 1).
buildQueryMaybeStatement :: forall model table.
    ( Table model
    , model ~ GetModelByTableName table, KnownSymbol table, FromRowHasql model
    ) => QueryBuilder table -> Hasql.Statement () (Maybe model)
buildQueryMaybeStatement !queryBuilder =
    buildStatement ((buildQuery queryBuilder) { limitClause = Just 1 }) (Decoders.rowMaybe (hasqlRowDecoder @model))

-- | Build a row query that also returns the canonical primary key as JSON.
-- The original query is wrapped in a CTE, preserving its filtering, ordering,
-- distinctness, limit and offset semantics.
buildTrackedQueryListStatement :: forall model table.
    ( Table model
    , model ~ GetModelByTableName table, KnownSymbol table, FromRowHasql model
    ) => QueryBuilder table -> Hasql.Statement () [(model, QueryBuilderPrimaryKey)]
buildTrackedQueryListStatement !queryBuilder =
    buildTrackedStatement @model (buildQuery queryBuilder) (Decoders.rowList trackedRowDecoder)

-- | Vector variant of 'buildTrackedQueryListStatement'.
buildTrackedQueryVectorStatement :: forall model table.
    ( Table model
    , model ~ GetModelByTableName table, KnownSymbol table, FromRowHasql model
    ) => QueryBuilder table -> Hasql.Statement () (Vector (model, QueryBuilderPrimaryKey))
buildTrackedQueryVectorStatement !queryBuilder =
    buildTrackedStatement @model (buildQuery queryBuilder) (Decoders.rowVector trackedRowDecoder)

-- | Maybe variant of 'buildTrackedQueryListStatement', adding @LIMIT 1@.
buildTrackedQueryMaybeStatement :: forall model table.
    ( Table model
    , model ~ GetModelByTableName table, KnownSymbol table, FromRowHasql model
    ) => QueryBuilder table -> Hasql.Statement () (Maybe (model, QueryBuilderPrimaryKey))
buildTrackedQueryMaybeStatement !queryBuilder =
    buildTrackedStatement @model ((buildQuery queryBuilder) { limitClause = Just 1 }) (Decoders.rowMaybe trackedRowDecoder)

buildTrackedStatement :: forall model result.
    (Table model, FromRowHasql model) => SQLQuery -> Decoders.Result result -> Hasql.Statement () result
buildTrackedStatement sqlQuery decoder =
    buildWrappedStatement
        "WITH _ihp_tracked_query AS ("
        sqlQuery
        (") SELECT _ihp_tracked_query.*, jsonb_build_array("
            <> Text.intercalate ", " (map qualifyPrimaryKeyColumn (primaryKeyColumnNames @model))
            <> ") AS _ihp_primary_key FROM _ihp_tracked_query")
        decoder

trackedRowDecoder :: forall model. (FromRowHasql model) => Decoders.Row (model, QueryBuilderPrimaryKey)
trackedRowDecoder = (,)
    <$> hasqlRowDecoder @model
    <*> (aesonPrimaryKey <$> Decoders.column (Decoders.nonNullable Decoders.jsonb))
    where
        aesonPrimaryKey (Aeson.Array values) = QueryBuilderPrimaryKey (Vector.toList values)
        aesonPrimaryKey value = QueryBuilderPrimaryKey [value]

qualifyPrimaryKeyColumn :: Text -> Text
qualifyPrimaryKeyColumn columnName =
    "_ihp_tracked_query.\"" <> Text.replace "\"" "\"\"" columnName <> "\""

-- | Build a @SELECT COUNT(*)@ statement wrapping a query builder.
buildCountStatement :: forall table.
    ( KnownSymbol table
    ) => QueryBuilder table -> Hasql.Statement () Int64
buildCountStatement !queryBuilder =
    buildWrappedStatement "SELECT COUNT(*) FROM (" (buildQuery queryBuilder) ") AS _count_values" (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))

-- | Build a @SELECT EXISTS@ statement wrapping a query builder.
buildExistsStatement :: forall table.
    ( KnownSymbol table
    ) => QueryBuilder table -> Hasql.Statement () Bool
buildExistsStatement !queryBuilder =
    buildWrappedStatement "SELECT EXISTS (" (buildQuery queryBuilder) ") AS _exists_values" (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
