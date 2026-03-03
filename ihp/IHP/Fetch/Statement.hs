{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications, DataKinds, TypeFamilies, PolyKinds, ConstraintKinds, TypeOperators, BangPatterns, FlexibleContexts #-}
{-|
Module: IHP.Fetch.Statement
Description: Prepared statements for fetch operations
Copyright: (c) digitally induced GmbH, 2025

Internal module containing hasql 'Statement' definitions shared by
'IHP.Fetch' and 'IHP.FetchPipelined'.
-}
module IHP.Fetch.Statement
( fetchByIdOneOrNothingStatement
, fetchByIdListStatement
, buildQueryListStatement
, buildQueryMaybeStatement
, buildCountStatement
, buildExistsStatement
) where

import Prelude
import IHP.ModelSupport (Table(..), primaryKeyConditionColumnSelector, GetModelByTableName)
import IHP.ModelSupport.Types (Id'(..), GetTableName)
import IHP.Hasql.FromRow (FromRowHasql(..))
import qualified Hasql.Statement as Hasql
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Implicits.Encoders (DefaultParamEncoder(..))
import qualified Data.Text as Text
import IHP.QueryBuilder.Types (HasQueryBuilder(..), SQLQuery(..))
import IHP.QueryBuilder.Compiler (buildQuery)
import IHP.QueryBuilder.HasqlCompiler (buildStatement, buildWrappedStatement)
import GHC.TypeLits (KnownSymbol)
import Data.Int (Int64)

-- | Prepared statement for fetching a single record by primary key.
--
-- > SELECT table.col1, table.col2, ... FROM table WHERE table.id = $1 LIMIT 1
{-# INLINE fetchByIdOneOrNothingStatement #-}
fetchByIdOneOrNothingStatement :: forall table model. (Table model, GetTableName model ~ table, FromRowHasql model, DefaultParamEncoder (Id' table)) => Hasql.Statement (Id' table) (Maybe model)
fetchByIdOneOrNothingStatement = Hasql.preparable sql (Encoders.param defaultParam) (Decoders.rowMaybe (hasqlRowDecoder @model))
  where
    sql = "SELECT " <> qualifiedColumns <> " FROM " <> tn <> " WHERE " <> pkCol <> " = $1 LIMIT 1"
    tn = tableName @model
    pkCol = primaryKeyConditionColumnSelector @model
    qualifiedColumns = Text.intercalate ", " (map (\c -> tn <> "." <> c) (columnNames @model))

-- | Prepared statement for fetching records by primary key (returns list).
--
-- > SELECT table.col1, table.col2, ... FROM table WHERE table.id = $1
{-# INLINE fetchByIdListStatement #-}
fetchByIdListStatement :: forall table model. (Table model, GetTableName model ~ table, FromRowHasql model, DefaultParamEncoder (Id' table)) => Hasql.Statement (Id' table) [model]
fetchByIdListStatement = Hasql.preparable sql (Encoders.param defaultParam) (Decoders.rowList (hasqlRowDecoder @model))
  where
    sql = "SELECT " <> qualifiedColumns <> " FROM " <> tn <> " WHERE " <> pkCol <> " = $1"
    tn = tableName @model
    pkCol = primaryKeyConditionColumnSelector @model
    qualifiedColumns = Text.intercalate ", " (map (\c -> tn <> "." <> c) (columnNames @model))

-- | Build a statement that fetches all rows matching a query builder.
{-# INLINE buildQueryListStatement #-}
buildQueryListStatement :: forall model table queryBuilderProvider joinRegister.
    ( Table model, HasQueryBuilder queryBuilderProvider joinRegister
    , model ~ GetModelByTableName table, KnownSymbol table, FromRowHasql model
    ) => queryBuilderProvider table -> Hasql.Statement () [model]
buildQueryListStatement !queryBuilder =
    buildStatement (buildQuery queryBuilder) (Decoders.rowList (hasqlRowDecoder @model))

-- | Build a statement that fetches at most one row (adds LIMIT 1).
{-# INLINE buildQueryMaybeStatement #-}
buildQueryMaybeStatement :: forall model table queryBuilderProvider joinRegister.
    ( Table model, HasQueryBuilder queryBuilderProvider joinRegister
    , model ~ GetModelByTableName table, KnownSymbol table, FromRowHasql model
    ) => queryBuilderProvider table -> Hasql.Statement () (Maybe model)
buildQueryMaybeStatement !queryBuilder =
    buildStatement ((buildQuery queryBuilder) { limitClause = Just 1 }) (Decoders.rowMaybe (hasqlRowDecoder @model))

-- | Build a @SELECT COUNT(*)@ statement wrapping a query builder.
{-# INLINE buildCountStatement #-}
buildCountStatement :: forall table queryBuilderProvider joinRegister.
    ( KnownSymbol table, HasQueryBuilder queryBuilderProvider joinRegister
    ) => queryBuilderProvider table -> Hasql.Statement () Int64
buildCountStatement !queryBuilder =
    buildWrappedStatement "SELECT COUNT(*) FROM (" (buildQuery queryBuilder) ") AS _count_values" (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))

-- | Build a @SELECT EXISTS@ statement wrapping a query builder.
{-# INLINE buildExistsStatement #-}
buildExistsStatement :: forall table queryBuilderProvider joinRegister.
    ( KnownSymbol table, HasQueryBuilder queryBuilderProvider joinRegister
    ) => queryBuilderProvider table -> Hasql.Statement () Bool
buildExistsStatement !queryBuilder =
    buildWrappedStatement "SELECT EXISTS (" (buildQuery queryBuilder) ") AS _exists_values" (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
