{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications, DataKinds, TypeFamilies #-}
{-|
Module: IHP.Fetch.Statement
Description: Prepared statements for fetch-by-ID operations
Copyright: (c) digitally induced GmbH, 2025

Internal module containing hasql 'Statement' definitions for primary-key
lookups. These bypass the QueryBuilder\/Snippet pipeline entirely.
-}
module IHP.Fetch.Statement
( fetchByIdOneOrNothingStatement
, fetchByIdListStatement
) where

import Prelude
import IHP.ModelSupport (Table(..), primaryKeyConditionColumnSelector)
import IHP.ModelSupport.Types (Id', GetTableName)
import IHP.Hasql.FromRow (FromRowHasql(..))
import qualified Hasql.Statement as Hasql
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Implicits.Encoders (DefaultParamEncoder(..))
import qualified Data.Text as Text

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
