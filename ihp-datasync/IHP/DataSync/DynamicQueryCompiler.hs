{-|
Module: IHP.DataSync.DynamicQueryCompiler
Description: Compiles a DynamicQuery to SQL
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.DataSync.DynamicQueryCompiler where

import IHP.Prelude
import IHP.DataSync.DynamicQuery
import IHP.DataSync.TypedEncoder (ColumnTypeInfo(..), typedValueParam)
import qualified IHP.QueryBuilder as QueryBuilder
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import Data.Int (Int32)
import qualified Data.Aeson as Aeson

data Renamer = Renamer
    { fieldToColumn :: Text -> Text
    , columnToField :: Text -> Text
    }

-- | Compile a 'DynamicSQLQuery' to a SQL 'Snippet' with typed parameter encoding.
--
-- Column types must be provided via 'ColumnTypeInfo' (from 'makeCachedColumnTypeLookup').
-- Missing column types in WHERE conditions will error at runtime.
--
-- This function:
-- 1. Converts field names from camelCase to snake_case for the query
-- 2. Generates SQL column aliases so results come back with camelCase names
-- 3. For 'SelectAll', expands to all columns from 'ColumnTypeInfo' in database schema order
compileQueryTyped :: Renamer -> ColumnTypeInfo -> DynamicSQLQuery -> Snippet
compileQueryTyped renamer columnInfo query =
    compileQueryMappedTyped renamer columnInfo (mapColumnNames renamer.fieldToColumn query)

-- | Default renamer used by DataSync.
--
-- Transforms JS inputs in @camelCase@ to snake_case for the database
-- and DB outputs in @snake_case@ back to @camelCase@
camelCaseRenamer :: Renamer
camelCaseRenamer =
    Renamer
    { fieldToColumn = fieldNameToColumnName
    , columnToField = columnNameToFieldName
    }

-- | When a Field is retrieved from the database, it's all in @snake_case@. This turns it into @camelCase@
renameField :: Renamer -> Field -> Field
renameField renamer field =
    field { fieldName = renamer.columnToField field.fieldName }

compileQueryMappedTyped :: Renamer -> ColumnTypeInfo -> DynamicSQLQuery -> Snippet
compileQueryMappedTyped renamer columnInfo DynamicSQLQuery { .. } =
    Snippet.sql "SELECT"
    <> distinctOnSnippet
    <> compileSelectedColumns renamer columnInfo selectedColumns
    <> Snippet.sql " FROM "
    <> quoteIdentifier table
    <> whereSnippet
    <> orderBySnippet
    <> limitSnippet
    <> offsetSnippet
    where
        distinctOnSnippet = case distinctOnColumn of
            Just column -> Snippet.sql " DISTINCT ON (" <> quoteIdentifier (cs column) <> Snippet.sql ") "
            Nothing     -> Snippet.sql " "

        orderBySnippet = case orderByClause of
                [] -> mempty
                orderByClauses ->
                    Snippet.sql " ORDER BY "
                    <> mconcat (List.intersperse (Snippet.sql ", ") (map compileOrderByClauseSnippet orderByClauses))

        whereSnippet = case whereCondition of
            Just condition -> Snippet.sql " WHERE " <> compileConditionTyped columnInfo.typeMap condition
            Nothing -> mempty

        limitSnippet = case limit of
                Just l  -> Snippet.sql " LIMIT " <> Snippet.param (fromIntegral l :: Int32)
                Nothing -> mempty

        offsetSnippet = case offset of
                Just o  -> Snippet.sql " OFFSET " <> Snippet.param (fromIntegral o :: Int32)
                Nothing -> mempty

-- | Used to transform column names from @camelCase@ to @snake_case@
mapColumnNames :: (Text -> Text) -> DynamicSQLQuery -> DynamicSQLQuery
mapColumnNames rename query =
    query
    { selectedColumns = mapSelectedColumns query.selectedColumns
    , whereCondition = mapConditionExpression <$> query.whereCondition
    , orderByClause = map mapOrderByClause query.orderByClause
    , distinctOnColumn = (cs . rename . cs) <$> query.distinctOnColumn
    }
    where
        mapSelectedColumns :: SelectedColumns -> SelectedColumns
        mapSelectedColumns SelectAll = SelectAll
        mapSelectedColumns (SelectSpecific columns) = SelectSpecific (map rename columns)

        mapConditionExpression :: ConditionExpression -> ConditionExpression
        mapConditionExpression ColumnExpression { field } = ColumnExpression { field = rename field }
        mapConditionExpression InfixOperatorExpression { left, op, right } = InfixOperatorExpression { left = mapConditionExpression left, op, right = mapConditionExpression right }
        mapConditionExpression otherwise = otherwise

        mapOrderByClause :: OrderByClause -> OrderByClause
        mapOrderByClause OrderByClause { orderByColumn, orderByDirection } = OrderByClause { orderByColumn = cs (rename (cs orderByColumn)), orderByDirection }
        mapOrderByClause otherwise = otherwise

compileOrderByClauseSnippet :: OrderByClause -> Snippet
compileOrderByClauseSnippet OrderByClause { orderByColumn, orderByDirection } =
    quoteIdentifier (cs orderByColumn)
    <> if orderByDirection == QueryBuilder.Desc
        then Snippet.sql " DESC"
        else mempty
compileOrderByClauseSnippet OrderByTSRank { tsvector, tsquery } =
    Snippet.sql "ts_rank(" <> quoteIdentifier tsvector <> Snippet.sql ", to_tsquery('english', " <> Snippet.param tsquery <> Snippet.sql "))"

-- | Compile selected columns, generating SQL aliases when the camelCase field name
-- differs from the snake_case column name.
--
-- For example, if the column is @user_id@ but the client expects @userId@, this will
-- generate @"user_id" AS "userId"@ instead of just @"user_id"@.
--
-- For 'SelectAll', expands to all columns from 'ColumnTypeInfo' in the order they
-- were defined in the database schema (from @pg_attribute.attnum@).
compileSelectedColumns :: Renamer -> ColumnTypeInfo -> SelectedColumns -> Snippet
compileSelectedColumns renamer columnInfo SelectAll =
    compileSelectedColumns renamer columnInfo (SelectSpecific columnInfo.orderedColumns)
compileSelectedColumns renamer _columnInfo (SelectSpecific columns) =
    mconcat (List.intersperse (Snippet.sql ", ") (map compileColumn columns))
    where
        compileColumn col =
            let alias = renamer.columnToField col
            in if alias == col
                then quoteIdentifier col
                else quoteIdentifier col <> Snippet.sql " AS " <> quoteIdentifier alias

-- | Compile a SQL @RETURNING@ clause with aliased columns.
--
-- For INSERT/UPDATE/DELETE statements that return data, this generates
-- @RETURNING "col_a" AS "colA", "col_b" AS "colB", ...@ with proper camelCase aliases.
compileReturningClause :: Renamer -> ColumnTypeInfo -> Snippet
compileReturningClause renamer columnInfo =
    Snippet.sql " RETURNING " <> compileSelectedColumns renamer columnInfo SelectAll

-- | Build an INSERT statement with RETURNING clause.
compileInsert :: Text -> [Text] -> [Snippet] -> Renamer -> ColumnTypeInfo -> Snippet
compileInsert table columns values renamer columnTypes =
    Snippet.sql "INSERT INTO " <> quoteIdentifier table
    <> Snippet.sql " (" <> columnSnippet <> Snippet.sql ") VALUES ("
    <> valueSnippet <> Snippet.sql ")"
    <> compileReturningClause renamer columnTypes
  where
    columnSnippet = mconcat $ List.intersperse (Snippet.sql ", ") (map quoteIdentifier columns)
    valueSnippet = mconcat $ List.intersperse (Snippet.sql ", ") values

-- | Build an INSERT statement for multiple rows with RETURNING clause.
compileInsertMany :: Text -> [Text] -> [[Snippet]] -> Renamer -> ColumnTypeInfo -> Snippet
compileInsertMany table columns valueRows renamer columnTypes =
    Snippet.sql "INSERT INTO " <> quoteIdentifier table
    <> Snippet.sql " (" <> columnSnippet <> Snippet.sql ") VALUES "
    <> valuesSnippet
    <> compileReturningClause renamer columnTypes
  where
    columnSnippet = mconcat $ List.intersperse (Snippet.sql ", ") (map quoteIdentifier columns)
    valueRowSnippets = map (\row -> Snippet.sql "(" <> mconcat (List.intersperse (Snippet.sql ", ") row) <> Snippet.sql ")") valueRows
    valuesSnippet = mconcat $ List.intersperse (Snippet.sql ", ") valueRowSnippets

-- | Build an UPDATE statement with RETURNING clause.
compileUpdate :: Text -> Snippet -> Snippet -> Renamer -> ColumnTypeInfo -> Snippet
compileUpdate table setSnippet whereSnippet renamer columnTypes =
    Snippet.sql "UPDATE " <> quoteIdentifier table
    <> Snippet.sql " SET " <> setSnippet
    <> Snippet.sql " WHERE " <> whereSnippet
    <> compileReturningClause renamer columnTypes

-- | Compile a condition expression to a SQL snippet, using typed parameter encoding when column types are known.
--
-- When a condition compares a column to a literal value, the column's type is looked up
-- in the 'ColumnTypeMap' and the value is encoded with the matching typed encoder.
-- This avoids type mismatches like @operator does not exist: uuid = text@ in the
-- extended query protocol.
compileConditionTyped :: ColumnTypeMap -> ConditionExpression -> Snippet
compileConditionTyped _ (ColumnExpression column) = quoteIdentifier column
compileConditionTyped types (InfixOperatorExpression a OpEqual (LiteralExpression Aeson.Null)) = compileConditionTyped types (InfixOperatorExpression a OpIs (LiteralExpression Aeson.Null))
compileConditionTyped types (InfixOperatorExpression a OpNotEqual (LiteralExpression Aeson.Null)) = compileConditionTyped types (InfixOperatorExpression a OpIsNot (LiteralExpression Aeson.Null))
compileConditionTyped _types (InfixOperatorExpression _a OpIn (ListExpression { values = [] })) = Snippet.sql "FALSE"
compileConditionTyped types (InfixOperatorExpression a OpIn (ListExpression { values })) | (Aeson.Null `List.elem` values) =
    case partition ((/=) Aeson.Null) values of
        ([], _nullValues) -> compileConditionTyped types (InfixOperatorExpression a OpIs (LiteralExpression Aeson.Null))
        (nonNullValues, _nullValues) -> compileConditionTyped types (InfixOperatorExpression (InfixOperatorExpression a OpIn (ListExpression { values = nonNullValues })) OpOr (InfixOperatorExpression a OpIs (LiteralExpression Aeson.Null)))
-- When comparing a column to a literal or list, look up the column's type for typed encoding.
-- Errors if the column type is not in the map — callers must provide complete type info.
compileConditionTyped types (InfixOperatorExpression (ColumnExpression col) operator (LiteralExpression literal)) =
    Snippet.sql "(" <> quoteIdentifier col <> Snippet.sql ") " <> compileOperator operator <> Snippet.sql " " <> rightOperand
    where
        colType = HashMap.lookup col types
        rightOperand = case literal of
            Aeson.Null -> Snippet.sql "NULL"
            _ -> Snippet.sql "(" <> typedValueParam colType literal <> Snippet.sql ")"
compileConditionTyped types (InfixOperatorExpression (ColumnExpression col) operator (ListExpression { values })) =
    Snippet.sql "(" <> quoteIdentifier col <> Snippet.sql ") " <> compileOperator operator <> Snippet.sql " "
    <> Snippet.sql "(" <> mconcat (List.intersperse (Snippet.sql ", ") (map (typedValueParam colType) values)) <> Snippet.sql ")"
    where
        colType = HashMap.lookup col types
compileConditionTyped types (InfixOperatorExpression a operator b) =
    Snippet.sql "(" <> compileConditionTyped types a <> Snippet.sql ") " <> compileOperator operator <> Snippet.sql " " <> rightOperand
    where
        rightOperand = if rightParentheses
                then Snippet.sql "(" <> compileConditionTyped types b <> Snippet.sql ")"
                else compileConditionTyped types b

        rightParentheses :: Bool
        rightParentheses =
            case b of
                LiteralExpression Aeson.Null -> False
                ListExpression {} -> False
                _ -> True
compileConditionTyped _types (LiteralExpression literal) = dynamicValueParam literal
compileConditionTyped _ (CallExpression { functionCall = ToTSQuery { text } }) = Snippet.sql "to_tsquery('english', " <> Snippet.param text <> Snippet.sql ")"
compileConditionTyped _types (ListExpression { values }) =
    Snippet.sql "(" <> mconcat (List.intersperse (Snippet.sql ", ") (map dynamicValueParam values)) <> Snippet.sql ")"

compileOperator :: ConditionOperator -> Snippet
compileOperator OpEqual = Snippet.sql "="
compileOperator OpGreaterThan = Snippet.sql ">"
compileOperator OpLessThan = Snippet.sql "<"
compileOperator OpGreaterThanOrEqual = Snippet.sql ">="
compileOperator OpLessThanOrEqual = Snippet.sql "<="
compileOperator OpNotEqual = Snippet.sql "<>"
compileOperator OpAnd = Snippet.sql "AND"
compileOperator OpOr = Snippet.sql "OR"
compileOperator OpIs = Snippet.sql "IS"
compileOperator OpIsNot = Snippet.sql "IS NOT"
compileOperator OpTSMatch = Snippet.sql "@@"
compileOperator OpIn = Snippet.sql "IN"
