{-|
Module: IHP.DataSync.DynamicQueryCompiler
Description: Compiles a DynamicQuery to SQL
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.DataSync.DynamicQueryCompiler where

import IHP.Prelude
import IHP.DataSync.DynamicQuery
import IHP.DataSync.TypedEncoder (typedValueParam)
import qualified IHP.QueryBuilder as QueryBuilder
import qualified Hasql.Encoders as Encoders
import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson as Aeson
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)

data Renamer = Renamer
    { fieldToColumn :: Text -> Text
    , columnToField :: Text -> Text
    }

-- | Compile a 'DynamicSQLQuery' to a 'Snippet' with typed parameter encoding.
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
    selectPart <> wherePart <> orderByPart <> limitPart <> offsetPart
    where
        selectPart = Snippet.sql ("SELECT"
            <> distinctOnText
            <> compileSelectedColumns renamer columnInfo selectedColumns
            <> " FROM "
            <> quoteIdentifier table)

        distinctOnText = case distinctOnColumn of
            Just column -> " DISTINCT ON (" <> quoteIdentifier (cs column) <> ") "
            Nothing     -> " "

        wherePart = case whereCondition of
            Just condition -> Snippet.sql " WHERE " <> compileConditionTyped columnInfo.typeMap condition
            Nothing -> mempty

        orderByPart = case orderByClause of
            [] -> mempty
            clauses -> Snippet.sql " ORDER BY " <> mconcat (List.intersperse (Snippet.sql ", ") (map compileOrderByClauseText clauses))

        limitPart = case limit of
            Just l -> Snippet.sql " LIMIT " <> Snippet.encoderAndParam (Encoders.nonNullable Encoders.int4) (fromIntegral l :: Int32)
            Nothing -> mempty

        offsetPart = case offset of
            Just o -> Snippet.sql " OFFSET " <> Snippet.encoderAndParam (Encoders.nonNullable Encoders.int4) (fromIntegral o :: Int32)
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

compileOrderByClauseText :: OrderByClause -> Snippet
compileOrderByClauseText OrderByClause { orderByColumn, orderByDirection } =
    Snippet.sql (quoteIdentifier (cs orderByColumn)
        <> if orderByDirection == QueryBuilder.Desc then " DESC" else "")
compileOrderByClauseText OrderByTSRank { tsvector, tsquery } =
    Snippet.sql ("ts_rank(" <> quoteIdentifier tsvector <> ", to_tsquery('english', ")
    <> Snippet.encoderAndParam (Encoders.nonNullable Encoders.text) tsquery
    <> Snippet.sql "))"

-- | Compile selected columns, generating SQL aliases when the camelCase field name
-- differs from the snake_case column name.
--
-- For example, if the column is @user_id@ but the client expects @userId@, this will
-- generate @"user_id" AS "userId"@ instead of just @"user_id"@.
--
-- For 'SelectAll', expands to all columns from 'ColumnTypeInfo' in the order they
-- were defined in the database schema (from @pg_attribute.attnum@).
compileSelectedColumns :: Renamer -> ColumnTypeInfo -> SelectedColumns -> Text
compileSelectedColumns renamer columnInfo SelectAll =
    compileSelectedColumns renamer columnInfo (SelectSpecific columnInfo.orderedColumns)
compileSelectedColumns renamer _columnInfo (SelectSpecific columns) =
    mconcat (List.intersperse ", " (map compileColumn columns))
    where
        compileColumn col =
            let alias = renamer.columnToField col
            in if alias == col
                then quoteIdentifier col
                else quoteIdentifier col <> " AS " <> quoteIdentifier alias

-- | Compile a SQL @RETURNING@ clause with aliased columns.
--
-- For INSERT/UPDATE/DELETE statements that return data, this generates
-- @RETURNING "col_a" AS "colA", "col_b" AS "colB", ...@ with proper camelCase aliases.
compileReturningClause :: Renamer -> ColumnTypeInfo -> Text
compileReturningClause renamer columnInfo =
    " RETURNING " <> compileSelectedColumns renamer columnInfo SelectAll

-- | Build an INSERT statement with RETURNING clause.
compileInsert :: Text -> [Text] -> [Snippet] -> Renamer -> ColumnTypeInfo -> Snippet
compileInsert table columns valueSnippets renamer columnTypes =
    Snippet.sql ("INSERT INTO " <> quoteIdentifier table
        <> " (" <> columnSql <> ") VALUES (")
    <> valueSql
    <> Snippet.sql (")" <> compileReturningClause renamer columnTypes)
  where
    columnSql = mconcat $ List.intersperse ", " (map quoteIdentifier columns)
    valueSql = mconcat $ List.intersperse (Snippet.sql ", ") valueSnippets

-- | Build an INSERT statement for multiple rows with RETURNING clause.
compileInsertMany :: Text -> [Text] -> [[Snippet]] -> Renamer -> ColumnTypeInfo -> Snippet
compileInsertMany table columns valueRows renamer columnTypes =
    Snippet.sql ("INSERT INTO " <> quoteIdentifier table
        <> " (" <> columnSql <> ") VALUES ")
    <> valuesSnippet
    <> Snippet.sql (compileReturningClause renamer columnTypes)
  where
    columnSql = mconcat $ List.intersperse ", " (map quoteIdentifier columns)
    valueRowSnippets = map (\row -> Snippet.sql "(" <> mconcat (List.intersperse (Snippet.sql ", ") row) <> Snippet.sql ")") valueRows
    valuesSnippet = mconcat $ List.intersperse (Snippet.sql ", ") valueRowSnippets

-- | Build an UPDATE statement with RETURNING clause.
compileUpdate :: Text -> Snippet -> Snippet -> Renamer -> ColumnTypeInfo -> Snippet
compileUpdate table setSql whereSql renamer columnTypes =
    Snippet.sql ("UPDATE " <> quoteIdentifier table <> " SET ")
    <> setSql
    <> Snippet.sql " WHERE "
    <> whereSql
    <> Snippet.sql (compileReturningClause renamer columnTypes)

-- | Compile a condition expression to a 'Snippet' with typed parameter encoding.
compileConditionTyped :: ColumnTypeMap -> ConditionExpression -> Snippet
compileConditionTyped _ (ColumnExpression column) = Snippet.sql (quoteIdentifier column)
compileConditionTyped types (InfixOperatorExpression a OpEqual (LiteralExpression Aeson.Null)) = compileConditionTyped types (InfixOperatorExpression a OpIs (LiteralExpression Aeson.Null))
compileConditionTyped types (InfixOperatorExpression a OpNotEqual (LiteralExpression Aeson.Null)) = compileConditionTyped types (InfixOperatorExpression a OpIsNot (LiteralExpression Aeson.Null))
compileConditionTyped _types (InfixOperatorExpression _a OpIn (ListExpression { values = [] })) = Snippet.sql "FALSE"
compileConditionTyped types (InfixOperatorExpression a OpIn (ListExpression { values })) | (Aeson.Null `List.elem` values) =
    let condition =
            case partition ((/=) Aeson.Null) values of
                ([], _nullValues) -> InfixOperatorExpression a OpIs (LiteralExpression Aeson.Null)
                (nonNullValues, _nullValues) -> InfixOperatorExpression (InfixOperatorExpression a OpIn (ListExpression { values = nonNullValues })) OpOr (InfixOperatorExpression a OpIs (LiteralExpression Aeson.Null))
    in
        compileConditionTyped types condition
-- When comparing a column to a literal or list, look up the column's type for typed encoding.
compileConditionTyped types (InfixOperatorExpression (ColumnExpression col) operator (LiteralExpression literal)) =
    let opText = compileOperator operator
        rightSnippet = case literal of
            Aeson.Null -> Snippet.sql "NULL"
            _ -> let colType = HashMap.lookup col types
                 in Snippet.sql "(" <> typedValueParam colType literal <> Snippet.sql ")"
    in Snippet.sql ("(" <> quoteIdentifier col <> ") " <> opText <> " ") <> rightSnippet
compileConditionTyped types (InfixOperatorExpression (ColumnExpression col) operator (ListExpression { values })) =
    let opText = compileOperator operator
        colType = HashMap.lookup col types
        valSnippets = map (typedValueParam colType) values
    in Snippet.sql ("(" <> quoteIdentifier col <> ") " <> opText <> " (") <> mconcat (List.intersperse (Snippet.sql ", ") valSnippets) <> Snippet.sql ")"
compileConditionTyped types (InfixOperatorExpression a operator b) =
    let aSnippet = compileConditionTyped types a
        opText = compileOperator operator
        bSnippet = compileConditionTyped types b
        rightSnippet = if rightParentheses
                then Snippet.sql "(" <> bSnippet <> Snippet.sql ")"
                else bSnippet

        rightParentheses :: Bool
        rightParentheses =
            case b of
                LiteralExpression Aeson.Null -> False
                ListExpression {} -> False
                _ -> True
    in Snippet.sql "(" <> aSnippet <> Snippet.sql (") " <> opText <> " ") <> rightSnippet
compileConditionTyped _types (LiteralExpression literal) = dynamicValueParam literal
compileConditionTyped _ (CallExpression { functionCall = ToTSQuery { text } }) =
    Snippet.sql "to_tsquery('english', " <> Snippet.encoderAndParam (Encoders.nonNullable Encoders.text) text <> Snippet.sql ")"
compileConditionTyped _types (ListExpression { values }) =
    Snippet.sql "(" <> mconcat (List.intersperse (Snippet.sql ", ") (map dynamicValueParam values)) <> Snippet.sql ")"

compileOperator :: ConditionOperator -> Text
compileOperator OpEqual = "="
compileOperator OpGreaterThan = ">"
compileOperator OpLessThan = "<"
compileOperator OpGreaterThanOrEqual = ">="
compileOperator OpLessThanOrEqual = "<="
compileOperator OpNotEqual = "<>"
compileOperator OpAnd = "AND"
compileOperator OpOr = "OR"
compileOperator OpIs = "IS"
compileOperator OpIsNot = "IS NOT"
compileOperator OpTSMatch = "@@"
compileOperator OpIn = "IN"
