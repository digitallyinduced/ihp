{-|
Module: IHP.DataSync.DynamicQueryCompiler
Description: Compiles a DynamicQuery to SQL
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.DataSync.DynamicQueryCompiler where

import IHP.Prelude
import IHP.DataSync.DynamicQuery
import IHP.DataSync.TypedEncoder (typedValueParam)
import IHP.QueryBuilder.HasqlCompiler (CompilerState(..), emptyCompilerState, nextParam)
import qualified IHP.QueryBuilder as QueryBuilder
import qualified Hasql.Encoders as Encoders
import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson as Aeson
import Data.Functor.Contravariant (contramap)

data Renamer = Renamer
    { fieldToColumn :: Text -> Text
    , columnToField :: Text -> Text
    }

-- | Compile a 'DynamicSQLQuery' to a 'CompiledQuery' with typed parameter encoding.
--
-- Column types must be provided via 'ColumnTypeInfo' (from 'makeCachedColumnTypeLookup').
-- Missing column types in WHERE conditions will error at runtime.
--
-- This function:
-- 1. Converts field names from camelCase to snake_case for the query
-- 2. Generates SQL column aliases so results come back with camelCase names
-- 3. For 'SelectAll', expands to all columns from 'ColumnTypeInfo' in database schema order
compileQueryTyped :: Renamer -> ColumnTypeInfo -> DynamicSQLQuery -> CompiledQuery
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

compileQueryMappedTyped :: Renamer -> ColumnTypeInfo -> DynamicSQLQuery -> CompiledQuery
compileQueryMappedTyped renamer columnInfo DynamicSQLQuery { .. } =
    CompiledQuery result ccFinal
    where
        selectPart = "SELECT"
            <> distinctOnText
            <> compileSelectedColumns renamer columnInfo selectedColumns
            <> " FROM "
            <> quoteIdentifier table

        distinctOnText = case distinctOnColumn of
            Just column -> " DISTINCT ON (" <> quoteIdentifier (cs column) <> ") "
            Nothing     -> " "

        (whereText, cc1) = case whereCondition of
            Just condition -> let (t, cc) = compileConditionTyped columnInfo.typeMap condition emptyCompilerState
                             in (" WHERE " <> t, cc)
            Nothing -> ("", emptyCompilerState)

        (orderByText, cc2) = case orderByClause of
            [] -> ("", cc1)
            clauses ->
                let (cc', texts) = List.mapAccumL (\st c -> let (t, st') = compileOrderByClauseText st c in (st', t)) cc1 clauses
                in (" ORDER BY " <> mconcat (List.intersperse ", " texts), cc')

        (limitText, cc3) = case limit of
            Just l ->
                let enc = contramap (const (fromIntegral l :: Int32)) (Encoders.param (Encoders.nonNullable Encoders.int4))
                    (ph, cc) = nextParam enc cc2
                in (" LIMIT " <> ph, cc)
            Nothing -> ("", cc2)

        (offsetText, cc4) = case offset of
            Just o ->
                let enc = contramap (const (fromIntegral o :: Int32)) (Encoders.param (Encoders.nonNullable Encoders.int4))
                    (ph, cc) = nextParam enc cc3
                in (" OFFSET " <> ph, cc)
            Nothing -> ("", cc3)

        result = selectPart <> whereText <> orderByText <> limitText <> offsetText
        ccFinal = cc4

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

compileOrderByClauseText :: CompilerState -> OrderByClause -> (Text, CompilerState)
compileOrderByClauseText cc OrderByClause { orderByColumn, orderByDirection } =
    ( quoteIdentifier (cs orderByColumn)
        <> if orderByDirection == QueryBuilder.Desc then " DESC" else ""
    , cc
    )
compileOrderByClauseText cc OrderByTSRank { tsvector, tsquery } =
    let enc = contramap (const tsquery) (Encoders.param (Encoders.nonNullable Encoders.text))
        (ph, cc') = nextParam enc cc
    in ("ts_rank(" <> quoteIdentifier tsvector <> ", to_tsquery('english', " <> ph <> "))", cc')

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
compileInsert :: Text -> [Text] -> [Text] -> CompilerState -> Renamer -> ColumnTypeInfo -> CompiledQuery
compileInsert table columns valueTexts cc renamer columnTypes =
    CompiledQuery sql cc
  where
    sql = "INSERT INTO " <> quoteIdentifier table
        <> " (" <> columnSql <> ") VALUES ("
        <> valueSql <> ")"
        <> compileReturningClause renamer columnTypes
    columnSql = mconcat $ List.intersperse ", " (map quoteIdentifier columns)
    valueSql = mconcat $ List.intersperse ", " valueTexts

-- | Build an INSERT statement for multiple rows with RETURNING clause.
compileInsertMany :: Text -> [Text] -> [[Text]] -> CompilerState -> Renamer -> ColumnTypeInfo -> CompiledQuery
compileInsertMany table columns valueRows cc renamer columnTypes =
    CompiledQuery sql cc
  where
    sql = "INSERT INTO " <> quoteIdentifier table
        <> " (" <> columnSql <> ") VALUES "
        <> valuesText
        <> compileReturningClause renamer columnTypes
    columnSql = mconcat $ List.intersperse ", " (map quoteIdentifier columns)
    valueRowTexts = map (\row -> "(" <> mconcat (List.intersperse ", " row) <> ")") valueRows
    valuesText = mconcat $ List.intersperse ", " valueRowTexts

-- | Build an UPDATE statement with RETURNING clause.
compileUpdate :: Text -> Text -> Text -> CompilerState -> Renamer -> ColumnTypeInfo -> CompiledQuery
compileUpdate table setSql whereSql cc renamer columnTypes =
    CompiledQuery sql cc
  where
    sql = "UPDATE " <> quoteIdentifier table
        <> " SET " <> setSql
        <> " WHERE " <> whereSql
        <> compileReturningClause renamer columnTypes

-- | Compile a condition expression to SQL text, threading 'CompilerState' for typed parameter encoding.
compileConditionTyped :: ColumnTypeMap -> ConditionExpression -> CompilerState -> (Text, CompilerState)
compileConditionTyped _ (ColumnExpression column) cc = (quoteIdentifier column, cc)
compileConditionTyped types (InfixOperatorExpression a OpEqual (LiteralExpression Aeson.Null)) cc = compileConditionTyped types (InfixOperatorExpression a OpIs (LiteralExpression Aeson.Null)) cc
compileConditionTyped types (InfixOperatorExpression a OpNotEqual (LiteralExpression Aeson.Null)) cc = compileConditionTyped types (InfixOperatorExpression a OpIsNot (LiteralExpression Aeson.Null)) cc
compileConditionTyped _types (InfixOperatorExpression _a OpIn (ListExpression { values = [] })) cc = ("FALSE", cc)
compileConditionTyped types (InfixOperatorExpression a OpIn (ListExpression { values })) cc | (Aeson.Null `List.elem` values) =
    let condition =
            case partition ((/=) Aeson.Null) values of
                ([], _nullValues) -> InfixOperatorExpression a OpIs (LiteralExpression Aeson.Null)
                (nonNullValues, _nullValues) -> InfixOperatorExpression (InfixOperatorExpression a OpIn (ListExpression { values = nonNullValues })) OpOr (InfixOperatorExpression a OpIs (LiteralExpression Aeson.Null))
    in
        compileConditionTyped types condition cc
-- When comparing a column to a literal or list, look up the column's type for typed encoding.
compileConditionTyped types (InfixOperatorExpression (ColumnExpression col) operator (LiteralExpression literal)) cc =
    let opText = compileOperator operator
        (rightText, cc') = case literal of
            Aeson.Null -> ("NULL", cc)
            _ -> let colType = HashMap.lookup col types
                     (valText, cc'') = typedValueParam colType literal cc
                 in ("(" <> valText <> ")", cc'')
    in ("(" <> quoteIdentifier col <> ") " <> opText <> " " <> rightText, cc')
compileConditionTyped types (InfixOperatorExpression (ColumnExpression col) operator (ListExpression { values })) cc =
    let opText = compileOperator operator
        colType = HashMap.lookup col types
        (cc', valTexts) = List.mapAccumL (\st v -> let (t, st') = typedValueParam colType v st in (st', t)) cc values
    in ("(" <> quoteIdentifier col <> ") " <> opText <> " (" <> mconcat (List.intersperse ", " valTexts) <> ")", cc')
compileConditionTyped types (InfixOperatorExpression a operator b) cc =
    let (aText, cc1) = compileConditionTyped types a cc
        opText = compileOperator operator
        (bText, cc2) = compileConditionTyped types b cc1
        rightText = if rightParentheses
                then "(" <> bText <> ")"
                else bText

        rightParentheses :: Bool
        rightParentheses =
            case b of
                LiteralExpression Aeson.Null -> False
                ListExpression {} -> False
                _ -> True
    in ("(" <> aText <> ") " <> opText <> " " <> rightText, cc2)
compileConditionTyped _types (LiteralExpression literal) cc = dynamicValueParam literal cc
compileConditionTyped _ (CallExpression { functionCall = ToTSQuery { text } }) cc =
    let enc = contramap (const text) (Encoders.param (Encoders.nonNullable Encoders.text))
        (ph, cc') = nextParam enc cc
    in ("to_tsquery('english', " <> ph <> ")", cc')
compileConditionTyped _types (ListExpression { values }) cc =
    let (cc', valTexts) = List.mapAccumL (\st v -> let (t, st') = dynamicValueParam v st in (st', t)) cc values
    in ("(" <> mconcat (List.intersperse ", " valTexts) <> ")", cc')

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
