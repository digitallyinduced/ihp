{-|
Module: IHP.DataSync.DynamicQueryCompiler
Description: Compiles a DynamicQuery to SQL
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.DataSync.DynamicQueryCompiler where

import IHP.Prelude
import IHP.DataSync.DynamicQuery
import qualified IHP.QueryBuilder as QueryBuilder
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Int (Int32)
import IHP.Postgres.Point (Point(..))

data Renamer = Renamer
    { fieldToColumn :: Text -> Text
    , columnToField :: Text -> Text
    }

compileQuery :: DynamicSQLQuery -> Snippet
compileQuery = compileQueryWithRenamer camelCaseRenamer

compileQueryWithRenamer :: Renamer -> DynamicSQLQuery -> Snippet
compileQueryWithRenamer renamer query = compileQueryMapped (mapColumnNames renamer.fieldToColumn query)

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

-- | Renamer that does not modify the column names
unmodifiedRenamer :: Renamer
unmodifiedRenamer =
    Renamer
    { fieldToColumn = id
    , columnToField = id
    }

-- | When a Field is retrieved from the database, it's all in @snake_case@. This turns it into @camelCase@
renameField :: Renamer -> Field -> Field
renameField renamer field =
    field { fieldName = renamer.columnToField field.fieldName }

compileQueryMapped :: DynamicSQLQuery -> Snippet
compileQueryMapped DynamicSQLQuery { .. } =
    Snippet.sql "SELECT"
    <> distinctOnSnippet
    <> compileSelectedColumns selectedColumns
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
            Just condition -> Snippet.sql " WHERE " <> compileCondition condition
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

compileSelectedColumns :: SelectedColumns -> Snippet
compileSelectedColumns SelectAll = Snippet.sql "*"
compileSelectedColumns (SelectSpecific fields) =
    mconcat (List.intersperse (Snippet.sql ", ") (map quoteIdentifier fields))

-- TODO: validate query against schema

compileCondition :: ConditionExpression -> Snippet
compileCondition (ColumnExpression column) = quoteIdentifier column
compileCondition (InfixOperatorExpression a OpEqual (LiteralExpression Null)) = compileCondition (InfixOperatorExpression a OpIs (LiteralExpression Null)) -- Turn 'a = NULL' into 'a IS NULL'
compileCondition (InfixOperatorExpression a OpNotEqual (LiteralExpression Null)) = compileCondition (InfixOperatorExpression a OpIsNot (LiteralExpression Null)) -- Turn 'a <> NULL' into 'a IS NOT NULL'
compileCondition (InfixOperatorExpression a OpIn (ListExpression { values })) | (Null `List.elem` values) =
    -- Turn 'a IN (NULL)' into 'a IS NULL'
    case partition ((/=) Null) values of
        ([], nullValues) -> compileCondition (InfixOperatorExpression a OpIs (LiteralExpression Null))
        (nonNullValues, nullValues) -> compileCondition (InfixOperatorExpression (InfixOperatorExpression a OpIn (ListExpression { values = nonNullValues })) OpOr (InfixOperatorExpression a OpIs (LiteralExpression Null)))
compileCondition (InfixOperatorExpression a operator b) =
    Snippet.sql "(" <> compileCondition a <> Snippet.sql ") " <> compileOperator operator <> Snippet.sql " " <> rightOperand
    where
        rightOperand = if rightParentheses
                then Snippet.sql "(" <> compileCondition b <> Snippet.sql ")"
                else compileCondition b

        rightParentheses :: Bool
        rightParentheses =
            case b of
                LiteralExpression Null -> False
                ListExpression {} -> False -- The () are built by compileCondition for ListExpression
                _ -> True
compileCondition (LiteralExpression literal) = dynamicValueParam literal
compileCondition (CallExpression { functionCall = ToTSQuery { text } }) = Snippet.sql "to_tsquery('english', " <> Snippet.param text <> Snippet.sql ")"
compileCondition (ListExpression { values }) =
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

-- | Encode a 'DynamicValue' as a Snippet parameter
dynamicValueParam :: DynamicValue -> Snippet
dynamicValueParam (IntValue int) = Snippet.param (fromIntegral int :: Int32)
dynamicValueParam (DoubleValue double) = Snippet.param double
dynamicValueParam (TextValue text) = Snippet.param text
dynamicValueParam (BoolValue bool) = Snippet.param bool
dynamicValueParam (UUIDValue uuid) = Snippet.param uuid
dynamicValueParam (DateTimeValue utcTime) = Snippet.param utcTime
dynamicValueParam (PointValue (Point x y)) = Snippet.sql ("point(" <> cs (tshow x) <> "," <> cs (tshow y) <> ")")
dynamicValueParam (IntervalValue interval) = Snippet.param (tshow interval)
dynamicValueParam (ArrayValue values) = Snippet.sql "ARRAY[" <> mconcat (List.intersperse (Snippet.sql ", ") (map dynamicValueParam values)) <> Snippet.sql "]"
dynamicValueParam Null = Snippet.sql "NULL"

-- | Quote a SQL identifier (table name, column name) to prevent SQL injection
quoteIdentifier :: Text -> Snippet
quoteIdentifier name = Snippet.sql (cs ("\"" <> Text.replace "\"" "\"\"" name <> "\""))
