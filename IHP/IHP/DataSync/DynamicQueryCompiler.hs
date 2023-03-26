{-|
Module: IHP.DataSync.DynamicQueryCompiler
Description: Compiles a DynamicQuery to SQL
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.DataSync.DynamicQueryCompiler where

import IHP.Prelude
import IHP.DataSync.DynamicQuery
import qualified IHP.QueryBuilder as QueryBuilder
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.FromRow as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import qualified Data.List as List

compileQuery :: DynamicSQLQuery -> (PG.Query, [PG.Action])
compileQuery DynamicSQLQuery { .. } = (sql, args)
    where
        sql = "SELECT" <> distinctOnSql <> "? FROM ?" <> whereSql <> orderBySql <> limitSql <> offsetSql
        args = distinctOnArgs
                <> catMaybes
                    [ Just (compileSelectedColumns selectedColumns)
                    , Just (PG.toField (PG.Identifier table))
                    ]
                <> whereArgs
                <> orderByArgs
                <> limitArgs
                <> offsetArgs

        (distinctOnSql, distinctOnArgs) = case distinctOnColumn of
            Just column -> (" DISTINCT ON (?) ", [PG.toField $ PG.Identifier (fieldNameToColumnName $ cs column)])
            Nothing     -> (" ", [])

        (orderBySql, orderByArgs) = case orderByClause of
                [] -> ("", [])
                orderByClauses ->
                    ( PG.Query $ cs $ " ORDER BY " <> (intercalate ", " (map compileOrderByClause orderByClauses))
                    , orderByClauses
                        |> map (\case
                            OrderByClause { orderByColumn, orderByDirection } ->
                                    [ PG.toField $ PG.Identifier (fieldNameToColumnName $ cs orderByColumn)
                                    , PG.toField $ if orderByDirection == QueryBuilder.Desc
                                        then PG.Plain "DESC"
                                        else PG.Plain ""
                                    ]
                            OrderByTSRank { tsvector, tsquery } ->
                                    [ PG.toField $ PG.Identifier (fieldNameToColumnName tsvector)
                                    , PG.toField tsquery
                                    ]
                        )
                        |> concat
                    )

        (whereSql, whereArgs) = case compileCondition <$> whereCondition of
            Just (sql, args) -> (" WHERE " <> sql, args)
            Nothing -> ("", [])

        (limitSql, limitArgs) = case limit of
                Just limit -> (" LIMIT ?", [PG.toField limit])
                Nothing -> ("", [])

        (offsetSql, offsetArgs) = case offset of
                Just offset -> (" OFFSET ?", [PG.toField offset])
                Nothing -> ("", [])

compileOrderByClause :: OrderByClause -> Text
compileOrderByClause OrderByClause {} = "? ?"
compileOrderByClause OrderByTSRank { tsvector, tsquery } = "ts_rank(?, to_tsquery('english', ?))"

compileSelectedColumns :: SelectedColumns -> PG.Action
compileSelectedColumns SelectAll = PG.Plain "*"
compileSelectedColumns (SelectSpecific fields) = PG.Many args
    where
        args :: [PG.Action]
        args = List.intercalate ([PG.Plain ", "]) fieldActions
        fieldActions :: [[PG.Action]]
        fieldActions = (map (\field -> [ PG.toField (PG.Identifier field) ]) fields)

-- TODO: validate query against schema

compileCondition :: ConditionExpression -> (PG.Query, [PG.Action])
compileCondition (ColumnExpression column) = ("?", [PG.toField $ PG.Identifier (fieldNameToColumnName column)])
compileCondition (InfixOperatorExpression a OpEqual (LiteralExpression Null)) = compileCondition (InfixOperatorExpression a OpIs (LiteralExpression Null)) -- Turn 'a = NULL' into 'a IS NULL'
compileCondition (InfixOperatorExpression a OpNotEqual (LiteralExpression Null)) = compileCondition (InfixOperatorExpression a OpIsNot (LiteralExpression Null)) -- Turn 'a <> NULL' into 'a IS NOT NULL'
compileCondition (InfixOperatorExpression a OpIn (ListExpression { values })) | (Null `List.elem` values) =
    -- Turn 'a IN (NULL)' into 'a IS NULL'
    case partition ((/=) Null) values of
        ([], nullValues) -> compileCondition (InfixOperatorExpression a OpIs (LiteralExpression Null))
        (nonNullValues, nullValues) -> compileCondition (InfixOperatorExpression (InfixOperatorExpression a OpIn (ListExpression { values = nonNullValues })) OpOr (InfixOperatorExpression a OpIs (LiteralExpression Null)))
compileCondition (InfixOperatorExpression a operator b) = ("(" <> queryA <> ") " <> compileOperator operator <> " " <> rightOperand, paramsA <> paramsB)
    where
        (queryA, paramsA) = compileCondition a
        (queryB, paramsB) = compileCondition b

        rightOperand = if rightParentheses
                then "(" <>  queryB <> ")"
                else queryB

        rightParentheses :: Bool
        rightParentheses =
            case b of
                LiteralExpression Null -> False
                ListExpression {} -> False -- The () are inserted already via @PG.In@
                _ -> True
compileCondition (LiteralExpression literal) = ("?", [PG.toField literal])
compileCondition (CallExpression { functionCall = ToTSQuery { text } }) = ("to_tsquery('english', ?)", [PG.toField text])
compileCondition (ListExpression { values }) = ("?", [PG.toField (PG.In values)])

compileOperator :: ConditionOperator -> PG.Query
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

instance PG.ToField DynamicValue where
    toField (IntValue int) = PG.toField int
    toField (DoubleValue double) = PG.toField double
    toField (TextValue text) = PG.toField text
    toField (BoolValue bool) = PG.toField bool
    toField (UUIDValue uuid) = PG.toField uuid
    toField (DateTimeValue utcTime) = PG.toField utcTime
    toField (PointValue point) = PG.toField point
    toField (ArrayValue values) = PG.toField (PG.PGArray values)
    toField Null = PG.toField PG.Null
