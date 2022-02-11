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
        sql = "SELECT ? FROM ?" <> whereSql <> orderBySql <> limitSql <> offsetSql
        args = catMaybes
                [ Just (compileSelectedColumns selectedColumns)
                , Just (PG.toField (PG.Identifier table))
                ]
                <> whereArgs
                <> orderByArgs
                <> limitArgs
                <> offsetArgs

        (orderBySql, orderByArgs) = case orderByClause of
                [] -> ("", [])
                orderByClauses ->
                    ( PG.Query $ cs $ " ORDER BY " <> (intercalate ", " (map (const "? ?") orderByClauses))
                    , orderByClauses
                        |> map (\QueryBuilder.OrderByClause { orderByColumn, orderByDirection } ->
                                    [ PG.toField $ PG.Identifier (fieldNameToColumnName $ cs orderByColumn)
                                    , PG.toField $ if orderByDirection == QueryBuilder.Desc
                                        then PG.Plain "DESC"
                                        else PG.Plain ""
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
compileCondition NullExpression = ("NULL", [])
compileCondition (InfixOperatorExpression a OpEqual NullExpression) = compileCondition (InfixOperatorExpression a OpIs NullExpression) -- Turn 'a = NULL' into 'a IS NULL'
compileCondition (InfixOperatorExpression a OpNotEqual NullExpression) = compileCondition (InfixOperatorExpression a OpIsNot NullExpression) -- Turn 'a <> NULL' into 'a IS NOT NULL'
compileCondition (InfixOperatorExpression a operator b) = ("(" <> queryA <> ") " <> compileOperator operator <> " " <> rightOperand, paramsA <> paramsB)
    where
        (queryA, paramsA) = compileCondition a
        (queryB, paramsB) = compileCondition b

        rightOperand = if rightParentheses
                then "(" <>  queryB <> ")"
                else queryB

        rightParentheses :: Bool
        rightParentheses = b /= NullExpression
compileCondition (LiteralExpression literal) = ("?", [toValue literal])
    where
        toValue (IntValue int) = PG.toField int
        toValue (DoubleValue double) = PG.toField double
        toValue (TextValue text) = PG.toField text
        toValue (BoolValue bool) = PG.toField bool
        toValue (UUIDValue uuid) = PG.toField uuid
        toValue (DateTimeValue utcTime) = PG.toField utcTime
        toValue (PointValue point) = PG.toField point
        toValue Null = PG.toField PG.Null

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