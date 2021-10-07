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
        sql = "SELECT ? FROM ?" <> orderBySql
        args = catMaybes
                [ Just (compileSelectedColumns selectedColumns)
                , Just (PG.toField (PG.Identifier table))
                ]
                <> orderByArgs

        (orderBySql, orderByArgs) = case orderByClause of
                [] -> ("?", [PG.Plain ""])
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

compileSelectedColumns :: SelectedColumns -> PG.Action
compileSelectedColumns SelectAll = PG.Plain "*"
compileSelectedColumns (SelectSpecific fields) = PG.Many args
    where
        args :: [PG.Action]
        args = List.intercalate ([PG.Plain ", "]) fieldActions
        fieldActions :: [[PG.Action]]
        fieldActions = (map (\field -> [ PG.toField (PG.Identifier field) ]) fields)

-- TODO: validate query against schema