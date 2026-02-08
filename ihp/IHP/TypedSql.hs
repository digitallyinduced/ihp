{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE RecordWildCards #-}

module IHP.TypedSql
    ( typedSql
    , TypedQuery (..)
    , sqlQueryTyped
    , sqlExecTyped
    ) where

import qualified Database.PostgreSQL.Simple         as PG
import qualified Database.PostgreSQL.Simple.ToField as PGTF
import qualified Database.PostgreSQL.Simple.ToRow   as PGTR
import           IHP.ModelSupport                   (ModelContext, measureTimeIfLogging,
                                                     withDatabaseConnection, withRLSParams)
import           IHP.Prelude

import           IHP.TypedSql.Quoter                (typedSql)
import           IHP.TypedSql.Types                 (TypedQuery (..))

-- Wrapper to turn a list of Action into a ToRow instance.
newtype PreparedRow = PreparedRow [PGTF.Action]
instance PGTR.ToRow PreparedRow where
    toRow (PreparedRow params) = params

-- | Run a typed query and return all rows.
-- High-level: delegates to postgres-simple with logging and RLS params.
sqlQueryTyped :: (?modelContext :: ModelContext) => TypedQuery result -> IO [result]
sqlQueryTyped TypedQuery { tqQuery, tqParams, tqRowParser } =
    withDatabaseConnection \connection ->
        withRLSParams
            (\query params ->
                measureTimeIfLogging "🔍" connection
                    (PG.queryWith tqRowParser connection query (PreparedRow params))
                    query
                    (PreparedRow params)
            )
            tqQuery
            (PreparedRow tqParams)

-- | Run a typed statement (INSERT/UPDATE/DELETE) and return affected row count.
-- High-level: mirrors 'sqlExec' but keeps typed SQL parameters.
sqlExecTyped :: (?modelContext :: ModelContext) => TypedQuery result -> IO Int64
sqlExecTyped TypedQuery { tqQuery, tqParams } =
    withDatabaseConnection \connection ->
        withRLSParams
            (\query params ->
                measureTimeIfLogging "💾" connection
                    (PG.execute connection query (PreparedRow params))
                    query
                    (PreparedRow params)
            )
            tqQuery
            (PreparedRow tqParams)
