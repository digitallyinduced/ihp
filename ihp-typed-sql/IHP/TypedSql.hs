{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module IHP.TypedSql
    ( typedSql
    , typedSqlStar
    , TypedQuery (..)
    , sqlQueryTyped
    , sqlQueryTypedScalar
    , sqlQueryTypedScalarOrNothing
    , sqlExecTyped
    ) where

import qualified Hasql.Decoders                  as HasqlDecoders
import qualified Hasql.DynamicStatements.Snippet as Snippet
import           IHP.ModelSupport                (sqlQueryHasql)
import           IHP.Prelude
import           Data.Kind                       (Constraint)
import           GHC.TypeLits                    (TypeError, ErrorMessage (..))

import           IHP.TypedSql.Quoter                 (typedSql, typedSqlStar)
import           IHP.TypedSql.RowType                (SqlRow)
import           IHP.TypedSql.Types                  (TypedQuery (..))

-- | Compile-time guard for the scalar query helpers.
--
-- Multi-column typed queries are inferred as 'SqlRow', so without this guard
-- @sqlQueryTypedScalar [typedSql| SELECT name, views FROM ... |]@ would silently
-- return a single 'SqlRow' instead of a scalar. This rejects that at compile
-- time, mirroring how the deprecated @sqlQueryScalar@ only accepts single-column
-- results.
type family ScalarResult result :: Constraint where
    ScalarResult (SqlRow fields) =
        TypeError
            ( 'Text "sqlQueryTypedScalar expects a query that returns a single column,"
            ':$$: 'Text "but this query returns multiple columns (a SqlRow)."
            ':$$: 'Text "Use sqlQueryTyped to fetch the rows, or select a single column."
            )
    ScalarResult result = ()

-- | Run a typed SELECT query and return all result rows.
--
-- Also works with INSERT\/UPDATE\/DELETE ... RETURNING statements
-- that return rows.
--
-- > users <- sqlQueryTyped [typedSql| SELECT name FROM users |]
-- > newIds <- sqlQueryTyped [typedSql| INSERT INTO items (name) VALUES (${name}) RETURNING id |]
sqlQueryTyped :: (?modelContext :: ModelContext) => TypedQuery result -> IO [result]
sqlQueryTyped TypedQuery { tqSnippet, tqResultDecoder } =
    runTypedSqlSession tqSnippet (HasqlDecoders.rowList tqResultDecoder)

-- | Run a typed query expected to return exactly one row of a single column (e.g. @count(*)@).
--
-- Throws if the query returns zero rows or more than one row. Use
-- 'sqlQueryTypedScalarOrNothing' when no row is a valid outcome, or
-- 'sqlQueryTyped' when you expect many rows.
--
-- Multi-column queries are rejected at compile time, so this always returns a
-- single scalar rather than a 'SqlRow'.
--
-- > total <- sqlQueryTypedScalar [typedSql| SELECT count(*) FROM users |]
sqlQueryTypedScalar :: (?modelContext :: ModelContext, ScalarResult result) => TypedQuery result -> IO result
sqlQueryTypedScalar TypedQuery { tqSnippet, tqResultDecoder } =
    runTypedSqlSession tqSnippet (HasqlDecoders.singleRow tqResultDecoder)

-- | Like 'sqlQueryTypedScalar' but returns 'Nothing' when there is no row.
--
-- > maybeName <- sqlQueryTypedScalarOrNothing [typedSql| SELECT name FROM users WHERE id = ${userId} |]
sqlQueryTypedScalarOrNothing :: (?modelContext :: ModelContext, ScalarResult result) => TypedQuery result -> IO (Maybe result)
sqlQueryTypedScalarOrNothing TypedQuery { tqSnippet, tqResultDecoder } =
    runTypedSqlSession tqSnippet (HasqlDecoders.rowMaybe tqResultDecoder)

-- | Run a typed statement (INSERT\/UPDATE\/DELETE) and return the affected row count.
--
-- Use 'sqlQueryTyped' instead if your statement has a RETURNING clause.
--
-- > rowsAffected <- sqlExecTyped [typedSql| DELETE FROM items WHERE id = ${itemId} |]
sqlExecTyped :: (?modelContext :: ModelContext) => TypedQuery result -> IO Int64
sqlExecTyped TypedQuery { tqSnippet } =
    runTypedSqlSession tqSnippet HasqlDecoders.rowsAffected

runTypedSqlSession :: (?modelContext :: ModelContext) => Snippet.Snippet -> HasqlDecoders.Result result -> IO result
runTypedSqlSession snippet decoder =
    sqlQueryHasql ?modelContext.hasqlPool snippet decoder
