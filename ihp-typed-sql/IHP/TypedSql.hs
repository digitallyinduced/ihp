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

import           IHP.TypedSql.Quoter                 (typedSql, typedSqlStar)
import           IHP.TypedSql.Types                  (TypedQuery (..))

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

-- | Run a typed query expected to return exactly one row of one value (e.g. @count(*)@).
--
-- Throws if the query returns zero rows or more than one row. Use
-- 'sqlQueryTypedScalarOrNothing' when no row is a valid outcome, or
-- 'sqlQueryTyped' when you expect many rows.
--
-- > total <- sqlQueryTypedScalar [typedSql| SELECT count(*) FROM users |]
sqlQueryTypedScalar :: (?modelContext :: ModelContext) => TypedQuery result -> IO result
sqlQueryTypedScalar TypedQuery { tqSnippet, tqResultDecoder } =
    runTypedSqlSession tqSnippet (HasqlDecoders.singleRow tqResultDecoder)

-- | Like 'sqlQueryTypedScalar' but returns 'Nothing' when there is no row.
--
-- > maybeName <- sqlQueryTypedScalarOrNothing [typedSql| SELECT name FROM users WHERE id = ${userId} |]
sqlQueryTypedScalarOrNothing :: (?modelContext :: ModelContext) => TypedQuery result -> IO (Maybe result)
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
