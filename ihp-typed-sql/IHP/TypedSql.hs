module IHP.TypedSql
    ( typedSql
    , TypedQuery (..)
    , sqlQueryTyped
    , sqlExecTyped
    ) where

import qualified Hasql.Decoders                  as HasqlDecoders
import qualified Hasql.DynamicStatements.Snippet as Snippet
import           IHP.ModelSupport                (sqlQueryHasql)
import           IHP.Prelude

import           IHP.TypedSql.Quoter                 (typedSql)
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
