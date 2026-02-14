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

-- | Run a typed query and return all rows.
-- High-level: executes the generated hasql snippet with its decoder.
sqlQueryTyped :: (?modelContext :: ModelContext) => TypedQuery result -> IO [result]
sqlQueryTyped TypedQuery { tqSnippet, tqResultDecoder } =
    runTypedSqlSession tqSnippet (HasqlDecoders.rowList tqResultDecoder)

-- | Run a typed statement (INSERT/UPDATE/DELETE) and return affected row count.
-- High-level: executes the generated hasql snippet and decodes rows affected.
sqlExecTyped :: (?modelContext :: ModelContext) => TypedQuery result -> IO Int64
sqlExecTyped TypedQuery { tqSnippet } =
    runTypedSqlSession tqSnippet HasqlDecoders.rowsAffected

runTypedSqlSession :: (?modelContext :: ModelContext) => Snippet.Snippet -> HasqlDecoders.Result result -> IO result
runTypedSqlSession snippet decoder =
    sqlQueryHasql ?modelContext.hasqlPool snippet decoder
