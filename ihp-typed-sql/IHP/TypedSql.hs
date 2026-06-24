{-# LANGUAGE ScopedTypeVariables #-}

module IHP.TypedSql
    ( typedSql
    , typedSqlStar
    , QueryCardinality (..)
    , TypedQuery (..)
    , TypedQueryResult
    , DecodeTypedQuery
    , sqlQueryTyped
    , sqlExecTyped
    ) where

import           Data.Proxy                       (Proxy (..))
import qualified Hasql.Decoders                  as HasqlDecoders
import qualified Hasql.DynamicStatements.Snippet as Snippet
import           IHP.ModelSupport                (sqlQueryHasql)
import           IHP.Prelude

import           IHP.TypedSql.Quoter                 (typedSql, typedSqlStar)
import           IHP.TypedSql.Types                  (QueryCardinality (..), TypedQuery (..), TypedQueryResult)

class DecodeTypedQuery (cardinality :: QueryCardinality) where
    typedQueryResultDecoder :: Proxy cardinality -> HasqlDecoders.Row result -> HasqlDecoders.Result (TypedQueryResult cardinality result)

instance DecodeTypedQuery 'ManyRows where
    typedQueryResultDecoder _ = HasqlDecoders.rowList

instance DecodeTypedQuery 'AtMostOneRow where
    typedQueryResultDecoder _ = HasqlDecoders.rowMaybe

instance DecodeTypedQuery 'ExactlyOneRow where
    typedQueryResultDecoder _ = HasqlDecoders.singleRow

-- | Run a typed SELECT query.
--
-- Also works with INSERT\/UPDATE\/DELETE ... RETURNING statements
-- that return rows.
--
-- The return type is inferred from the query cardinality:
--
-- * many rows: @[result]@
-- * at most one row: @Maybe result@
-- * exactly one row: @result@
--
-- > users <- sqlQueryTyped [typedSql| SELECT name FROM users |] -- IO [Text]
-- > total <- sqlQueryTyped [typedSql| SELECT count(*) FROM users |] -- IO Int64
sqlQueryTyped :: forall cardinality result. (?modelContext :: ModelContext, DecodeTypedQuery cardinality) => TypedQuery cardinality result -> IO (TypedQueryResult cardinality result)
sqlQueryTyped TypedQuery { tqSnippet, tqResultDecoder } =
    runTypedSqlSession tqSnippet (typedQueryResultDecoder (Proxy :: Proxy cardinality) tqResultDecoder)

-- | Run a typed statement (INSERT\/UPDATE\/DELETE) and return the affected row count.
--
-- Use 'sqlQueryTyped' instead if your statement has a RETURNING clause.
--
-- > rowsAffected <- sqlExecTyped [typedSql| DELETE FROM items WHERE id = ${itemId} |]
sqlExecTyped :: (?modelContext :: ModelContext) => TypedQuery cardinality result -> IO Int64
sqlExecTyped TypedQuery { tqSnippet } =
    runTypedSqlSession tqSnippet HasqlDecoders.rowsAffected

runTypedSqlSession :: (?modelContext :: ModelContext) => Snippet.Snippet -> HasqlDecoders.Result result -> IO result
runTypedSqlSession snippet decoder =
    sqlQueryHasql ?modelContext.hasqlPool snippet decoder
