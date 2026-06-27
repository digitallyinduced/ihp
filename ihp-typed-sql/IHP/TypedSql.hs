{-# LANGUAGE ScopedTypeVariables #-}

module IHP.TypedSql
    ( typedSql
    , typedSqlStar
    , QueryCardinality (..)
    , TypedQuery (..)
    , TypedQueryResult
    , DecodeTypedQuery
    , sqlQueryTyped
    , sqlQueryTypedRows
    , sqlQueryTypedOneOrNothing
    , sqlQueryTypedSingle
    , sqlQueryTypedMaybeColumn
    , sqlQueryTypedPipelined
    , sqlQueryTypedMaybeColumnPipelined
    , sqlExecTyped
    ) where

import qualified Hasql.Decoders                  as HasqlDecoders
import qualified Hasql.DynamicStatements.Snippet as Snippet
import qualified Hasql.Pipeline                  as HasqlPipeline
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

-- | Run a typed query that can return many rows.
--
-- This is equivalent to 'sqlQueryTyped', but fixes the expected cardinality in
-- the function name. It can make type errors easier to read when migrating code
-- from the old list-shaped 'sqlQueryTyped' result.
sqlQueryTypedRows :: (?modelContext :: ModelContext) => TypedQuery 'ManyRows result -> IO [result]
sqlQueryTypedRows = sqlQueryTyped

-- | Run a typed query that can return at most one row.
--
-- This is equivalent to 'sqlQueryTyped', but fixes the expected cardinality in
-- the function name.
sqlQueryTypedOneOrNothing :: (?modelContext :: ModelContext) => TypedQuery 'AtMostOneRow result -> IO (Maybe result)
sqlQueryTypedOneOrNothing = sqlQueryTyped

-- | Run a typed query that must return exactly one row.
--
-- This is equivalent to 'sqlQueryTyped', but fixes the expected cardinality in
-- the function name.
sqlQueryTypedSingle :: (?modelContext :: ModelContext) => TypedQuery 'ExactlyOneRow result -> IO result
sqlQueryTypedSingle = sqlQueryTyped

-- | Run an at-most-one-row query selecting a nullable single column and flatten
-- the two independent failure modes into one 'Maybe'.
--
-- Useful for queries like:
--
-- > email <- sqlQueryTypedMaybeColumn [typedSql|
-- >     SELECT optional_email FROM users WHERE id = ${userId}
-- > |]
--
-- Without this helper, the precise 'sqlQueryTyped' result is
-- @Maybe (Maybe Text)@: the outer 'Maybe' is "no row", the inner 'Maybe' is
-- "the selected column was NULL".
sqlQueryTypedMaybeColumn :: (?modelContext :: ModelContext) => TypedQuery 'AtMostOneRow (Maybe result) -> IO (Maybe result)
sqlQueryTypedMaybeColumn query = do
    value <- sqlQueryTyped query
    pure case value of
        Nothing -> Nothing
        Just inner -> inner

-- | Pipeline variant of 'sqlQueryTyped'.
--
-- Compose this with 'IHP.FetchPipelined.pipeline' to run independent typed SQL
-- queries in one PostgreSQL pipeline batch.
sqlQueryTypedPipelined :: forall cardinality result. DecodeTypedQuery cardinality => TypedQuery cardinality result -> HasqlPipeline.Pipeline (TypedQueryResult cardinality result)
sqlQueryTypedPipelined TypedQuery { tqSnippet, tqResultDecoder } =
    HasqlPipeline.statement () $
        Snippet.toPreparableStatement tqSnippet (typedQueryResultDecoder (Proxy :: Proxy cardinality) tqResultDecoder)

-- | Pipeline variant of 'sqlQueryTypedMaybeColumn'.
sqlQueryTypedMaybeColumnPipelined :: TypedQuery 'AtMostOneRow (Maybe result) -> HasqlPipeline.Pipeline (Maybe result)
sqlQueryTypedMaybeColumnPipelined query =
    flatten <$> sqlQueryTypedPipelined query
  where
    flatten = \case
        Nothing -> Nothing
        Just inner -> inner

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
