{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module IHP.TypedSql
    ( typedSql
    , typedSqlStar
    , QueryCardinality (..)
    , QueryExecResult (..)
    , TypedQuery (..)
    , TypedQueryResult
    , SqlExecTypedResult
    , DecodeTypedQuery
    , RunTypedExec
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
import           IHP.ModelSupport                (isTableReadTrackingEnabled, sqlExecHasql, sqlExecHasqlCount, sqlQueryHasql, trackTableReadWithQuery)
import           IHP.Prelude
import           GHC.TypeLits                    (ErrorMessage (Text), TypeError)
import qualified Data.List                       as List
import qualified Data.Set                        as Set
import qualified Data.Text                       as Text
import           IHP.QueryBuilder.HasqlHelpers   (quoteIdentifier)

import           IHP.TypedSql.Quoter                 (typedSql, typedSqlStar)
import           IHP.TypedSql.Types                  (QueryCardinality (..), QueryExecResult (..), SqlExecTypedResult,
                                                     TypedQuery (..), TypedQueryResult)

class DecodeTypedQuery (cardinality :: QueryCardinality) where
    typedQueryResultDecoder :: Proxy cardinality -> HasqlDecoders.Row result -> HasqlDecoders.Result (TypedQueryResult cardinality result)
    typedQueryResultWithIdsDecoder :: Proxy cardinality -> HasqlDecoders.Row result -> HasqlDecoders.Result (TypedQueryResult cardinality result, [Text])

instance DecodeTypedQuery 'ManyRows where
    typedQueryResultDecoder _ = HasqlDecoders.rowList
    typedQueryResultWithIdsDecoder _ rowDecoder =
        unzip <$> HasqlDecoders.rowList ((,) <$> rowDecoder <*> textColumn)

instance DecodeTypedQuery 'AtMostOneRow where
    typedQueryResultDecoder _ = HasqlDecoders.rowMaybe
    typedQueryResultWithIdsDecoder _ rowDecoder =
        toResult <$> HasqlDecoders.rowMaybe ((,) <$> rowDecoder <*> textColumn)
      where
        toResult Nothing = (Nothing, [])
        toResult (Just (row, rowId)) = (Just row, [rowId])

instance DecodeTypedQuery 'ExactlyOneRow where
    typedQueryResultDecoder _ = HasqlDecoders.singleRow
    typedQueryResultWithIdsDecoder _ rowDecoder =
        toResult <$> HasqlDecoders.singleRow ((,) <$> rowDecoder <*> textColumn)
      where
        toResult (row, rowId) = (row, [rowId])

textColumn :: HasqlDecoders.Row Text
textColumn = HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.text)

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
sqlQueryTyped :: forall cardinality result. (?modelContext :: ModelContext, DecodeTypedQuery cardinality) => TypedQuery cardinality 'ReturnsRows result -> IO (TypedQueryResult cardinality result)
sqlQueryTyped TypedQuery { tqSnippet, tqResultDecoder, tqAutoRefreshTables, tqAutoRefreshRowMatchingSafe } = do
    structuredTracking <- isTableReadTrackingEnabled
    case (structuredTracking, tqAutoRefreshRowMatchingSafe, firstTrackableTable tqAutoRefreshTables) of
      (True, True, Just (trackedTable, idColumn)) -> do
        let trackedSnippet = appendTrackedId tqSnippet idColumn
        (result, rowIds) <- runTypedSqlSession trackedSnippet (typedQueryResultWithIdsDecoder (Proxy :: Proxy cardinality) tqResultDecoder)
        trackTypedSqlQuery tqSnippet tqAutoRefreshTables (Just (trackedTable, idColumn, Set.fromList rowIds))
        pure result
      _ -> do
        result <- runTypedSqlSession tqSnippet (typedQueryResultDecoder (Proxy :: Proxy cardinality) tqResultDecoder)
        when structuredTracking $
            trackTypedSqlQuery tqSnippet tqAutoRefreshTables Nothing
        pure result

-- | Run a typed query that can return many rows.
--
-- This is equivalent to 'sqlQueryTyped', but fixes the expected cardinality in
-- the function name. It can make type errors easier to read when migrating code
-- from the old list-shaped 'sqlQueryTyped' result.
sqlQueryTypedRows :: (?modelContext :: ModelContext) => TypedQuery 'ManyRows 'ReturnsRows result -> IO [result]
sqlQueryTypedRows = sqlQueryTyped

-- | Run a typed query that can return at most one row.
--
-- This is equivalent to 'sqlQueryTyped', but fixes the expected cardinality in
-- the function name.
sqlQueryTypedOneOrNothing :: (?modelContext :: ModelContext) => TypedQuery 'AtMostOneRow 'ReturnsRows result -> IO (Maybe result)
sqlQueryTypedOneOrNothing = sqlQueryTyped

-- | Run a typed query that must return exactly one row.
--
-- This is equivalent to 'sqlQueryTyped', but fixes the expected cardinality in
-- the function name.
sqlQueryTypedSingle :: (?modelContext :: ModelContext) => TypedQuery 'ExactlyOneRow 'ReturnsRows result -> IO result
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
sqlQueryTypedMaybeColumn :: (?modelContext :: ModelContext) => TypedQuery 'AtMostOneRow 'ReturnsRows (Maybe result) -> IO (Maybe result)
sqlQueryTypedMaybeColumn query = do
    value <- sqlQueryTyped query
    pure case value of
        Nothing -> Nothing
        Just inner -> inner

-- | Pipeline variant of 'sqlQueryTyped'.
--
-- Compose this with 'IHP.FetchPipelined.pipeline' to run independent typed SQL
-- queries in one PostgreSQL pipeline batch.
sqlQueryTypedPipelined :: forall cardinality result. DecodeTypedQuery cardinality => TypedQuery cardinality 'ReturnsRows result -> HasqlPipeline.Pipeline (TypedQueryResult cardinality result)
sqlQueryTypedPipelined TypedQuery { tqSnippet, tqResultDecoder } =
    HasqlPipeline.statement () $
        Snippet.toPreparableStatement tqSnippet (typedQueryResultDecoder (Proxy :: Proxy cardinality) tqResultDecoder)

-- | Pipeline variant of 'sqlQueryTypedMaybeColumn'.
sqlQueryTypedMaybeColumnPipelined :: TypedQuery 'AtMostOneRow 'ReturnsRows (Maybe result) -> HasqlPipeline.Pipeline (Maybe result)
sqlQueryTypedMaybeColumnPipelined query =
    flatten <$> sqlQueryTypedPipelined query
  where
    flatten = \case
        Nothing -> Nothing
        Just inner -> inner

class RunTypedExec (execResult :: QueryExecResult) where
    runTypedExec :: Proxy execResult -> ModelContext -> Snippet.Snippet -> IO (SqlExecTypedResult execResult)

instance RunTypedExec 'ReturnsAffectedRows where
    runTypedExec _ modelContext snippet =
        let ?modelContext = modelContext in
        sqlExecHasqlCount modelContext.hasqlPool snippet

instance RunTypedExec 'ReturnsNoResult where
    runTypedExec _ modelContext snippet =
        let ?modelContext = modelContext in
        sqlExecHasql modelContext.hasqlPool snippet

instance TypeError ('Text "sqlExecTyped cannot run SQL statements that return rows. Use sqlQueryTyped instead.") => RunTypedExec 'ReturnsRows where
    runTypedExec _ _ _ = error "unreachable"

-- | Run a typed statement.
--
-- Use 'sqlQueryTyped' instead if your statement has a RETURNING clause.
-- Known utility statements without a row-count result, such as
-- @SET CONSTRAINTS@, are run with Hasql's no-result decoder and return @()@.
--
-- > rowsAffected <- sqlExecTyped [typedSql| DELETE FROM items WHERE id = ${itemId} |]
-- > sqlExecTyped [typedSql| SET CONSTRAINTS ALL DEFERRED |] -- IO ()
sqlExecTyped :: forall cardinality execResult result. (?modelContext :: ModelContext, RunTypedExec execResult) => TypedQuery cardinality execResult result -> IO (SqlExecTypedResult execResult)
sqlExecTyped TypedQuery { tqSnippet } =
    runTypedExec (Proxy :: Proxy execResult) ?modelContext tqSnippet

runTypedSqlSession :: (?modelContext :: ModelContext) => Snippet.Snippet -> HasqlDecoders.Result result -> IO result
runTypedSqlSession snippet decoder =
    sqlQueryHasql ?modelContext.hasqlPool snippet decoder

-- | Find the one result column that can identify rows for a physical table.
firstTrackableTable :: [(Text, Maybe Text)] -> Maybe (Text, Text)
firstTrackableTable [] = Nothing
firstTrackableTable ((tableName, Just idColumn):_) = Just (tableName, idColumn)
firstTrackableTable (_:tables) = firstTrackableTable tables

-- | Append a textual row ID while preserving the original result columns.
appendTrackedId :: Snippet.Snippet -> Text -> Snippet.Snippet
appendTrackedId snippet idColumn =
    Snippet.sql "SELECT _ihp_auto_refresh_record.*, _ihp_auto_refresh_record."
    <> quoteIdentifier idColumn
    <> Snippet.sql "::text FROM ("
    <> snippet
    <> Snippet.sql ") AS _ihp_auto_refresh_record"

-- | Register Typed SQL reads with AutoRefresh.
--
-- The matcher closes over 'tqSnippet', so every bound parameter remains encoded
-- exactly as it was for the original query. The captured model context retains
-- RLS identity but deliberately drops an active transaction runner and tracker.
trackTypedSqlQuery :: (?modelContext :: ModelContext) => Snippet.Snippet -> [(Text, Maybe Text)] -> Maybe (Text, Text, Set.Set Text) -> IO ()
trackTypedSqlQuery snippet tables tracked = do
    let capturedModelContext = ?modelContext
            { transactionRunner = Nothing
            , trackTableReadCallback = Nothing
            }
    forM_ tables \(tableName, maybeIdColumn) ->
        case (tracked, maybeIdColumn) of
            (Just (trackedTable, idColumn, rowIds), Just _)
                | tableName == trackedTable -> do
                    let matchesIds changedIds =
                            let matchSnippet =
                                    Snippet.sql "SELECT EXISTS (SELECT 1 FROM ("
                                    <> snippet
                                    <> Snippet.sql ") AS _ihp_auto_refresh_records WHERE _ihp_auto_refresh_records."
                                    <> quoteIdentifier idColumn
                                    <> Snippet.sql " = ANY (ARRAY(SELECT (jsonb_populate_record(NULL::"
                                    <> quoteIdentifierPath tableName
                                    <> Snippet.sql ", jsonb_build_object('id', _ihp_changed_id))).id FROM unnest("
                                    <> Snippet.param (Set.toList changedIds)
                                    <> Snippet.sql "::text[]) AS _ihp_changed_id)))"
                            in let ?modelContext = capturedModelContext
                               in sqlQueryHasql capturedModelContext.hasqlPool matchSnippet (HasqlDecoders.singleRow (HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.bool)))
                    trackTableReadWithQuery tableName (Just rowIds) (Just matchesIds)
            _ -> trackTableReadWithQuery tableName Nothing Nothing

quoteIdentifierPath :: Text -> Snippet.Snippet
quoteIdentifierPath = mconcat . List.intersperse (Snippet.sql ".") . map quoteIdentifier . Text.splitOn "."
