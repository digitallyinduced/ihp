{-# LANGUAGE BangPatterns, TypeFamilies, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, FlexibleContexts, AllowAmbiguousTypes #-}

{-|
Module: IHP.FetchPipelined
Description: Fetch multiple independent queries in a single database round trip
Copyright: (c) digitally induced GmbH, 2026

Uses PostgreSQL's pipeline mode (via hasql) to send multiple independent queries
in a single network round trip. This is especially beneficial for cloud database
deployments where round-trip latency is 1-5ms.

Compose queries using 'Applicative' operators:

> (users, posts) <- fetchPipelined $ (,)
>     <$> pipelineQuery (query @User)
>     <*> pipelineQuery (query @Post |> orderByDesc #createdAt)

The 'PipelinedQuery' type is 'Applicative' but NOT 'Monad', which enforces at the
type level that only independent queries can be pipelined.
-}
module IHP.FetchPipelined
( PipelinedQuery
, pipelineQuery
, pipelineQueryOneOrNothing
, pipelineQueryCount
, pipelineQueryExists
, fetchPipelined
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.QueryBuilder.HasqlCompiler (buildStatement, buildWrappedStatement)
import IHP.Hasql.FromRow (FromRowHasql(..))
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Pipeline as Pipeline
import qualified Hasql.Session as HasqlSession
import qualified Hasql.Pool as HasqlPool
import qualified IHP.Log as Log
import Control.Exception (finally)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- | A composable query for pipeline execution.
--
-- Wraps a hasql 'Pipeline' step with tracked table names for AutoRefresh.
-- Compose using 'Applicative' operators:
--
-- > (,) <$> pipelineQuery (query @User) <*> pipelineQuery (query @Post)
data PipelinedQuery a = PipelinedQuery ![Text] !(Pipeline.Pipeline a)

instance Functor PipelinedQuery where
    fmap f (PipelinedQuery t p) = PipelinedQuery t (fmap f p)
    {-# INLINE fmap #-}

instance Applicative PipelinedQuery where
    pure a = PipelinedQuery [] (pure a)
    {-# INLINE pure #-}
    PipelinedQuery t1 p1 <*> PipelinedQuery t2 p2 = PipelinedQuery (t1 <> t2) (p1 <*> p2)
    {-# INLINE (<*>) #-}

-- | Prepare a query builder for pipeline execution, returning all matching rows.
--
-- __Example:__ Fetching users and posts in a single round trip
--
-- > (users, posts) <- fetchPipelined $ (,)
-- >     <$> pipelineQuery (query @User |> filterWhere (#active, True))
-- >     <*> pipelineQuery (query @Post |> orderByDesc #createdAt)
pipelineQuery :: forall model table queryBuilderProvider joinRegister.
    ( Table model
    , HasQueryBuilder queryBuilderProvider joinRegister
    , model ~ GetModelByTableName table
    , KnownSymbol table
    , FromRowHasql model
    ) => queryBuilderProvider table -> PipelinedQuery [model]
pipelineQuery !queryBuilder =
    let !sqlQuery = buildQuery queryBuilder
        !statement = buildStatement sqlQuery (Decoders.rowList (hasqlRowDecoder @model))
    in PipelinedQuery [tableName @model] (Pipeline.statement () statement)
{-# INLINE pipelineQuery #-}

-- | Prepare a query builder for pipeline execution, returning at most one row.
--
-- __Example:__
--
-- > (maybeUser, posts) <- fetchPipelined $ (,)
-- >     <$> pipelineQueryOneOrNothing (query @User |> filterWhere (#email, email))
-- >     <*> pipelineQuery (query @Post)
pipelineQueryOneOrNothing :: forall model table queryBuilderProvider joinRegister.
    ( Table model
    , HasQueryBuilder queryBuilderProvider joinRegister
    , model ~ GetModelByTableName table
    , KnownSymbol table
    , FromRowHasql model
    ) => queryBuilderProvider table -> PipelinedQuery (Maybe model)
pipelineQueryOneOrNothing !queryBuilder =
    let !sqlQuery = buildQuery queryBuilder |> setJust #limitClause 1
        !statement = buildStatement sqlQuery (Decoders.rowMaybe (hasqlRowDecoder @model))
    in PipelinedQuery [tableName @model] (Pipeline.statement () statement)
{-# INLINE pipelineQueryOneOrNothing #-}

-- | Prepare a count query for pipeline execution.
--
-- __Example:__
--
-- > (users, userCount) <- fetchPipelined $ (,)
-- >     <$> pipelineQuery (query @User)
-- >     <*> pipelineQueryCount (query @User |> filterWhere (#active, True))
pipelineQueryCount :: forall table queryBuilderProvider joinRegister.
    ( KnownSymbol table
    , HasQueryBuilder queryBuilderProvider joinRegister
    ) => queryBuilderProvider table -> PipelinedQuery Int
pipelineQueryCount !queryBuilder =
    let !statement = buildWrappedStatement
            "SELECT COUNT(*) FROM ("
            (buildQuery queryBuilder)
            ") AS _count_values"
            (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
    in PipelinedQuery [symbolToText @table] (fromIntegral <$> Pipeline.statement () statement)
{-# INLINE pipelineQueryCount #-}

-- | Prepare an existence check for pipeline execution.
--
-- __Example:__
--
-- > (users, hasUnread) <- fetchPipelined $ (,)
-- >     <$> pipelineQuery (query @User)
-- >     <*> pipelineQueryExists (query @Message |> filterWhere (#isUnread, True))
pipelineQueryExists :: forall table queryBuilderProvider joinRegister.
    ( KnownSymbol table
    , HasQueryBuilder queryBuilderProvider joinRegister
    ) => queryBuilderProvider table -> PipelinedQuery Bool
pipelineQueryExists !queryBuilder =
    let !statement = buildWrappedStatement
            "SELECT EXISTS ("
            (buildQuery queryBuilder)
            ") AS _exists_values"
            (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
    in PipelinedQuery [symbolToText @table] (Pipeline.statement () statement)
{-# INLINE pipelineQueryExists #-}

-- | Execute a composed pipeline query in a single database round trip.
--
-- All queries composed via 'Applicative' are sent to PostgreSQL in pipeline mode,
-- typically completing in a single network round trip.
--
-- __Example:__
--
-- > action DashboardAction = do
-- >     (users, posts, commentCount) <- fetchPipelined $ (,,)
-- >         <$> pipelineQuery (query @User)
-- >         <*> pipelineQuery (query @Post |> orderByDesc #createdAt |> limit 10)
-- >         <*> pipelineQueryCount (query @Comment)
-- >     render DashboardView { .. }
fetchPipelined :: (?modelContext :: ModelContext) => PipelinedQuery a -> IO a
fetchPipelined (PipelinedQuery tables thePipeline) = do
    mapM_ trackTableRead tables
    let pool = ?modelContext.hasqlPool
    let session = HasqlSession.pipeline thePipeline
    let ?context = ?modelContext
    let currentLogLevel = ?modelContext.logger.level
    let runQuery = case ?modelContext.transactionRunner of
            Just (TransactionRunner runner) -> runner session
            Nothing -> do
                result <- HasqlPool.use pool session
                case result of
                    Left err
                        | isCachedPlanError err -> do
                            Log.info ("Resetting hasql connection pool due to stale prepared statements (e.g. after 'make db')" :: Text)
                            HasqlPool.release pool
                            retryResult <- HasqlPool.use pool session
                            case retryResult of
                                Left retryErr -> throwIO (HasqlError retryErr)
                                Right a -> pure a
                        | otherwise -> throwIO (HasqlError err)
                    Right a -> pure a
    if currentLogLevel == Log.Debug
        then do
            start <- getCurrentTime
            runQuery `finally` do
                end <- getCurrentTime
                let queryTimeInMs = round (realToFrac (end `diffUTCTime` start) * 1000 :: Double)
                Log.debug ("🔍 Pipeline (" <> tshow (length tables) <> " queries, " <> tshow queryTimeInMs <> "ms)")
        else runQuery
{-# INLINABLE fetchPipelined #-}
