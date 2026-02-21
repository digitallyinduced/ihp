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
>     <$> toStatement (query @User)
>     <*> toStatement (query @Post |> orderByDesc #createdAt)

The 'PipelinedQuery' type is 'Applicative' but NOT 'Monad', which enforces at the
type level that only independent queries can be pipelined.
-}
module IHP.FetchPipelined
( PipelinedQuery
, toStatement
, toStatementOneOrNothing
, toStatementCount
, toStatementExists
, fetchPipelined
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.QueryBuilder.HasqlCompiler (buildStatement, buildWrappedStatement)
import IHP.Hasql.FromRow (FromRowHasql(..))
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Pipeline as Pipeline
import qualified Hasql.Session as HasqlSession
import qualified Hasql.Statement as HasqlStatement
import qualified Hasql.Pool as HasqlPool
import qualified IHP.Log as Log
import Control.Exception (finally)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Functor.Contravariant (contramap)
import Data.Functor.Contravariant.Divisible (conquer)

-- | A composable query for pipeline execution.
--
-- Wraps a hasql 'Pipeline' step with tracked table names for AutoRefresh.
-- Compose using 'Applicative' operators:
--
-- > (,) <$> toStatement (query @User) <*> toStatement (query @Post)
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
-- >     <$> toStatement (query @User |> filterWhere (#active, True))
-- >     <*> toStatement (query @Post |> orderByDesc #createdAt)
toStatement :: forall model table queryBuilderProvider joinRegister.
    ( Table model
    , HasQueryBuilder queryBuilderProvider joinRegister
    , model ~ GetModelByTableName table
    , KnownSymbol table
    , FromRowHasql model
    ) => queryBuilderProvider table -> PipelinedQuery [model]
toStatement !queryBuilder =
    let !sqlQuery = buildQuery queryBuilder
        !statement = buildStatement sqlQuery (Decoders.rowList (hasqlRowDecoder @model))
    in PipelinedQuery [tableName @model] (Pipeline.statement () statement)
{-# INLINE toStatement #-}

-- | Prepare a query builder for pipeline execution, returning at most one row.
--
-- __Example:__
--
-- > (maybeUser, posts) <- fetchPipelined $ (,)
-- >     <$> toStatementOneOrNothing (query @User |> filterWhere (#email, email))
-- >     <*> toStatement (query @Post)
toStatementOneOrNothing :: forall model table queryBuilderProvider joinRegister.
    ( Table model
    , HasQueryBuilder queryBuilderProvider joinRegister
    , model ~ GetModelByTableName table
    , KnownSymbol table
    , FromRowHasql model
    ) => queryBuilderProvider table -> PipelinedQuery (Maybe model)
toStatementOneOrNothing !queryBuilder =
    let !sqlQuery = buildQuery queryBuilder |> setJust #limitClause 1
        !statement = buildStatement sqlQuery (Decoders.rowMaybe (hasqlRowDecoder @model))
    in PipelinedQuery [tableName @model] (Pipeline.statement () statement)
{-# INLINE toStatementOneOrNothing #-}

-- | Prepare a count query for pipeline execution.
--
-- __Example:__
--
-- > (users, userCount) <- fetchPipelined $ (,)
-- >     <$> toStatement (query @User)
-- >     <*> toStatementCount (query @User |> filterWhere (#active, True))
toStatementCount :: forall table queryBuilderProvider joinRegister.
    ( KnownSymbol table
    , HasQueryBuilder queryBuilderProvider joinRegister
    ) => queryBuilderProvider table -> PipelinedQuery Int
toStatementCount !queryBuilder =
    let !statement = buildWrappedStatement
            "SELECT COUNT(*) FROM ("
            (buildQuery queryBuilder)
            ") AS _count_values"
            (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
    in PipelinedQuery [symbolToText @table] (fromIntegral <$> Pipeline.statement () statement)
{-# INLINE toStatementCount #-}

-- | Prepare an existence check for pipeline execution.
--
-- __Example:__
--
-- > (users, hasUnread) <- fetchPipelined $ (,)
-- >     <$> toStatement (query @User)
-- >     <*> toStatementExists (query @Message |> filterWhere (#isUnread, True))
toStatementExists :: forall table queryBuilderProvider joinRegister.
    ( KnownSymbol table
    , HasQueryBuilder queryBuilderProvider joinRegister
    ) => queryBuilderProvider table -> PipelinedQuery Bool
toStatementExists !queryBuilder =
    let !statement = buildWrappedStatement
            "SELECT EXISTS ("
            (buildQuery queryBuilder)
            ") AS _exists_values"
            (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
    in PipelinedQuery [symbolToText @table] (Pipeline.statement () statement)
{-# INLINE toStatementExists #-}

-- | Execute a composed pipeline query in a single database round trip.
--
-- All queries composed via 'Applicative' are sent to PostgreSQL in pipeline mode,
-- typically completing in a single network round trip.
--
-- When row-level security (RLS) is enabled, the pipeline is automatically wrapped
-- with @set_config@ / reset statements to preserve the request's RLS context.
-- These are included in the same pipeline batch, adding no extra round trips.
--
-- __Example:__
--
-- > action DashboardAction = do
-- >     (users, posts, commentCount) <- fetchPipelined $ (,,)
-- >         <$> toStatement (query @User)
-- >         <*> toStatement (query @Post |> orderByDesc #createdAt |> limit 10)
-- >         <*> toStatementCount (query @Comment)
-- >     render DashboardView { .. }
fetchPipelined :: (?modelContext :: ModelContext) => PipelinedQuery a -> IO a
fetchPipelined (PipelinedQuery tables thePipeline) = do
    mapM_ trackTableRead tables
    let pool = ?modelContext.hasqlPool
    -- When RLS is enabled and we're not already in a transaction, wrap the
    -- pipeline with session-scoped set_config/reset statements.  These are
    -- part of the same pipeline batch so they add no extra round trips.
    -- In pipeline mode the server processes statements sequentially, so the
    -- set_config takes effect before the user queries execute.
    let effectivePipeline = case (?modelContext.transactionRunner, ?modelContext.rowLevelSecurity) of
            (Nothing, Just RowLevelSecurityContext { rlsAuthenticatedRole, rlsUserId }) ->
                (\_ a _ -> a)
                    <$> Pipeline.statement (rlsAuthenticatedRole, rlsUserId) setRLSConfigPipelineStatement
                    <*> thePipeline
                    <*> Pipeline.statement () resetRLSConfigPipelineStatement
            _ -> thePipeline
    let session = HasqlSession.pipeline effectivePipeline
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

-- | Session-scoped RLS config for pipeline mode.
--
-- Uses @is_local = false@ (session-scoped) instead of @true@ (transaction-local)
-- because pipeline mode runs each statement in its own implicit transaction.
-- The companion 'resetRLSConfigPipelineStatement' resets these at the end of
-- the pipeline batch.
setRLSConfigPipelineStatement :: HasqlStatement.Statement (Text, Text) ()
setRLSConfigPipelineStatement = HasqlStatement.preparable
    "SELECT set_config('role', $1, false), set_config('rls.ihp_user_id', $2, false)"
    (contramap fst (Encoders.param (Encoders.nonNullable Encoders.text))
     <> contramap snd (Encoders.param (Encoders.nonNullable Encoders.text)))
    (Decoders.singleRow (Decoders.column (Decoders.nullable Decoders.text) *> Decoders.column (Decoders.nullable Decoders.text) *> pure ()))

-- | Reset role and RLS user to connection defaults after the pipeline completes.
--
-- Uses @session_user@ to restore the original connection role, matching
-- the behavior of @RESET ROLE@.
resetRLSConfigPipelineStatement :: HasqlStatement.Statement () ()
resetRLSConfigPipelineStatement = HasqlStatement.preparable
    "SELECT set_config('role', session_user::text, false), set_config('rls.ihp_user_id', '', false)"
    conquer
    (Decoders.singleRow (Decoders.column (Decoders.nullable Decoders.text) *> Decoders.column (Decoders.nullable Decoders.text) *> pure ()))
