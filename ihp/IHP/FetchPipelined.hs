{-# LANGUAGE ApplicativeDo, BangPatterns, TypeFamilies, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, FlexibleContexts, AllowAmbiguousTypes #-}

{-|
Module: IHP.FetchPipelined
Description: Fetch multiple independent queries in a single database round trip
Copyright: (c) digitally induced GmbH, 2026

Uses PostgreSQL's pipeline mode (via hasql) to send multiple independent queries
in a single network round trip. This is especially beneficial for cloud database
deployments where round-trip latency is 1-5ms.

Compose queries using @do@ notation (via @ApplicativeDo@):

> (users, posts) <- pipeline do
>     users <- query @User |> fetchPipelined
>     posts <- query @Post |> orderByDesc #createdAt |> fetchPipelined
>     pure (users, posts)

'Pipeline' is 'Applicative' but NOT 'Monad', which enforces at the type level
that only independent queries can be pipelined. @ApplicativeDo@ desugars the
@do@ block into applicative operations, so each line runs as a separate query
in the same pipeline batch.
-}
module IHP.FetchPipelined
( fetchPipelined
, fetchOneOrNothingPipelined
, fetchCountPipelined
, fetchExistsPipelined
, pipeline
, Pipeline
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Hasql.FromRow (FromRowHasql(..))
import IHP.Fetch.Statement (buildQueryListStatement, buildQueryMaybeStatement, buildCountStatement, buildExistsStatement)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Pipeline as HasqlPipeline
import qualified Hasql.Session as HasqlSession
import qualified Hasql.Statement as HasqlStatement
import qualified Hasql.Pool as HasqlPool
import qualified IHP.Log as Log
import Data.Functor.Contravariant (contramap)
import Data.Functor.Contravariant.Divisible (conquer)

-- | An Applicative pipeline that accumulates table reads for AutoRefresh
-- tracking alongside the underlying hasql pipeline.
--
-- This is 'Applicative' but NOT 'Monad', which enforces at the type level
-- that only independent queries can be pipelined.
data Pipeline a = Pipeline
    { readTables :: ![Text]
    , hasqlPipeline :: !(HasqlPipeline.Pipeline a)
    }

instance Functor Pipeline where
    fmap f (Pipeline tables p) = Pipeline tables (fmap f p)
    {-# INLINE fmap #-}

instance Applicative Pipeline where
    pure a = Pipeline [] (pure a)
    {-# INLINE pure #-}
    Pipeline t1 f <*> Pipeline t2 a = Pipeline (t1 <> t2) (f <*> a)
    {-# INLINE (<*>) #-}

-- | Convert a query builder into a 'Pipeline' step returning all matching rows.
--
-- __Example:__ Fetching users and posts in a single round trip
--
-- > (users, posts) <- pipeline do
-- >     users <- query @User |> filterWhere (#active, True) |> fetchPipelined
-- >     posts <- query @Post |> orderByDesc #createdAt |> fetchPipelined
-- >     pure (users, posts)
fetchPipelined :: forall model table queryBuilderProvider joinRegister.
    ( Table model
    , HasQueryBuilder queryBuilderProvider joinRegister
    , model ~ GetModelByTableName table
    , KnownSymbol table
    , FromRowHasql model
    ) => queryBuilderProvider table -> Pipeline [model]
fetchPipelined !queryBuilder = Pipeline
    { readTables = [tableName @model]
    , hasqlPipeline = HasqlPipeline.statement () (buildQueryListStatement queryBuilder)
    }
{-# INLINE fetchPipelined #-}

-- | Convert a query builder into a 'Pipeline' step returning at most one row.
--
-- __Example:__
--
-- > (maybeUser, posts) <- pipeline do
-- >     maybeUser <- query @User |> filterWhere (#email, email) |> fetchOneOrNothingPipelined
-- >     posts <- query @Post |> fetchPipelined
-- >     pure (maybeUser, posts)
fetchOneOrNothingPipelined :: forall model table queryBuilderProvider joinRegister.
    ( Table model
    , HasQueryBuilder queryBuilderProvider joinRegister
    , model ~ GetModelByTableName table
    , KnownSymbol table
    , FromRowHasql model
    ) => queryBuilderProvider table -> Pipeline (Maybe model)
fetchOneOrNothingPipelined !queryBuilder = Pipeline
    { readTables = [tableName @model]
    , hasqlPipeline = HasqlPipeline.statement () (buildQueryMaybeStatement queryBuilder)
    }
{-# INLINE fetchOneOrNothingPipelined #-}

-- | Convert a query builder into a 'Pipeline' step returning a count.
--
-- __Example:__
--
-- > (users, userCount) <- pipeline do
-- >     users <- query @User |> fetchPipelined
-- >     userCount <- query @User |> filterWhere (#active, True) |> fetchCountPipelined
-- >     pure (users, userCount)
fetchCountPipelined :: forall table queryBuilderProvider joinRegister.
    ( KnownSymbol table
    , HasQueryBuilder queryBuilderProvider joinRegister
    ) => queryBuilderProvider table -> Pipeline Int
fetchCountPipelined !queryBuilder = Pipeline
    { readTables = [symbolToText @table]
    , hasqlPipeline = fromIntegral <$> HasqlPipeline.statement () (buildCountStatement queryBuilder)
    }
{-# INLINE fetchCountPipelined #-}

-- | Convert a query builder into a 'Pipeline' step returning a boolean.
--
-- __Example:__
--
-- > (users, hasUnread) <- pipeline do
-- >     users <- query @User |> fetchPipelined
-- >     hasUnread <- query @Message |> filterWhere (#isUnread, True) |> fetchExistsPipelined
-- >     pure (users, hasUnread)
fetchExistsPipelined :: forall table queryBuilderProvider joinRegister.
    ( KnownSymbol table
    , HasQueryBuilder queryBuilderProvider joinRegister
    ) => queryBuilderProvider table -> Pipeline Bool
fetchExistsPipelined !queryBuilder = Pipeline
    { readTables = [symbolToText @table]
    , hasqlPipeline = HasqlPipeline.statement () (buildExistsStatement queryBuilder)
    }
{-# INLINE fetchExistsPipelined #-}

-- | Execute a 'Pipeline' in a single database round trip.
--
-- When row-level security (RLS) is enabled, the pipeline is automatically wrapped
-- with @set_config@ / reset statements to preserve the request's RLS context.
-- These are included in the same pipeline batch, adding no extra round trips.
--
-- Table reads are automatically tracked for AutoRefresh support.
--
-- __Example:__
--
-- > action DashboardAction = do
-- >     (users, posts, commentCount) <- pipeline do
-- >         users <- query @User |> fetchPipelined
-- >         posts <- query @Post |> orderByDesc #createdAt |> limit 10 |> fetchPipelined
-- >         commentCount <- query @Comment |> fetchCountPipelined
-- >         pure (users, posts, commentCount)
-- >     render DashboardView { .. }
pipeline :: (?modelContext :: ModelContext) => Pipeline a -> IO a
pipeline (Pipeline tables thePipeline) = do
    let pool = ?modelContext.hasqlPool
    -- When RLS is enabled and we're not already in a transaction, wrap the
    -- pipeline with session-scoped set_config/reset statements.  These are
    -- part of the same pipeline batch so they add no extra round trips.
    -- In pipeline mode the server processes statements sequentially, so the
    -- set_config takes effect before the user queries execute.
    let effectivePipeline = case (?modelContext.transactionRunner, ?modelContext.rowLevelSecurity) of
            (Nothing, Just RowLevelSecurityContext { rlsAuthenticatedRole, rlsUserId }) ->
                (\_ a _ -> a)
                    <$> HasqlPipeline.statement (rlsAuthenticatedRole, rlsUserId) setRLSConfigPipelineStatement
                    <*> thePipeline
                    <*> HasqlPipeline.statement () resetRLSConfigPipelineStatement
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
    logQueryTiming currentLogLevel "🔍 Pipeline" do
        mapM_ trackTableRead tables
        runQuery
{-# INLINABLE pipeline #-}

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
