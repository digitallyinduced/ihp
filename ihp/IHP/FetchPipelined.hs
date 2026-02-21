{-# LANGUAGE ApplicativeDo, BangPatterns, TypeFamilies, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, FlexibleContexts, AllowAmbiguousTypes #-}

{-|
Module: IHP.FetchPipelined
Description: Fetch multiple independent queries in a single database round trip
Copyright: (c) digitally induced GmbH, 2026

Uses PostgreSQL's pipeline mode (via hasql) to send multiple independent queries
in a single network round trip. This is especially beneficial for cloud database
deployments where round-trip latency is 1-5ms.

Compose queries using @do@ notation (via @ApplicativeDo@):

> (users, posts) <- fetchPipelined do
>     users <- query @User |> toPipelineStatement
>     posts <- query @Post |> orderByDesc #createdAt |> toPipelineStatement
>     pure (users, posts)

'Pipeline' is 'Applicative' but NOT 'Monad', which enforces at the type level
that only independent queries can be pipelined. @ApplicativeDo@ desugars the
@do@ block into applicative operations, so each line runs as a separate query
in the same pipeline batch.
-}
module IHP.FetchPipelined
( toPipelineStatement
, toPipelineStatementOneOrNothing
, toPipelineStatementCount
, toPipelineStatementExists
, fetchPipelined
, Pipeline.Pipeline
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Hasql.FromRow (FromRowHasql(..))
import IHP.Fetch.Statement (buildQueryListStatement, buildQueryMaybeStatement, buildCountStatement, buildExistsStatement)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Pipeline as Pipeline
import qualified Hasql.Session as HasqlSession
import qualified Hasql.Statement as HasqlStatement
import qualified Hasql.Pool as HasqlPool
import qualified IHP.Log as Log
import Control.Exception (finally)
import Data.Functor.Contravariant (contramap)
import Data.Functor.Contravariant.Divisible (conquer)

-- | Convert a query builder into a 'Pipeline' step returning all matching rows.
--
-- __Example:__ Fetching users and posts in a single round trip
--
-- > (users, posts) <- fetchPipelined do
-- >     users <- query @User |> filterWhere (#active, True) |> toPipelineStatement
-- >     posts <- query @Post |> orderByDesc #createdAt |> toPipelineStatement
-- >     pure (users, posts)
toPipelineStatement :: forall model table queryBuilderProvider joinRegister.
    ( Table model
    , HasQueryBuilder queryBuilderProvider joinRegister
    , model ~ GetModelByTableName table
    , KnownSymbol table
    , FromRowHasql model
    ) => queryBuilderProvider table -> Pipeline.Pipeline [model]
toPipelineStatement !queryBuilder = Pipeline.statement () (buildQueryListStatement queryBuilder)
{-# INLINE toPipelineStatement #-}

-- | Convert a query builder into a 'Pipeline' step returning at most one row.
--
-- __Example:__
--
-- > (maybeUser, posts) <- fetchPipelined do
-- >     maybeUser <- query @User |> filterWhere (#email, email) |> toPipelineStatementOneOrNothing
-- >     posts <- query @Post |> toPipelineStatement
-- >     pure (maybeUser, posts)
toPipelineStatementOneOrNothing :: forall model table queryBuilderProvider joinRegister.
    ( Table model
    , HasQueryBuilder queryBuilderProvider joinRegister
    , model ~ GetModelByTableName table
    , KnownSymbol table
    , FromRowHasql model
    ) => queryBuilderProvider table -> Pipeline.Pipeline (Maybe model)
toPipelineStatementOneOrNothing !queryBuilder = Pipeline.statement () (buildQueryMaybeStatement queryBuilder)
{-# INLINE toPipelineStatementOneOrNothing #-}

-- | Convert a query builder into a 'Pipeline' step returning a count.
--
-- __Example:__
--
-- > (users, userCount) <- fetchPipelined do
-- >     users <- query @User |> toPipelineStatement
-- >     userCount <- query @User |> filterWhere (#active, True) |> toPipelineStatementCount
-- >     pure (users, userCount)
toPipelineStatementCount :: forall table queryBuilderProvider joinRegister.
    ( KnownSymbol table
    , HasQueryBuilder queryBuilderProvider joinRegister
    ) => queryBuilderProvider table -> Pipeline.Pipeline Int
toPipelineStatementCount !queryBuilder = fromIntegral <$> Pipeline.statement () (buildCountStatement queryBuilder)
{-# INLINE toPipelineStatementCount #-}

-- | Convert a query builder into a 'Pipeline' step returning a boolean.
--
-- __Example:__
--
-- > (users, hasUnread) <- fetchPipelined do
-- >     users <- query @User |> toPipelineStatement
-- >     hasUnread <- query @Message |> filterWhere (#isUnread, True) |> toPipelineStatementExists
-- >     pure (users, hasUnread)
toPipelineStatementExists :: forall table queryBuilderProvider joinRegister.
    ( KnownSymbol table
    , HasQueryBuilder queryBuilderProvider joinRegister
    ) => queryBuilderProvider table -> Pipeline.Pipeline Bool
toPipelineStatementExists !queryBuilder = Pipeline.statement () (buildExistsStatement queryBuilder)
{-# INLINE toPipelineStatementExists #-}

-- | Execute a 'Pipeline' in a single database round trip.
--
-- When row-level security (RLS) is enabled, the pipeline is automatically wrapped
-- with @set_config@ / reset statements to preserve the request's RLS context.
-- These are included in the same pipeline batch, adding no extra round trips.
--
-- __Example:__
--
-- > action DashboardAction = do
-- >     (users, posts, commentCount) <- fetchPipelined do
-- >         users <- query @User |> toPipelineStatement
-- >         posts <- query @Post |> orderByDesc #createdAt |> limit 10 |> toPipelineStatement
-- >         commentCount <- query @Comment |> toPipelineStatementCount
-- >         pure (users, posts, commentCount)
-- >     render DashboardView { .. }
fetchPipelined :: (?modelContext :: ModelContext) => Pipeline.Pipeline a -> IO a
fetchPipelined thePipeline = do
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
                Log.debug ("🔍 Pipeline (" <> tshow queryTimeInMs <> "ms)")
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
