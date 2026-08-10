{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Request-context-independent execution for typed SQL.
--
-- This module exposes ordinary Hasql statements, sessions, pipelines, and pool
-- runners. It can be used by services that manage their own database resources
-- without importing IHP's request-scoped database context.
module IHP.TypedSql.Hasql
    ( typedSql
    , typedSqlStar
    , QueryCardinality (..)
    , QueryExecResult (..)
    , TypedQuery (..)
    , TypedQueryResult
    , SqlExecTypedResult
    , DecodeTypedQuery (..)
    , DecodeTypedExec (..)
    , sqlQueryTypedStatement
    , sqlQueryTypedSession
    , sqlQueryTypedWithPool
    , sqlQueryTypedPipelined
    , sqlQueryTypedMaybeColumnPipelined
    , sqlExecTypedStatement
    , sqlExecTypedSession
    , sqlExecTypedWithPool
    ) where

import           Data.Proxy                      (Proxy (..))
import           GHC.TypeLits                    (ErrorMessage (Text), TypeError)
import qualified Hasql.Decoders                  as HasqlDecoders
import qualified Hasql.DynamicStatements.Snippet as Snippet
import qualified Hasql.Pipeline                  as HasqlPipeline
import qualified Hasql.Pool                      as HasqlPool
import qualified Hasql.Session                   as HasqlSession
import qualified Hasql.Statement                 as HasqlStatement
import           Prelude

import           IHP.TypedSql.Quoter             (typedSql, typedSqlStar)
import           IHP.TypedSql.Types              (QueryCardinality (..), QueryExecResult (..),
                                                  SqlExecTypedResult, TypedQuery (..),
                                                  TypedQueryResult)

class DecodeTypedQuery (cardinality :: QueryCardinality) where
    typedQueryResultDecoder :: Proxy cardinality -> HasqlDecoders.Row result -> HasqlDecoders.Result (TypedQueryResult cardinality result)

instance DecodeTypedQuery 'ManyRows where
    typedQueryResultDecoder _ = HasqlDecoders.rowList

instance DecodeTypedQuery 'AtMostOneRow where
    typedQueryResultDecoder _ = HasqlDecoders.rowMaybe

instance DecodeTypedQuery 'ExactlyOneRow where
    typedQueryResultDecoder _ = HasqlDecoders.singleRow

class DecodeTypedExec (execResult :: QueryExecResult) where
    typedExecResultDecoder :: Proxy execResult -> HasqlDecoders.Result (SqlExecTypedResult execResult)

instance DecodeTypedExec 'ReturnsAffectedRows where
    typedExecResultDecoder _ = HasqlDecoders.rowsAffected

instance DecodeTypedExec 'ReturnsNoResult where
    typedExecResultDecoder _ = HasqlDecoders.noResult

instance TypeError ('Text "sqlExecTyped cannot run SQL statements that return rows. Use sqlQueryTyped instead.") => DecodeTypedExec 'ReturnsRows where
    typedExecResultDecoder _ = error "unreachable"

-- | Convert a typed row-returning query to a normal Hasql 'HasqlStatement.Statement'.
sqlQueryTypedStatement :: forall cardinality result. DecodeTypedQuery cardinality => TypedQuery cardinality 'ReturnsRows result -> HasqlStatement.Statement () (TypedQueryResult cardinality result)
sqlQueryTypedStatement TypedQuery { tqSnippet, tqResultDecoder } =
    Snippet.toPreparableStatement tqSnippet (typedQueryResultDecoder (Proxy :: Proxy cardinality) tqResultDecoder)

-- | Convert a typed row-returning query to a normal Hasql 'HasqlSession.Session'.
sqlQueryTypedSession :: DecodeTypedQuery cardinality => TypedQuery cardinality 'ReturnsRows result -> HasqlSession.Session (TypedQueryResult cardinality result)
sqlQueryTypedSession query =
    HasqlSession.statement () (sqlQueryTypedStatement query)

-- | Run a typed row-returning query on an explicit Hasql pool.
--
-- This has the same error semantics as 'HasqlPool.use'.
sqlQueryTypedWithPool :: DecodeTypedQuery cardinality => HasqlPool.Pool -> TypedQuery cardinality 'ReturnsRows result -> IO (Either HasqlPool.UsageError (TypedQueryResult cardinality result))
sqlQueryTypedWithPool pool query =
    HasqlPool.use pool (sqlQueryTypedSession query)

-- | Pipeline variant of 'sqlQueryTypedStatement'.
sqlQueryTypedPipelined :: forall cardinality result. DecodeTypedQuery cardinality => TypedQuery cardinality 'ReturnsRows result -> HasqlPipeline.Pipeline (TypedQueryResult cardinality result)
sqlQueryTypedPipelined query =
    HasqlPipeline.statement () (sqlQueryTypedStatement query)

-- | Flatten the nullable-column result of an at-most-one-row query in a
-- pipeline.
sqlQueryTypedMaybeColumnPipelined :: TypedQuery 'AtMostOneRow 'ReturnsRows (Maybe result) -> HasqlPipeline.Pipeline (Maybe result)
sqlQueryTypedMaybeColumnPipelined query =
    flatten <$> sqlQueryTypedPipelined query
  where
    flatten = \case
        Nothing -> Nothing
        Just inner -> inner

-- | Convert a typed non-row-returning statement to a normal Hasql
-- 'HasqlStatement.Statement'.
sqlExecTypedStatement :: forall cardinality execResult result. DecodeTypedExec execResult => TypedQuery cardinality execResult result -> HasqlStatement.Statement () (SqlExecTypedResult execResult)
sqlExecTypedStatement TypedQuery { tqSnippet } =
    Snippet.toPreparableStatement tqSnippet (typedExecResultDecoder (Proxy :: Proxy execResult))

-- | Convert a typed non-row-returning statement to a normal Hasql
-- 'HasqlSession.Session'.
sqlExecTypedSession :: DecodeTypedExec execResult => TypedQuery cardinality execResult result -> HasqlSession.Session (SqlExecTypedResult execResult)
sqlExecTypedSession query =
    HasqlSession.statement () (sqlExecTypedStatement query)

-- | Run a typed non-row-returning statement on an explicit Hasql pool.
--
-- This has the same error semantics as 'HasqlPool.use'.
sqlExecTypedWithPool :: DecodeTypedExec execResult => HasqlPool.Pool -> TypedQuery cardinality execResult result -> IO (Either HasqlPool.UsageError (SqlExecTypedResult execResult))
sqlExecTypedWithPool pool query =
    HasqlPool.use pool (sqlExecTypedSession query)
