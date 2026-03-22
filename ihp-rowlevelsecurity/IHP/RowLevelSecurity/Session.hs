module IHP.RowLevelSecurity.Session
( setRLSConfig
, withRLS
, withRLSPipeline
, hasRLSEnabledSession
, ensureRLSEnabledSession
) where

import Prelude
import Data.Text (Text)
import IHP.RowLevelSecurity.Types
import IHP.RowLevelSecurity.Statement
import qualified Hasql.Session as Session
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Tx
import qualified Hasql.Pipeline as Pipeline
import qualified Hasql.Statement as Statement

-- | Set the RLS role and user id in the current session.
--
-- Use this after a manual @BEGIN@ (e.g. in 'withTransaction') to activate
-- RLS for all subsequent statements in the transaction.
setRLSConfig :: RowLevelSecurityContext -> Session.Session ()
setRLSConfig ctx = Session.statement (ctx.rlsAuthenticatedRole, ctx.rlsUserId) setRLSConfigStatement

-- | Wrap a transaction body with RLS config.
--
-- Opens a @ReadCommitted@ transaction in the given mode, sets the RLS role
-- and user id, then runs the provided transaction body.
--
-- > withRLS ctx Tx.Read $ do
-- >     Tx.statement input myStatement
withRLS :: RowLevelSecurityContext -> Tx.Mode -> Tx.Transaction a -> Session.Session a
withRLS ctx mode tx = Tx.transaction Tx.ReadCommitted mode $ do
    Tx.statement (ctx.rlsAuthenticatedRole, ctx.rlsUserId) setRLSConfigStatement
    tx

-- | Sandwich a pipeline with RLS set/reset statements.
--
-- Uses session-scoped config (not transaction-local) because each pipelined
-- statement runs in its own implicit transaction. The reset at the end
-- restores the connection to its default role.
--
-- > withRLSPipeline ctx $ do
-- >     result1 <- Pipeline.statement input1 stmt1
-- >     result2 <- Pipeline.statement input2 stmt2
-- >     pure (result1, result2)
withRLSPipeline :: RowLevelSecurityContext -> Pipeline.Pipeline a -> Pipeline.Pipeline a
withRLSPipeline ctx p =
    (\_ a _ -> a)
        <$> Pipeline.statement (ctx.rlsAuthenticatedRole, ctx.rlsUserId) setRLSConfigPipelineStatement
        <*> p
        <*> Pipeline.statement () resetRLSConfigPipelineStatement

-- | Check whether a table has RLS enabled.
hasRLSEnabledSession :: Text -> Session.Session Bool
hasRLSEnabledSession table = Session.statement table hasRLSEnabledStatement

-- | Check that a table has RLS enabled, returning a 'TableWithRLS' proof.
-- Throws an error if RLS is not enabled.
ensureRLSEnabledSession :: Text -> Session.Session TableWithRLS
ensureRLSEnabledSession table = do
    rlsEnabled <- hasRLSEnabledSession table
    if rlsEnabled
        then pure (TableWithRLS table)
        else error "Row level security is required for accessing this table"
