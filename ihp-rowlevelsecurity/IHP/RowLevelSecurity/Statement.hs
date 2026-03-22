module IHP.RowLevelSecurity.Statement
( setRLSConfigStatement
, setRLSConfigPipelineStatement
, resetRLSConfigPipelineStatement
, hasRLSEnabledStatement
, rlsPolicyColumnsStatement
, doesRoleExistStatement
) where

import Prelude
import Data.Functor.Contravariant (contramap)
import Data.Functor.Contravariant.Divisible (conquer)
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Statement as Statement

-- | Set the PostgreSQL role and @rls.ihp_user_id@ GUC for the current transaction.
--
-- Uses @set_config(setting, value, is_local)@ with @is_local = true@,
-- making the settings transaction-local (equivalent to @SET LOCAL@).
-- The role switch ensures queries are subject to RLS policies.
--
-- Input: @(authenticatedRole, userId)@
setRLSConfigStatement :: Statement.Statement (Text, Text) ()
setRLSConfigStatement = Statement.preparable
    "SELECT set_config('role', $1, true), set_config('rls.ihp_user_id', $2, true)"
    (contramap fst (Encoders.param (Encoders.nonNullable Encoders.text))
     <> contramap snd (Encoders.param (Encoders.nonNullable Encoders.text)))
    setConfigDecoder

-- | Session-scoped RLS config for pipeline mode.
--
-- Uses @is_local = false@ because pipeline mode runs each statement in its own
-- implicit transaction, so transaction-local settings would not carry across
-- statements. Must be paired with 'resetRLSConfigPipelineStatement'.
--
-- Input: @(authenticatedRole, userId)@
setRLSConfigPipelineStatement :: Statement.Statement (Text, Text) ()
setRLSConfigPipelineStatement = Statement.preparable
    "SELECT set_config('role', $1, false), set_config('rls.ihp_user_id', $2, false)"
    (contramap fst (Encoders.param (Encoders.nonNullable Encoders.text))
     <> contramap snd (Encoders.param (Encoders.nonNullable Encoders.text)))
    setConfigDecoder

-- | Reset role and RLS user to connection defaults after a pipeline completes.
--
-- Uses @session_user@ to restore the original connection role, matching
-- the behavior of @RESET ROLE@.
resetRLSConfigPipelineStatement :: Statement.Statement () ()
resetRLSConfigPipelineStatement = Statement.preparable
    "SELECT set_config('role', session_user::text, false), set_config('rls.ihp_user_id', '', false)"
    conquer
    setConfigDecoder

-- | Check whether a table has RLS enabled via @pg_class.relrowsecurity@.
hasRLSEnabledStatement :: Statement.Statement Text Bool
hasRLSEnabledStatement = Statement.preparable
    "SELECT relrowsecurity FROM pg_class WHERE oid = quote_ident($1)::regclass"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))

-- | Query which columns a table's RLS policies reference.
--
-- Checks both @USING@ (@polqual@) and @WITH CHECK@ (@polwithcheck@) expressions
-- by introspecting @pg_policy@ and @pg_attribute@.
rlsPolicyColumnsStatement :: Statement.Statement Text [Text]
rlsPolicyColumnsStatement = Statement.preparable
    "SELECT DISTINCT a.attname::text FROM pg_policy p JOIN pg_class c ON c.oid = p.polrelid JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum > 0 WHERE c.relname = $1 AND (pg_get_expr(p.polqual, p.polrelid) LIKE '%' || a.attname || '%' OR pg_get_expr(p.polwithcheck, p.polrelid) LIKE '%' || a.attname || '%')"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.text)))

-- | Check whether a PostgreSQL role exists via @pg_roles@.
doesRoleExistStatement :: Statement.Statement Text Bool
doesRoleExistStatement = Statement.preparable
    "SELECT EXISTS(SELECT 1 FROM pg_roles WHERE rolname = $1 LIMIT 1)"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))

-- Internal: shared decoder for set_config() results (two text columns, discarded)
setConfigDecoder :: Decoders.Result ()
setConfigDecoder = Decoders.singleRow
    (Decoders.column (Decoders.nullable Decoders.text)
     *> Decoders.column (Decoders.nullable Decoders.text)
     *> pure ())
