{-|
Module: IHP.DataSync.Role
Description: Postgres role management for RLS
Copyright: (c) digitally induced GmbH, 2021

The default user that creates a table in postgres always
has access to all rows inside the table. The default user is not restricted
to the RLS policies.

Therefore we need to use a second role whenever we want to
make a query with RLS enabled. Basically for every query we do, we'll
wrap it in a transaction and then use 'SET LOCAL ROLE ..' to switch to
our second role for the duration of the transaction.

-}
module IHP.DataSync.Role
( -- * Re-exported from "IHP.RowLevelSecurity.Role"
  ensureAuthenticatedRole
, createAuthenticatedRole
, grantPermissions
, quoteIdentifier
  -- * IHP-specific wrappers
, authenticatedRole
, ensureAuthenticatedRoleExists
, ensureAuthenticatedRoleExistsWithRole
, ensureAuthenticatedRoleSession
) where

import IHP.Prelude
import IHP.FrameworkConfig
import qualified Hasql.Pool
import qualified Hasql.Session as Session
import IHP.DataSync.Hasql (runSession)

-- Re-exports from the dedicated package
import IHP.RowLevelSecurity.Role
    ( ensureAuthenticatedRole
    , createAuthenticatedRole
    , grantPermissions
    , quoteIdentifier
    )

-- | Idempotent: ensure the role exists and has permissions.
-- Reads the role name from 'FrameworkConfig'.
ensureAuthenticatedRoleSession :: (?context :: context, ConfigProvider context) => Session.Session ()
ensureAuthenticatedRoleSession = ensureAuthenticatedRole authenticatedRole

-- | IO wrapper using the pool. Reads the role name from 'FrameworkConfig'.
ensureAuthenticatedRoleExists :: (?context :: context, ConfigProvider context) => Hasql.Pool.Pool -> IO ()
ensureAuthenticatedRoleExists pool = runSession pool ensureAuthenticatedRoleSession

-- | Like 'ensureAuthenticatedRoleExists', but takes the role name directly
-- instead of reading it from the framework config. Used by 'initHasqlPoolWithRLS'
-- at config-build time when 'FrameworkConfig' is not yet available.
ensureAuthenticatedRoleExistsWithRole :: Hasql.Pool.Pool -> Text -> IO ()
ensureAuthenticatedRoleExistsWithRole pool role = runSession pool (ensureAuthenticatedRole role)

-- | The authenticated role name from 'FrameworkConfig'.
authenticatedRole :: (?context :: context, ConfigProvider context) => Text
authenticatedRole = ?context.frameworkConfig.rlsAuthenticatedRole
