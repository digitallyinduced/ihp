module IHP.DataSync.RowLevelSecurity
( -- * Re-exported from "IHP.RowLevelSecurity"
  ensureRLSEnabled
, hasRLSEnabled
, TableWithRLS (..)
, makeCachedEnsureRLSEnabled
, hasRLSEnabledSession
, ensureRLSEnabledSession
, setRLSConfigStatement
, rlsPolicyColumns
, makeCachedRLSPolicyColumns
  -- * IHP-specific session wrappers
, sqlQueryWithRLS
, sqlQueryWriteWithRLS
, sqlExecWithRLS
, sqlQueryScalarWithRLS
, sqlQueryWithRLSSession
, sqlQueryWriteWithRLSSession
, sqlExecWithRLSSession
, sqlQueryScalarWithRLSSession
, setRLSConfigSession
)
where

import IHP.ControllerPrelude hiding (sqlQuery, sqlExec, sqlQueryScalar, setRLSConfigStatement)
import qualified Hasql.Pool
import qualified Hasql.Session
import qualified Hasql.Statement as Statement
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Tx
import qualified IHP.DataSync.Role as Role
import IHP.DataSync.Hasql (runSession)

-- Re-exports from the dedicated package
import IHP.RowLevelSecurity
    ( TableWithRLS(..)
    , ensureRLSEnabled
    , hasRLSEnabled
    , makeCachedEnsureRLSEnabled
    , hasRLSEnabledSession
    , ensureRLSEnabledSession
    , rlsPolicyColumns
    , makeCachedRLSPolicyColumns
    )
import IHP.RowLevelSecurity.Statement (setRLSConfigStatement)
import qualified IHP.RowLevelSecurity.Session as RLS

-- | Set RLS config (role and user id) in the current session.
--
-- This is a Session-level action for use in user-managed transactions
-- (e.g. after a manual @BEGIN@). Reads the user from 'currentUserOrNothing'.
setRLSConfigSession ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Hasql.Session.Session ()
setRLSConfigSession = RLS.setRLSConfig rlsContext
    where
        rlsContext = RowLevelSecurityContext
            { rlsAuthenticatedRole = Role.authenticatedRole
            , rlsUserId = encodedUserId
            }
        encodedUserId = case (.id) <$> currentUserOrNothing of
            Just userId -> tshow userId
            Nothing -> ""

sqlQueryWithRLSSession ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Statement.Statement () [result] -> Hasql.Session.Session [result]
sqlQueryWithRLSSession statement =
    RLS.withRLS rlsContext Tx.Read (Tx.statement () statement)
    where
        rlsContext = RowLevelSecurityContext
            { rlsAuthenticatedRole = Role.authenticatedRole
            , rlsUserId = encodedUserId
            }
        encodedUserId = case (.id) <$> currentUserOrNothing of
            Just userId -> tshow userId
            Nothing -> ""
{-# INLINE sqlQueryWithRLSSession #-}

-- | Like 'sqlQueryWithRLSSession', but uses a write transaction.
sqlQueryWriteWithRLSSession ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Statement.Statement () [result] -> Hasql.Session.Session [result]
sqlQueryWriteWithRLSSession statement =
    RLS.withRLS rlsContext Tx.Write (Tx.statement () statement)
    where
        rlsContext = RowLevelSecurityContext
            { rlsAuthenticatedRole = Role.authenticatedRole
            , rlsUserId = encodedUserId
            }
        encodedUserId = case (.id) <$> currentUserOrNothing of
            Just userId -> tshow userId
            Nothing -> ""
{-# INLINE sqlQueryWriteWithRLSSession #-}

sqlExecWithRLSSession ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Statement.Statement () () -> Hasql.Session.Session ()
sqlExecWithRLSSession statement =
    RLS.withRLS rlsContext Tx.Write (Tx.statement () statement)
    where
        rlsContext = RowLevelSecurityContext
            { rlsAuthenticatedRole = Role.authenticatedRole
            , rlsUserId = encodedUserId
            }
        encodedUserId = case (.id) <$> currentUserOrNothing of
            Just userId -> tshow userId
            Nothing -> ""
{-# INLINE sqlExecWithRLSSession #-}

sqlQueryScalarWithRLSSession ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Statement.Statement () result -> Hasql.Session.Session result
sqlQueryScalarWithRLSSession statement =
    RLS.withRLS rlsContext Tx.Read (Tx.statement () statement)
    where
        rlsContext = RowLevelSecurityContext
            { rlsAuthenticatedRole = Role.authenticatedRole
            , rlsUserId = encodedUserId
            }
        encodedUserId = case (.id) <$> currentUserOrNothing of
            Just userId -> tshow userId
            Nothing -> ""
{-# INLINE sqlQueryScalarWithRLSSession #-}

-- IO API (thin wrappers)

sqlQueryWithRLS ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Hasql.Pool.Pool -> Statement.Statement () [result] -> IO [result]
sqlQueryWithRLS pool statement = runSession pool (sqlQueryWithRLSSession statement)
{-# INLINE sqlQueryWithRLS #-}

sqlQueryWriteWithRLS ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Hasql.Pool.Pool -> Statement.Statement () [result] -> IO [result]
sqlQueryWriteWithRLS pool statement = runSession pool (sqlQueryWriteWithRLSSession statement)
{-# INLINE sqlQueryWriteWithRLS #-}

sqlExecWithRLS ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Hasql.Pool.Pool -> Statement.Statement () () -> IO ()
sqlExecWithRLS pool statement = runSession pool (sqlExecWithRLSSession statement)
{-# INLINE sqlExecWithRLS #-}

sqlQueryScalarWithRLS ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Hasql.Pool.Pool -> Statement.Statement () result -> IO result
sqlQueryScalarWithRLS pool statement = runSession pool (sqlQueryScalarWithRLSSession statement)
{-# INLINE sqlQueryScalarWithRLS #-}
