module IHP.DataSync.RowLevelSecurity
( ensureRLSEnabled
, hasRLSEnabled
, TableWithRLS (tableName)
, makeCachedEnsureRLSEnabled
, sqlQueryWithRLS
, sqlQueryWriteWithRLS
, sqlExecWithRLS
, sqlQueryScalarWithRLS
, hasRLSEnabledSession
, ensureRLSEnabledSession
, sqlQueryWithRLSSession
, sqlQueryWriteWithRLSSession
, sqlExecWithRLSSession
, sqlQueryScalarWithRLSSession
, setRLSConfigStatement
, setRLSConfigSession
)
where

import IHP.ControllerPrelude hiding (sqlQuery, sqlExec, sqlQueryScalar)
import qualified Hasql.Pool
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Statement as DynStatement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Statement as Statement
import qualified Hasql.Session as Session
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Tx
import qualified IHP.DataSync.Role as Role
import qualified Data.Set as Set
import IHP.DataSync.Hasql (runSession)
import Data.Functor.Contravariant (contramap)

-- Statements

hasRLSEnabledStatement :: Statement.Statement Text Bool
hasRLSEnabledStatement = Statement.Statement
    "SELECT relrowsecurity FROM pg_class WHERE oid = quote_ident($1)::regclass"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
    True

-- | Prepared statement that sets the RLS role and user id using set_config().
--
-- Uses @set_config(setting, value, is_local)@ which is a regular SQL function
-- that supports parameterized values in the extended query protocol, unlike
-- @SET LOCAL@ which is a utility command that cannot be parameterized.
--
-- The third argument @true@ makes the setting local to the current transaction,
-- equivalent to @SET LOCAL@.
setRLSConfigStatement :: Statement.Statement (Text, Text) ()
setRLSConfigStatement = Statement.Statement
    "SELECT set_config('role', $1, true), set_config('rls.ihp_user_id', $2, true)"
    (contramap fst (Encoders.param (Encoders.nonNullable Encoders.text))
     <> contramap snd (Encoders.param (Encoders.nonNullable Encoders.text)))
    Decoders.noResult
    True

-- Sessions

hasRLSEnabledSession :: Text -> Session.Session Bool
hasRLSEnabledSession table = Session.statement table hasRLSEnabledStatement

ensureRLSEnabledSession :: Text -> Session.Session TableWithRLS
ensureRLSEnabledSession table = do
    rlsEnabled <- hasRLSEnabledSession table
    unless rlsEnabled (error "Row level security is required for accessing this table")
    pure (TableWithRLS table)

-- | Set RLS config (role and user id) in the current transaction.
--
-- This is a Session-level action for use in user-managed transactions
-- (e.g. after a manual @BEGIN@).
setRLSConfigSession ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Session.Session ()
setRLSConfigSession = Session.statement (Role.authenticatedRole, encodedUserId) setRLSConfigStatement
    where
        encodedUserId = case (.id) <$> currentUserOrNothing of
            Just userId -> tshow userId
            Nothing -> ""

sqlQueryWithRLSSession ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Snippet -> Decoders.Result [result] -> Session.Session [result]
sqlQueryWithRLSSession snippet decoder =
    Tx.transaction Tx.ReadCommitted Tx.Read $ do
        Tx.statement (Role.authenticatedRole, encodedUserId) setRLSConfigStatement
        Tx.statement () (DynStatement.dynamicallyParameterized snippet decoder True)
    where
        encodedUserId = case (.id) <$> currentUserOrNothing of
            Just userId -> tshow userId
            Nothing -> ""
{-# INLINE sqlQueryWithRLSSession #-}

-- | Like 'sqlQueryWithRLSSession', but uses a write transaction.
--
-- Use this for INSERT, UPDATE, or DELETE statements with RETURNING that need
-- to return results (e.g. wrapped with 'wrapDynamicQuery').
sqlQueryWriteWithRLSSession ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Snippet -> Decoders.Result [result] -> Session.Session [result]
sqlQueryWriteWithRLSSession snippet decoder =
    Tx.transaction Tx.ReadCommitted Tx.Write $ do
        Tx.statement (Role.authenticatedRole, encodedUserId) setRLSConfigStatement
        Tx.statement () (DynStatement.dynamicallyParameterized snippet decoder True)
    where
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
    ) => Snippet -> Session.Session ()
sqlExecWithRLSSession snippet =
    Tx.transaction Tx.ReadCommitted Tx.Write $ do
        Tx.statement (Role.authenticatedRole, encodedUserId) setRLSConfigStatement
        Tx.statement () (DynStatement.dynamicallyParameterized snippet Decoders.noResult True)
    where
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
    ) => Snippet -> Decoders.Result result -> Session.Session result
sqlQueryScalarWithRLSSession snippet decoder =
    Tx.transaction Tx.ReadCommitted Tx.Read $ do
        Tx.statement (Role.authenticatedRole, encodedUserId) setRLSConfigStatement
        Tx.statement () (DynStatement.dynamicallyParameterized snippet decoder True)
    where
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
    ) => Hasql.Pool.Pool -> Snippet -> Decoders.Result [result] -> IO [result]
sqlQueryWithRLS pool snippet decoder = runSession pool (sqlQueryWithRLSSession snippet decoder)
{-# INLINE sqlQueryWithRLS #-}

-- | Like 'sqlQueryWithRLS', but uses a write transaction.
--
-- Use this for INSERT, UPDATE, or DELETE statements with RETURNING that need
-- to return results (e.g. wrapped with 'wrapDynamicQuery').
sqlQueryWriteWithRLS ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Hasql.Pool.Pool -> Snippet -> Decoders.Result [result] -> IO [result]
sqlQueryWriteWithRLS pool snippet decoder = runSession pool (sqlQueryWriteWithRLSSession snippet decoder)
{-# INLINE sqlQueryWriteWithRLS #-}

sqlExecWithRLS ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Hasql.Pool.Pool -> Snippet -> IO ()
sqlExecWithRLS pool snippet = runSession pool (sqlExecWithRLSSession snippet)
{-# INLINE sqlExecWithRLS #-}

sqlQueryScalarWithRLS ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Hasql.Pool.Pool -> Snippet -> Decoders.Result result -> IO result
sqlQueryScalarWithRLS pool snippet decoder = runSession pool (sqlQueryScalarWithRLSSession snippet decoder)
{-# INLINE sqlQueryScalarWithRLS #-}

-- | Returns a proof that RLS is enabled for a table
ensureRLSEnabled :: Hasql.Pool.Pool -> Text -> IO TableWithRLS
ensureRLSEnabled pool table = runSession pool (ensureRLSEnabledSession table)

-- | Returns a factory for 'ensureRLSEnabled' that memoizes when a table has RLS enabled.
--
-- When a table doesn't have RLS enabled yet, the result is not memoized.
--
-- __Example:__
--
-- > -- Setup
-- > ensureRLSEnabled <- makeCachedEnsureRLSEnabled hasqlPool
-- >
-- > ensureRLSEnabled "projects" -- Runs a database query to check if row level security is enabled for the projects table
-- >
-- > -- Asuming 'ensureRLSEnabled "projects"' proceeded without errors:
-- >
-- > ensureRLSEnabled "projects" -- Now this will instantly return True and don't fire any SQL queries anymore
--
makeCachedEnsureRLSEnabled :: Hasql.Pool.Pool -> IO (Text -> IO TableWithRLS)
makeCachedEnsureRLSEnabled pool = do
    tables <- newIORef Set.empty
    pure \tableName -> do
        rlsEnabled <- Set.member tableName <$> readIORef tables

        if rlsEnabled
            then pure TableWithRLS { tableName }
            else do
                proof <- ensureRLSEnabled pool tableName
                modifyIORef' tables (Set.insert tableName)
                pure proof

-- | Returns 'True' if row level security has been enabled on a table
--
-- RLS can be enabled with this SQL statement:
--
-- > ALTER TABLE my_table ENABLE ROW LEVEL SECURITY;
--
-- After this 'hasRLSEnabled' will return true:
--
-- >>> hasRLSEnabled pool "my_table"
-- True
hasRLSEnabled :: Hasql.Pool.Pool -> Text -> IO Bool
hasRLSEnabled pool table = runSession pool (hasRLSEnabledSession table)

-- | Can be constructed using 'ensureRLSEnabled'
--
-- > tableWithRLS <- ensureRLSEnabled "my_table"
--
-- Useful to carry a proof that the RLS is actually enabled
newtype TableWithRLS = TableWithRLS { tableName :: Text } deriving (Eq, Ord)
