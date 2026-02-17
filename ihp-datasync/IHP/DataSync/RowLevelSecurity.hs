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
, rlsPolicyColumns
, makeCachedRLSPolicyColumns
)
where

import IHP.ControllerPrelude hiding (sqlQuery, sqlExec, sqlQueryScalar)
import qualified Hasql.Pool
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Statement as Statement
import qualified Hasql.Session as Session
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Tx
import qualified IHP.DataSync.Role as Role
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import IHP.DataSync.Hasql (runSession)

-- Statements

hasRLSEnabledStatement :: Statement.Statement Text Bool
hasRLSEnabledStatement = Statement.preparable
    "SELECT relrowsecurity FROM pg_class WHERE oid = quote_ident($1)::regclass"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))

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
    ) => Statement.Statement () [result] -> Session.Session [result]
sqlQueryWithRLSSession statement =
    Tx.transaction Tx.ReadCommitted Tx.Read $ do
        Tx.statement (Role.authenticatedRole, encodedUserId) setRLSConfigStatement
        Tx.statement () statement
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
    ) => Statement.Statement () [result] -> Session.Session [result]
sqlQueryWriteWithRLSSession statement =
    Tx.transaction Tx.ReadCommitted Tx.Write $ do
        Tx.statement (Role.authenticatedRole, encodedUserId) setRLSConfigStatement
        Tx.statement () statement
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
    ) => Statement.Statement () () -> Session.Session ()
sqlExecWithRLSSession statement =
    Tx.transaction Tx.ReadCommitted Tx.Write $ do
        Tx.statement (Role.authenticatedRole, encodedUserId) setRLSConfigStatement
        Tx.statement () statement
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
    ) => Statement.Statement () result -> Session.Session result
sqlQueryScalarWithRLSSession statement =
    Tx.transaction Tx.ReadCommitted Tx.Read $ do
        Tx.statement (Role.authenticatedRole, encodedUserId) setRLSConfigStatement
        Tx.statement () statement
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
    ) => Hasql.Pool.Pool -> Statement.Statement () [result] -> IO [result]
sqlQueryWithRLS pool statement = runSession pool (sqlQueryWithRLSSession statement)
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

-- | Prepared statement to query which columns a table's RLS policies reference.
--
-- Checks both @USING@ (@polqual@) and @WITH CHECK@ (@polwithcheck@) expressions.
rlsPolicyColumnsStatement :: Statement.Statement Text [Text]
rlsPolicyColumnsStatement = Statement.preparable
    "SELECT DISTINCT a.attname::text FROM pg_policy p JOIN pg_class c ON c.oid = p.polrelid JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum > 0 WHERE c.relname = $1 AND (pg_get_expr(p.polqual, p.polrelid) LIKE '%' || a.attname || '%' OR pg_get_expr(p.polwithcheck, p.polrelid) LIKE '%' || a.attname || '%')"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.text)))

-- | Returns the set of column names referenced in a table's RLS policies.
--
-- >>> rlsPolicyColumns pool "messages"
-- fromList ["user_id"]
rlsPolicyColumns :: Hasql.Pool.Pool -> Text -> IO (Set.Set Text)
rlsPolicyColumns pool table = do
    results <- runSession pool (Session.statement table rlsPolicyColumnsStatement)
    pure (Set.fromList results)

-- | Returns a cached version of 'rlsPolicyColumns'.
--
-- Queries once per table, caches forever for the connection lifetime.
makeCachedRLSPolicyColumns :: Hasql.Pool.Pool -> IO (Text -> IO (Set.Set Text))
makeCachedRLSPolicyColumns pool = do
    cache <- newIORef HashMap.empty
    pure \tableName -> do
        cached <- HashMap.lookup tableName <$> readIORef cache
        case cached of
            Just columns -> pure columns
            Nothing -> do
                columns <- rlsPolicyColumns pool tableName
                modifyIORef' cache (HashMap.insert tableName columns)
                pure columns
