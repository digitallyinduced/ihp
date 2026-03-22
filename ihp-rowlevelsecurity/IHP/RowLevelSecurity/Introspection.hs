module IHP.RowLevelSecurity.Introspection
( hasRLSEnabled
, ensureRLSEnabled
, makeCachedEnsureRLSEnabled
, rlsPolicyColumns
, makeCachedRLSPolicyColumns
) where

import Prelude
import Control.Exception (throwIO)
import Data.IORef
import Data.Text (Text)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import IHP.RowLevelSecurity.Types
import IHP.RowLevelSecurity.Session (hasRLSEnabledSession, ensureRLSEnabledSession)
import IHP.RowLevelSecurity.Statement (rlsPolicyColumnsStatement)

-- | Check whether a table has RLS enabled.
--
-- >>> hasRLSEnabled pool "my_table"
-- True
hasRLSEnabled :: Pool.Pool -> Text -> IO Bool
hasRLSEnabled pool table = runSession pool (hasRLSEnabledSession table)

-- | Check that a table has RLS enabled, returning a 'TableWithRLS' proof.
-- Throws an error if RLS is not enabled.
ensureRLSEnabled :: Pool.Pool -> Text -> IO TableWithRLS
ensureRLSEnabled pool table = runSession pool (ensureRLSEnabledSession table)

-- | Returns a memoizing wrapper around 'ensureRLSEnabled'.
--
-- Once a table has been verified to have RLS, subsequent checks for the
-- same table return instantly without hitting the database. Tables that
-- fail the check are not cached (they may have RLS enabled later).
--
-- > ensureRLS <- makeCachedEnsureRLSEnabled pool
-- > ensureRLS "projects"  -- hits database
-- > ensureRLS "projects"  -- instant, cached
makeCachedEnsureRLSEnabled :: Pool.Pool -> IO (Text -> IO TableWithRLS)
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

-- | Returns the set of column names referenced in a table's RLS policies.
--
-- >>> rlsPolicyColumns pool "messages"
-- fromList ["user_id"]
rlsPolicyColumns :: Pool.Pool -> Text -> IO (Set.Set Text)
rlsPolicyColumns pool table = do
    results <- runSession pool (Session.statement table rlsPolicyColumnsStatement)
    pure (Set.fromList results)

-- | Returns a cached version of 'rlsPolicyColumns'.
--
-- Queries once per table, caches forever for the pool lifetime.
makeCachedRLSPolicyColumns :: Pool.Pool -> IO (Text -> IO (Set.Set Text))
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

-- | Run a session on the pool, throwing on failure.
runSession :: Pool.Pool -> Session.Session a -> IO a
runSession pool session = do
    result <- Pool.use pool session
    case result of
        Left err -> throwIO err
        Right val -> pure val
