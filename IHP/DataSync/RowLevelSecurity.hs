module IHP.DataSync.RowLevelSecurity where

import IHP.ControllerPrelude
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.ToField as PG


import Network.HTTP.Types (status400)

withRLS :: forall userId result.
    ( PG.ToField userId
    , userId ~ Id CurrentUserRecord
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , ?context :: ControllerContext
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    , ?modelContext :: ModelContext
    ) => ((?modelContext :: ModelContext) => IO result) -> IO result
withRLS callback = withTransaction inner
    where
        -- The inner call is required here as we need to capture the right ?modelContext
        -- from withTransaction, otherwise the wrong database connection is used
        inner :: (?modelContext :: ModelContext) => IO result
        inner = do
            let maybeUserId :: Maybe userId = get #id <$> currentUserOrNothing
            sqlExec "SET LOCAL ROLE api" ()
            sqlExec "SET LOCAL rls.ihp_user_id = ?" (PG.Only maybeUserId)
            callback

ensureRLSEnabled :: (?modelContext :: ModelContext) => Text -> IO ()
ensureRLSEnabled table = do
    rlsEnabled <- hasRLSEnabled table
    unless rlsEnabled (error "Row level security is required for accessing this table")

-- | Returns 'True' if row level security has been enabled on a table
--
-- RLS can be enabled with this SQL statement:
--
-- > ALTER TABLE my_table ENABLE ROW LEVEL SECURITY;
--
-- After this 'hasRLSEnabled' will return true:
--
-- >>> hasRLSEnabled "my_table"
-- True
hasRLSEnabled :: (?modelContext :: ModelContext) => Text -> IO Bool
hasRLSEnabled table = sqlQueryScalar "SELECT relrowsecurity FROM pg_class WHERE oid = ?::regclass" [table]
