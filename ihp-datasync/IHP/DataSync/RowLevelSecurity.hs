module IHP.DataSync.RowLevelSecurity
( ensureRLSEnabled
, hasRLSEnabled
, TableWithRLS (tableName)
, makeCachedEnsureRLSEnabled
, sqlQueryWithRLS
, sqlExecWithRLS
, sqlQueryScalarWithRLS
, wrapStatementWithRLS
)
where

import IHP.ControllerPrelude hiding (sqlQuery, sqlExec, sqlQueryScalar)
import qualified Control.Exception.Safe as Exception
import qualified Hasql.Connection as Hasql
import qualified Hasql.Connection.Setting as HasqlSetting
import qualified Hasql.Connection.Setting.Connection as HasqlConnection
import qualified Hasql.Session as Hasql
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Session as DynSession
import qualified Hasql.Decoders as Decoders
import qualified IHP.DataSync.Role as Role
import qualified Data.Set as Set

-- | Acquire a temporary hasql connection, run a snippet, release the connection.
runHasql :: (?modelContext :: ModelContext) => Snippet -> Decoders.Result a -> IO a
runHasql snippet decoder = do
    let settings = [HasqlSetting.connection (HasqlConnection.string (cs ?modelContext.databaseUrl))]
    Exception.bracket
        (Hasql.acquire settings >>= either (\err -> Exception.throwIO (userError (cs $ tshow err))) pure)
        Hasql.release
        (\conn -> do
            result <- Hasql.run (DynSession.dynamicallyParameterizedStatement snippet decoder True) conn
            case result of
                Left err -> Exception.throwIO (userError (cs $ tshow err))
                Right val -> pure val
        )
{-# INLINE runHasql #-}

sqlQueryWithRLS ::
    ( ?modelContext :: ModelContext
    , ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Snippet -> Decoders.Result [result] -> IO [result]
sqlQueryWithRLS snippet decoder = runHasql queryWithRLS decoder
    where
        queryWithRLS = wrapStatementWithRLS snippet
{-# INLINE sqlQueryWithRLS #-}

sqlExecWithRLS ::
    ( ?modelContext :: ModelContext
    , ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Snippet -> IO ()
sqlExecWithRLS snippet = runHasql queryWithRLS Decoders.noResult
    where
        queryWithRLS = wrapStatementWithRLS snippet
{-# INLINE sqlExecWithRLS #-}

sqlQueryScalarWithRLS ::
    ( ?modelContext :: ModelContext
    , ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Snippet -> Decoders.Result result -> IO result
sqlQueryScalarWithRLS snippet decoder = runHasql queryWithRLS decoder
    where
        queryWithRLS = wrapStatementWithRLS snippet
{-# INLINE sqlQueryScalarWithRLS #-}

wrapStatementWithRLS ::
    ( ?modelContext :: ModelContext
    , ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Snippet -> Snippet
wrapStatementWithRLS snippet = "SET LOCAL ROLE " <> Snippet.param (Role.authenticatedRole) <> "; SET LOCAL rls.ihp_user_id = " <> encodedUserId <> "; " <> snippet <> ";"
    where
        maybeUserId = (.id) <$> currentUserOrNothing

        -- When the user is not logged in and maybeUserId is Nothing, we cannot
        -- just pass @NULL@ to postgres. The @SET LOCAL@ values can only be strings.
        --
        -- Therefore we map Nothing to an empty string here. The empty string
        -- means "not logged in".
        --
        encodedUserId = case maybeUserId of
                Just userId -> Snippet.param (tshow userId)
                Nothing -> Snippet.param ("" :: Text)
{-# INLINE wrapStatementWithRLS #-}

-- | Returns a proof that RLS is enabled for a table
ensureRLSEnabled :: (?modelContext :: ModelContext) => Text -> IO TableWithRLS
ensureRLSEnabled table = do
    rlsEnabled <- hasRLSEnabled table
    unless rlsEnabled (error "Row level security is required for accessing this table")
    pure (TableWithRLS table)

-- | Returns a factory for 'ensureRLSEnabled' that memoizes when a table has RLS enabled.
--
-- When a table doesn't have RLS enabled yet, the result is not memoized.
--
-- __Example:__
--
-- > -- Setup
-- > ensureRLSEnabled <- makeCachedEnsureRLSEnabled
-- >
-- > ensureRLSEnabled "projects" -- Runs a database query to check if row level security is enabled for the projects table
-- >
-- > -- Asuming 'ensureRLSEnabled "projects"' proceeded without errors:
-- >
-- > ensureRLSEnabled "projects" -- Now this will instantly return True and don't fire any SQL queries anymore
--
makeCachedEnsureRLSEnabled :: (?modelContext :: ModelContext) => IO (Text -> IO TableWithRLS)
makeCachedEnsureRLSEnabled = do
    tables <- newIORef Set.empty
    pure \tableName -> do
        rlsEnabled <- Set.member tableName <$> readIORef tables

        if rlsEnabled
            then pure TableWithRLS { tableName }
            else do
                proof <- ensureRLSEnabled tableName
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
-- >>> hasRLSEnabled "my_table"
-- True
hasRLSEnabled :: (?modelContext :: ModelContext) => Text -> IO Bool
hasRLSEnabled table = runHasql ("SELECT relrowsecurity FROM pg_class WHERE oid = quote_ident(" <> Snippet.param table <> ")::regclass") (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))

-- | Can be constructed using 'ensureRLSEnabled'
--
-- > tableWithRLS <- ensureRLSEnabled "my_table"
--
-- Useful to carry a proof that the RLS is actually enabled
newtype TableWithRLS = TableWithRLS { tableName :: Text } deriving (Eq, Ord)
