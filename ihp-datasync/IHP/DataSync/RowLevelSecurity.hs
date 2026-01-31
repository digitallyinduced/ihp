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
import qualified Hasql.Pool
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Statement as Statement
import qualified Hasql.Session as Session
import qualified IHP.DataSync.Role as Role
import qualified Data.Set as Set
import IHP.DataSync.Hasql (runHasql)

sqlQueryWithRLS ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Hasql.Pool.Pool -> Snippet -> Decoders.Result [result] -> IO [result]
sqlQueryWithRLS pool snippet decoder = runHasql pool queryWithRLS decoder
    where
        queryWithRLS = wrapStatementWithRLS snippet
{-# INLINE sqlQueryWithRLS #-}

sqlExecWithRLS ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Hasql.Pool.Pool -> Snippet -> IO ()
sqlExecWithRLS pool snippet = runHasql pool queryWithRLS Decoders.noResult
    where
        queryWithRLS = wrapStatementWithRLS snippet
{-# INLINE sqlExecWithRLS #-}

sqlQueryScalarWithRLS ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Hasql.Pool.Pool -> Snippet -> Decoders.Result result -> IO result
sqlQueryScalarWithRLS pool snippet decoder = runHasql pool queryWithRLS decoder
    where
        queryWithRLS = wrapStatementWithRLS snippet
{-# INLINE sqlQueryScalarWithRLS #-}

wrapStatementWithRLS ::
    ( ?context :: ControllerContext
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
ensureRLSEnabled :: Hasql.Pool.Pool -> Text -> IO TableWithRLS
ensureRLSEnabled pool table = do
    rlsEnabled <- hasRLSEnabled pool table
    unless rlsEnabled (error "Row level security is required for accessing this table")
    pure (TableWithRLS table)

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
hasRLSEnabled pool table = do
    let stmt = Statement.Statement
            "SELECT relrowsecurity FROM pg_class WHERE oid = quote_ident($1)::regclass"
            (Encoders.param (Encoders.nonNullable Encoders.text))
            (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
            True
    result <- Hasql.Pool.use pool (Session.statement table stmt)
    case result of
        Left err -> throwIO err
        Right val -> pure val

-- | Can be constructed using 'ensureRLSEnabled'
--
-- > tableWithRLS <- ensureRLSEnabled "my_table"
--
-- Useful to carry a proof that the RLS is actually enabled
newtype TableWithRLS = TableWithRLS { tableName :: Text } deriving (Eq, Ord)
