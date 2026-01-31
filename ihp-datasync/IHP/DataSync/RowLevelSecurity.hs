module IHP.DataSync.RowLevelSecurity
( ensureRLSEnabled
, hasRLSEnabled
, TableWithRLS (tableName)
, makeCachedEnsureRLSEnabled
, sqlQueryWithRLS
, sqlExecWithRLS
, sqlQueryScalarWithRLS
, wrapStatementWithRLS
, hasRLSEnabledSession
, ensureRLSEnabledSession
, sqlQueryWithRLSSession
, sqlExecWithRLSSession
, sqlQueryScalarWithRLSSession
)
where

import IHP.ControllerPrelude hiding (sqlQuery, sqlExec, sqlQueryScalar)
import qualified Hasql.Pool
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Session as DynSession
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Statement as Statement
import qualified Hasql.Session as Session
import qualified IHP.DataSync.Role as Role
import qualified Data.Set as Set
import qualified Data.Text as Text
import IHP.DataSync.Hasql (runSession)

-- Statements

hasRLSEnabledStatement :: Statement.Statement Text Bool
hasRLSEnabledStatement = Statement.Statement
    "SELECT relrowsecurity FROM pg_class WHERE oid = quote_ident($1)::regclass"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
    True

-- Sessions

hasRLSEnabledSession :: Text -> Session.Session Bool
hasRLSEnabledSession table = Session.statement table hasRLSEnabledStatement

ensureRLSEnabledSession :: Text -> Session.Session TableWithRLS
ensureRLSEnabledSession table = do
    rlsEnabled <- hasRLSEnabledSession table
    unless rlsEnabled (error "Row level security is required for accessing this table")
    pure (TableWithRLS table)

sqlQueryWithRLSSession ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Snippet -> Decoders.Result [result] -> Session.Session [result]
sqlQueryWithRLSSession snippet decoder = DynSession.dynamicallyParameterizedStatement (wrapStatementWithRLS snippet) decoder True
{-# INLINE sqlQueryWithRLSSession #-}

sqlExecWithRLSSession ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Snippet -> Session.Session ()
sqlExecWithRLSSession snippet = DynSession.dynamicallyParameterizedStatement (wrapStatementWithRLS snippet) Decoders.noResult True
{-# INLINE sqlExecWithRLSSession #-}

sqlQueryScalarWithRLSSession ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Snippet -> Decoders.Result result -> Session.Session result
sqlQueryScalarWithRLSSession snippet decoder = DynSession.dynamicallyParameterizedStatement (wrapStatementWithRLS snippet) decoder True
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

wrapStatementWithRLS ::
    ( ?context :: ControllerContext
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Snippet -> Snippet
wrapStatementWithRLS snippet = Snippet.sql (cs setLocalStatements) <> snippet <> Snippet.sql ";"
    where
        -- SET LOCAL doesn't support parameterized values ($1, $2, etc.) in the
        -- extended query protocol. We must inline the values directly into the SQL
        -- string with proper escaping.
        setLocalStatements :: Text
        setLocalStatements =
            "SET LOCAL ROLE " <> escapeIdentifier Role.authenticatedRole
            <> "; SET LOCAL rls.ihp_user_id = " <> escapeLiteral encodedUserIdText
            <> "; "

        maybeUserId = (.id) <$> currentUserOrNothing

        -- When the user is not logged in and maybeUserId is Nothing, we cannot
        -- just pass @NULL@ to postgres. The @SET LOCAL@ values can only be strings.
        --
        -- Therefore we map Nothing to an empty string here. The empty string
        -- means "not logged in".
        --
        encodedUserIdText :: Text
        encodedUserIdText = case maybeUserId of
                Just userId -> tshow userId
                Nothing -> ""

        -- | Escape a SQL identifier with double quotes
        escapeIdentifier :: Text -> Text
        escapeIdentifier name = "\"" <> Text.replace "\"" "\"\"" name <> "\""

        -- | Escape a SQL string literal with single quotes
        escapeLiteral :: Text -> Text
        escapeLiteral value = "'" <> Text.replace "'" "''" value <> "'"
{-# INLINE wrapStatementWithRLS #-}

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
