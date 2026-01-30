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
module IHP.DataSync.Role where

import IHP.Prelude
import IHP.FrameworkConfig
import IHP.ModelSupport hiding (sqlQuery, sqlExec, sqlQueryScalar)
import qualified Control.Exception.Safe as Exception
import qualified Hasql.Connection as Hasql
import qualified Hasql.Connection.Setting as HasqlSetting
import qualified Hasql.Connection.Setting.Connection as HasqlConnection
import qualified Hasql.Session as Hasql
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Session as DynSession
import qualified Hasql.Decoders as Decoders

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

doesRoleExists :: (?modelContext :: ModelContext) => Text -> IO Bool
doesRoleExists name = runHasql ("SELECT EXISTS(SELECT 1 FROM pg_roles WHERE rolname = " <> Snippet.param name <> " LIMIT 1)") (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))

ensureAuthenticatedRoleExists :: (?context :: context, ConfigProvider context, ?modelContext :: ModelContext) => IO ()
ensureAuthenticatedRoleExists = do
    roleExists <- doesRoleExists authenticatedRole
    unless roleExists (createAuthenticatedRole authenticatedRole)
    grantPermissions authenticatedRole

createAuthenticatedRole :: (?modelContext :: ModelContext) => Text -> IO ()
createAuthenticatedRole role = do
    -- The role is only going to be used from 'SET ROLE ..' calls
    -- Therefore we can disallow direct connection with NOLOGIN
    runHasql ("CREATE ROLE " <> Snippet.sql (quoteIdentifier role) <> " NOLOGIN") Decoders.noResult

    pure ()

grantPermissions :: (?modelContext :: ModelContext) => Text -> IO ()
grantPermissions role = do
    -- From SO https://stackoverflow.com/a/17355059/14144232
    --
    -- GRANTs on different objects are separate. GRANTing on a database doesn't GRANT rights to the schema within. Similiarly, GRANTing on a schema doesn't grant rights on the tables within.
    --
    -- If you have rights to SELECT from a table, but not the right to see it in the schema that contains it then you can't access the table.
    --
    -- The rights tests are done in order:
    --
    -- Do you have `USAGE` on the schema?
    --     No:  Reject access.
    --     Yes: Do you also have the appropriate rights on the table?
    --         No:  Reject access.
    --         Yes: Check column privileges.

    -- The role should have access to all existing tables in our schema
    runHasql ("GRANT USAGE ON SCHEMA public TO " <> Snippet.sql (quoteIdentifier role)) Decoders.noResult

    -- The role should have access to all existing tables in our schema
    runHasql ("GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO " <> Snippet.sql (quoteIdentifier role)) Decoders.noResult

    -- Also grant access to all tables created in the future
    runHasql ("ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL PRIVILEGES ON TABLES TO " <> Snippet.sql (quoteIdentifier role)) Decoders.noResult

    pure ()

authenticatedRole :: (?context :: context, ConfigProvider context) => Text
authenticatedRole = ?context.frameworkConfig.rlsAuthenticatedRole

-- | Quote a SQL identifier (role name, table name, etc.) to prevent SQL injection
quoteIdentifier :: Text -> ByteString
quoteIdentifier name = cs ("\"" <> name <> "\"")
