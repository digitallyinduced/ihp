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
import qualified Control.Exception.Safe as Exception
import qualified Hasql.Pool
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Session as DynSession
import qualified Hasql.Decoders as Decoders

-- | Run a hasql snippet using a connection from the given pool.
runHasql :: Hasql.Pool.Pool -> Snippet -> Decoders.Result a -> IO a
runHasql pool snippet decoder = do
    let session = DynSession.dynamicallyParameterizedStatement snippet decoder True
    result <- Hasql.Pool.use pool session
    case result of
        Left err -> Exception.throwIO (userError (cs $ tshow err))
        Right val -> pure val
{-# INLINE runHasql #-}

doesRoleExists :: Hasql.Pool.Pool -> Text -> IO Bool
doesRoleExists pool name = runHasql pool ("SELECT EXISTS(SELECT 1 FROM pg_roles WHERE rolname = " <> Snippet.param name <> " LIMIT 1)") (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))

ensureAuthenticatedRoleExists :: (?context :: context, ConfigProvider context) => Hasql.Pool.Pool -> IO ()
ensureAuthenticatedRoleExists pool = do
    roleExists <- doesRoleExists pool authenticatedRole
    unless roleExists (createAuthenticatedRole pool authenticatedRole)
    grantPermissions pool authenticatedRole

createAuthenticatedRole :: Hasql.Pool.Pool -> Text -> IO ()
createAuthenticatedRole pool role = do
    -- The role is only going to be used from 'SET ROLE ..' calls
    -- Therefore we can disallow direct connection with NOLOGIN
    runHasql pool ("CREATE ROLE " <> Snippet.sql (quoteIdentifier role) <> " NOLOGIN") Decoders.noResult

    pure ()

grantPermissions :: Hasql.Pool.Pool -> Text -> IO ()
grantPermissions pool role = do
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
    runHasql pool ("GRANT USAGE ON SCHEMA public TO " <> Snippet.sql (quoteIdentifier role)) Decoders.noResult

    -- The role should have access to all existing tables in our schema
    runHasql pool ("GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO " <> Snippet.sql (quoteIdentifier role)) Decoders.noResult

    -- Also grant access to all tables created in the future
    runHasql pool ("ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL PRIVILEGES ON TABLES TO " <> Snippet.sql (quoteIdentifier role)) Decoders.noResult

    pure ()

authenticatedRole :: (?context :: context, ConfigProvider context) => Text
authenticatedRole = ?context.frameworkConfig.rlsAuthenticatedRole

-- | Quote a SQL identifier (role name, table name, etc.) to prevent SQL injection
quoteIdentifier :: Text -> ByteString
quoteIdentifier name = cs ("\"" <> name <> "\"")
