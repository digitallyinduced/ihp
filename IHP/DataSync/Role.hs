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
import Data.Aeson
import IHP.QueryBuilder
import IHP.DataSync.DynamicQuery
import IHP.FrameworkConfig
import IHP.ModelSupport
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG

doesRoleExists :: (?modelContext :: ModelContext) => Text -> IO Bool
doesRoleExists name = sqlQueryScalar "SELECT EXISTS(SELECT 1 FROM pg_roles WHERE rolname = ? LIMIT 1)" [name]

ensureAuthenticatedRoleExists :: (?context :: context, ConfigProvider context, ?modelContext :: ModelContext) => IO ()
ensureAuthenticatedRoleExists = do
    roleExists <- doesRoleExists authenticatedRole
    unless roleExists (createAuthenticatedRole authenticatedRole)

createAuthenticatedRole :: (?modelContext :: ModelContext) => Text -> IO ()
createAuthenticatedRole role = do
    -- The role is only going to be used from 'SET ROLE ..' calls
    -- Therefore we can disallow direct connection with NOLOGIN
    sqlExec "CREATE ROLE ? NOLOGIN" [PG.Identifier role]

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
    sqlExec "GRANT USAGE ON SCHEMA public TO ?" [PG.Identifier role]
    
    -- The role should have access to all existing tables in our schema
    sqlExec "GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO ?" [PG.Identifier role]

    -- Also grant access to all tables created in the future
    sqlExec "ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL PRIVILEGES ON TABLES TO ?" [PG.Identifier role]

    pure ()

authenticatedRole :: (?context :: context, ConfigProvider context) => Text
authenticatedRole = ?context
        |> getFrameworkConfig
        |> get #rlsAuthenticatedRole