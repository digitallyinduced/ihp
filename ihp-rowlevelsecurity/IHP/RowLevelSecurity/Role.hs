module IHP.RowLevelSecurity.Role
( ensureAuthenticatedRole
, createAuthenticatedRole
, grantPermissions
, doesRoleExist
, quoteIdentifier
) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Statement as Statement
import qualified Hasql.Session as Session
import IHP.RowLevelSecurity.Statement (doesRoleExistStatement)

-- | Idempotent: ensure the authenticated role exists and has permissions.
--
-- Checks @pg_roles@, creates the role with @NOLOGIN@ if missing,
-- then grants @USAGE ON SCHEMA public@ and @ALL PRIVILEGES ON ALL TABLES@.
ensureAuthenticatedRole :: Text -> Session.Session ()
ensureAuthenticatedRole role = do
    roleExists <- Session.statement role doesRoleExistStatement
    unless roleExists (createAuthenticatedRole role)
    grantPermissions role

-- | Create a PostgreSQL role with @NOLOGIN@.
--
-- The role is intended to be used only via @SET ROLE@ inside transactions.
-- Direct connections are disallowed.
createAuthenticatedRole :: Text -> Session.Session ()
createAuthenticatedRole role =
    Session.statement () (Statement.unpreparable
        ("CREATE ROLE " <> quoteIdentifier role <> " NOLOGIN")
        Encoders.noParams
        Decoders.noResult)

-- | Grant the role access to the @public@ schema and all tables within it.
grantPermissions :: Text -> Session.Session ()
grantPermissions role = do
    let exec sql = Session.statement () (Statement.unpreparable sql Encoders.noParams Decoders.noResult)
    exec ("GRANT USAGE ON SCHEMA public TO " <> quoteIdentifier role)
    exec ("GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO " <> quoteIdentifier role)
    exec ("ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL PRIVILEGES ON TABLES TO " <> quoteIdentifier role)

-- | Check whether a PostgreSQL role exists.
doesRoleExist :: Text -> Session.Session Bool
doesRoleExist role = Session.statement role doesRoleExistStatement

-- | Quote a SQL identifier to prevent SQL injection.
-- Escapes embedded double quotes by doubling them per SQL standard.
quoteIdentifier :: Text -> Text
quoteIdentifier name = "\"" <> Text.replace "\"" "\"\"" name <> "\""

-- Local Prelude bits (we don't depend on ihp)
unless :: Applicative f => Bool -> f () -> f ()
unless p action = if p then pure () else action
