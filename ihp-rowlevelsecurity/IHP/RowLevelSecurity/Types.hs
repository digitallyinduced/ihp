module IHP.RowLevelSecurity.Types
( RowLevelSecurityContext (..)
, TableWithRLS (..)
) where

import Prelude
import Data.Text (Text)

-- | Runtime context for PostgreSQL Row Level Security.
--
-- Carried through request processing to transparently wrap queries
-- with @SET LOCAL ROLE@ and @set_config('rls.ihp_user_id', ...)@.
data RowLevelSecurityContext = RowLevelSecurityContext
    { rlsAuthenticatedRole :: Text -- ^ The PostgreSQL role to switch to (e.g. @"ihp_authenticated"@). This role is subject to RLS policies, unlike the table owner role.
    , rlsUserId :: Text -- ^ The current user's id, serialized as text. Stored in the @rls.ihp_user_id@ GUC for use by the @ihp_user_id()@ SQL function.
    }

-- | Proof that a table has RLS enabled. Constructed via 'IHP.RowLevelSecurity.Introspection.ensureRLSEnabled'.
--
-- Carrying this value proves that we checked @pg_class.relrowsecurity@
-- for the table, so downstream code can skip redundant checks.
newtype TableWithRLS = TableWithRLS { tableName :: Text } deriving (Eq, Ord)
