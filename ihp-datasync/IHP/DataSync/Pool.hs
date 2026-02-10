module IHP.DataSync.Pool
( requestHasqlPool
, initHasqlPool
, initHasqlPoolWithRLS
) where

import IHP.Prelude
import qualified Hasql.Pool
import IHP.FrameworkConfig (findOptionOrNothing, addInitializer)
import IHP.FrameworkConfig.Types (RLSAuthenticatedRole(..))
import IHP.ModelSupport.Types (ModelContext(..))
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.TMap as TMap
import qualified IHP.DataSync.Role as Role

-- | Returns the hasql pool from the model context.
requestHasqlPool :: (?modelContext :: ModelContext) => Hasql.Pool.Pool
requestHasqlPool = case ?modelContext.hasqlPool of
    Just pool -> pool
    Nothing -> error "requestHasqlPool: No hasql pool available in ModelContext"

-- | No-op for backwards compatibility. The hasql pool is now always created
-- as part of the ModelContext. You can remove this call from your Config.hs.
initHasqlPool :: State.StateT TMap.TMap IO ()
initHasqlPool = pure ()

-- | Ensures the RLS authenticated role exists in postgres at startup.
-- Reads 'RLSAuthenticatedRole' from the config (default: @ihp_authenticated@).
--
-- Use this in your @Config.hs@:
--
-- > config :: ConfigBuilder
-- > config = do
-- >     initHasqlPoolWithRLS
initHasqlPoolWithRLS :: State.StateT TMap.TMap IO ()
initHasqlPoolWithRLS = do
    rlsRole <- findOptionOrNothing @RLSAuthenticatedRole >>= \case
        Just (RLSAuthenticatedRole role) -> pure role
        Nothing -> pure "ihp_authenticated"
    addInitializer do
        case ?modelContext.hasqlPool of
            Just pool -> Role.ensureAuthenticatedRoleExistsWithRole pool rlsRole
            Nothing -> pure ()
