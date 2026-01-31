{-# LANGUAGE ConstraintKinds, ConstrainedClassMethods, AllowAmbiguousTypes #-}

module IHP.LoginSupport.Types
( HasNewSessionUrl (newSessionUrl)
, CurrentUserRecord
, CurrentAdminRecord
, currentUserVaultKey
, currentAdminVaultKey
, lookupAuthVault
) where

import IHP.Prelude
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai as Wai
import System.IO.Unsafe (unsafePerformIO)

class HasNewSessionUrl user where
    newSessionUrl :: Proxy user -> Text

type family CurrentUserRecord
type family CurrentAdminRecord

-- | Vault key for the current user record.
-- Used by 'authMiddleware' to store the authenticated user in the WAI request vault.
currentUserVaultKey :: Vault.Key (Maybe CurrentUserRecord)
currentUserVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE currentUserVaultKey #-}

-- | Vault key for the current admin record.
-- Used by 'adminAuthMiddleware' to store the authenticated admin in the WAI request vault.
currentAdminVaultKey :: Vault.Key (Maybe CurrentAdminRecord)
currentAdminVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE currentAdminVaultKey #-}

-- | Pure lookup of an auth record from the WAI request vault.
lookupAuthVault :: Vault.Key (Maybe user) -> Wai.Request -> Maybe user
lookupAuthVault key req = join (Vault.lookup key (Wai.vault req))
{-# INLINE lookupAuthVault #-}
