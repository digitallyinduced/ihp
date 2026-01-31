{-# LANGUAGE AllowAmbiguousTypes #-}

module IHP.LoginSupport.Middleware
    ( authMiddleware
    , adminAuthMiddleware
    , authMiddlewareFor
    , currentUserVaultKey
    , currentAdminVaultKey
    , lookupAuthVault
    ) where

import IHP.Prelude
import IHP.LoginSupport.Types
import IHP.LoginSupport.Helper.Controller (sessionKey)
import IHP.Controller.Session
import IHP.QueryBuilder
import IHP.Fetch
import IHP.ModelSupport
import qualified Network.Wai as Wai
import qualified Data.Vault.Lazy as Vault

-- | Middleware that authenticates the current user and stores it in the request vault
-- using 'currentUserVaultKey'.
--
-- This is the standard middleware for user authentication. Add it to your Config.hs:
--
-- > import IHP.LoginSupport.Middleware
-- >
-- > config :: ConfigBuilder
-- > config = do
-- >     option $ AuthMiddleware (authMiddleware @User)
--
-- For both user and admin authentication:
--
-- > option $ AuthMiddleware (authMiddleware @User . adminAuthMiddleware @Admin)
--
authMiddleware :: forall user normalizedModel.
    ( normalizedModel ~ NormalizeModel user
    , normalizedModel ~ CurrentUserRecord
    , Typeable normalizedModel
    , Table normalizedModel
    , FromRow normalizedModel
    , PrimaryKey (GetTableName normalizedModel) ~ UUID
    , GetTableName normalizedModel ~ GetTableName user
    , FilterPrimaryKey (GetTableName normalizedModel)
    , KnownSymbol (GetModelName user)
    , HasNewSessionUrl user
    ) => Wai.Middleware
authMiddleware = authMiddlewareFor @user currentUserVaultKey
{-# INLINE authMiddleware #-}

-- | Middleware that authenticates the current admin and stores it in the request vault
-- using 'currentAdminVaultKey'.
--
-- > option $ AuthMiddleware (authMiddleware @User . adminAuthMiddleware @Admin)
--
adminAuthMiddleware :: forall admin normalizedModel.
    ( normalizedModel ~ NormalizeModel admin
    , normalizedModel ~ CurrentAdminRecord
    , Typeable normalizedModel
    , Table normalizedModel
    , FromRow normalizedModel
    , PrimaryKey (GetTableName normalizedModel) ~ UUID
    , GetTableName normalizedModel ~ GetTableName admin
    , FilterPrimaryKey (GetTableName normalizedModel)
    , KnownSymbol (GetModelName admin)
    , HasNewSessionUrl admin
    ) => Wai.Middleware
adminAuthMiddleware = authMiddlewareFor @admin currentAdminVaultKey
{-# INLINE adminAuthMiddleware #-}

-- | Building block: middleware with an explicit vault key for custom auth record types.
--
-- Prefer 'authMiddleware' or 'adminAuthMiddleware' for standard use cases.
-- Use this when you need a custom vault key.
authMiddlewareFor :: forall user normalizedModel.
    ( normalizedModel ~ NormalizeModel user
    , Typeable normalizedModel
    , Table normalizedModel
    , FromRow normalizedModel
    , PrimaryKey (GetTableName normalizedModel) ~ UUID
    , GetTableName normalizedModel ~ GetTableName user
    , FilterPrimaryKey (GetTableName normalizedModel)
    , KnownSymbol (GetModelName user)
    , HasNewSessionUrl user
    ) => Vault.Key (Maybe normalizedModel) -> Wai.Middleware
authMiddlewareFor key app req respond = do
    let ?request = req
    let ?modelContext = req.modelContext
    user <- getSession @(Id user) (sessionKey @user)
            >>= fetchOneOrNothing
    let req' = req { Wai.vault = Vault.insert key user (Wai.vault req) }
    app req' respond
{-# INLINE authMiddlewareFor #-}

