{-# LANGUAGE AllowAmbiguousTypes #-}

module IHP.LoginSupport.Middleware
    ( authMiddleware
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

-- | Middleware that authenticates a user type and stores it in the request vault.
--
-- This is the standard middleware for user authentication. Add it to your Config.hs:
--
-- > import IHP.LoginSupport.Middleware
-- >
-- > config :: ConfigBuilder
-- > config = do
-- >     option $ AuthMiddleware (authMiddleware @User currentUserVaultKey)
--
-- For both user and admin authentication:
--
-- > option $ AuthMiddleware (authMiddleware @User currentUserVaultKey . authMiddleware @Admin currentAdminVaultKey)
--
authMiddleware :: forall user normalizedModel.
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
authMiddleware = authMiddlewareFor @user
{-# INLINE authMiddleware #-}

-- | Building block: middleware with an explicit vault key for custom auth record types.
--
-- This is the same as 'authMiddleware' and is provided as an alias.
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

