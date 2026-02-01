{-# LANGUAGE AllowAmbiguousTypes #-}

module IHP.LoginSupport.Middleware
    ( authMiddleware
    , adminAuthMiddleware
    , userIdMiddleware
    , adminIdMiddleware
    , fetchUserMiddleware
    , fetchAdminMiddleware
    , fetchUserMiddlewareFor
    , parseSessionUUID
    , authMiddlewareWith
    , currentUserVaultKey
    , currentAdminVaultKey
    , currentUserIdVaultKey
    , currentAdminIdVaultKey
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
import qualified Data.UUID as UUID
import qualified Data.ByteString as BS

-- | Middleware that reads a userId from the session and stores it in
-- 'currentUserIdVaultKey'. No database query is performed.
--
-- This is useful when you only need the user's UUID (e.g. for row-level
-- security) and want to avoid the cost of a database fetch.
--
-- > option $ AuthMiddleware (userIdMiddleware (sessionKey @User))
--
-- For full user record access, compose with 'fetchUserMiddleware':
--
-- > option $ AuthMiddleware (userIdMiddleware (sessionKey @User) . fetchUserMiddleware @User)
--
userIdMiddleware :: ByteString -> Wai.Middleware
userIdMiddleware sessionKeyName = userIdMiddlewareFor sessionKeyName currentUserIdVaultKey
{-# INLINE userIdMiddleware #-}

-- | Same as 'userIdMiddleware' but stores the admin ID in 'currentAdminIdVaultKey'.
--
-- > option $ AuthMiddleware (adminIdMiddleware (sessionKey @Admin))
--
adminIdMiddleware :: ByteString -> Wai.Middleware
adminIdMiddleware sessionKeyName = userIdMiddlewareFor sessionKeyName currentAdminIdVaultKey
{-# INLINE adminIdMiddleware #-}

-- | Building block: reads a session key and stores the parsed UUID in the given vault key.
userIdMiddlewareFor :: ByteString -> Vault.Key (Maybe UUID) -> Wai.Middleware
userIdMiddlewareFor sessionKeyName idKey app req respond = do
    userId <- case lookupSessionVault req of
        Just (lookupFn, _) -> do
            rawValue <- lookupFn sessionKeyName
            pure $ case rawValue of
                Nothing -> Nothing
                Just "" -> Nothing
                Just bs -> parseSessionUUID bs
        Nothing -> pure Nothing
    let req' = req { Wai.vault = Vault.insert idKey userId (Wai.vault req) }
    app req' respond
{-# INLINE userIdMiddlewareFor #-}

-- | Parse UUID from session bytes. Handles both:
--
--   - New format: raw 36-byte UUID ASCII (e.g. \"550e8400-e29b-41d4-a716-446655440000\")
--   - Old format: 8-byte cereal length prefix + 36-byte UUID ASCII (44 bytes total)
--
-- The old format comes from sessions written with @Serialize (Id' table)@ which
-- prepends an 8-byte big-endian length prefix via cereal. We support both formats
-- so existing sessions continue to work without logging users out on upgrade.
--
-- TODO: Remove old format support after 2026-05-01. At that point all
-- session cookies using the cereal encoding will have expired.
parseSessionUUID :: ByteString -> Maybe UUID
parseSessionUUID bs
    | Just uuid <- UUID.fromASCIIBytes bs = Just uuid
    | BS.length bs == 44 = UUID.fromASCIIBytes (BS.drop 8 bs)
    | otherwise = Nothing
{-# INLINE parseSessionUUID #-}

-- | Middleware that reads the userId from 'currentUserIdVaultKey', fetches
-- the full user record from the database, and stores it in 'currentUserVaultKey'.
--
-- Must be composed after 'userIdMiddleware':
--
-- > userIdMiddleware (sessionKey @User) . fetchUserMiddleware @User
--
fetchUserMiddleware :: forall user normalizedModel.
    ( normalizedModel ~ NormalizeModel user
    , normalizedModel ~ CurrentUserRecord
    , Typeable normalizedModel
    , Table normalizedModel
    , FromRow normalizedModel
    , PrimaryKey (GetTableName normalizedModel) ~ UUID
    , GetTableName normalizedModel ~ GetTableName user
    , FilterPrimaryKey (GetTableName normalizedModel)
    ) => Wai.Middleware
fetchUserMiddleware = fetchUserMiddlewareFor @user currentUserIdVaultKey currentUserVaultKey
{-# INLINE fetchUserMiddleware #-}

-- | Middleware that reads the adminId from 'currentAdminIdVaultKey', fetches
-- the full admin record from the database, and stores it in 'currentAdminVaultKey'.
--
-- Must be composed after 'adminIdMiddleware':
--
-- > adminIdMiddleware (sessionKey @Admin) . fetchAdminMiddleware @Admin
--
fetchAdminMiddleware :: forall admin normalizedModel.
    ( normalizedModel ~ NormalizeModel admin
    , normalizedModel ~ CurrentAdminRecord
    , Typeable normalizedModel
    , Table normalizedModel
    , FromRow normalizedModel
    , PrimaryKey (GetTableName normalizedModel) ~ UUID
    , GetTableName normalizedModel ~ GetTableName admin
    , FilterPrimaryKey (GetTableName normalizedModel)
    ) => Wai.Middleware
fetchAdminMiddleware = fetchUserMiddlewareFor @admin currentAdminIdVaultKey currentAdminVaultKey
{-# INLINE fetchAdminMiddleware #-}

-- | Building block: reads a UUID from the given ID vault key, fetches the
-- record from the database, and stores it in the given user vault key.
fetchUserMiddlewareFor :: forall user normalizedModel.
    ( normalizedModel ~ NormalizeModel user
    , Typeable normalizedModel
    , Table normalizedModel
    , FromRow normalizedModel
    , PrimaryKey (GetTableName normalizedModel) ~ UUID
    , GetTableName normalizedModel ~ GetTableName user
    , FilterPrimaryKey (GetTableName normalizedModel)
    ) => Vault.Key (Maybe UUID) -> Vault.Key (Maybe normalizedModel) -> Wai.Middleware
fetchUserMiddlewareFor idKey userKey app req respond = do
    let ?modelContext = req.modelContext
    user <- case lookupAuthVault idKey req of
        Just uuid -> fetchOneOrNothing (Id uuid)
        Nothing -> pure Nothing
    let req' = req { Wai.vault = Vault.insert userKey user (Wai.vault req) }
    app req' respond
{-# INLINE fetchUserMiddlewareFor #-}

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
-- This is equivalent to @userIdMiddleware (sessionKey \@User) . fetchUserMiddleware \@User@.
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
    ) => Wai.Middleware
authMiddleware = userIdMiddleware (sessionKey @user) . fetchUserMiddleware @user
{-# INLINE authMiddleware #-}

-- | Middleware that authenticates the current admin and stores it in the request vault
-- using 'currentAdminVaultKey'.
--
-- > option $ AuthMiddleware (authMiddleware @User . adminAuthMiddleware @Admin)
--
-- This is equivalent to @adminIdMiddleware (sessionKey \@Admin) . fetchAdminMiddleware \@Admin@.
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
    ) => Wai.Middleware
adminAuthMiddleware = adminIdMiddleware (sessionKey @admin) . fetchAdminMiddleware @admin
{-# INLINE adminAuthMiddleware #-}

-- | Low-level building block: middleware that runs a fetch function and stores
-- the result in the request vault under the given key.
--
-- This decouples the vault insertion from the database lookup, making it
-- useful for testing and custom authentication schemes.
authMiddlewareWith :: Vault.Key (Maybe user) -> (Wai.Request -> IO (Maybe user)) -> Wai.Middleware
authMiddlewareWith key fetchUser app req respond = do
    user <- fetchUser req
    let req' = req { Wai.vault = Vault.insert key user (Wai.vault req) }
    app req' respond
{-# INLINE authMiddlewareWith #-}
