module IHP.LoginSupport.Helper.View
( currentUser
, currentUserId
, currentUserOrNothing
, currentUserIdOrNothing
, currentAdmin
, currentAdminOrNothing
, currentAdminIdOrNothing
)
where

import IHP.Prelude
import IHP.LoginSupport.Helper.Controller (CurrentUserRecord, CurrentAdminRecord)
import IHP.LoginSupport.Types (currentUserVaultKey, currentAdminVaultKey, currentUserIdVaultKey, currentAdminIdVaultKey, lookupAuthVault)
import qualified IHP.ModelSupport as ModelSupport
import qualified Network.Wai as Wai

currentUser :: (?request :: Wai.Request, user ~ CurrentUserRecord, Typeable user) => user
currentUser = fromMaybe (error "Application.Helper.View.currentUser: Not logged in") currentUserOrNothing

currentUserId :: forall user userId. (?request :: Wai.Request, HasField "id" user userId, Typeable user, user ~ CurrentUserRecord) => userId
currentUserId = (currentUser @user).id

-- | Returns the current user or 'Nothing'.
--
-- Reads from the WAI request vault, populated by 'authMiddleware'.
currentUserOrNothing :: forall user. (?request :: Wai.Request, user ~ CurrentUserRecord, Typeable user) => Maybe user
currentUserOrNothing = lookupAuthVault currentUserVaultKey ?request

-- | Returns the current user's UUID or 'Nothing' if not logged in.
--
-- This only requires 'userIdMiddleware', no database query is needed.
currentUserIdOrNothing :: (?request :: Wai.Request, ModelSupport.PrimaryKey (ModelSupport.GetTableName CurrentUserRecord) ~ UUID) => Maybe (ModelSupport.Id CurrentUserRecord)
currentUserIdOrNothing = ModelSupport.Id <$> lookupAuthVault currentUserIdVaultKey ?request

currentAdmin :: (?request :: Wai.Request, admin ~ CurrentAdminRecord, Typeable admin) => admin
currentAdmin = fromMaybe (error "Application.Helper.View.currentAdmin: Not logged in") currentAdminOrNothing

-- | Returns the current admin or 'Nothing'.
--
-- Reads from the WAI request vault, populated by 'authMiddleware'.
currentAdminOrNothing :: forall admin. (?request :: Wai.Request, admin ~ CurrentAdminRecord, Typeable admin) => Maybe admin
currentAdminOrNothing = lookupAuthVault currentAdminVaultKey ?request

-- | Returns the current admin's UUID or 'Nothing' if not logged in.
--
-- This only requires 'adminIdMiddleware', no database query is needed.
currentAdminIdOrNothing :: (?request :: Wai.Request, ModelSupport.PrimaryKey (ModelSupport.GetTableName CurrentAdminRecord) ~ UUID) => Maybe (ModelSupport.Id CurrentAdminRecord)
currentAdminIdOrNothing = ModelSupport.Id <$> lookupAuthVault currentAdminIdVaultKey ?request
