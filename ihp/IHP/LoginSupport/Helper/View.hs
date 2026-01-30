module IHP.LoginSupport.Helper.View
( currentUser
, currentUserId
, currentUserOrNothing
, currentAdmin
, currentAdminOrNothing
)
where

import IHP.Prelude
import IHP.Controller.Context
import IHP.LoginSupport.Helper.Controller (CurrentUserRecord, CurrentAdminRecord)
import IHP.LoginSupport.Types (currentUserVaultKey, currentAdminVaultKey, lookupAuthVault)

currentUser :: (?context :: ControllerContext, user ~ CurrentUserRecord, Typeable user) => user
currentUser = fromMaybe (error "Application.Helper.View.currentUser: Not logged in") currentUserOrNothing

currentUserId :: forall user userId. (?context :: ControllerContext, HasField "id" user userId, Typeable user, user ~ CurrentUserRecord) => userId
currentUserId = (currentUser @user).id

-- | Returns the current user or 'Nothing'.
--
-- Reads from the WAI request vault, populated by 'authMiddleware'.
currentUserOrNothing :: forall user. (?context :: ControllerContext, user ~ CurrentUserRecord, Typeable user) => Maybe user
currentUserOrNothing = lookupAuthVault currentUserVaultKey ?context.request

currentAdmin :: (?context :: ControllerContext, admin ~ CurrentAdminRecord, Typeable admin) => admin
currentAdmin = fromMaybe (error "Application.Helper.View.currentAdmin: Not logged in") currentAdminOrNothing

-- | Returns the current admin or 'Nothing'.
--
-- Reads from the WAI request vault, populated by 'authMiddleware'.
currentAdminOrNothing :: forall admin. (?context :: ControllerContext, admin ~ CurrentAdminRecord, Typeable admin) => Maybe admin
currentAdminOrNothing = lookupAuthVault currentAdminVaultKey ?context.request
