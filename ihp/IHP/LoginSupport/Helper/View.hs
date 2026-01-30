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
-- Reads from the WAI request vault (populated by 'authMiddleware').
-- Falls back to the frozen controller context for backward compatibility.
currentUserOrNothing :: forall user. (?context :: ControllerContext, user ~ CurrentUserRecord, Typeable user) => Maybe user
currentUserOrNothing =
    case lookupAuthVault currentUserVaultKey ?context.request of
        Just user -> Just user
        Nothing -> fromFrozenContext @(Maybe user)

currentAdmin :: (?context :: ControllerContext, admin ~ CurrentAdminRecord, Typeable admin) => admin
currentAdmin = fromMaybe (error "Application.Helper.View.currentAdmin: Not logged in") currentAdminOrNothing

-- | Returns the current admin or 'Nothing'.
--
-- Reads from the WAI request vault (populated by 'adminAuthMiddleware').
-- Falls back to the frozen controller context for backward compatibility.
currentAdminOrNothing :: forall admin. (?context :: ControllerContext, admin ~ CurrentAdminRecord, Typeable admin) => Maybe admin
currentAdminOrNothing =
    case lookupAuthVault currentAdminVaultKey ?context.request of
        Just admin -> Just admin
        Nothing -> fromFrozenContext @(Maybe admin)
