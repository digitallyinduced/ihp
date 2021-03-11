module IHP.LoginSupport.Helper.View
( currentUser
, currentUserOrNothing
, currentAdmin
, currentAdminOrNothing
)
where

import IHP.Prelude
import IHP.Controller.Context
import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)

currentUser :: (?context :: ControllerContext, user ~ CurrentUserRecord, Typeable user) => user
currentUser = fromMaybe (error "Application.Helper.View.currentUser: Not logged in") currentUserOrNothing

currentUserOrNothing :: forall user. (?context :: ControllerContext, user ~ CurrentUserRecord, Typeable user) => Maybe user
currentUserOrNothing = fromFrozenContext @(Maybe user)

currentAdmin :: (?context :: ControllerContext, admin ~ CurrentAdminRecord, Typeable admin) => admin
currentAdmin = fromMaybe (error "Application.Helper.View.currentAdmin: Not logged in") currentAdminOrNothing

currentAdminOrNothing :: forall admin. (?context :: ControllerContext, admin ~ CurrentAdminRecord, Typeable admin) => Maybe admin
currentAdminOrNothing = fromFrozenContext @(Maybe admin)
