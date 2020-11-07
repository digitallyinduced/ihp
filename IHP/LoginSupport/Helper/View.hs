module IHP.LoginSupport.Helper.View
( currentUser
, currentUserOrNothing
)
where

import IHP.Prelude
import IHP.Controller.Context
import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)

currentUser :: (?context :: ControllerContext, user ~ CurrentUserRecord, Typeable user) => user
currentUser = fromMaybe (error "Application.Helper.View.currentUser: Not logged in") currentUserOrNothing

currentUserOrNothing :: forall user. (?context :: ControllerContext, user ~ CurrentUserRecord, Typeable user) => Maybe user
currentUserOrNothing = fromFrozenContext @(Maybe user)
