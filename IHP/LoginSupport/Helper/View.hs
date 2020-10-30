module IHP.LoginSupport.Helper.View
( currentUser
, currentUserOrNothing
)
where

import IHP.Prelude

currentUser :: (?context :: viewContext, HasField "user" viewContext (Maybe user)) => user
currentUser = fromMaybe (error "Application.Helper.View.currentUser: Not logged in") currentUserOrNothing

currentUserOrNothing :: (?context :: viewContext, HasField "user" viewContext (Maybe user)) => Maybe user
currentUserOrNothing = getField @"user" ?context 
