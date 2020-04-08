module TurboHaskell.LoginSupport.Helper.View where

import TurboHaskell.Prelude

currentUser :: (?viewContext :: viewContext, HasField "user" viewContext (Maybe user)) => user
currentUser = fromMaybe (error "Application.Helper.View.currentUser: Not logged in") currentUserOrNothing

currentUserOrNothing :: (?viewContext :: viewContext, HasField "user" viewContext (Maybe user)) => Maybe user
currentUserOrNothing = getField @"user" ?viewContext 
