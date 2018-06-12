module Foundation.LoginSupport.Helper.View where

import Foundation.HaskellSupport
import GHC.Records
import ClassyPrelude

currentUser :: (?viewContext :: viewContext, HasField "user" viewContext (Maybe user)) => user
currentUser = fromMaybe (error "Helper.View.currentUser: Not logged in") currentUserOrNothing

currentUserOrNothing :: (?viewContext :: viewContext, HasField "user" viewContext (Maybe user)) => Maybe user
currentUserOrNothing = ?viewContext |> get #user
