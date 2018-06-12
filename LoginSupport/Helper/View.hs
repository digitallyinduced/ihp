module Foundation.LoginSupport.Helper.View

import Foundation.HaskellSupport
import GHC.Records
import ClassyPrelude

currentUser :: forall viewContext user. (?viewContext :: viewContext, HasField "user" viewContext (Maybe user)) => user
currentUser = fromMaybe (error "Helper.View.currentUser: Not logged in") currentUserOrNothing

currentUserOrNothing :: forall viewContext user. (?viewContext :: viewContext, HasField "user" viewContext (Maybe user)) => Maybe user
currentUserOrNothing = ?viewContext |> get #user
