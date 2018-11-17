module Apps.Web.Routes where
import ClassyPrelude hiding (index, delete, show)
import Foundation.Router
import Model.Generated.Types

-- Controller Imports
import qualified Foundation.Welcome.Controller as Welcome
import qualified Apps.AdminBackend.Controller.EmailDomains as EmailDomains
import qualified Apps.AdminBackend.Controller.GapTests as GapTests
import qualified Apps.AdminBackend.Controller.Sessions as Sessions
import qualified Apps.AdminBackend.Controller.Managers as Managers

match :: AppRouter
match = prefix "/" [
		get $ action Foundation.Welcome.Controller.welcome
        -- Generator Marker
    ]
