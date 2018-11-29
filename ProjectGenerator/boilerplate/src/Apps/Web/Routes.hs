module Apps.Web.Routes where
import ClassyPrelude hiding (index, delete, show)
import Foundation.Router
import Model.Generated.Types

-- Controller Imports
import qualified Foundation.Welcome.Controller as Welcome

match :: AppRouter
match = prefix "/" [
		get $ action Welcome.welcome
        -- Generator Marker
    ]
