module Routes where
import ClassyPrelude hiding (index, delete, show)
import Foundation.Router

-- Controller Imports
import qualified Foundation.Welcome.Controller

match :: AppRouter
match = prefix "/" [
        get $ action Foundation.Welcome.Controller.welcome
        -- Generator Marker
    ]
