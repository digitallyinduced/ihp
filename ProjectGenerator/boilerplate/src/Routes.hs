module Routes where
import ClassyPrelude hiding (index, delete, show)
import Foundation.Router

-- Router Imports
import qualified Apps.Web.Routes

match :: AppRouter
match = prefix "" [
        Apps.Web.Routes.match
        -- Generator Marker
    ]