module Routes where
import ClassyPrelude hiding (index, delete, show)
import Foundation.RouterSupport

import Apps.Web.App
import Apps.Web.Types

data RootApplication = RootApplication deriving (Eq)

instance HasPath RootApplication where
	pathTo _ = ""
instance CanRoute RootApplication () where
    parseRoute = parseRoute @WebApplication