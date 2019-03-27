module Apps.Web.App where
import Foundation.RouterPrelude
import Apps.Web.Types

-- Controller Imports
import Foundation.Welcome.Controller

instance HasPath WebApplication where
	pathTo WebApplication = ""

instance CanRoute WebApplication () where
    parseRoute = withPrefix "/"
	    		[ parseRoute @WelcomeController
	    		-- Generator Marker
		    	]

