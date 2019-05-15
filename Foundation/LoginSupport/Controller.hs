module Foundation.LoginSupport.Controller where

import Foundation.ControllerPrelude
import qualified Control.Exception
import Network.Wai (rawPathInfo)
import Foundation.LoginSupport.Types
import qualified Foundation.Controller.Context as ControllerContext
import qualified Foundation.FrameworkConfig as FrameworkConfig
import Foundation.ApplicationContext
import Foundation.FrameworkConfig

instance FrameworkConfig => Controller AuthenticationController () where
	action NotLoggedInAction { newSessionUrl } = do
	    setSuccessMessage "Please log in to access this page"
	    setSession "Foundation.LoginSupport.redirectAfterLogin" (cs getRequestUrl)
	    case newSessionUrl of
	        Just newSessionPath -> redirectToPath newSessionPath
	        Nothing -> renderPlain "Please log in to access this page"
