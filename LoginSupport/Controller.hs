module Foundation.LoginSupport.Controller where

import Foundation.ControllerPrelude
import qualified Control.Exception
import Network.Wai (rawPathInfo)

notLoggedIn :: Action
notLoggedIn = do
    setSuccessMessage "Please log in to access this page"
    setSession "Foundation.LoginSupport.redirectAfterLogin" (cs getRequestUrl)
    redirectTo newSessionPath