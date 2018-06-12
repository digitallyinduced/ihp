module Foundation.LoginSupport.Controller where

import Foundation.ControllerPrelude
import qualified Control.Exception
import Network.Wai (rawPathInfo)

notLoggedIn :: Maybe Text -> Action
notLoggedIn newSessionUrl = do
    setSuccessMessage "Please log in to access this page"
    setSession "Foundation.LoginSupport.redirectAfterLogin" (cs getRequestUrl)
    case newSessionUrl of
        Just newSessionPath -> redirectTo newSessionPath
        Nothing -> renderPlain "Please log in to access this page"
