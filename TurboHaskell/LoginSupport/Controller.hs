module TurboHaskell.LoginSupport.Controller where

import TurboHaskell.ControllerPrelude
import qualified Control.Exception
import Network.Wai (rawPathInfo)
import TurboHaskell.LoginSupport.Types
import qualified TurboHaskell.Controller.Context as ControllerContext
import qualified TurboHaskell.FrameworkConfig as FrameworkConfig
import TurboHaskell.ApplicationContext
import TurboHaskell.FrameworkConfig

instance FrameworkConfig => Controller AuthenticationController () where
    action NotLoggedInAction { newSessionUrl } = do
        setSuccessMessage "Please log in to access this page"
        setSession "TurboHaskell.LoginSupport.redirectAfterLogin" (cs getRequestUrl)
        case newSessionUrl of
            Just newSessionPath -> redirectToPath newSessionPath
            Nothing -> renderPlain "Please log in to access this page"
