{-# LANGUAGE ScopedTypeVariables #-}

module Foundation.LoginSupport.Middleware (middleware) where

import           Control.Exception
import Network.Wai (Application)
import Foundation.ApplicationContext
import qualified Foundation.LoginSupport.Controller
import qualified Foundation.LoginSupport.Types
import Foundation.ControllerSupport (withContext)



middleware applicationContext application request respond = (application request respond) `catch` (\(e :: Foundation.LoginSupport.Types.NotLoggedIn) -> handleNotLoggedIn)
    where
        handleNotLoggedIn = withContext Foundation.LoginSupport.Controller.notLoggedIn applicationContext request respond