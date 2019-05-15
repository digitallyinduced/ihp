{-# LANGUAGE ScopedTypeVariables #-}

module Foundation.LoginSupport.Middleware (middleware) where

import ClassyPrelude hiding (catch)
import           Control.Exception
import Network.Wai (Application)
import Foundation.ApplicationContext
import qualified Foundation.LoginSupport.Controller
import Foundation.LoginSupport.Types
import qualified Foundation.ControllerSupport as ControllerSupport
import Foundation.FrameworkConfig (FrameworkConfig)

import Network.Wai (Middleware, ResponseReceived)

middleware :: FrameworkConfig => ApplicationContext -> Middleware
middleware applicationContext application request respond = (application request respond) `catch` (\(e :: NotLoggedIn) -> handleNotLoggedIn e)
    where
        handleNotLoggedIn ::  Foundation.LoginSupport.Types.NotLoggedIn -> IO ResponseReceived
        handleNotLoggedIn (Foundation.LoginSupport.Types.NotLoggedIn newSessionUrl) = do
            requestContext <- ControllerSupport.createRequestContext applicationContext request respond
            let ?requestContext = requestContext
            let ?applicationContext = applicationContext
            ControllerSupport.runAction NotLoggedInAction { newSessionUrl }
