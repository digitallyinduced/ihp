{-# LANGUAGE ScopedTypeVariables #-}

module Foundation.LoginSupport.Middleware (middleware) where

import ClassyPrelude hiding (catch)
import           Control.Exception
import Network.Wai (Application)
import Foundation.ApplicationContext
import qualified Foundation.LoginSupport.Controller
import qualified Foundation.LoginSupport.Types
import Foundation.ControllerSupport (withContext, Action, Action')

import Network.Wai (Middleware, ResponseReceived)


middleware :: ApplicationContext -> Middleware
middleware applicationContext application request respond = (application request respond) `catch` (\(e :: Foundation.LoginSupport.Types.NotLoggedIn) -> handleNotLoggedIn e)
    where
        handleNotLoggedIn ::  Foundation.LoginSupport.Types.NotLoggedIn -> IO ResponseReceived
        handleNotLoggedIn (Foundation.LoginSupport.Types.NotLoggedIn newSessionUrl) = do
                boundAction <- withContext (Foundation.LoginSupport.Controller.notLoggedIn newSessionUrl) applicationContext request respond
                boundAction
