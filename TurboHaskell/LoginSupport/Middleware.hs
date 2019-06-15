{-# LANGUAGE ScopedTypeVariables #-}

module TurboHaskell.LoginSupport.Middleware (middleware) where

import ClassyPrelude hiding (catch)
import           Control.Exception
import Network.Wai (Application, Middleware, ResponseReceived)
import TurboHaskell.ApplicationContext
import qualified TurboHaskell.LoginSupport.Controller
import TurboHaskell.LoginSupport.Types
import qualified TurboHaskell.ControllerSupport as ControllerSupport
import TurboHaskell.FrameworkConfig (FrameworkConfig)

middleware :: FrameworkConfig => ApplicationContext -> Middleware
middleware applicationContext application request respond = (application request respond) `catch` (\(e :: NotLoggedIn) -> handleNotLoggedIn e)
    where
        handleNotLoggedIn ::  TurboHaskell.LoginSupport.Types.NotLoggedIn -> IO ResponseReceived
        handleNotLoggedIn (TurboHaskell.LoginSupport.Types.NotLoggedIn newSessionUrl) = do
            requestContext <- ControllerSupport.createRequestContext applicationContext request respond
            let ?requestContext = requestContext
            let ?applicationContext = applicationContext
            ControllerSupport.runAction NotLoggedInAction { newSessionUrl }
