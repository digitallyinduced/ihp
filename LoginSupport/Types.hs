{-# LANGUAGE ConstraintKinds, ConstrainedClassMethods, AllowAmbiguousTypes #-}

module Foundation.LoginSupport.Types (throwNotLoggedIn, NotLoggedIn (NotLoggedIn), HasNewSessionUrl (newSessionUrl)) where

import           ClassyPrelude hiding (throw)
import           Control.Exception             (throw)
import Data.Proxy (Proxy (Proxy))

data NotLoggedIn = NotLoggedIn { newSessionUrl :: Maybe Text } deriving Show

throwNotLoggedIn newSessionUrl = throw (Foundation.LoginSupport.Types.NotLoggedIn newSessionUrl)

instance Exception NotLoggedIn

class HasNewSessionUrl user where
    newSessionUrl :: Proxy user -> Text
