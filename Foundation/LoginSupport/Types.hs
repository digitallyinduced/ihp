{-# LANGUAGE ConstraintKinds, ConstrainedClassMethods, AllowAmbiguousTypes #-}

module Foundation.LoginSupport.Types
( throwNotLoggedIn
, NotLoggedIn (NotLoggedIn)
, HasNewSessionUrl (newSessionUrl)
, AuthenticationController (..)
) where

import ClassyPrelude hiding (throw)
import qualified Control.Exception as Exception
import Data.Proxy (Proxy (Proxy))
import Foundation.FrameworkConfig

data NotLoggedIn = NotLoggedIn { newSessionUrl :: Maybe Text } deriving Show

data AuthenticationController
    = NotLoggedInAction { newSessionUrl :: Maybe Text }

throwNotLoggedIn newSessionUrl = Exception.throw (Foundation.LoginSupport.Types.NotLoggedIn newSessionUrl)

instance Exception NotLoggedIn

class HasNewSessionUrl user where
    newSessionUrl :: Proxy user -> Text
