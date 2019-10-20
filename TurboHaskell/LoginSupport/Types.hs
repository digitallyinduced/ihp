{-# LANGUAGE ConstraintKinds, ConstrainedClassMethods, AllowAmbiguousTypes #-}

module TurboHaskell.LoginSupport.Types
( throwNotLoggedIn
, NotLoggedIn (NotLoggedIn)
, HasNewSessionUrl (newSessionUrl)
, AuthenticationController (..)
) where

import ClassyPrelude hiding (throw)
import qualified Control.Exception as Exception
import Data.Proxy (Proxy (Proxy))
import TurboHaskell.FrameworkConfig

data NotLoggedIn = NotLoggedIn { newSessionUrl :: Maybe Text } deriving Show

data AuthenticationController = NotLoggedInAction { newSessionUrl :: Maybe Text } deriving (Eq, Show)

throwNotLoggedIn newSessionPath = Exception.throw (TurboHaskell.LoginSupport.Types.NotLoggedIn newSessionPath)

instance Exception NotLoggedIn

class HasNewSessionUrl user where
    newSessionUrl :: Proxy user -> Text
