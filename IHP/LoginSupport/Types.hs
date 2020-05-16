{-# LANGUAGE ConstraintKinds, ConstrainedClassMethods, AllowAmbiguousTypes #-}

module IHP.LoginSupport.Types ( HasNewSessionUrl (newSessionUrl) ) where

import IHP.Prelude

class HasNewSessionUrl user where
    newSessionUrl :: Proxy user -> Text
