{-# LANGUAGE ConstraintKinds, ConstrainedClassMethods, AllowAmbiguousTypes #-}

module TurboHaskell.LoginSupport.Types ( HasNewSessionUrl (newSessionUrl) ) where

import TurboHaskell.Prelude

class HasNewSessionUrl user where
    newSessionUrl :: Proxy user -> Text
