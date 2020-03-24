{-# LANGUAGE ConstraintKinds, ConstrainedClassMethods, AllowAmbiguousTypes #-}

module TurboHaskell.LoginSupport.Types ( HasNewSessionUrl (newSessionUrl) ) where

import ClassyPrelude hiding (throw)
import Data.Proxy (Proxy (Proxy))

class HasNewSessionUrl user where
    newSessionUrl :: Proxy user -> Text
