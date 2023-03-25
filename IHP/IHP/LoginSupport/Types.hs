{-# LANGUAGE ConstraintKinds, ConstrainedClassMethods, AllowAmbiguousTypes #-}

module IHP.LoginSupport.Types
( HasNewSessionUrl (newSessionUrl)
, CurrentUserRecord
, CurrentAdminRecord
) where

import IHP.Prelude

class HasNewSessionUrl user where
    newSessionUrl :: Proxy user -> Text

type family CurrentUserRecord
type family CurrentAdminRecord