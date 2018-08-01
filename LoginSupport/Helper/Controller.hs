{-# LANGUAGE AllowAmbiguousTypes #-}

module Foundation.LoginSupport.Helper.Controller (currentUser, currentUserOrNothing, currentUserId, HasNewSessionUrl) where

import Foundation.HaskellSupport
import GHC.Records
import Data.Proxy (Proxy (Proxy))
import ClassyPrelude
import Foundation.LoginSupport.Types (throwNotLoggedIn, HasNewSessionUrl (newSessionUrl))

currentUser :: forall user controllerContext. (?controllerContext :: controllerContext, HasField "user" controllerContext (Maybe user), HasNewSessionUrl user) => user
currentUser = fromMaybe (throwNotLoggedIn (pure (newSessionUrl (Proxy @user)))) currentUserOrNothing

currentUserOrNothing :: forall controllerContext user. (?controllerContext :: controllerContext, HasField "user" controllerContext (Maybe user), HasNewSessionUrl user) => (Maybe user)
currentUserOrNothing = ?controllerContext |> get #user

currentUserId :: forall controllerContext user userId. (?controllerContext :: controllerContext, HasField "user" controllerContext (Maybe user), HasNewSessionUrl user, HasField "id" user userId) => userId
currentUserId = currentUser |> get #id
