{-# LANGUAGE AllowAmbiguousTypes #-}

module Foundation.LoginSupport.Helper.Controller (currentUser, currentUserOrNothing, currentUserId, ensureIsUser, HasNewSessionUrl) where

import Foundation.HaskellSupport
import Control.Lens hiding ((|>))
import Data.Generics.Product
import Data.Proxy (Proxy (Proxy))
import ClassyPrelude
import Foundation.LoginSupport.Types (throwNotLoggedIn, HasNewSessionUrl (newSessionUrl))

currentUser :: forall user controllerContext. (?controllerContext :: controllerContext, HasField' "user" controllerContext (Maybe user), HasNewSessionUrl user, Generic user, Generic controllerContext) => user
currentUser = fromMaybe (throwNotLoggedIn (pure (newSessionUrl (Proxy @user)))) currentUserOrNothing

{-# INLINE currentUserOrNothing #-}
currentUserOrNothing :: forall controllerContext user. (?controllerContext :: controllerContext, HasField' "user" controllerContext (Maybe user), HasNewSessionUrl user, Generic controllerContext) => (Maybe user)
currentUserOrNothing = ?controllerContext |> get #user

{-# INLINE currentUserId #-}
currentUserId :: forall controllerContext user userId. (?controllerContext :: controllerContext, HasField' "user" controllerContext (Maybe user), HasNewSessionUrl user, HasField' "id" user userId, Generic user, Generic controllerContext) => userId
currentUserId = currentUser |> get #id

ensureIsUser :: forall controllerContext user userId. (?controllerContext :: controllerContext, HasField' "user" controllerContext (Maybe user), HasNewSessionUrl user, HasField' "id" user userId, Generic user, Generic controllerContext) => IO ()
ensureIsUser =
    case currentUserOrNothing of
        Just _ -> return ()
        Nothing -> Foundation.LoginSupport.Types.throwNotLoggedIn (Just (newSessionUrl (Proxy :: Proxy user)))