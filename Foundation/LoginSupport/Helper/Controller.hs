{-# LANGUAGE AllowAmbiguousTypes #-}

module Foundation.LoginSupport.Helper.Controller (currentUser, currentUserOrNothing, currentUserId, ensureIsUser, HasNewSessionUrl, currentAdmin, currentAdminOrNothing, currentAdminId, ensureIsAdmin) where

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

currentAdmin :: forall admin controllerContext. (?controllerContext :: controllerContext, HasField' "admin" controllerContext (Maybe admin), HasNewSessionUrl admin, Generic admin, Generic controllerContext) => admin
currentAdmin = fromMaybe (throwNotLoggedIn (pure (newSessionUrl (Proxy @admin)))) currentAdminOrNothing

{-# INLINE currentAdminOrNothing #-}
currentAdminOrNothing :: forall controllerContext admin. (?controllerContext :: controllerContext, HasField' "admin" controllerContext (Maybe admin), HasNewSessionUrl admin, Generic controllerContext) => (Maybe admin)
currentAdminOrNothing = ?controllerContext |> get #admin

{-# INLINE currentAdminId #-}
currentAdminId :: forall controllerContext admin adminId. (?controllerContext :: controllerContext, HasField' "admin" controllerContext (Maybe admin), HasNewSessionUrl admin, HasField' "id" admin adminId, Generic admin, Generic controllerContext) => adminId
currentAdminId = currentAdmin |> get #id

ensureIsAdmin :: forall controllerContext admin adminId. (?controllerContext :: controllerContext, HasField' "admin" controllerContext (Maybe admin), HasNewSessionUrl admin, HasField' "id" admin adminId, Generic admin, Generic controllerContext) => IO ()
ensureIsAdmin =
    case currentAdminOrNothing of
        Just _ -> return ()
        Nothing -> Foundation.LoginSupport.Types.throwNotLoggedIn (Just (newSessionUrl (Proxy :: Proxy admin)))