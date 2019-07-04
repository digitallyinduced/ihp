{-# LANGUAGE AllowAmbiguousTypes #-}

module TurboHaskell.LoginSupport.Helper.Controller (currentUser, currentUserOrNothing, currentUserId, ensureIsUser, HasNewSessionUrl, currentAdmin, currentAdminOrNothing, currentAdminId, ensureIsAdmin, login, sessionKey, logout) where

import TurboHaskell.HaskellSupport
import Control.Lens hiding ((|>))
import Data.Generics.Product
import Data.Proxy (Proxy (Proxy))
import ClassyPrelude
import TurboHaskell.LoginSupport.Types (throwNotLoggedIn, HasNewSessionUrl (newSessionUrl))
import TurboHaskell.Controller.RequestContext
import qualified TurboHaskell.Controller.Session as Session
import qualified TurboHaskell.ModelSupport as ModelSupport
import GHC.TypeLits

{-# INLINE currentUser #-}
currentUser :: forall user controllerContext. (?controllerContext :: controllerContext, HasField' "user" controllerContext (Maybe user), HasNewSessionUrl user, Generic user, Generic controllerContext) => user
currentUser = fromMaybe (throwNotLoggedIn (pure (newSessionUrl (Proxy @user)))) currentUserOrNothing

{-# INLINE currentUserOrNothing #-}
currentUserOrNothing :: forall controllerContext user. (?controllerContext :: controllerContext, HasField' "user" controllerContext (Maybe user), HasNewSessionUrl user, Generic controllerContext) => (Maybe user)
currentUserOrNothing = ?controllerContext |> get #user

{-# INLINE currentUserId #-}
currentUserId :: forall controllerContext user userId. (?controllerContext :: controllerContext, HasField' "user" controllerContext (Maybe user), HasNewSessionUrl user, HasField' "id" user userId, Generic user, Generic controllerContext) => userId
currentUserId = currentUser |> get #id

{-# INLINE ensureIsUser #-}
ensureIsUser :: forall controllerContext user userId. (?controllerContext :: controllerContext, HasField' "user" controllerContext (Maybe user), HasNewSessionUrl user, HasField' "id" user userId, Generic user, Generic controllerContext) => IO ()
ensureIsUser =
    case currentUserOrNothing of
        Just _ -> return ()
        Nothing -> TurboHaskell.LoginSupport.Types.throwNotLoggedIn (Just (newSessionUrl (Proxy :: Proxy user)))

{-# INLINE currentAdmin #-}
currentAdmin :: forall admin controllerContext. (?controllerContext :: controllerContext, HasField' "admin" controllerContext (Maybe admin), HasNewSessionUrl admin, Generic admin, Generic controllerContext) => admin
currentAdmin = fromMaybe (throwNotLoggedIn (pure (newSessionUrl (Proxy @admin)))) currentAdminOrNothing

{-# INLINE currentAdminOrNothing #-}
currentAdminOrNothing :: forall controllerContext admin. (?controllerContext :: controllerContext, HasField' "admin" controllerContext (Maybe admin), HasNewSessionUrl admin, Generic controllerContext) => (Maybe admin)
currentAdminOrNothing = ?controllerContext |> get #admin

{-# INLINE currentAdminId #-}
currentAdminId :: forall controllerContext admin adminId. (?controllerContext :: controllerContext, HasField' "admin" controllerContext (Maybe admin), HasNewSessionUrl admin, HasField' "id" admin adminId, Generic admin, Generic controllerContext) => adminId
currentAdminId = currentAdmin |> get #id

{-# INLINE ensureIsAdmin #-}
ensureIsAdmin :: forall controllerContext admin adminId. (?controllerContext :: controllerContext, HasField' "admin" controllerContext (Maybe admin), HasNewSessionUrl admin, HasField' "id" admin adminId, Generic admin, Generic controllerContext) => IO ()
ensureIsAdmin =
    case currentAdminOrNothing of
        Just _ -> return ()
        Nothing -> TurboHaskell.LoginSupport.Types.throwNotLoggedIn (Just (newSessionUrl (Proxy :: Proxy admin)))

-- Log's in an entity
-- Examples:
-- ```
-- let user :: User = ... in login user
-- let admin :: Admin = ... in login admin
-- ```
{-# INLINE login #-}
login :: forall user controllerContext id. (?controllerContext :: controllerContext, ?requestContext :: RequestContext, KnownSymbol (ModelSupport.GetModelName user), HasField' "id" user id, Generic user, Show id) => user -> IO ()
login user = Session.setSession (sessionKey @user) (tshow (get #id user))

-- Log's out an entity
{-# INLINE logout #-}
logout :: forall user controllerContext id. (?controllerContext :: controllerContext, ?requestContext :: RequestContext, KnownSymbol (ModelSupport.GetModelName user), HasField' "id" user id, Generic user, Show id) => user -> IO ()
logout user = Session.setSession (sessionKey @user) ""

sessionKey :: forall user. (KnownSymbol (ModelSupport.GetModelName user)) => Text
sessionKey = "login." <> ModelSupport.getModelName @user
