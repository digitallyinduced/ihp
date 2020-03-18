{-# LANGUAGE AllowAmbiguousTypes #-}

module TurboHaskell.LoginSupport.Helper.Controller (currentUser, currentUserOrNothing, currentUserId, ensureIsUser, HasNewSessionUrl, currentAdmin, currentAdminOrNothing, currentAdminId, ensureIsAdmin, login, sessionKey, logout, CurrentUserRecord, CurrentAdminRecord) where

import TurboHaskell.HaskellSupport
import Data.Proxy (Proxy (Proxy))
import ClassyPrelude
import TurboHaskell.LoginSupport.Types (throwNotLoggedIn, HasNewSessionUrl (newSessionUrl))
import TurboHaskell.Controller.RequestContext
import qualified TurboHaskell.Controller.Session as Session
import qualified TurboHaskell.ModelSupport as ModelSupport
import TurboHaskell.ControllerSupport
import GHC.TypeLits
import GHC.Records

type family CurrentUserRecord
type family CurrentAdminRecord

{-# INLINE currentUser #-}
currentUser :: forall user. (?controllerContext :: ControllerContext, HasNewSessionUrl user, Typeable user, user ~ CurrentUserRecord) => user
currentUser = fromMaybe (throwNotLoggedIn (pure (newSessionUrl (Proxy @user)))) currentUserOrNothing

{-# INLINE currentUserOrNothing #-}
currentUserOrNothing :: forall user. (?controllerContext :: ControllerContext, HasNewSessionUrl user, Typeable user, user ~ CurrentUserRecord) => (Maybe user)
currentUserOrNothing = fromControllerContext @(Maybe user)

{-# INLINE currentUserId #-}
currentUserId :: forall user userId. (?controllerContext :: ControllerContext, HasNewSessionUrl user, HasField "id" user userId, Typeable user, user ~ CurrentUserRecord) => userId
currentUserId = currentUser @user |> get #id

{-# INLINE ensureIsUser #-}
ensureIsUser :: forall user userId. (?controllerContext :: ControllerContext, HasNewSessionUrl user, HasField "id" user userId, Typeable user, user ~ CurrentUserRecord) => IO ()
ensureIsUser =
    case currentUserOrNothing @user of
        Just _ -> pure ()
        Nothing -> TurboHaskell.LoginSupport.Types.throwNotLoggedIn (Just (newSessionUrl (Proxy :: Proxy user)))

{-# INLINE currentAdmin #-}
currentAdmin :: forall admin. (?controllerContext :: ControllerContext, HasNewSessionUrl admin, Typeable admin) => admin
currentAdmin = fromMaybe (throwNotLoggedIn (pure (newSessionUrl (Proxy @admin)))) currentAdminOrNothing

{-# INLINE currentAdminOrNothing #-}
currentAdminOrNothing :: forall admin. (?controllerContext :: ControllerContext, HasNewSessionUrl admin, Typeable admin) => (Maybe admin)
currentAdminOrNothing = fromControllerContext @(Maybe admin)

{-# INLINE currentAdminId #-}
currentAdminId :: forall admin adminId. (?controllerContext :: ControllerContext, HasNewSessionUrl admin, HasField "id" admin adminId, Typeable admin) => adminId
currentAdminId = currentAdmin @admin |> get #id

{-# INLINE ensureIsAdmin #-}
ensureIsAdmin :: forall admin adminId. (?controllerContext :: ControllerContext, HasNewSessionUrl admin, Typeable admin) => IO ()
ensureIsAdmin =
    case currentAdminOrNothing @admin of
        Just _ -> pure ()
        Nothing -> TurboHaskell.LoginSupport.Types.throwNotLoggedIn (Just (newSessionUrl (Proxy :: Proxy admin)))

-- Log's in an entity
-- Examples:
-- ```
-- let user :: User = ... in login user
-- let admin :: Admin = ... in login admin
-- ```
{-# INLINE login #-}
login :: forall user id. (?controllerContext :: ControllerContext, ?requestContext :: RequestContext, KnownSymbol (ModelSupport.GetModelName user), HasField "id" user id, Show id) => user -> IO ()
login user = Session.setSession (sessionKey @user) (tshow (get #id user))

-- Log's out an entity
{-# INLINE logout #-}
logout :: forall user id. (?controllerContext :: ControllerContext, ?requestContext :: RequestContext, KnownSymbol (ModelSupport.GetModelName user), HasField "id" user id, Show id) => user -> IO ()
logout user = Session.setSession (sessionKey @user) ""

{-# INLINE sessionKey #-}
sessionKey :: forall user. (KnownSymbol (ModelSupport.GetModelName user)) => Text
sessionKey = "login." <> ModelSupport.getModelName @user
