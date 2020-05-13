{-# LANGUAGE AllowAmbiguousTypes #-}

module TurboHaskell.LoginSupport.Helper.Controller
( currentUser
, currentUserOrNothing
, currentUserId
, ensureIsUser
, HasNewSessionUrl
, currentAdmin
, currentAdminOrNothing
, currentAdminId
, ensureIsAdmin
, login
, sessionKey
, logout
, CurrentUserRecord
, CurrentAdminRecord
, accessDeniedUnless
) where

import TurboHaskell.ControllerPrelude
import TurboHaskell.LoginSupport.Types
import TurboHaskell.Controller.RequestContext
import qualified TurboHaskell.Controller.Session as Session
import qualified TurboHaskell.ModelSupport as ModelSupport
import TurboHaskell.ControllerSupport
import TurboHaskell.FrameworkConfig
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Fail

type family CurrentUserRecord
type family CurrentAdminRecord

{-# INLINE currentUser #-}
currentUser :: forall user. (?controllerContext :: ControllerContext, ?requestContext :: RequestContext, FrameworkConfig, HasNewSessionUrl user, Typeable user, user ~ CurrentUserRecord) => user
currentUser = fromMaybe (redirectToLogin (newSessionUrl (Proxy @user))) currentUserOrNothing

{-# INLINE currentUserOrNothing #-}
currentUserOrNothing :: forall user. (?controllerContext :: ControllerContext, ?requestContext :: RequestContext, FrameworkConfig, HasNewSessionUrl user, Typeable user, user ~ CurrentUserRecord) => (Maybe user)
currentUserOrNothing = fromControllerContext @(Maybe user)

{-# INLINE currentUserId #-}
currentUserId :: forall user userId. (?controllerContext :: ControllerContext, ?requestContext :: RequestContext, FrameworkConfig, HasNewSessionUrl user, HasField "id" user userId, Typeable user, user ~ CurrentUserRecord) => userId
currentUserId = currentUser @user |> get #id

{-# INLINE ensureIsUser #-}
ensureIsUser :: forall user userId. (?controllerContext :: ControllerContext, ?requestContext :: RequestContext, FrameworkConfig, HasNewSessionUrl user, HasField "id" user userId, Typeable user, user ~ CurrentUserRecord) => IO ()
ensureIsUser =
    case currentUserOrNothing @user of
        Just _ -> pure ()
        Nothing -> redirectToLoginWithMessage (newSessionUrl (Proxy :: Proxy user))

{-# INLINE currentAdmin #-}
currentAdmin :: forall admin. (?controllerContext :: ControllerContext, ?requestContext :: RequestContext, FrameworkConfig, HasNewSessionUrl admin, Typeable admin) => admin
currentAdmin = fromMaybe (redirectToLogin (newSessionUrl (Proxy @admin))) currentAdminOrNothing

{-# INLINE currentAdminOrNothing #-}
currentAdminOrNothing :: forall admin. (?controllerContext :: ControllerContext, ?requestContext :: RequestContext, FrameworkConfig, HasNewSessionUrl admin, Typeable admin) => (Maybe admin)
currentAdminOrNothing = fromControllerContext @(Maybe admin)

{-# INLINE currentAdminId #-}
currentAdminId :: forall admin adminId. (?controllerContext :: ControllerContext, ?requestContext :: RequestContext, FrameworkConfig, HasNewSessionUrl admin, HasField "id" admin adminId, Typeable admin) => adminId
currentAdminId = currentAdmin @admin |> get #id

{-# INLINE ensureIsAdmin #-}
ensureIsAdmin :: forall admin adminId. (?controllerContext :: ControllerContext, ?requestContext :: RequestContext, FrameworkConfig, HasNewSessionUrl admin, Typeable admin) => IO ()
ensureIsAdmin =
    case currentAdminOrNothing @admin of
        Just _ -> pure ()
        Nothing -> redirectToLoginWithMessage (newSessionUrl (Proxy :: Proxy admin))

-- | Log's in an entity
-- Examples:
-- ```
-- let user :: User = ... in login user
-- let admin :: Admin = ... in login admin
-- ```
{-# INLINE login #-}
login :: forall user id. (?controllerContext :: ControllerContext, ?requestContext :: RequestContext, ?requestContext :: RequestContext, KnownSymbol (ModelSupport.GetModelName user), HasField "id" user id, Show id) => user -> IO ()
login user = Session.setSession (sessionKey @user) (tshow (get #id user))

-- Log's out an entity
{-# INLINE logout #-}
logout :: forall user id. (?controllerContext :: ControllerContext, ?requestContext :: RequestContext, ?requestContext :: RequestContext, KnownSymbol (ModelSupport.GetModelName user), HasField "id" user id, Show id) => user -> IO ()
logout user = Session.setSession (sessionKey @user) ""

{-# INLINE sessionKey #-}
sessionKey :: forall user. (KnownSymbol (ModelSupport.GetModelName user)) => Text
sessionKey = "login." <> ModelSupport.getModelName @user

redirectToLoginWithMessage :: (?requestContext :: RequestContext, FrameworkConfig) => Text -> IO ()
redirectToLoginWithMessage newSessionPath = do 
    setSuccessMessage "Please log in to access this page"
    setSession "TurboHaskell.LoginSupport.redirectAfterLogin" (cs getRequestUrl)
    redirectToPath newSessionPath
    error "Unreachable"


redirectToLogin :: (?requestContext :: RequestContext, FrameworkConfig) => Text -> a
redirectToLogin newSessionPath = unsafePerformIO $ do
    redirectToPath newSessionPath
    error "Unreachable"

-- | Stops the action execution with an error message when the access condition is false.
--
-- __Example:__ Checking a user is author of a blog post.
-- 
-- > action EditPostAction { postId } = do
-- >     post <- fetch postId
-- >     accessDeniedUnless (get #authorId post == currentUserId)
-- >     
-- >     renderHtml EditView { .. }
--
-- This will throw an error and prevent the view from being rendered when the current user is not author of the post.
accessDeniedUnless :: Bool -> IO ()
accessDeniedUnless condition = if condition then pure () else fail "Access denied"