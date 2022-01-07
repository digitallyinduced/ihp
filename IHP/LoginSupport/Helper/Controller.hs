{-# LANGUAGE AllowAmbiguousTypes #-}

module IHP.LoginSupport.Helper.Controller
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
, module IHP.AuthSupport.Authorization
, module IHP.AuthSupport.Authentication
, enableRowLevelSecurityIfLoggedIn
) where

import IHP.Prelude
import IHP.Controller.Redirect
import IHP.Controller.Session
import IHP.LoginSupport.Types
import qualified IHP.Controller.Session as Session
import IHP.FlashMessages.ControllerFunctions
import qualified IHP.ModelSupport as ModelSupport
import IHP.ControllerSupport
import System.IO.Unsafe (unsafePerformIO)
import IHP.AuthSupport.Authorization
import IHP.AuthSupport.Authentication
import IHP.Controller.Context
import qualified IHP.FrameworkConfig as FrameworkConfig
import qualified Database.PostgreSQL.Simple.ToField as PG

{-# INLINABLE currentUser #-}
currentUser :: forall user. (?context :: ControllerContext, HasNewSessionUrl user, Typeable user, user ~ CurrentUserRecord) => user
currentUser = fromMaybe (redirectToLogin (newSessionUrl (Proxy @user))) currentUserOrNothing

{-# INLINABLE currentUserOrNothing #-}
currentUserOrNothing :: forall user. (?context :: ControllerContext, HasNewSessionUrl user, Typeable user, user ~ CurrentUserRecord) => (Maybe user)
currentUserOrNothing = case unsafePerformIO (maybeFromContext @(Maybe user)) of
    Just user -> user
    Nothing -> error "currentUserOrNothing: initAuthentication @User has not been called in initContext inside FrontController of this application"

{-# INLINABLE currentUserId #-}
currentUserId :: forall user userId. (?context :: ControllerContext, HasNewSessionUrl user, HasField "id" user userId, Typeable user, user ~ CurrentUserRecord) => userId
currentUserId = currentUser @user |> get #id

{-# INLINABLE ensureIsUser #-}
ensureIsUser :: forall user userId. (?context :: ControllerContext, HasNewSessionUrl user, HasField "id" user userId, Typeable user, user ~ CurrentUserRecord) => IO ()
ensureIsUser =
    case currentUserOrNothing @user of
        Just _ -> pure ()
        Nothing -> redirectToLoginWithMessage (newSessionUrl (Proxy :: Proxy user))

{-# INLINABLE currentAdmin #-}
currentAdmin :: forall admin. (?context :: ControllerContext, HasNewSessionUrl admin, Typeable admin) => admin
currentAdmin = fromMaybe (redirectToLogin (newSessionUrl (Proxy @admin))) currentAdminOrNothing

{-# INLINABLE currentAdminOrNothing #-}
currentAdminOrNothing :: forall admin. (?context :: ControllerContext, HasNewSessionUrl admin, Typeable admin) => (Maybe admin)
currentAdminOrNothing = case unsafePerformIO (maybeFromContext @(Maybe admin)) of
    Just admin -> admin
    Nothing -> error "currentAdminOrNothing: initAuthentication @Admin has not been called in initContext inside FrontController of this application"

{-# INLINABLE currentAdminId #-}
currentAdminId :: forall admin adminId. (?context :: ControllerContext, HasNewSessionUrl admin, HasField "id" admin adminId, Typeable admin) => adminId
currentAdminId = currentAdmin @admin |> get #id

{-# INLINABLE ensureIsAdmin #-}
ensureIsAdmin :: forall admin adminId. (?context :: ControllerContext, HasNewSessionUrl admin, Typeable admin) => IO ()
ensureIsAdmin =
    case currentAdminOrNothing @admin of
        Just _ -> pure ()
        Nothing -> redirectToLoginWithMessage (newSessionUrl (Proxy :: Proxy admin))

-- | Log's in a user
--
-- Examples:
-- 
-- > action ExampleAction = do
-- >     user <- query @User |> fetchOne
-- >     login user
-- >     
-- >     redirectToPath "/"
--
login :: forall user id. (?context :: ControllerContext, KnownSymbol (ModelSupport.GetModelName user), HasField "id" user id, Show id) => user -> IO ()
login user = Session.setSession (sessionKey @user) (tshow (get #id user))
{-# INLINABLE login #-}

-- | Log's out a user
--
-- Example:
--
-- > action LogoutAction = do
-- >     let user = currentUser
-- >     logout user
-- >     
-- >     redirectToPath "/"
--
logout :: forall user. (?context :: ControllerContext, KnownSymbol (ModelSupport.GetModelName user)) => user -> IO ()
logout user = Session.setSession (sessionKey @user) ("" :: Text)
{-# INLINABLE logout #-}

{-# INLINABLE sessionKey #-}
sessionKey :: forall user. (KnownSymbol (ModelSupport.GetModelName user)) => ByteString
sessionKey = "login." <> cs (ModelSupport.getModelName @user)

redirectToLoginWithMessage :: (?context :: ControllerContext) => Text -> IO ()
redirectToLoginWithMessage newSessionPath = do
    setSuccessMessage "Please log in to access this page"
    setSession "IHP.LoginSupport.redirectAfterLogin" getRequestPathAndQuery
    redirectToPath newSessionPath
    error "Unreachable"


redirectToLogin :: (?context :: ControllerContext) => Text -> a
redirectToLogin newSessionPath = unsafePerformIO $ do
    redirectToPath newSessionPath
    error "Unreachable"

-- | After this call the security policies defined in your Schema.sql will be applied to the controller actions called after this
--
-- __Example:__
--
-- > instance InitControllerContext WebApplication where
-- >     initContext = do
-- >         initAuthentication @User
-- >         enableRowLevelSecurityIfLoggedIn
--
-- Let's assume we have a policy defined in our Schema.sql that only allows users to see and edit rows in the projects table that have @projects.user_id = current_user_id@:
--
-- > CREATE POLICY "Users can manage their projects" ON projects USING (user_id = ihp_user_id()) WITH CHECK (user_id = ihp_user_id());
--
-- Now any database queries to our @projects@ table will have this policy applied.
--
-- E.g. this action will now only show the users projects, even though no explicit @filterWhere (#userId, currentUserId)@ is specified on the query:
--
-- > action ProjectsAction = do
-- >     projects <- query @Project |> fetch
--
enableRowLevelSecurityIfLoggedIn ::
    ( ?context :: ControllerContext
    , Typeable CurrentUserRecord
    , HasNewSessionUrl CurrentUserRecord
    , HasField "id" CurrentUserRecord userId
    , PG.ToField userId
    ) => IO ()
enableRowLevelSecurityIfLoggedIn = do
    case currentUserOrNothing of
        Just user -> do
            let rlsAuthenticatedRole = ?context
                    |> FrameworkConfig.getFrameworkConfig
                    |> get #rlsAuthenticatedRole
            let rlsUserId = PG.toField (get #id user)
            let rlsContext = ModelSupport.RowLevelSecurityContext { rlsAuthenticatedRole, rlsUserId}
            putContext rlsContext
        Nothing -> pure ()