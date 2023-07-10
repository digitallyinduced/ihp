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
, module IHP.AuthSupport.Authentication
, enableRowLevelSecurityIfLoggedIn
, currentRoleOrNothing
, currentRole
, currentRoleId
, ensureIsRole
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
import IHP.AuthSupport.Authentication
import IHP.Controller.Context
import qualified IHP.FrameworkConfig as FrameworkConfig
import qualified Database.PostgreSQL.Simple.ToField as PG
import Data.Kind
import Data.Typeable

currentRoleOrNothing :: forall user. (?context :: ControllerContext, HasNewSessionUrl user, Typeable user) => Maybe user
currentRoleOrNothing = case unsafePerformIO (maybeFromContext @(Maybe user)) of
    Just user -> user
    Nothing -> error ("initAuthentication @" <> show (typeRep (Proxy @user)) <> " has not been called in initContext inside FrontController of this application")
{-# INLINE currentRoleOrNothing #-}

currentRole :: forall user. (?context :: ControllerContext, HasNewSessionUrl user, Typeable user) => user
currentRole = fromMaybe (redirectToLogin (newSessionUrl (Proxy @user))) (currentRoleOrNothing @user)
{-# INLINE currentRole #-}

currentRoleId :: forall user userId. (?context :: ControllerContext, HasNewSessionUrl user, HasField "id" user userId, Typeable user) => userId
currentRoleId = (currentRole @user).id
{-# INLINE currentRoleId #-}

ensureIsRole :: forall (user :: Type). (?context :: ControllerContext, HasNewSessionUrl user, Typeable user) => IO ()
ensureIsRole =
    case currentRoleOrNothing @user of
        Just _ -> pure ()
        Nothing -> redirectToLoginWithMessage (newSessionUrl (Proxy :: Proxy user))
{-# INLINABLE ensureIsRole #-}

currentUser :: forall user. (?context :: ControllerContext, HasNewSessionUrl user, Typeable user, user ~ CurrentUserRecord) => user
currentUser = currentRole @user
{-# INLINABLE currentUser #-}

currentUserOrNothing :: forall user. (?context :: ControllerContext, HasNewSessionUrl user, Typeable user, user ~ CurrentUserRecord) => (Maybe user)
currentUserOrNothing = currentRoleOrNothing @user
{-# INLINABLE currentUserOrNothing #-}

currentUserId :: forall user userId. (?context :: ControllerContext, HasNewSessionUrl user, HasField "id" user userId, Typeable user, user ~ CurrentUserRecord) => userId
currentUserId = currentRoleId @user
{-# INLINABLE currentUserId #-}

ensureIsUser :: forall user userId. (?context :: ControllerContext, HasNewSessionUrl user, Typeable user, user ~ CurrentUserRecord) => IO ()
ensureIsUser = ensureIsRole @user
{-# INLINABLE ensureIsUser #-}

currentAdmin :: forall admin. (?context :: ControllerContext, HasNewSessionUrl admin, Typeable admin, admin ~ CurrentAdminRecord) => admin
currentAdmin = currentRole @admin
{-# INLINABLE currentAdmin #-}

currentAdminOrNothing :: forall admin. (?context :: ControllerContext, HasNewSessionUrl admin, Typeable admin, admin ~ CurrentAdminRecord) => (Maybe admin)
currentAdminOrNothing = currentRoleOrNothing @admin
{-# INLINABLE currentAdminOrNothing #-}

currentAdminId :: forall admin adminId. (?context :: ControllerContext, HasNewSessionUrl admin, HasField "id" admin adminId, Typeable admin, admin ~ CurrentAdminRecord) => adminId
currentAdminId = currentRoleId @admin
{-# INLINABLE currentAdminId #-}

ensureIsAdmin :: forall (admin :: Type) adminId. (?context :: ControllerContext, HasNewSessionUrl admin, Typeable admin, admin ~ CurrentAdminRecord) => IO ()
ensureIsAdmin = ensureIsRole @admin
{-# INLINABLE ensureIsAdmin #-}

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
login user = Session.setSession (sessionKey @user) (tshow (user.id))
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

sessionKey :: forall user. (KnownSymbol (ModelSupport.GetModelName user)) => ByteString
sessionKey = "login." <> cs (ModelSupport.getModelName @user)
{-# INLINABLE sessionKey #-}

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
            let rlsAuthenticatedRole = ?context.frameworkConfig.rlsAuthenticatedRole
            let rlsUserId = PG.toField user.id
            let rlsContext = ModelSupport.RowLevelSecurityContext { rlsAuthenticatedRole, rlsUserId}
            putContext rlsContext
        Nothing -> pure ()