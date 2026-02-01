{-# LANGUAGE AllowAmbiguousTypes #-}

module IHP.LoginSupport.Helper.Controller
( currentUser
, currentUserOrNothing
, currentUserId
, currentUserIdOrNothing
, ensureIsUser
, HasNewSessionUrl
, currentAdmin
, currentAdminOrNothing
, currentAdminId
, currentAdminIdOrNothing
, ensureIsAdmin
, login
, sessionKey
, logout
, CurrentUserRecord
, CurrentAdminRecord
, module IHP.AuthSupport.Authentication
, enableRowLevelSecurityIfLoggedIn
) where

import IHP.Prelude
import IHP.Controller.Redirect
import IHP.Controller.Session
import IHP.LoginSupport.Types
import qualified IHP.Controller.Session as Session
import qualified Network.Wai as Wai
import IHP.FlashMessages
import qualified IHP.ModelSupport as ModelSupport
import IHP.ControllerSupport
import System.IO.Unsafe (unsafePerformIO)
import IHP.AuthSupport.Authentication
import IHP.Controller.Context
import qualified IHP.FrameworkConfig as FrameworkConfig
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Data.UUID as UUID

-- | Returns the current user or 'Nothing' if not logged in.
--
-- Reads from the WAI request vault, populated by 'authMiddleware'.
--
-- Requires @AuthMiddleware (authMiddleware \@User)@ in Config.hs.
currentUserOrNothing :: forall user. (?request :: Wai.Request, user ~ CurrentUserRecord, Typeable user) => Maybe user
currentUserOrNothing = lookupAuthVault currentUserVaultKey ?request
{-# INLINE currentUserOrNothing #-}

-- | Returns the current user. Redirects to login if not logged in.
currentUser :: forall user. (?context :: ControllerContext, ?request :: Wai.Request, HasNewSessionUrl user, Typeable user, user ~ CurrentUserRecord) => user
currentUser = fromMaybe (redirectToLogin (newSessionUrl (Proxy @user))) currentUserOrNothing
{-# INLINABLE currentUser #-}

-- | Returns the ID of the current user. Redirects to login if not logged in.
currentUserId :: forall user userId. (?context :: ControllerContext, ?request :: Wai.Request, HasNewSessionUrl user, HasField "id" user userId, Typeable user, user ~ CurrentUserRecord) => userId
currentUserId = (currentUser @user).id
{-# INLINABLE currentUserId #-}

-- | Ensures that a user is logged in. Redirects to login page if not.
ensureIsUser :: forall user. (?context :: ControllerContext, ?request :: Wai.Request, HasNewSessionUrl user, Typeable user, user ~ CurrentUserRecord) => IO ()
ensureIsUser =
    case currentUserOrNothing @user of
        Just _ -> pure ()
        Nothing -> redirectToLoginWithMessage (newSessionUrl (Proxy :: Proxy user))
{-# INLINABLE ensureIsUser #-}

-- | Returns the current user's UUID or 'Nothing' if not logged in.
--
-- This only requires 'userIdMiddleware', no database query is needed.
--
-- > userId <- currentUserIdOrNothing
currentUserIdOrNothing :: (?request :: Wai.Request, ModelSupport.PrimaryKey (ModelSupport.GetTableName CurrentUserRecord) ~ UUID) => Maybe (ModelSupport.Id CurrentUserRecord)
currentUserIdOrNothing = ModelSupport.Id <$> lookupAuthVault currentUserIdVaultKey ?request
{-# INLINE currentUserIdOrNothing #-}

-- | Returns the current admin or 'Nothing' if not logged in.
--
-- Reads from the WAI request vault, populated by 'authMiddleware'.
--
-- Requires @AdminAuthMiddleware (adminAuthMiddleware \@Admin)@ in Config.hs.
currentAdminOrNothing :: forall admin. (?request :: Wai.Request, admin ~ CurrentAdminRecord, Typeable admin) => Maybe admin
currentAdminOrNothing = lookupAuthVault currentAdminVaultKey ?request
{-# INLINE currentAdminOrNothing #-}

-- | Returns the current admin. Redirects to login if not logged in.
currentAdmin :: forall admin. (?context :: ControllerContext, ?request :: Wai.Request, HasNewSessionUrl admin, Typeable admin, admin ~ CurrentAdminRecord) => admin
currentAdmin = fromMaybe (redirectToLogin (newSessionUrl (Proxy @admin))) currentAdminOrNothing
{-# INLINABLE currentAdmin #-}

-- | Returns the ID of the current admin. Redirects to login if not logged in.
currentAdminId :: forall admin adminId. (?context :: ControllerContext, ?request :: Wai.Request, HasNewSessionUrl admin, HasField "id" admin adminId, Typeable admin, admin ~ CurrentAdminRecord) => adminId
currentAdminId = (currentAdmin @admin).id
{-# INLINABLE currentAdminId #-}

-- | Returns the current admin's UUID or 'Nothing' if not logged in.
--
-- This only requires 'adminIdMiddleware', no database query is needed.
currentAdminIdOrNothing :: (?request :: Wai.Request, ModelSupport.PrimaryKey (ModelSupport.GetTableName CurrentAdminRecord) ~ UUID) => Maybe (ModelSupport.Id CurrentAdminRecord)
currentAdminIdOrNothing = ModelSupport.Id <$> lookupAuthVault currentAdminIdVaultKey ?request
{-# INLINE currentAdminIdOrNothing #-}

-- | Ensures that an admin is logged in. Redirects to login page if not.
ensureIsAdmin :: forall (admin :: Type). (?context :: ControllerContext, ?request :: Wai.Request, HasNewSessionUrl admin, Typeable admin, admin ~ CurrentAdminRecord) => IO ()
ensureIsAdmin =
    case currentAdminOrNothing @admin of
        Just _ -> pure ()
        Nothing -> redirectToLoginWithMessage (newSessionUrl (Proxy :: Proxy admin))
{-# INLINABLE ensureIsAdmin #-}

-- | Log in a user
--
-- Stores the user's UUID in the session as raw ASCII bytes (36 bytes).
--
-- Examples:
--
-- > action ExampleAction = do
-- >     user <- query @User |> fetchOne
-- >     login user
-- >
-- >     redirectToPath "/"
--
login :: forall user.
    ( ?request :: Wai.Request
    , KnownSymbol (ModelSupport.GetModelName user)
    , HasField "id" user (ModelSupport.Id user)
    , ModelSupport.PrimaryKey (ModelSupport.GetTableName user) ~ UUID
    ) => user -> IO ()
login user = sessionInsert (sessionKey @user) (UUID.toASCIIBytes (ModelSupport.unpackId user.id))
{-# INLINABLE login #-}

-- | Log out a user
--
-- Example:
--
-- > action LogoutAction = do
-- >     let user = currentUser
-- >     logout user
-- >
-- >     redirectToPath "/"
--
logout :: forall user. (?request :: Wai.Request, KnownSymbol (ModelSupport.GetModelName user)) => user -> IO ()
logout user = deleteSession (sessionKey @user)
{-# INLINABLE logout #-}

sessionKey :: forall user. (KnownSymbol (ModelSupport.GetModelName user)) => ByteString
sessionKey = "login." <> cs (ModelSupport.getModelName @user)
{-# INLINABLE sessionKey #-}

redirectToLoginWithMessage :: (?request :: Wai.Request) => Text -> IO ()
redirectToLoginWithMessage newSessionPath = do
    setSuccessMessage "Please log in to access this page"
    setSession "IHP.LoginSupport.redirectAfterLogin" getRequestPathAndQuery
    redirectToPath newSessionPath
    error "Unreachable"


redirectToLogin :: (?request :: Wai.Request) => Text -> a
redirectToLogin newSessionPath = unsafePerformIO $ do
    redirectToPath newSessionPath
    error "Unreachable"

-- | After this call the security policies defined in your Schema.sql will be applied to the controller actions called after this
--
-- __Example:__
--
-- > instance InitControllerContext WebApplication where
-- >     initContext = do
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
    , ?request :: Wai.Request
    , ModelSupport.PrimaryKey (ModelSupport.GetTableName CurrentUserRecord) ~ UUID
    ) => IO ()
enableRowLevelSecurityIfLoggedIn = do
    case currentUserIdOrNothing of
        Just userId -> do
            let rlsAuthenticatedRole = ?context.frameworkConfig.rlsAuthenticatedRole
            let rlsUserId = PG.toField userId
            let rlsContext = ModelSupport.RowLevelSecurityContext { rlsAuthenticatedRole, rlsUserId }
            putContext rlsContext
        Nothing -> pure ()
