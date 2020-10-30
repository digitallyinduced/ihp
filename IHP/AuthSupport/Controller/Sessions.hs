{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module: IHP.AuthSupport.Controller.Sessions
Description: Provides action implementations for SessionControllers
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AuthSupport.Controller.Sessions
( newSessionAction
, createSessionAction
, deleteSessionAction
, SessionsControllerConfig (..)
)
where

import IHP.Prelude
import IHP.ControllerPrelude hiding (Success, currentUserOrNothing)
import IHP.AuthSupport.View.Sessions.New
import IHP.AuthSupport.Authentication
import IHP.FrameworkConfig
import IHP.ViewSupport (View, CreateViewContext, Layout)
import IHP.LoginSupport.Types
import IHP.LoginSupport.Helper.Controller hiding (currentUserOrNothing)
import Data.Data
import Data.Maybe (fromJust)
import qualified IHP.AuthSupport.Lockable as Lockable

-- | Displays the login form.
--
-- In case the user is already logged in, redirects to the home page ('afterLoginRedirectPath').
newSessionAction :: forall record action viewContext.
    ( ?theAction :: action
    , ?controllerContext :: ControllerContext
    , ?requestContext :: RequestContext
    , HasNewSessionUrl record
    , ?modelContext :: ModelContext
    , Typeable record
    , View (NewView record) viewContext
    , CreateViewContext viewContext
    , HasField "layout" viewContext Layout
    , Data action
    , Record record
    , HasPath action
    , SessionsControllerConfig record
    , FrameworkConfig
    ) => IO ()
newSessionAction = do
    let alreadyLoggedIn = isJust (currentUserOrNothing @record)
    when alreadyLoggedIn (redirectToPath (afterLoginRedirectPath @record))

    let user = newRecord @record
    render NewView { .. }
{-# INLINE newSessionAction #-}

-- | Logs in a user when a valid email and password is given
--
-- After 10 failed attempts, the user is locked for an hours. See 'maxFailedLoginAttemps' to customize this.
--
-- After a successful login, the user is redirect to 'afterLoginRedirectPath'.
createSessionAction :: forall record action passwordField.
    (?theAction :: action
    , ?controllerContext :: ControllerContext
    , ?requestContext :: RequestContext
    , ?modelContext :: ModelContext
    , Data action
    , HasField "email" record Text
    , HasPath action
    , HasField "id" record (Id record)
    , HasField "passwordHash" record Text
    , SessionsControllerConfig record
    , UpdateField "lockedAt" record record (Maybe UTCTime) (Maybe UTCTime)
    , HasField "failedLoginAttempts" record Int
    , SetField "failedLoginAttempts" record Int
    , CanUpdate record
    , FrameworkConfig
    , Show (PrimaryKey (GetTableName record))
    , record ~ GetModelByTableName (GetTableName record)
    ) => IO ()
createSessionAction = do
    query @record
    |> findMaybeBy #email (param @Text "email")
    >>= \case
        Just (user :: record) -> do
            isLocked <- Lockable.isLocked user
            when isLocked do
                setErrorMessage "User is locked"
                redirectTo buildNewSessionAction

            if verifyPassword user (param @Text "password")
                then do
                    beforeLogin user
                    login user
                    user <- user
                            |> set #failedLoginAttempts 0
                            |> updateRecord
                    redirectUrl <- getSessionAndClear "IHP.LoginSupport.redirectAfterLogin"
                    redirectToPath (fromMaybe (afterLoginRedirectPath @record) redirectUrl)
                else do
                    setErrorMessage "Invalid Credentials"
                    user :: record <- user
                            |> incrementField #failedLoginAttempts
                            |> updateRecord
                    when (get #failedLoginAttempts user >= maxFailedLoginAttemps user) do
                        Lockable.lock user
                        pure ()
                    redirectTo buildNewSessionAction
        Nothing -> do
            setErrorMessage "Invalid Credentials"
            redirectTo buildNewSessionAction
{-# INLINE createSessionAction #-}

-- | Logs out the user and redirect back to the login page
deleteSessionAction :: forall record action id.
    ( ?theAction :: action
    , ?controllerContext :: ControllerContext
    , ?requestContext :: RequestContext
    , ?modelContext :: ModelContext
    , Data action
    , HasPath action
    , Show id
    , HasField "id" record id
    , SessionsControllerConfig record
    , FrameworkConfig
    ) => IO ()
deleteSessionAction = do
    case currentUserOrNothing @record of
        Just user -> logout user
        Nothing -> pure ()
    redirectTo buildNewSessionAction
{-# INLINE deleteSessionAction #-}


currentUserOrNothing :: forall user. (?controllerContext :: ControllerContext, ?requestContext :: RequestContext, FrameworkConfig, HasNewSessionUrl user, Typeable user) => (Maybe user)
currentUserOrNothing =
    case maybeFromControllerContext @(Maybe user) of
        Just user -> user
        Nothing -> error "currentUserOrNothing: initAuthentication has not been called in initContext inside FrontController of this application"
{-# INLINE currentUserOrNothing #-}

-- | Returns the NewSessionAction action for the given SessionsController
buildNewSessionAction :: forall controller. (?theAction :: controller, Data controller) => controller
buildNewSessionAction = fromConstr createConstructor
    where
        createConstructor :: Constr
        (Just createConstructor) = find isNewSessionConstructor allConstructors

        allConstructors :: [Constr]
        allConstructors = dataTypeConstrs (dataTypeOf ?theAction)

        isNewSessionConstructor :: Constr -> Bool
        isNewSessionConstructor constructor = "NewSessionAction" == showConstr constructor
{-# INLINE buildNewSessionAction #-}

-- | Configuration for the session controller actions
class ( Typeable record
    , Show record
    , KnownSymbol (GetModelName record)
    , HasNewSessionUrl record
    , KnownSymbol (GetTableName record)
    , FromRow record
    ) => SessionsControllerConfig record where

    -- | Your home page, where the user is redirect after login
    afterLoginRedirectPath :: Text 
    afterLoginRedirectPath = "/"

    -- | After 10 failed login attempts the user will be locked for an hour
    maxFailedLoginAttemps :: record -> Int
    maxFailedLoginAttemps _ = 10

    -- | Callback that is executed just before the user is logged
    -- 
    -- This is called only after checking that the password is correct. When a wrong password is given this callback is not executed.
    -- 
    -- __Example: Disallow login until user is confirmed__
    --
    -- > beforeLogin user = do
    -- >     unless (get #isConfirmed user) do
    -- >         setErrorMessage "Please click the confirmation link we sent to your email before you can use IHP Cloud"
    -- >         redirectTo NewSessionAction
    beforeLogin :: (?requestContext :: RequestContext, ?controllerContext :: ControllerContext) => record -> IO ()
    beforeLogin _ = pure ()