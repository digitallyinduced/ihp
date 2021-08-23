{-# LANGUAGE AllowAmbiguousTypes #-}
module IHP.OAuth.Github.Controller where

import IHP.ControllerPrelude
import qualified IHP.OAuth.Github.Types as Github
import qualified IHP.OAuth.Github.GithubApi as Github
import IHP.LoginSupport.Types
import qualified IHP.OAuth.Github.Config as Config
import qualified Data.TMap as TMap
import qualified IHP.AuthSupport.Lockable as Lockable
import qualified IHP.AuthSupport.Controller.Sessions as Sessions

newSessionWithGithubAction :: forall user. (?context :: ControllerContext, HasPath Github.GithubOAuthController) => IO ()
newSessionWithGithubAction = do
    state <- Github.initState
    let options = Github.AuthorizeOptions
            { clientId = githubOAuthConfig |> get #clientId
            , redirectUrl = urlTo Github.GithubConnectCallbackAction
            , state }
    Github.redirectToGithubConnect options

githubConnectCallbackAction :: forall user.
    ( GithubOAuthControllerConfig user
    , (HasField "email" user Text)
    , user ~ GetModelByTableName (GetTableName user)
    , FromRow user
    , ?modelContext :: ModelContext
    , KnownSymbol (GetTableName user)
    , GithubOAuthControllerConfig user
    , user ~ CurrentUserRecord
    , HasNewSessionUrl user
    , Typeable user
    , ?context :: ControllerContext
    , SetField "githubUserId" user (Maybe Int)
    , HasField "githubUserId" user (Maybe Int)
    , SetField "email" user Text
    , SetField "passwordHash" user Text
    , Record user
    , CanUpdate user
    , SetField "failedLoginAttempts" user Int
    , KnownSymbol (GetModelName user)
    , HasField "id" user (Id user)
    , Show (PrimaryKey (GetTableName user))
    , Sessions.SessionsControllerConfig user
    , HasField "lockedAt" user (Maybe UTCTime)
    , CanCreate user
    ) => IO ()
githubConnectCallbackAction = do
    let code = param @Text "code"
    state <- Github.verifyState


    accessTokenResponse <- Github.requestGithubAccessToken Github.RequestAccessTokenOptions
            { clientId = githubOAuthConfig |> get #clientId
            , clientSecret = githubOAuthConfig |> get #clientSecret
            , code
            , state }

    let accessToken = get #accessToken accessTokenResponse
    githubUser <- Github.requestGithubUser accessToken

    case currentUserOrNothing of
        Just user -> do
            when (isJust (get #githubUserId user)) do
                setErrorMessage "You're already connected with a Github Account. Please remove the existing Github Account connection first."
                redirectToPath (newSessionUrl (Proxy @user))
            
            user <- user
                    |> setJust #githubUserId (get #id githubUser)
                    |> \user -> beforeLogin user githubUser
                    >>= updateRecord
            
            setSuccessMessage "Your GitHub Account has been linked to this account now"
            redirectToPath (newSessionUrl (Proxy @user))
        Nothing -> do
            user <- query @user
                |> filterWhere (#githubUserId, Just (get #id githubUser))
                |> fetchOneOrNothing

            case user of
                Just user -> do
                    ensureIsNotLocked user
                    Sessions.beforeLogin user
                    login user
                    
                    user
                            |> set #failedLoginAttempts 0
                            |> \user -> beforeLogin user githubUser
                            >>= updateRecord

                    pure ()
                    -- loginExistingUser githubUser user accessToken
                Nothing -> do
                    githubUser <- case githubUser of
                        Github.GithubUser { .. } -> pure Github.GithubUser { .. }
                        Github.GithubUserWithoutEmail { .. } -> do
                            email <- Github.requestGithubUserEmail accessToken
                            pure Github.GithubUser { .. }

                    githubEmail <- case githubUser of
                          Github.GithubUser { email } -> pure email
                          Github.GithubUserWithoutEmail {} -> error "No email available"

                    userWithSameEmail <- query @user
                        |> filterWhere (#email, githubEmail)
                        |> fetchOneOrNothing

                    randomPassword <- generateAuthenticationToken
                    hashed <- hashPassword randomPassword

                    case userWithSameEmail of
                        Just user -> do
                            setErrorMessage "There's an existing user with the same email address already. Please log in with your password and then connect your GitHub account in the account settings."
                            redirectToPath (newSessionUrl (Proxy @user))
                        Nothing -> do
                            let user = newRecord @user
                                    |> set #passwordHash hashed
                                    |> set #email githubEmail
                                    |> setJust #githubUserId (get #id githubUser)

                            user <- createUser (beforeCreateUser user githubUser) githubUser
                            afterCreateUser user
                            login user

            redirectUrl <- getSessionAndClear "IHP.LoginSupport.redirectAfterLogin"
            redirectToPath (fromMaybe (Sessions.afterLoginRedirectPath @user) redirectUrl)

class GithubOAuthControllerConfig user where
    createUser :: (?context :: ControllerContext, ?modelContext :: ModelContext, CanCreate user) => user -> Github.GithubUser -> IO user
    createUser user githubUser = createRecord user
    
    beforeCreateUser :: (?context :: ControllerContext, ?modelContext :: ModelContext, CanCreate user) => user -> Github.GithubUser -> user
    beforeCreateUser user githubUser = user

    afterCreateUser :: (?context :: ControllerContext, ?modelContext :: ModelContext, CanCreate user) => user -> IO ()
    afterCreateUser user = pure ()

    beforeLogin :: (?context :: ControllerContext, ?modelContext :: ModelContext, CanCreate user) => user -> Github.GithubUser -> IO user
    beforeLogin user githubUser = pure user


githubOAuthConfig :: (?context :: ControllerContext) => Github.GithubOAuthConfig
githubOAuthConfig = ?context
            |> getFrameworkConfig
            |> get #appConfig
            |> TMap.lookup @Github.GithubOAuthConfig
            |> fromMaybe (error "Could not find GithubOAuthConfig in config. Did you forgot to call 'initGithubOAuth' inside your Config.hs?")



ensureIsNotLocked :: forall user. (?context :: ControllerContext, HasNewSessionUrl user, HasField "lockedAt" user (Maybe UTCTime)) => user -> IO ()
ensureIsNotLocked user = do
    isLocked <- Lockable.isLocked user
    when isLocked do
        setErrorMessage "User is locked"
        redirectToPath (newSessionUrl (Proxy @user))
