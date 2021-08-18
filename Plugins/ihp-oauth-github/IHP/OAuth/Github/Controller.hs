{-# LANGUAGE AllowAmbiguousTypes #-}
module IHP.OAuth.Github.Controller where

import IHP.ControllerPrelude
import qualified IHP.OAuth.Github.Types as Github
import qualified IHP.OAuth.Github.GithubApi as Github
import IHP.LoginSupport.Types
import qualified IHP.OAuth.Github.Config as Config
import qualified Data.TMap as TMap

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
    , HasField "githubUserId" user (Maybe Int)
    , GithubOAuthControllerConfig user
    , user ~ CurrentUserRecord
    , HasNewSessionUrl user
    , Typeable user
    , ?context :: ControllerContext
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

            connectGithubWithExistingUser githubUser user accessToken -- User wants to connect Github to existing account
        Nothing -> do
            user <- query @user
                |> filterWhere (#githubUserId, Just (get #id githubUser))
                |> fetchOneOrNothing

            case user of
                Just user -> loginExistingUser githubUser user accessToken
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

                    case userWithSameEmail of
                        Just user -> do
                            setErrorMessage "There's an existing user with the same email address already. Please log in with your password and then connect your GitHub account in the account settings."
                            redirectToPath (newSessionUrl (Proxy @user))
                        Nothing -> loginNewUser @user githubUser accessToken

class GithubOAuthControllerConfig user where
    loginExistingUser :: (?context :: ControllerContext, ?modelContext :: ModelContext) => Github.GithubUser -> user -> Text -> IO ()
    loginNewUser :: (?context :: ControllerContext, ?modelContext :: ModelContext) => Github.GithubUser -> Text -> IO ()
    connectGithubWithExistingUser :: (?context :: ControllerContext, ?modelContext :: ModelContext) => Github.GithubUser -> user -> Text -> IO ()


githubOAuthConfig :: (?context :: ControllerContext) => Github.GithubOAuthConfig
githubOAuthConfig = ?context
            |> getFrameworkConfig
            |> get #appConfig
            |> TMap.lookup @Github.GithubOAuthConfig
            |> fromMaybe (error "Could not find GithubOAuthConfig in config. Did you forgot to call 'initGithubOAuth' inside your Config.hs?")


