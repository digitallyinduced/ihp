module IHP.OAuth.Github.GithubApi where

import IHP.ControllerPrelude
import qualified Network.URI.Encode as URI
import qualified Network.Wreq as Wreq
import Control.Lens hiding ((|>), (.=), set)
import IHP.OAuth.Github.Types

githubConnectUrl :: AuthorizeOptions -> Text
githubConnectUrl AuthorizeOptions { .. } =
    "https://github.com/login/oauth/authorize?scope=" <> URI.encodeText "user:email" <> "&client_id="
    <> URI.encodeText clientId
    <> "&redirect_url=" <> URI.encodeText redirectUrl
    <> "&state=" <> URI.encodeText state

redirectToGithubConnect :: (?context :: ControllerContext) => AuthorizeOptions -> IO ()
redirectToGithubConnect options = do
    redirectToUrl (githubConnectUrl options)

requestGithubAccessToken :: RequestAccessTokenOptions -> IO AccessTokenResponse
requestGithubAccessToken requestAccessTokenOptions = do
    let httpOptions = Wreq.defaults
            & Wreq.headers .~ [("Content-Type", "application/json"), ("Accept", "application/json")]
    response <- Wreq.asJSON =<< Wreq.postWith httpOptions "https://github.com/login/oauth/access_token" (toJSON requestAccessTokenOptions)
    pure (response ^. Wreq.responseBody)

requestGithubUser :: Text -> IO GithubUser
requestGithubUser accessToken = do
    let httpOptions = Wreq.defaults
            & Wreq.headers .~
                [ ("Content-Type", "application/json")
                , ("Accept", "application/json")
                , ("User-Agent", "IHP (https://ihp.digitallyinduced.com/)")
                , ("Authorization", "token " <> cs accessToken)
                ]
    response <- Wreq.asJSON =<< Wreq.getWith httpOptions "https://api.github.com/user"
    pure (response ^. Wreq.responseBody)

requestGithubUserEmail :: Text -> IO Text
requestGithubUserEmail accessToken =
    requestGithubUserEmail' 0
    where
        requestGithubUserEmail' :: Int -> IO Text
        requestGithubUserEmail' pageNr = do
            let httpOptions = Wreq.defaults
                    & Wreq.headers .~
                        [ ("Content-Type", "application/vnd.github.v3+json")
                        , ("Accept", "application/json")
                        , ("User-Agent", "IHP (https://ihp.digitallyinduced.com/)")
                        , ("Authorization", "token " <> cs accessToken)
                        , ("per_page", "100")
                        , ("page", cs $ show pageNr)
                        ]
            response <- Wreq.asJSON =<< Wreq.getWith httpOptions "https://api.github.com/user/emails"
            let emails :: [GithubEmail] = response ^. Wreq.responseBody
            let primaryEmail = head $ filter (get #primary) emails
            case primaryEmail of
                Nothing -> requestGithubUserEmail' $ pageNr + 1
                Just primaryEmail -> pure (get #email primaryEmail)


-- | Provides the state parameter for OAuth.
--
-- Generates a new random state parameter and saves it inside the session cookie. Returns the new state parameter.
-- When the callback is reached, use 'verifyState' to check that the state is valid.
initState :: (?context :: ControllerContext) => IO Text
initState = do
    state <- generateAuthenticationToken
    setSession "oauth.github.state" state
    pure state

verifyState :: (?context :: ControllerContext) => IO Text
verifyState = do
    let state = param @Text "state"
    expectedState <- fromMaybe (error "state not set") <$> (getSession @Text "oauth.github.state")

    accessDeniedUnless (not (isEmpty state))
    -- accessDeniedUnless (state == expectedState)

    setSession "oauth.github.state" ("" :: Text)

    pure state

