module IHP.OAuth.Github.Types where

import IHP.Prelude
import Data.Aeson hiding (Error, Success)

data GithubOAuthController
    = NewSessionWithGithubAction
    | GithubConnectCallbackAction
    deriving (Eq, Show, Data)

data GithubOAuthConfig = GithubOAuthConfig
    { clientId :: !Text
    , clientSecret :: !Text
    } deriving (Eq, Show)

data AuthorizeOptions = AuthorizeOptions
    { clientId :: Text -- ^ The client ID for your GitHub App
    , redirectUrl :: Text -- ^ The URL in your application where users will be sent after authorization.
    , state :: Text -- ^ This should contain a random string to protect against forgery attacks and could contain any other arbitrary data.
    }

data RequestAccessTokenOptions = RequestAccessTokenOptions
    { clientId :: Text
    , clientSecret :: Text
    , code :: Text
    , state :: Text
    }

data AccessTokenResponse = AccessTokenResponse { accessToken :: Text }

data GithubUser
    = GithubUser
        { id :: Int
        , login :: Text
        , avatarUrl :: Text
        , name :: Maybe Text
        , email :: Text
        }
    | GithubUserWithoutEmail
        { id :: Int
        , login :: Text
        , avatarUrl :: Text
        , name :: Maybe Text
        }

data GithubEmail = GithubEmail
    { email :: Text
    , verified :: Bool
    , primary :: Bool
    -- , visibility :: Text
    }

instance FromJSON GithubEmail where
    parseJSON (Object response) = do
        email <- response .: "email"
        verified <- response .: "verified"
        primary <- response .: "primary"
        -- visibility <- response .: "visibility"
        pure GithubEmail { .. }
    parseJSON _ = undefined


instance ToJSON RequestAccessTokenOptions where
    toJSON RequestAccessTokenOptions { .. } = object
            [ "client_id" .= clientId
            , "client_secret" .= clientSecret
            , "code" .= code
            , "state" .= state
            ]

instance FromJSON AccessTokenResponse where
    parseJSON (Object response) = do
        accessToken <- response .: "access_token"
        pure AccessTokenResponse { .. }

instance FromJSON GithubUser where
    parseJSON (Object user) = do
        id <- user .: "id"
        login <- user .: "login"
        avatarUrl <- user .: "avatar_url"
        name <- user .: "name"
        email <- user .:? "email"
        pure $ case email of
           Nothing -> GithubUserWithoutEmail { .. }
           Just email -> GithubUser { .. }

