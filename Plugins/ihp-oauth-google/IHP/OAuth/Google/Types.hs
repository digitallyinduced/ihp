module IHP.OAuth.Google.Types where

import IHP.Prelude
import Data.Aeson hiding (Error, Success)

data GoogleOAuthController
    = NewSessionWithGoogleAction
    | GoogleConnectCallbackAction
    deriving (Eq, Show, Data)

data GoogleOAuthConfig = GoogleOAuthConfig
    { clientId :: !Text
    } deriving (Eq, Show)


data GoogleClaims = GoogleClaims
        { email :: Text
        , emailVerified :: Bool
        , name :: Text
        , picture :: Maybe Text
        , givenName :: Text
        , familyName :: Maybe Text
        , hostedDomain :: Maybe Text
        }

instance FromJSON GoogleClaims where
    parseJSON = withObject "GoogleClaims" $ \v -> GoogleClaims
        <$> v .: "email"
        <*> v .: "email_verified"
        <*> v .: "name"
        <*> v .:? "picture"
        <*> v .: "given_name"
        <*> v .:? "family_name"
        <*> v .:? "hd"