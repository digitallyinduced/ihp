{-# LANGUAGE AllowAmbiguousTypes #-}
module IHP.OAuth.Google.Controller where

import IHP.ControllerPrelude

import qualified IHP.OAuth.Google.Types as Google
import IHP.LoginSupport.Types
import qualified IHP.OAuth.Google.Config as Config
import qualified Data.TMap as TMap

import Control.Lens hiding ((|>), set)
import qualified Network.Wreq as Wreq
import qualified Jose.Jwk as Jwk
import qualified Jose.Jwt as Jwt
import qualified Data.Aeson as Aeson
import qualified IHP.AuthSupport.Lockable as Lockable
import qualified IHP.AuthSupport.Controller.Sessions as Sessions
import IHP.OAuth.Google.View.NewSessionWithGoogle

newSessionWithGoogleAction :: forall user. (?context :: ControllerContext, HasPath Google.GoogleOAuthController, ?modelContext :: ModelContext) => IO ()
newSessionWithGoogleAction = do
    let clientId = googleOAuthConfig |> get #clientId
    render NewSessionWithGoogleView { .. }

googleConnectCallbackAction :: forall user.
    ( GoogleOAuthControllerConfig user
    , (HasField "email" user Text)
    , user ~ GetModelByTableName (GetTableName user)
    , FromRow user
    , ?modelContext :: ModelContext
    , KnownSymbol (GetTableName user)
    , HasField "googleUserId" user (Maybe Text)
    , GoogleOAuthControllerConfig user
    , user ~ CurrentUserRecord
    , HasNewSessionUrl user
    , Typeable user
    , ?context :: ControllerContext
    , HasField "id" user (Id user)
    , CanUpdate user
    , SetField "failedLoginAttempts" user Int
    , KnownSymbol (GetModelName user)
    , Show (PrimaryKey (GetTableName user))
    , HasField "lockedAt" user (Maybe UTCTime)
    , Sessions.SessionsControllerConfig user
    , SetField "googleUserId" user (Maybe Text)
    , SetField "email" user Text
    , SetField "passwordHash" user Text
    , Record user
    , CanCreate user
    ) => IO ()
googleConnectCallbackAction = do
    let jwtString = param @ByteString "jwt"

    response <- Wreq.get "https://www.googleapis.com/oauth2/v3/certs"
    keySet :: Jwk.JwkSet <- (response ^. Wreq.responseBody)
            |> Aeson.decode
            |> \case
                Nothing -> error "Failed getting keyset from google"
                Just value -> pure value

    jwt <- Jwt.decode (get #keys keySet) Nothing jwtString

    let googleClientId = googleOAuthConfig |> get #clientId

    case jwt of
        Left jwtError -> do
            setErrorMessage ("Failed to decode JWT:" <> tshow jwtError)
            redirectToPath (newSessionUrl (Proxy @user))
        Right (Jwt.Jws (_, claimsString)) -> do
            let standardClaims :: Jwt.JwtClaims = claimsString |> Aeson.decodeStrict |> fromMaybe (error "failed to decode claims")
            let googleClaims :: Google.GoogleClaims = claimsString |> Aeson.decodeStrict |> fromMaybe (error $ "failed to decode claims: " <> cs claimsString)

            accessDeniedUnless (get #jwtIss standardClaims == Just "accounts.google.com")
            accessDeniedUnless (get #jwtAud standardClaims == Just [googleClientId])

            unless (get #emailVerified googleClaims) do
                setErrorMessage "Your google email needs to be verified before you can use your google account for signing up."
                redirectToPath (newSessionUrl (Proxy @user))

            let googleUserId = get #jwtSub standardClaims
                    |> fromMaybe (error "No google user id provided")
            maybeUser :: Maybe user <- Sessions.usersQueryBuilder @user
                    |> filterWhere (#googleUserId, Just googleUserId)
                    |> fetchOneOrNothing
            
            case maybeUser of
                Just user -> do
                    ensureIsNotLocked user
                    Sessions.beforeLogin user
                    login user
                    
                    user
                            |> set #failedLoginAttempts 0
                            |> updateRecord

                    pure ()
                Nothing -> do
                    randomPassword <- generateAuthenticationToken
                    hashed <- hashPassword randomPassword
                    let user = newRecord @user
                            |> set #passwordHash hashed
                            |> set #email (get #email googleClaims)
                            |> setJust #googleUserId googleUserId
                    user <- loginNewUser user googleClaims

                    login @user user
                    pure ()
                    
            redirectUrl <- getSessionAndClear "IHP.LoginSupport.redirectAfterLogin"
            redirectToPath (fromMaybe (Sessions.afterLoginRedirectPath @user) redirectUrl)

ensureIsNotLocked :: forall user. (?context :: ControllerContext, HasNewSessionUrl user, HasField "lockedAt" user (Maybe UTCTime)) => user -> IO ()
ensureIsNotLocked user = do
    isLocked <- Lockable.isLocked user
    when isLocked do
        setErrorMessage "User is locked"
        redirectToPath (newSessionUrl (Proxy @user))

class GoogleOAuthControllerConfig user where
    loginNewUser :: (?context :: ControllerContext, ?modelContext :: ModelContext, CanCreate user) => user -> Google.GoogleClaims -> IO user
    loginNewUser user googleClaims = createRecord user


googleOAuthConfig :: (?context :: ControllerContext) => Google.GoogleOAuthConfig
googleOAuthConfig = ?context
            |> getFrameworkConfig
            |> get #appConfig
            |> TMap.lookup @Google.GoogleOAuthConfig
            |> fromMaybe (error "Could not find GoogleOAuthConfig in config. Did you forgot to call 'initGoogleOAuth' inside your Config.hs?")


