# Passkeys & WebAuthn

```toc

```

## Introduction

Passkeys provide passwordless authentication using the [WebAuthn standard](https://www.w3.org/TR/webauthn-2/). Instead of typing a password, users authenticate with their device's built-in authenticator — a fingerprint sensor, Face ID, a hardware security key, or a password manager like 1Password.

**Why passkeys?**

- **Phishing-resistant**: Credentials are cryptographically bound to the website's origin, so they can't be stolen by fake login pages.
- **No passwords to leak**: There's no shared secret stored on the server — only a public key.
- **Better UX**: One-tap login, no passwords to remember.

**How it works (simplified):**

1. **Registration**: The server sends a random challenge. The user's authenticator creates a new public/private key pair, signs the challenge, and returns the public key. The server stores the public key.
2. **Authentication**: The server sends a new challenge. The authenticator signs it with the stored private key. The server verifies the signature using the stored public key.

This guide walks through adding passkey authentication to an IHP application. For traditional password-based authentication, see the [Authentication guide](authentication.html).

## Database Schema

You need a `passkeys` table to store registered credentials, and a `users` table. The `users` table does **not** need `password_hash`, `locked_at`, or `failed_login_attempts` columns — passkey auth doesn't use passwords.

Add these to your `Application/Schema.sql`:

```sql
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    logged_in_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX users_email_index ON users (LOWER(email));

CREATE TABLE passkeys (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    credential_id BYTEA NOT NULL UNIQUE,
    public_key BYTEA NOT NULL,
    sign_count BIGINT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    last_used_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    name TEXT DEFAULT 'not named' NOT NULL
);
CREATE INDEX passkeys_user_id_index ON passkeys (user_id);
ALTER TABLE passkeys ADD CONSTRAINT passkeys_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
```

Key columns in the `passkeys` table:

- **`credential_id`** (`BYTEA`): Unique identifier for this credential, assigned by the authenticator.
- **`public_key`** (`BYTEA`): The authenticator's public key, used to verify signatures.
- **`sign_count`** (`BIGINT`): A counter incremented by the authenticator on each use. Helps detect cloned authenticators.
- **`name`**: A user-friendly label (e.g. "MacBook fingerprint", "YubiKey").

You can adjust the `users` table to your needs — use `email`, `nickname`, or any other identifier. The passkey system only requires a user ID. If you want to support passkeys **alongside** passwords on an existing app, keep your existing `users` table and just add the `passkeys` table.

## Dependencies

You need the [`webauthn`](https://hackage.haskell.org/package/webauthn) package. Add it to your `flake.nix` under `haskellPackages`. The package may need jailbreaking depending on your resolver:

```nix
haskellPackages = p:
    let
        webauthn = pkgs.haskell.lib.doJailbreak (p.callHackageDirect {
            pkg = "webauthn";
            ver = "0.11.0.0";
            sha256 = "sha256-iJygaLPu0NyOHxUKwpd7vMkUnIXNRtAnnCumqKqces8=";
        } {});
    in with p; [
        p.ihp
        # ... your other packages ...
        webauthn
    ];
```

The `webauthn` package brings in its own transitive dependencies including `hourglass` (for date/time types used in verification) and `validation` (for error reporting). You don't need to add those explicitly.

After updating `flake.nix`, run `nix flake update` and restart your dev environment.

No extra JavaScript libraries are needed on the frontend. The [Web Authentication API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Authentication_API) is built into all modern browsers (Chrome, Safari, Firefox, Edge).

## Helper Module

Create `Application/Helper/Passkeys.hs` with utility functions for the WebAuthn protocol. This module handles Relying Party configuration, challenge options, and converting between database records and WebAuthn types.

```haskell
{-# LANGUAGE PackageImports #-}

module Application.Helper.Passkeys
    ( allowedOrigins
    , authenticationCredentialOptions
    , credentialEntryForPasskey
    , passkeyCredentialDescriptor
    , passkeyRelyingPartyName
    , registrationCredentialOptions
    , rpIdTextFromHost
    , rpIdTextFromRequest
    , rpIdHashFromRequest
    , userHandleForUserId
    ) where

import qualified "crypton" Crypto.Hash as Hash
import Crypto.WebAuthn.Cose.SignAlg
import Crypto.WebAuthn.Model.Kinds (CeremonyKind (Authentication, Registration))
import Crypto.WebAuthn.Model.Types
import Crypto.WebAuthn.Operation.CredentialEntry
import qualified Data.ByteString as ByteString
import qualified Data.CaseInsensitive as CI
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Database.PostgreSQL.Simple.Types (Binary (Binary))
import Generated.Types
import IHP.InputValue (inputValue)
import IHP.Prelude
import Network.Wai (Request, isSecure, requestHeaderHost, requestHeaders)

-- Change this to your app's name
passkeyRelyingPartyName :: RelyingPartyName
passkeyRelyingPartyName = RelyingPartyName "My IHP App"

-- Strips port and path from the host header to get the RP ID
-- e.g. "localhost:8000" -> "localhost", "myapp.com" -> "myapp.com"
rpIdTextFromHost :: Text -> Text
rpIdTextFromHost = Text.takeWhile (\char -> char /= ':' && char /= '/')

rpIdTextFromRequest :: (?request :: Request) => Text
rpIdTextFromRequest = rpIdTextFromHost requestHostText

rpIdHashFromRequest :: (?request :: Request) => RpIdHash
rpIdHashFromRequest = RpIdHash (Hash.hash (Text.encodeUtf8 rpIdTextFromRequest))

allowedOrigins :: (?request :: Request) => NonEmpty Origin
allowedOrigins = Origin (requestSchemeText <> "://" <> requestHostText) :| []

userHandleForUserId :: Id User -> UserHandle
userHandleForUserId userId = UserHandle (cs (inputValue userId))

-- Options for navigator.credentials.create() (registration)
registrationCredentialOptions ::
    (?request :: Request) =>
    Challenge ->
    Id User ->
    Text ->
    [CredentialDescriptor] ->
    CredentialOptions 'Registration
registrationCredentialOptions challenge userId displayName excludeCredentials =
    CredentialOptionsRegistration
        { corRp = CredentialRpEntity
            { creId = Just (RpId rpIdTextFromRequest)
            , creName = passkeyRelyingPartyName
            }
        , corUser = CredentialUserEntity
            { cueId = userHandleForUserId userId
            , cueDisplayName = UserAccountDisplayName displayName
            , cueName = UserAccountName displayName
            }
        , corChallenge = challenge
        , corPubKeyCredParams =
            [ CredentialParameters CredentialTypePublicKey
                (CoseSignAlgECDSA CoseHashAlgECDSASHA256) -- ES256
            , CredentialParameters CredentialTypePublicKey
                CoseSignAlgEdDSA                          -- EdDSA
            , CredentialParameters CredentialTypePublicKey
                (CoseSignAlgRSA CoseHashAlgRSASHA256)     -- RS256
            ]
        , corTimeout = Just (Timeout 60000)  -- 60 seconds
        , corExcludeCredentials = excludeCredentials
        , corAuthenticatorSelection = Just AuthenticatorSelectionCriteria
            { ascAuthenticatorAttachment = Nothing
            , ascResidentKey = ResidentKeyRequirementPreferred
            , ascUserVerification = UserVerificationRequirementPreferred
            }
        , corAttestation = AttestationConveyancePreferenceNone
        , corExtensions = Nothing
        }

-- Options for navigator.credentials.get() (authentication)
authenticationCredentialOptions ::
    (?request :: Request) =>
    Challenge ->
    CredentialOptions 'Authentication
authenticationCredentialOptions challenge =
    CredentialOptionsAuthentication
        { coaChallenge = challenge
        , coaTimeout = Just (Timeout 60000)
        , coaRpId = Just (RpId rpIdTextFromRequest)
        , coaAllowCredentials = []  -- empty = let the authenticator pick
        , coaUserVerification = UserVerificationRequirementPreferred
        , coaExtensions = Nothing
        }

-- Convert a stored Passkey to a CredentialDescriptor (for exclude lists)
passkeyCredentialDescriptor :: Passkey -> CredentialDescriptor
passkeyCredentialDescriptor Passkey { credentialId = Binary credentialId } =
    CredentialDescriptor
        { cdTyp = CredentialTypePublicKey
        , cdId = CredentialId credentialId
        , cdTransports = Nothing
        }

-- Convert a stored Passkey to a CredentialEntry (for verification)
credentialEntryForPasskey :: Passkey -> CredentialEntry
credentialEntryForPasskey Passkey
    { userId
    , credentialId = Binary credentialId
    , publicKey = Binary publicKey
    , signCount
    } = CredentialEntry
        { ceCredentialId = CredentialId credentialId
        , ceUserHandle = userHandleForUserId userId
        , cePublicKeyBytes = PublicKeyBytes publicKey
        , ceSignCounter = fromIntegral signCount
        , ceTransports = []
        }

-- Internal helpers for deriving scheme and host from the request,
-- with support for X-Forwarded-* headers behind reverse proxies.

requestSchemeText :: (?request :: Request) => Text
requestSchemeText =
    fromMaybe fallbackScheme (headerValue "x-forwarded-proto")
    where
        fallbackScheme = if isSecure ?request then "https" else "http"

requestHostText :: (?request :: Request) => Text
requestHostText =
    fromMaybe fallbackHost (headerValue "x-forwarded-host" <|> headerHost)
    where
        fallbackHost = "localhost:8000"
        headerHost = cs <$> requestHeaderHost ?request

headerValue :: (?request :: Request) => ByteString.ByteString -> Maybe Text
headerValue headerName =
    cs . snd <$> find matchesHeader (requestHeaders ?request)
    where
        matchesHeader (name, _) = CI.foldedCase name == headerName
```

## Types & Routes

### Controller Types

Add these to `Web/Types.hs`:

```haskell
import IHP.LoginSupport.Types

-- Auth controller with passkey registration and authentication actions
data AuthController
    = LoginAction
    | BeginPasskeyRegistrationAction
    | FinishPasskeyRegistrationAction
    | BeginPasskeyAuthenticationAction
    | FinishPasskeyAuthenticationAction
    deriving (Eq, Show, Data)

-- For managing registered passkeys (rename, delete)
data PasskeysController
    = UpdatePasskeyNameAction { passkeyId :: !(Id Passkey) }
    | DeletePasskeyAction { passkeyId :: !(Id Passkey) }
    deriving (Eq, Show, Data)

-- Session controller for logout
data SessionsController
    = DeleteSessionAction
    deriving (Eq, Show, Data)

instance HasNewSessionUrl User where
    newSessionUrl _ = "/NewSession"

type instance CurrentUserRecord = User
```

### Routes

In `Web/Routes.hs`, define custom routes for the auth and passkeys controllers. The passkey endpoints should be POST-only since they receive JSON payloads:

```haskell
module Web.Routes where

import IHP.RouterPrelude
import Web.Types

instance AutoRoute SessionsController

instance HasPath AuthController where
    pathTo LoginAction                       = "/NewSession"
    pathTo BeginPasskeyRegistrationAction    = "/passkeys/registration/begin"
    pathTo FinishPasskeyRegistrationAction   = "/passkeys/registration/finish"
    pathTo BeginPasskeyAuthenticationAction  = "/passkeys/authentication/begin"
    pathTo FinishPasskeyAuthenticationAction = "/passkeys/authentication/finish"

instance CanRoute AuthController where
    parseRoute' =
        (string "/NewSession" >> pure LoginAction)
        <|> (string "/passkeys/registration/begin"
            >> onlyAllowMethods [POST] >> pure BeginPasskeyRegistrationAction)
        <|> (string "/passkeys/registration/finish"
            >> onlyAllowMethods [POST] >> pure FinishPasskeyRegistrationAction)
        <|> (string "/passkeys/authentication/begin"
            >> onlyAllowMethods [POST] >> pure BeginPasskeyAuthenticationAction)
        <|> (string "/passkeys/authentication/finish"
            >> onlyAllowMethods [POST] >> pure FinishPasskeyAuthenticationAction)

instance HasPath PasskeysController where
    pathTo UpdatePasskeyNameAction { passkeyId } =
        "/UpdatePasskeyName?passkeyId=" <> inputValue passkeyId
    pathTo DeletePasskeyAction { passkeyId } =
        "/DeletePasskey?passkeyId=" <> inputValue passkeyId

instance CanRoute PasskeysController where
    parseRoute' =
        (string "/UpdatePasskeyName"
            >> onlyAllowMethods [POST] >> pure (UpdatePasskeyNameAction def))
        <|> (string "/DeletePasskey"
            >> onlyAllowMethods [POST] >> pure (DeletePasskeyAction def))
```

### FrontController

Wire the controllers in `Web/FrontController.hs`:

```haskell
import Web.Controller.Auth
import Web.Controller.Passkeys
import Web.Controller.Sessions

instance FrontController WebApplication where
    controllers =
        [ parseRoute @SessionsController
        , parseRoute @AuthController
        , parseRoute @PasskeysController
        -- ... your other controllers
        ]

instance InitControllerContext WebApplication where
    initContext = do
        initAuthentication @User
```

### Sessions Controller

Create `Web/Controller/Sessions.hs` for logout:

```haskell
module Web.Controller.Sessions where

import Web.Controller.Prelude
import qualified IHP.AuthSupport.Controller.Sessions as Sessions

instance Controller SessionsController where
    action DeleteSessionAction = Sessions.deleteSessionAction @User

instance Sessions.SessionsControllerConfig User
```

## Auth Controller

Create `Web/Controller/Auth.hs`. This handles all four passkey endpoints (begin/finish for both registration and authentication):

```haskell
module Web.Controller.Auth where

import Web.Controller.Prelude
import Application.Helper.Passkeys
import qualified Crypto.WebAuthn.Encoding.WebAuthnJson as WebAuthnJson
import Crypto.WebAuthn.Model.Types
import Crypto.WebAuthn.Operation.Authentication
import Crypto.WebAuthn.Operation.CredentialEntry (CredentialEntry (..))
import Crypto.WebAuthn.Operation.Registration
import qualified Data.Aeson as Aeson
import Data.Hourglass (timeConvert)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.Validation as Validation
import Database.PostgreSQL.Simple.Types (Binary (Binary))
import Network.HTTP.Types.Status (Status, status400, status409, status422)
import Web.View.Sessions.New

instance Controller AuthController where

    action LoginAction = do
        when (isJust (currentUserOrNothing @User)) do
            redirectToPath "/"
        render NewView

    -- Step 1 of registration: generate challenge, return credential options
    action BeginPasskeyRegistrationAction = do
        payloadResult <- parseJsonBody @BeginPasskeyRegistrationPayload
        payload <- case payloadResult of
            Left errorMessage -> jsonError status400 errorMessage
            Right payload     -> pure payload

        -- For logged-in users adding another passkey, use their existing info.
        -- For new users, validate the email and generate a new user ID.
        (pendingUserId, email, excludeCredentials) <- case currentUserOrNothing @User of
            Just user -> do
                existingPasskeys <- query @Passkey
                    |> filterWhere (#userId, user.id)
                    |> fetch
                pure ( user.id
                     , user.email
                     , map passkeyCredentialDescriptor existingPasskeys
                     )
            Nothing -> do
                email <- requireAvailableEmail payload.email
                userUuid <- sqlQueryScalar "SELECT uuid_generate_v4()" ()
                pure (Id userUuid, email, [])

        challenge <- liftIO generateChallenge
        setSession registrationChallengeSessionKey (unChallenge challenge)
        setSession registrationEmailSessionKey email
        setSession registrationUserIdSessionKey (inputValue pendingUserId)

        renderJson
            (WebAuthnJson.wjEncodeCredentialOptionsRegistration
                (registrationCredentialOptions challenge pendingUserId
                    email excludeCredentials))

    -- Step 2 of registration: verify attestation, create user + passkey
    action FinishPasskeyRegistrationAction = do
        challenge <- sessionChallenge registrationChallengeSessionKey
        pendingUserId <- sessionUserId registrationUserIdSessionKey
        pendingEmail <- sessionText registrationEmailSessionKey

        credentialResult <- parseJsonBody @WebAuthnJson.WJCredentialRegistration
        credentialPayload <- case credentialResult of
            Left errorMessage -> do
                clearRegistrationSession
                jsonError status400 errorMessage
            Right payload -> pure payload

        credential <- case WebAuthnJson.wjDecodeCredentialRegistration
                               credentialPayload of
            Left errorMessage -> do
                clearRegistrationSession
                jsonError status422 errorMessage
            Right credential -> pure credential

        currentDateTime <- liftIO (timeConvert <$> getCurrentTime)

        existingPasskeys <- case currentUserOrNothing @User of
            Just user -> query @Passkey
                |> filterWhere (#userId, user.id) |> fetch
            Nothing -> pure []

        let verification = verifyRegistrationResponse
                allowedOrigins
                rpIdHashFromRequest
                mempty
                currentDateTime
                (registrationCredentialOptions challenge pendingUserId
                    pendingEmail
                    (map passkeyCredentialDescriptor existingPasskeys))
                credential

        clearRegistrationSession

        registrationResult <- case verification of
            Validation.Failure errors ->
                jsonError status422 (validationErrors errors)
            Validation.Success result -> pure result

        let entry = rrEntry registrationResult
            credentialId = unCredentialId entry.ceCredentialId

        credentialAlreadyExists <- query @Passkey
            |> filterWhere (#credentialId, Binary credentialId)
            |> fetchExists

        when credentialAlreadyExists do
            jsonError status409 "This passkey is already registered."

        case currentUserOrNothing @User of
            Just user -> do
                _ <- createPasskeyRecord user.id entry
                renderJson (Aeson.object
                    [ "ok" Aeson..= True
                    , "message" Aeson..= ("Passkey added successfully" :: Text)
                    ])
            Nothing -> do
                user <- newRecord @User
                    |> set #id pendingUserId
                    |> set #email pendingEmail
                    |> createRecord

                _ <- createPasskeyRecord user.id entry

                login user
                renderJson (Aeson.object
                    [ "ok" Aeson..= True
                    , "redirectTo" Aeson..= ("/" :: Text)
                    ])

    -- Step 1 of authentication: generate challenge
    action BeginPasskeyAuthenticationAction = do
        challenge <- liftIO generateChallenge
        setSession authenticationChallengeSessionKey (unChallenge challenge)
        renderJson
            (WebAuthnJson.wjEncodeCredentialOptionsAuthentication
                (authenticationCredentialOptions challenge))

    -- Step 2 of authentication: verify assertion, log user in
    action FinishPasskeyAuthenticationAction = do
        challenge <- sessionChallenge authenticationChallengeSessionKey

        credentialResult <- parseJsonBody @WebAuthnJson.WJCredentialAuthentication
        credentialPayload <- case credentialResult of
            Left errorMessage -> do
                clearAuthenticationSession
                jsonError status400 errorMessage
            Right payload -> pure payload

        credential <- case WebAuthnJson.wjDecodeCredentialAuthentication
                               credentialPayload of
            Left errorMessage -> do
                clearAuthenticationSession
                jsonError status422 errorMessage
            Right credential -> pure credential

        clearAuthenticationSession

        -- Look up the passkey by credential ID
        let CredentialId credentialId = cIdentifier credential
        passkey <- query @Passkey
            |> filterWhere (#credentialId, Binary credentialId)
            |> fetchOneOrNothing
            >>= maybe (jsonError status422
                    "No account matched that passkey.") pure

        user <- fetch passkey.userId

        -- Verify the assertion cryptographically
        let verification = verifyAuthenticationResponse
                allowedOrigins
                rpIdHashFromRequest
                (Just (userHandleForUserId user.id))
                (credentialEntryForPasskey passkey)
                (authenticationCredentialOptions challenge)
                credential

        authenticationResult <- case verification of
            Validation.Failure errors ->
                jsonError status422 (validationErrors errors)
            Validation.Success result -> pure result

        -- Check the signature counter for cloned authenticator detection
        case arSignatureCounterResult authenticationResult of
            SignatureCounterPotentiallyCloned ->
                jsonError status422
                    "This passkey could not be verified safely."
            SignatureCounterUpdated newSignCount -> do
                passkey
                    |> set #signCount
                        (fromIntegral (unSignatureCounter newSignCount))
                    |> updateRecordDiscardResult
            SignatureCounterZero -> pure ()

        login user

        currentDateTime <- liftIO (timeConvert <$> getCurrentTime)
        user
            |> set #loggedInAt currentDateTime
            |> updateRecordDiscardResult
        passkey
            |> set #lastUsedAt currentDateTime
            |> updateRecordDiscardResult

        renderJson (Aeson.object
            [ "ok" Aeson..= True
            , "redirectTo" Aeson..= ("/" :: Text)
            ])

-- Session keys
registrationChallengeSessionKey :: ByteString
registrationChallengeSessionKey = "passkey-registration-challenge"

registrationEmailSessionKey :: ByteString
registrationEmailSessionKey = "passkey-registration-email"

registrationUserIdSessionKey :: ByteString
registrationUserIdSessionKey = "passkey-registration-user-id"

authenticationChallengeSessionKey :: ByteString
authenticationChallengeSessionKey = "passkey-authentication-challenge"

-- JSON body parsing
data BeginPasskeyRegistrationPayload = BeginPasskeyRegistrationPayload
    { email :: Maybe Text
    }

instance Aeson.FromJSON BeginPasskeyRegistrationPayload where
    parseJSON = Aeson.withObject "BeginPasskeyRegistrationPayload" $ \object -> do
        email <- object Aeson..:? "email"
        pure BeginPasskeyRegistrationPayload { .. }

parseJsonBody ::
    (?request :: Request, Aeson.FromJSON payload) =>
    IO (Either Text payload)
parseJsonBody = do
    jsonValue <- requestBodyJSON
    pure $ case Aeson.fromJSON jsonValue of
        Aeson.Error errorMessage -> Left (cs errorMessage)
        Aeson.Success payload    -> Right payload

-- Email validation
requireAvailableEmail ::
    (?request :: Request, ?modelContext :: ModelContext) =>
    Maybe Text -> IO Text
requireAvailableEmail maybeEmail = do
    email <- case Text.strip <$> maybeEmail of
        Just email | not (Text.null email) -> pure email
        _ -> jsonError status422 "Please provide an email address."

    emailInUse <- emailAlreadyExists email
    when emailInUse do
        jsonError status422 "That email is already taken."
    pure email

emailAlreadyExists ::
    (?modelContext :: ModelContext) => Text -> IO Bool
emailAlreadyExists email = do
    checkedUser <- newRecord @User
        |> set #email email
        |> validateIsUniqueCaseInsensitive #email
    pure (isJust (getValidationFailure #email checkedUser))

-- Session retrieval helpers
sessionChallenge :: (?request :: Request) => ByteString -> IO Challenge
sessionChallenge sessionKey = do
    maybeChallenge <- getSession @ByteString sessionKey
    case maybeChallenge of
        Just challenge -> pure (Challenge challenge)
        Nothing -> jsonError status422
            "This passkey request has expired. Please try again."

sessionText :: (?request :: Request) => ByteString -> IO Text
sessionText sessionKey = do
    maybeValue <- getSession @Text sessionKey
    case maybeValue of
        Just value -> pure value
        Nothing -> jsonError status422
            "This passkey request has expired. Please try again."

sessionUserId :: (?request :: Request) => ByteString -> IO (Id User)
sessionUserId sessionKey = do
    userIdText <- sessionText sessionKey
    case UUID.fromText userIdText of
        Just userId -> pure (Id userId)
        Nothing -> jsonError status422
            "The pending passkey registration is invalid."

createPasskeyRecord ::
    (?modelContext :: ModelContext) =>
    Id User -> CredentialEntry -> IO Passkey
createPasskeyRecord userId entry =
    newRecord @Passkey
        |> set #userId userId
        |> set #credentialId
            (Binary (unCredentialId entry.ceCredentialId))
        |> set #publicKey
            (Binary (unPublicKeyBytes entry.cePublicKeyBytes))
        |> set #signCount
            (fromIntegral (unSignatureCounter entry.ceSignCounter))
        |> createRecord

clearRegistrationSession :: (?request :: Request) => IO ()
clearRegistrationSession = do
    deleteSession registrationChallengeSessionKey
    deleteSession registrationEmailSessionKey
    deleteSession registrationUserIdSessionKey

clearAuthenticationSession :: (?request :: Request) => IO ()
clearAuthenticationSession =
    deleteSession authenticationChallengeSessionKey

-- Respond with a JSON error and exit the controller action.
-- The `error` call is unreachable — `renderJsonWithStatusCode` calls
-- `respondAndExit` internally — but it satisfies the polymorphic
-- return type so `jsonError` can be used in any `IO a` context.
jsonError :: (?request :: Request) => Status -> Text -> IO a
jsonError statusCode errorMessage =
    renderJsonWithStatusCode statusCode
        (Aeson.object ["error" Aeson..= errorMessage])
        >> error "unreachable"

validationErrors :: Show error => NonEmpty.NonEmpty error -> Text
validationErrors errors =
    Text.intercalate "; " (map (cs . show) (NonEmpty.toList errors))
```

## Frontend JavaScript

The frontend JavaScript handles calling the browser's WebAuthn API and communicating with the server. Add this to your `static/app.js` (or a separate file included in your layout).

### Initialization

Attach click handlers to login and registration buttons. Call `initPasskeyAuth()` on page load (and on `turbolinks:load` if you use Turbolinks):

```javascript
$(document).on('ready turbolinks:load', function () {
    initPasskeyAuth()
})

function initPasskeyAuth() {
    document.querySelectorAll('.js-passkey-login').forEach(container => {
        if (container.dataset.passkeyInitialized === 'true') return
        container.dataset.passkeyInitialized = 'true'

        const button = container.querySelector('.js-passkey-login-button')
        if (!button) return

        button.addEventListener('click', async function () {
            await runPasskeyLogin(container, button)
        })
    })

    document.querySelectorAll('.js-passkey-register').forEach(container => {
        if (container.dataset.passkeyInitialized === 'true') return
        container.dataset.passkeyInitialized = 'true'

        const button = container.querySelector('.js-passkey-register-button')
        if (!button) return

        button.addEventListener('click', async function () {
            await runPasskeyRegistration(container, button)
        })
    })
}
```

### Login Flow

```javascript
async function runPasskeyLogin(container, button) {
    await withPasskeyButton(button, async () => {
        setPasskeyStatus(container, 'info', 'Waiting for your passkey...')

        // Step 1: Get challenge from server
        const beginResponse = await postJson(container.dataset.beginUrl, {})

        // Step 2: Call the browser's WebAuthn API
        const credential = await navigator.credentials.get({
            publicKey: authenticationOptionsToNative(beginResponse)
        })

        if (!credential) throw new Error('No passkey was selected.')

        // Step 3: Send the signed assertion to the server
        const finishResponse = await postJson(
            container.dataset.finishUrl,
            serializeAuthenticationCredential(credential)
        )

        setPasskeyStatus(container, 'success', 'Logged in successfully.')
        redirectAfterPasskeySuccess(container, finishResponse)
    })
}
```

### Registration Flow

```javascript
async function runPasskeyRegistration(container, button) {
    await withPasskeyButton(button, async () => {
        const email = emailForContainer(container)
        if (container.dataset.emailInputId && email === '') {
            throw new Error('Please provide an email address.')
        }

        setPasskeyStatus(container, 'info', 'Waiting for your passkey...')

        // Step 1: Get challenge and credential options from server
        const beginResponse = await postJson(
            container.dataset.beginUrl,
            email ? { email } : {}
        )

        // Step 2: Create the credential via the browser's WebAuthn API
        const credential = await navigator.credentials.create({
            publicKey: registrationOptionsToNative(beginResponse)
        })

        if (!credential) throw new Error('Passkey registration was cancelled.')

        // Step 3: Send the attestation to the server
        const finishResponse = await postJson(
            container.dataset.finishUrl,
            serializeRegistrationCredential(credential)
        )

        setPasskeyStatus(container, 'success',
            finishResponse.message || 'Passkey saved successfully.')
        redirectAfterPasskeySuccess(container, finishResponse)
    })
}
```

### UI Helpers

```javascript
// Manages button loading state and error display
async function withPasskeyButton(button, callback) {
    if (!window.PublicKeyCredential || !navigator.credentials) {
        setPasskeyStatus(
            button.closest('.js-passkey-login, .js-passkey-register'),
            'danger',
            'Passkeys are not supported in this browser.'
        )
        return
    }

    const originalHtml = button.innerHTML
    button.disabled = true
    button.innerHTML =
        '<span class="spinner-border spinner-border-sm me-2"></span>Please wait'

    try {
        await callback()
    } catch (error) {
        setPasskeyStatus(
            button.closest('.js-passkey-login, .js-passkey-register'),
            'danger',
            error.message || 'Passkey request failed.'
        )
    } finally {
        button.disabled = false
        button.innerHTML = originalHtml
    }
}

// POST JSON to the server
async function postJson(url, payload) {
    const response = await fetch(url, {
        method: 'POST',
        credentials: 'same-origin',
        headers: {
            'Accept': 'application/json',
            'Content-Type': 'application/json'
        },
        body: JSON.stringify(payload)
    })

    const json = await response.json().catch(() => ({}))
    if (!response.ok) {
        throw new Error(json.error || 'Passkey request failed.')
    }
    return json
}

function emailForContainer(container) {
    const inputId = container.dataset.emailInputId
    if (!inputId) return ''
    const input = document.getElementById(inputId)
    return input ? input.value.trim() : ''
}

function setPasskeyStatus(container, tone, message) {
    if (!container) return
    const targetId = container.dataset.statusId
    if (!targetId) return
    const element = document.getElementById(targetId)
    if (!element) return

    element.className = `alert alert-${tone} mb-3`
    element.textContent = message
}

function redirectAfterPasskeySuccess(container, response) {
    const redirectTo = response.redirectTo || container.dataset.successRedirect
    if (!redirectTo) return
    window.location.assign(redirectTo)
}
```

### Base64url Encoding/Decoding

WebAuthn uses binary data encoded as base64url. These utilities convert between base64url strings and `ArrayBuffer`:

```javascript
function base64UrlToBuffer(value) {
    const normalized = value.replace(/-/g, '+').replace(/_/g, '/')
    const padded = normalized + '='.repeat((4 - normalized.length % 4) % 4)
    const binary = window.atob(padded)
    const bytes = new Uint8Array(binary.length)

    for (let index = 0; index < binary.length; index += 1) {
        bytes[index] = binary.charCodeAt(index)
    }

    return bytes.buffer
}

function bufferToBase64Url(buffer) {
    const bytes = new Uint8Array(buffer)
    let binary = ''

    bytes.forEach(byte => {
        binary += String.fromCharCode(byte)
    })

    return window.btoa(binary)
        .replace(/\+/g, '-')
        .replace(/\//g, '_')
        .replace(/=+$/g, '')
}
```

### Credential Serialization

Convert browser credential objects to JSON for the server:

```javascript
function serializeRegistrationCredential(credential) {
    return {
        rawId: bufferToBase64Url(credential.rawId),
        response: {
            clientDataJSON:
                bufferToBase64Url(credential.response.clientDataJSON),
            attestationObject:
                bufferToBase64Url(credential.response.attestationObject),
            transports:
                typeof credential.response.getTransports === 'function'
                    ? credential.response.getTransports()
                    : []
        },
        clientExtensionResults: credential.getClientExtensionResults()
    }
}

function serializeAuthenticationCredential(credential) {
    return {
        rawId: bufferToBase64Url(credential.rawId),
        response: {
            clientDataJSON:
                bufferToBase64Url(credential.response.clientDataJSON),
            authenticatorData:
                bufferToBase64Url(credential.response.authenticatorData),
            signature:
                bufferToBase64Url(credential.response.signature),
            userHandle: credential.response.userHandle
                ? bufferToBase64Url(credential.response.userHandle)
                : null
        },
        clientExtensionResults: credential.getClientExtensionResults()
    }
}
```

### Options Conversion

Convert server JSON options to the format expected by the browser's WebAuthn API (binary buffers):

```javascript
function registrationOptionsToNative(options) {
    if (!options.user?.id) {
        throw new Error('Invalid registration options: missing user information')
    }
    return {
        ...options,
        challenge: base64UrlToBuffer(options.challenge),
        user: {
            ...options.user,
            id: base64UrlToBuffer(options.user.id)
        },
        excludeCredentials: (options.excludeCredentials || []).map(
            descriptor => ({
                ...descriptor,
                id: base64UrlToBuffer(descriptor.id)
            })
        )
    }
}

function authenticationOptionsToNative(options) {
    return {
        ...options,
        challenge: base64UrlToBuffer(options.challenge),
        allowCredentials: (options.allowCredentials || []).map(
            descriptor => ({
                ...descriptor,
                id: base64UrlToBuffer(descriptor.id)
            })
        )
    }
}
```

## Views

### Login View

Create `Web/View/Sessions/New.hs` with a login/registration page. The view uses `data-*` attributes to connect the JavaScript handlers to the backend endpoints:

```haskell
module Web.View.Sessions.New where

import IHP.ViewPrelude
import Web.Routes ()
import Web.Types

data NewView = NewView

instance View NewView where
    html NewView = [hsx|
        <div class="container mt-5">
            <div class="row justify-content-center">
                <div class="col-md-7 col-lg-5">
                    <div class="card shadow">
                        <div class="card-body p-4">
                            <h4 class="text-center mb-3">
                                Continue with a passkey
                            </h4>

                            {-- Login with existing passkey --}
                            <div class="js-passkey-login"
                                 data-begin-url={pathTo BeginPasskeyAuthenticationAction}
                                 data-finish-url={pathTo FinishPasskeyAuthenticationAction}
                                 data-status-id="passkey-status"
                                 data-success-redirect="/">
                                <button type="button"
                                    class="btn btn-success btn-lg w-100
                                           js-passkey-login-button">
                                    Log in with a passkey
                                </button>
                            </div>

                            <div class="d-flex align-items-center my-4">
                                <hr class="flex-grow-1" />
                                <span class="px-3 small text-muted">or</span>
                                <hr class="flex-grow-1" />
                            </div>

                            {-- Status messages --}
                            <div id="passkey-status"
                                 class="alert d-none mb-3"></div>

                            {-- Register new account --}
                            <div class="js-passkey-register"
                                 data-begin-url={pathTo BeginPasskeyRegistrationAction}
                                 data-finish-url={pathTo FinishPasskeyRegistrationAction}
                                 data-status-id="passkey-status"
                                 data-success-redirect="/"
                                 data-email-input-id="signup-email">
                                <input id="signup-email"
                                       type="email"
                                       class="form-control mb-3"
                                       placeholder="Email address" />
                                <button type="button"
                                    class="btn btn-primary btn-lg w-100
                                           js-passkey-register-button">
                                    Create account with a passkey
                                </button>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    |]
```

The key pattern is:

- **`.js-passkey-login`** container with `data-begin-url` and `data-finish-url` pointing to the auth endpoints.
- **`.js-passkey-register`** container with the same pattern, plus `data-email-input-id` referencing the email input.
- A shared **`data-status-id`** pointing to an alert div for error/success messages.
- **`data-success-redirect`** for where to go after login/registration.

## Managing Passkeys

Users should be able to register multiple passkeys and manage them (rename, delete). Create `Web/Controller/Passkeys.hs`:

```haskell
module Web.Controller.Passkeys where

import Web.Controller.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

instance Controller PasskeysController where
    beforeAction = ensureIsUser

    action UpdatePasskeyNameAction { passkeyId } = do
        passkey <- fetch passkeyId
        accessDeniedUnless (passkey.userId == currentUserId)
        let newName = Text.strip (param @Text "name")

        passkey
            |> set #name newName
            |> updateRecordDiscardResult

        renderJson (Aeson.object
            ["ok" Aeson..= True, "name" Aeson..= newName])

    action DeletePasskeyAction { passkeyId } = do
        passkey <- fetch passkeyId
        accessDeniedUnless (passkey.userId == currentUserId)

        passkeyCount <- query @Passkey
            |> filterWhere (#userId, currentUserId)
            |> fetchCount

        if passkeyCount <= 1
            then do
                setErrorMessage "Cannot remove your only passkey."
                redirectTo EditUserAction { userId = currentUserId }
            else do
                deleteRecord passkey
                setSuccessMessage "Passkey removed."
                redirectTo EditUserAction { userId = currentUserId }
```

### Passkey Management View

In a user profile/settings view, show the passkeys table with rename and delete options:

```haskell
renderPasskeyTable :: [Passkey] -> Html
renderPasskeyTable [] = [hsx|
    <p class="text-muted small mb-0">
        You have no passkeys yet. Add one to log in without a password.
    </p>
|]
renderPasskeyTable passkeys = [hsx|
    <table class="table table-sm align-middle">
        <thead>
            <tr>
                <th>Name</th>
                <th>Created</th>
                <th>Last used</th>
                <th></th>
            </tr>
        </thead>
        <tbody>
            {forEach passkeys (renderPasskeyRow (length passkeys == 1))}
        </tbody>
    </table>
|]

renderPasskeyRow :: Bool -> Passkey -> Html
renderPasskeyRow isOnly passkey = [hsx|
    <tr>
        <td>
            <input type="text"
                   class="passkey-name-input border-0 bg-transparent"
                   value={passkey.name}
                   readonly=""
                   data-update-url={pathTo (UpdatePasskeyNameAction passkey.id)} />
        </td>
        <td class="text-muted small">{passkey.createdAt}</td>
        <td class="text-muted small">{passkey.lastUsedAt}</td>
        <td>
            {renderDeleteButton isOnly passkey}
        </td>
    </tr>
|]

renderDeleteButton :: Bool -> Passkey -> Html
renderDeleteButton True _ = [hsx|
    <button class="btn btn-sm text-muted" disabled
            title="Cannot remove your only passkey">
        Delete
    </button>
|]
renderDeleteButton False passkey = [hsx|
    <form method="POST" action={pathTo (DeletePasskeyAction passkey.id)}
          class="d-inline"
          onsubmit="return confirm('Remove this passkey?')">
        <button type="submit" class="btn btn-sm text-danger">
            Delete
        </button>
    </form>
|]
```

To add another passkey from the profile page, reuse the same `.js-passkey-register` pattern without the email input:

```haskell
<div class="js-passkey-register"
     data-begin-url={pathTo BeginPasskeyRegistrationAction}
     data-finish-url={pathTo FinishPasskeyRegistrationAction}
     data-status-id="profile-passkey-status"
     data-success-redirect={pathTo (EditUserAction currentUserId)}>
    <button type="button"
            class="btn btn-outline-primary js-passkey-register-button">
        Add another passkey
    </button>
</div>
<div id="profile-passkey-status" class="alert d-none"></div>
```

## Development Notes

WebAuthn requires a [secure context](https://developer.mozilla.org/en-US/docs/Web/Security/Secure_Contexts). During development, `localhost` is special-cased by browsers as a secure context even over plain HTTP, so passkeys work out of the box with IHP's default `localhost:8000`. However, if you access your dev server via a LAN IP (e.g. `192.168.1.x:8000`) or a non-localhost hostname without HTTPS, the WebAuthn API will not be available and passkey buttons will silently fail.

## Security Considerations

### Challenge Freshness

Each registration and authentication attempt generates a fresh random challenge. Challenges are stored in the server-side session (not in cookies or URLs) and cleared after use, preventing replay attacks.

### Signature Counter

The `sign_count` field detects cloned authenticators. On each successful authentication:

- **`SignatureCounterZero`**: The authenticator doesn't support counters — accepted.
- **`SignatureCounterUpdated`**: Counter increased normally — update the database.
- **`SignatureCounterPotentiallyCloned`**: Counter didn't increase — reject the authentication. This means someone may have cloned the authenticator.

### Ownership Verification

All passkey management operations (rename, delete) check `passkey.userId == currentUserId` before proceeding. Never trust client-supplied IDs without this check.

### Last Passkey Guard

Users cannot delete their only passkey. Without at least one passkey, they'd be locked out of their account permanently.

### Supported Algorithms

The implementation supports three standard algorithms:

- **ES256** (ECDSA with SHA-256): Most widely supported, used by most platform authenticators
- **EdDSA**: Modern and efficient
- **RS256** (RSA with SHA-256): Legacy support for older authenticators

### Allowed Origins

Origins are constructed from the request headers to ensure the credential was created for the correct website. The implementation supports `X-Forwarded-Proto` and `X-Forwarded-Host` headers for apps behind reverse proxies.
