{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module: IHP.Controller.Session
Description: Functions to work with session cookies, provides 'setSession', 'getSession' and friends
Copyright: (c) digitally induced GmbH, 2021

The session provides a way for your application to store small amounts of information that will be persisted between requests. It’s mainly used from inside your controller actions.

In general, you should not store complex data structures in the session. It’s better to store scalar values in there only. For example: Store the current user-id instead of the current user record.

The session works by storing the data inside a cryptographically signed and encrypted cookie on the client. The encryption key is generated automatically and is stored at @Config/client_session_key.aes@. Internally IHP uses the clientsession library. You can find more technical details on the implementation in the <https://hackage.haskell.org/package/clientsession-0.9.1.2/docs/Web-ClientSession.html clientsession> documentation.

The cookie @max-age@ is set to 30 days by default. To protect against CSRF, the @SameSite@ Policy is set to @Lax@.
-}
module IHP.Controller.Session
  (
  -- * Session Error
  SessionError (..)

  -- * Interacting with session store
  , setSession
  , getSession
  , getSessionEither
  , deleteSession
  , getSessionAndClear
  ) where

import IHP.Prelude
import IHP.Controller.RequestContext
import IHP.Controller.Context
import IHP.ModelSupport
import Data.Either (isRight)
import qualified Data.Aeson as Aeson
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text.Read as Read
import qualified Data.UUID as UUID
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai as Wai
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Data.Serialize.Text ()
import Cereal.Uuid.Serialize ()

-- | Types of possible errors as a result of
-- requesting a value from the session storage
data SessionError
    -- | Value not found in the session storage
    = NotFoundError
    -- | Error occurce during parsing value
    | ParseError String
    deriving (Show, Eq)

-- | Stores a value inside the session:
--
-- > action SessionExampleAction { userId } = do
-- >     setSession "userId" userId
--
-- For cases where setSession is used with literals,
-- to avoid type ambiguity, you can use one of the options below
--
-- __Example:__ Annotate a literal with a type
--
-- > action LogoutAction = do
-- >     setSession "userEmail" ("hi@digitallyinduced.com" :: Text)
--
-- __Example:__ Using setSession with type application
--
-- > action LogoutAction = do
-- >     setSession @Text "userEmail" "hi@digitallyinduced.com"
--
setSession :: (?context :: ControllerContext, Serialize value)
           => ByteString -> value -> IO ()
setSession name value = sessionInsert name (Serialize.encode value)
{-# INLINABLE setSession #-}

-- | Retrives a value from the session:
--
-- > action SessionExampleAction = do
-- >     userEmail <- getSession @Text "userEmail"
-- >     counter <- getSession @Int "counter"
-- >     userId <- getSession @(Id User) "userId"
--
-- @userEmail@ is set to @Just' "hi@digitallyinduced.com"@
-- when the value has been set before. Otherwise, it will be 'Nothing'.
--
-- If an error occurs while getting the value, the result will be 'Nothing'.
getSession :: forall value
            . (?context :: ControllerContext, Serialize value)
           => ByteString -> IO (Maybe value)
getSession name = getSessionEither name >>= \case
    Left _ -> pure Nothing
    Right result -> pure (Just result)
{-# INLINABLE getSession #-}

-- | Retrives a value from the session:
--
-- 'getSession' variant, which returns 'SessionError' if an error occurs
-- while getting value from session storage
--
-- > action SessionExampleAction = do
-- >     counter <- getSessionEither @Int "counter"
-- >     case counter of
-- >         Right value -> ...
-- >         Left (ParseError errorMessage) -> ...
-- >         Left NotFoundError -> ...
-- >         Left VaultError -> ...
getSessionEither :: forall value
            . (?context :: ControllerContext, Serialize value)
           => ByteString -> IO (Either SessionError value)
getSessionEither name = sessionLookup name >>= \case
        Nothing -> pure $ Left NotFoundError
        Just "" -> pure $ Left NotFoundError
        Just stringValue -> case Serialize.decode stringValue of
            Left error -> pure . Left $ ParseError error
            Right value -> pure $ Right value
{-# INLINABLE getSessionEither #-}

-- | Remove session values from storage:
--
-- __Example:__ Deleting a @userId@ field from the session
--
-- > action LogoutAction = do
-- >     deleteSession "userId"
--
-- __Example:__ Calling 'getSession' after
-- using 'deleteSession' will return @Nothing@
--
-- > setSession "userId" (1337 :: Int)
-- > userId <- getSession @Int "userId" -- Returns: Just 1337
-- >
-- > deleteSession "userId"
-- > userId <- getSession @Int "userId" -- Returns: Nothing
deleteSession :: (?context :: ControllerContext) => ByteString -> IO ()
deleteSession name = setSession name ("" :: ByteString)

-- | Returns a value from the session, and deletes it after retrieving:
--
-- > action SessionExampleAction = do
-- >     notification <- getSessionAndClear @Text "notification"
getSessionAndClear :: forall value
                    . (?context :: ControllerContext, Serialize value)
                   => ByteString -> IO (Maybe value)
getSessionAndClear name = do
    value <- getSession @value name
    when (isJust value) (deleteSession name)
    pure value
{-# INLINABLE getSessionAndClear #-}

instance (Serialize (PrimaryKey table)) => Serialize (Id' table) where
    put (Id value) = Serialize.put value
    get = Id <$> Serialize.get

sessionInsert :: (?context :: ControllerContext) => ByteString -> ByteString -> IO ()
sessionInsert = snd sessionVault

sessionLookup :: (?context :: ControllerContext) => ByteString -> IO (Maybe ByteString)
sessionLookup = fst sessionVault

sessionVault :: (?context :: ControllerContext) => (ByteString -> IO (Maybe ByteString), ByteString -> ByteString -> IO ())
sessionVault = case vaultLookup of
        Just session -> session
        Nothing -> error "sessionInsert: The session vault is missing in the request"
    where
        RequestContext { request, vault } = get #requestContext ?context
        vaultLookup = Vault.lookup vault (Wai.vault request)
