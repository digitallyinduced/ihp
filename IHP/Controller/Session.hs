{-# LANGUAGE AllowAmbiguousTypes #-}
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
  -- * Session Value
    SessionValue (..)
  -- * Interacting with session store
  , setSession
  , getSession
  , deleteSession
  , getSessionAndClear
  -- * Helper functions for getSession
  -- | Helper functions for calling getSession
  -- without type applications syntax.
  -- If an error occurs while getting the value, the result will be @Nothing@.
  , getSessionInt
  , getSessionUUID
  , getSessionRecordId
  ) where

import IHP.Prelude
import IHP.Controller.RequestContext
import IHP.Controller.Context
import IHP.ModelSupport
import qualified Data.Aeson as Aeson
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text.Read as Read
import qualified Data.UUID as UUID
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai as Wai

-- | Provides functions for converting values between custom
-- representations and text to store.
--
-- Instead of manually writing your SessionValue instance,
-- there are option for default implementation for types with
-- FromJSON and ToJSON instances.
--
-- __Example:__
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import GHC.Generics
-- import Data.Aeson
--
-- data Coord = Coord { x :: Int, y :: Int } deriving Generic
--
-- instance FromJSON Coord
-- instance ToJSON Coord
-- instance SessionValue Coord
-- @
--
-- __Example:__ with deriving strategies
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
-- {-\# LANGUAGE DeriveAnyClass \#-}
-- {-\# LANGUAGE DerivingStrategies \#-}
--
-- import GHC.Generics
-- import Data.Aeson
--
-- data Point = Point { x :: Int, y :: Int }
--     deriving stock Generic
--     deriving anyclass (ToJSON, FromJSON, SessionValue)
-- @
class SessionValue value where
    -- | Convert 'value' to 'Text'.
    toSessionValue :: value -> Text

    -- | Parse 'Text' to @value@. Return 'Right' @value@ if parsing succssed
    -- or 'Left' @errorMessage@ if parsing failed.
    fromSessionValue :: Text -> Either Text value

    default toSessionValue
      :: (Aeson.ToJSON value) => value -> Text
    toSessionValue = cs . Aeson.encode

    default fromSessionValue
      :: (Aeson.FromJSON value) => Text -> Either Text value
    fromSessionValue = Bifunctor.first wrap . Aeson.eitherDecode @value . cs
        where
            wrap err = "SessionValue JSON error: " <> cs err

instance SessionValue Text where
    toSessionValue = id
    fromSessionValue = Right

instance SessionValue String where
    toSessionValue = cs
    fromSessionValue = Right . cs

instance SessionValue ByteString where
    toSessionValue = cs
    fromSessionValue = Right . cs

instance SessionValue Int where
    toSessionValue = show
    fromSessionValue = Bifunctor.bimap wrap fst . Read.signed Read.decimal
        where
            wrap err = "SessionValue Int error: " <> cs err

instance SessionValue Integer where
    toSessionValue = show
    fromSessionValue = Bifunctor.bimap wrap fst . Read.signed Read.decimal
        where
            wrap err = "SessionValue Integer error: " <> cs err

instance SessionValue Double where
    toSessionValue = show
    fromSessionValue = Bifunctor.bimap wrap fst . Read.signed Read.double
        where
            wrap err = "SessionValue Double error: " <> cs err

instance SessionValue UUID where
    toSessionValue = UUID.toText
    fromSessionValue = maybe wrap Right . UUID.fromText
        where
            wrap = Left "SessionValue UUID parse error"

instance SessionValue (PrimaryKey record) => SessionValue (Id' record) where
    toSessionValue (Id value) = toSessionValue value
    fromSessionValue = Bifunctor.bimap wrap Id . fromSessionValue
        where
            wrap err = "SessionValue Id error: " <> cs err

-- | Stores a value inside the session:
--
-- > action SessionExampleAction { userId } = do
-- >     setSession "userEmail" ("hi@digitallyinduced.com" :: Text)
-- >     setSession "userId" userId
--
setSession :: (?context :: ControllerContext, SessionValue value)
           => Text -> value -> IO ()
setSession name value = case vaultLookup of
    Just (_, sessionInsert) -> sessionInsert stringName stringValue
    Nothing -> pure ()
    where
        RequestContext { request, vault } = get #requestContext ?context
        vaultLookup = Vault.lookup vault (Wai.vault request)
        stringValue = cs $ toSessionValue value
        stringName = cs name

-- | Retrives a value from the session:
--
-- > action SessionExampleAction = do
-- >     userEmail <- getSession @Text "userEmail"
-- >     counter <- getSession @Int "counter"
-- >     userId <- getSession @UUID "userId"
--
-- @userEmail@ is set to 'Just' \"hi@digitallyinduced.com\"
-- when the value has been set before. Otherwise, it will be 'Nothing'.
-- If an error occurs while getting the value, the result will be 'Nothing'.
getSession :: forall value
            . (?context :: ControllerContext, SessionValue value)
           => Text -> IO (Maybe value)
getSession name = case vaultLookup of
    Just (sessionLookup, _) -> sessionLookup (cs name) >>= \case
        Nothing -> pure Nothing
        Just "" -> pure Nothing
        Just stringValue -> case fromSessionValue (cs stringValue) of
            Left _ -> pure Nothing
            Right value -> pure $ Just value
    Nothing -> pure Nothing
    where
        RequestContext { request, vault } = get #requestContext ?context
        vaultLookup = Vault.lookup vault (Wai.vault request)

-- | Remove session values ​​from storage:
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
deleteSession :: (?context :: ControllerContext) => Text -> IO ()
deleteSession name = setSession name ("" :: Text)

-- | Returns a value from the session, and deletes it after retrieving:
--
-- > action SessionExampleAction = do
-- >     notification <- getSessionAndClear @Text "notification"
getSessionAndClear :: forall value
                    . (?context :: ControllerContext, SessionValue value)
                   => Text -> IO (Maybe value)
getSessionAndClear name = do
    value <- getSession @value name
    when (isJust value) (deleteSession name)
    pure value

-- | Retrives a value from the session, and parses it as an 'Int':
--
-- > action SessionExampleAction = do
-- >     counter :: Maybe Int <- getSessionInt "counter"
getSessionInt :: (?context :: ControllerContext) => Text -> IO (Maybe Int)
getSessionInt = getSession

-- | Retrives a value from the session, and parses it as an 'UUID':
--
-- > action SessionExampleAction = do
-- >     userId :: Maybe UUID <- getSessionUUID "userId"
getSessionUUID :: (?context :: ControllerContext) => Text -> IO (Maybe UUID)
getSessionUUID = getSession @UUID

-- | Retrives e.g. an @Id User@ or @Id Project@ from the session:
--
-- > action SessionExampleAction = do
-- >     userId :: Maybe (Id User) <- getSessionRecordId @User "userId"
getSessionRecordId :: forall record
                    . ( ?context :: ControllerContext
                      , SessionValue (PrimaryKey (GetTableName record))
                      )
                   => Text -> IO (Maybe (Id record))
getSessionRecordId = getSession @(Id record)
