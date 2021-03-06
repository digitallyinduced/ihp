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
  -- * Session Value
    SessionValue (..)

  -- * Session Error
  , SessionError (..)

  -- * Interacting with session store
  , setSession
  , getSession
  , getSessionEither
  , deleteSession
  , getSessionAndClear
  , getSessionAndClearEither

  -- * Helper functions for setSession
  -- | Helper functions for calling setSession
  -- without type applications syntax.
  , setSessionInt
  , setSessionInteger
  , setSessionDouble
  , setSessionFloat
  , setSessionText
  , setSessionString
  , setSessionBS
  , setSessionUUID

  -- * Helper functions for getSession
  -- | Helper functions for calling getSession
  -- without type applications syntax.
  -- If an error occurs while getting the value, the result will be @Nothing@.
  , getSessionInt
  , getSessionInteger
  , getSessionDouble
  , getSessionFloat
  , getSessionText
  , getSessionString
  , getSessionBS
  , getSessionUUID
  , getSessionRecordId

  -- * Helper functions for getSessionEither
  -- | Helper functions for calling getSessionEither
  -- without type applications syntax.
  -- If an error occurs while getting the value, the
  -- result will be 'SessionError'.
  , getSessionEitherInt
  , getSessionEitherInteger
  , getSessionEitherDouble
  , getSessionEitherFloat
  , getSessionEitherText
  , getSessionEitherString
  , getSessionEitherBS
  , getSessionEitherUUID
  , getSessionEitherRecordId
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
    fromSessionValue = Bifunctor.first wrap . checkAllInput Read.decimal
        where
            wrap err = "SessionValue Int error: " <> cs err

instance SessionValue Integer where
    toSessionValue = show
    fromSessionValue = Bifunctor.first wrap . checkAllInput Read.decimal
        where
            wrap err = "SessionValue Integer error: " <> cs err

instance SessionValue Double where
    toSessionValue = show
    fromSessionValue = Bifunctor.first wrap . checkAllInput Read.double
        where
            wrap err = "SessionValue Double error: " <> cs err

instance SessionValue Float where
    toSessionValue = show
    fromSessionValue = Bifunctor.first wrap . checkAllInput Read.rational
        where
            wrap err = "SessionValue Float error: " <> cs err

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

-- | Types of possible errors as a result of
-- requesting a value from the session storage
data SessionError
    -- | Session storage not found in the context of the request
    = VaultError
    -- | Value not found in the session storage
    | NotFoundError
    -- | Error occurce during parsing value
    | ParseError Text
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
-- __Example:__ Using helper functions for setSession
--
-- > action LogoutAction = do
-- >     setSessionText "userEmail" "hi@digitallyinduced.com"
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

-- | Retrives a value from the session:
--
-- getSession variant, which returns 'SessionError' if an error occurs
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
            . (?context :: ControllerContext, SessionValue value)
           => Text -> IO (Either SessionError value)
getSessionEither name = case vaultLookup of
    Just (sessionLookup, _) -> sessionLookup (cs name) >>= \case
        Nothing -> pure $ Left NotFoundError
        Just "" -> pure $ Left NotFoundError
        Just stringValue -> case fromSessionValue (cs stringValue) of
            Left error -> pure . Left $ ParseError error
            Right value -> pure $ Right value
    Nothing -> pure $ Left VaultError
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

-- | Returns a value from the session, and deletes it after retrieving:
--
-- getSessionAndClear variant, which returns 'SessionError' if an error
-- occurs while getting value from session storage
--
-- > action SessionExampleAction = do
-- >     counter <- getSessionAndClearEither @Int "counter"
-- >     case counter of
-- >         Right value -> ...
-- >         Left (ParseError errorMessage) -> ...
-- >         Left NotFoundError -> ...
-- >         Left VaultError -> ...
getSessionAndClearEither :: forall value
                          . ( ?context :: ControllerContext
                            , SessionValue value
                            )
                         => Text -> IO (Either SessionError value)
getSessionAndClearEither name = do
    value <- getSessionEither @value name
    when (isRight value) (deleteSession name)
    pure value

-- | Stores an 'Int' value inside the session:
--
-- > action SessionExampleAction = do
-- >     setSessionInt "counter" 1
setSessionInt :: (?context :: ControllerContext) => Text -> Int -> IO ()
setSessionInt name value = setSession @Int name value

-- | Stores an 'Integer' value inside the session:
--
-- > action SessionExampleAction = do
-- >     setSessionInteger "counter" 1
setSessionInteger :: (?context :: ControllerContext)
                  => Text -> Integer -> IO ()
setSessionInteger name value = setSession @Integer name value

-- | Stores a 'Double' value inside the session:
--
-- > action SessionExampleAction = do
-- >     setSessionDouble "double" 1.234
setSessionDouble :: (?context :: ControllerContext)
                 => Text -> Double -> IO ()
setSessionDouble name value = setSession @Double name value

-- | Stores a 'Float' value inside the session:
--
-- > action SessionExampleAction = do
-- >     setSessionDouble "float" 1.234
setSessionFloat :: (?context :: ControllerContext) => Text -> Float -> IO ()
setSessionFloat name value = setSession @Float name value

-- | Stores a 'Text' value inside the session:
--
-- > action SessionExampleAction = do
-- >     setSessionText "userEmail" "hi@digitallyinduced.com"
setSessionText :: (?context :: ControllerContext) => Text -> Text -> IO ()
setSessionText name value = setSession @Text name value

-- | Stores a 'String' value inside the session:
--
-- > action SessionExampleAction = do
-- >     setSessionString "userEmail" "hi@digitallyinduced.com"
setSessionString :: (?context :: ControllerContext)
                 => Text -> String -> IO ()
setSessionString name value = setSession @String name value

-- | Stores a 'ByteString' value inside the session:
--
-- > action SessionExampleAction = do
-- >     setSessionBS "userEmail" "hi@digitallyinduced.com"
setSessionBS :: (?context :: ControllerContext)
             => Text -> ByteString -> IO ()
setSessionBS name value = setSession @ByteString name value

-- | Stores a 'UUID' value inside the session:
--
-- > action SessionExampleAction = do
-- >     setSessionUUID "userUUID" "a020ba17-a94e-453f-9414-c54aa30caa54"
setSessionUUID :: (?context :: ControllerContext) => Text -> UUID -> IO ()
setSessionUUID name value = setSession @UUID name value

-- | Retrives a value from the session, and parses it as an 'Int':
--
-- > action SessionExampleAction = do
-- >     counter <- getSessionInt "counter"
getSessionInt :: (?context :: ControllerContext) => Text -> IO (Maybe Int)
getSessionInt = getSession @Int

-- | Retrives a value from the session, and parses it as an 'Integer':
--
-- > action SessionExampleAction = do
-- >     counter <- getSessionInteger "counter"
getSessionInteger :: (?context :: ControllerContext)
                  => Text -> IO (Maybe Integer)
getSessionInteger = getSession @Integer

-- | Retrives a value from the session, and parses it as an 'Double':
--
-- > action SessionExampleAction = do
-- >     vDouble <- getSessionDouble "double"
getSessionDouble :: (?context :: ControllerContext)
                 => Text -> IO (Maybe Double)
getSessionDouble = getSession @Double

-- | Retrives a value from the session, and parses it as an 'Float':
--
-- > action SessionExampleAction = do
-- >     vFloat <- getSessionFloat "float"
getSessionFloat :: (?context :: ControllerContext)
                => Text -> IO (Maybe Float)
getSessionFloat = getSession @Float

-- | Retrives a value from the session, and parses it as an 'Text':
--
-- > action SessionExampleAction = do
-- >     userEmail <- getSessionText "userEmail"
getSessionText :: (?context :: ControllerContext) => Text -> IO (Maybe Text)
getSessionText = getSession @Text

-- | Retrives a value from the session, and parses it as an 'String':
--
-- > action SessionExampleAction = do
-- >     userEmail <- getSessionString "userEmail"
getSessionString :: (?context :: ControllerContext)
                 => Text -> IO (Maybe String)
getSessionString = getSession @String

-- | Retrives a value from the session, and parses it as an 'ByteString':
--
-- > action SessionExampleAction = do
-- >     userEmail <- getSessionBS "userEmail"
getSessionBS :: (?context :: ControllerContext)
             => Text -> IO (Maybe ByteString)
getSessionBS = getSession @ByteString

-- | Retrives a value from the session, and parses it as an 'UUID':
--
-- > action SessionExampleAction = do
-- >     userId <- getSessionUUID "userId"
getSessionUUID :: (?context :: ControllerContext) => Text -> IO (Maybe UUID)
getSessionUUID = getSession @UUID

-- | Retrives e.g. an @Id User@ or @Id Project@ from the session:
--
-- > action SessionExampleAction = do
-- >     userId <- getSessionRecordId @User "userId"
getSessionRecordId :: forall record
                    . ( ?context :: ControllerContext
                      , SessionValue (PrimaryKey (GetTableName record))
                      )
                   => Text -> IO (Maybe (Id record))
getSessionRecordId = getSession @(Id record)

-- | Retrives a value from the session, and parses it as an 'Int':
--
-- > action SessionExampleAction = do
-- >     counter <- getSessionEitherInt "counter"
getSessionEitherInt :: (?context :: ControllerContext)
                    => Text -> IO (Either SessionError Int)
getSessionEitherInt = getSessionEither @Int

-- | Retrives a value from the session, and parses it as an 'Integer':
--
-- > action SessionExampleAction = do
-- >     counter <- getSessionEitherInteger "counter"
getSessionEitherInteger :: (?context :: ControllerContext)
                        => Text -> IO (Either SessionError Integer)
getSessionEitherInteger = getSessionEither @Integer

-- | Retrives a value from the session, and parses it as an 'Double':
--
-- > action SessionExampleAction = do
-- >     vDouble <- getSessionEitherDouble "double"
getSessionEitherDouble :: (?context :: ControllerContext)
                       => Text -> IO (Either SessionError Double)
getSessionEitherDouble = getSessionEither @Double

-- | Retrives a value from the session, and parses it as an 'Float':
--
-- > action SessionExampleAction = do
-- >     vFloat <- getSessionEitherFloat "float"
getSessionEitherFloat :: (?context :: ControllerContext)
                      => Text -> IO (Either SessionError Float)
getSessionEitherFloat = getSessionEither @Float

-- | Retrives a value from the session, and parses it as an 'Text':
--
-- > action SessionExampleAction = do
-- >     userEmail <- getSessionEitherText "userEmail"
getSessionEitherText :: (?context :: ControllerContext)
                     => Text -> IO (Either SessionError Text)
getSessionEitherText = getSessionEither @Text

-- | Retrives a value from the session, and parses it as an 'String':
--
-- > action SessionExampleAction = do
-- >     userEmail <- getSessionEitherString "userEmail"
getSessionEitherString :: (?context :: ControllerContext)
                       => Text -> IO (Either SessionError String)
getSessionEitherString = getSessionEither @String

-- | Retrives a value from the session, and parses it as an 'ByteString':
--
-- > action SessionExampleAction = do
-- >     userEmail <- getSessionEitherBS "userEmail"
getSessionEitherBS :: (?context :: ControllerContext)
                   => Text -> IO (Either SessionError ByteString)
getSessionEitherBS = getSessionEither @ByteString

-- | Retrives a value from the session, and parses it as an 'UUID':
--
-- > action SessionExampleAction = do
-- >     userId <- getSessionEitherUUID "userId"
getSessionEitherUUID :: (?context :: ControllerContext)
                     => Text -> IO (Either SessionError UUID)
getSessionEitherUUID = getSessionEither @UUID

-- | Retrives e.g. an @Id User@ or @Id Project@ from the session:
--
-- > action SessionExampleAction = do
-- >     userId :: <- getSessionEitherRecordId @User "userId"
getSessionEitherRecordId :: forall record
                          . ( ?context :: ControllerContext
                            , SessionValue (PrimaryKey (GetTableName record))
                            )
                         => Text -> IO (Either SessionError (Id record))
getSessionEitherRecordId = getSessionEither @(Id record)

-- | Internal helper function for parsing numeric values
-- for an input starting with numbers but containing text afterwards
checkAllInput :: forall value . Num value
              => Read.Reader value -> Text -> Either Text value
checkAllInput reader input = case Read.signed reader input of
    Right (value, "") -> Right value
    Right (value, rest) -> Left "input contains non digit chars"
    Left _ -> Left "input contains non digit chars"
