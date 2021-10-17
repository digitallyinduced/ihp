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
-- __Example:__ Using helper functions for setSession
--
-- > action LogoutAction = do
-- >     setSessionText "userEmail" "hi@digitallyinduced.com"
setSession :: (?context :: ControllerContext, Serialize value)
           => ByteString -> value -> IO ()
setSession name value = sessionInsert name (Serialize.encode value)

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
            . (?context :: ControllerContext, Serialize value)
           => ByteString -> IO (Maybe value)
getSession name = getSessionEither name >>= \case
    Left _ -> pure Nothing
    Right result -> pure (Just result)

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
            . (?context :: ControllerContext, Serialize value)
           => ByteString -> IO (Either SessionError value)
getSessionEither name = sessionLookup name >>= \case
        Nothing -> pure $ Left NotFoundError
        Just "" -> pure $ Left NotFoundError
        Just stringValue -> case Serialize.decode stringValue of
            Left error -> pure . Left $ ParseError error
            Right value -> pure $ Right value

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
deleteSession :: (?context :: ControllerContext) => ByteString -> IO ()
deleteSession name = setSession name ("" :: Text)

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
                            , Serialize value
                            )
                         => ByteString -> IO (Either SessionError value)
getSessionAndClearEither name = do
    value <- getSessionEither @value name
    when (isRight value) (deleteSession name)
    pure value

-- | Stores an 'Int' value inside the session:
--
-- > action SessionExampleAction = do
-- >     setSessionInt "counter" 1
setSessionInt :: (?context :: ControllerContext) => ByteString -> Int -> IO ()
setSessionInt name value = setSession @Int name value

-- | Stores an 'Integer' value inside the session:
--
-- > action SessionExampleAction = do
-- >     setSessionInteger "counter" 1
setSessionInteger :: (?context :: ControllerContext)
                  => ByteString -> Integer -> IO ()
setSessionInteger name value = setSession @Integer name value

-- | Stores a 'Double' value inside the session:
--
-- > action SessionExampleAction = do
-- >     setSessionDouble "double" 1.234
setSessionDouble :: (?context :: ControllerContext)
                 => ByteString -> Double -> IO ()
setSessionDouble name value = setSession @Double name value

-- | Stores a 'Float' value inside the session:
--
-- > action SessionExampleAction = do
-- >     setSessionDouble "float" 1.234
setSessionFloat :: (?context :: ControllerContext) => ByteString -> Float -> IO ()
setSessionFloat name value = setSession @Float name value

-- | Stores a 'Text' value inside the session:
--
-- > action SessionExampleAction = do
-- >     setSessionText "userEmail" "hi@digitallyinduced.com"
setSessionText :: (?context :: ControllerContext) => ByteString -> Text -> IO ()
setSessionText name value = setSession @Text name value

-- | Stores a 'String' value inside the session:
--
-- > action SessionExampleAction = do
-- >     setSessionString "userEmail" "hi@digitallyinduced.com"
setSessionString :: (?context :: ControllerContext)
                 => ByteString -> String -> IO ()
setSessionString name value = setSession @String name value

-- | Stores a 'ByteString' value inside the session:
--
-- > action SessionExampleAction = do
-- >     setSessionBS "userEmail" "hi@digitallyinduced.com"
setSessionBS :: (?context :: ControllerContext)
             => ByteString -> ByteString -> IO ()
setSessionBS name value = setSession @ByteString name value

-- | Stores a 'UUID' value inside the session:
--
-- > action SessionExampleAction = do
-- >     setSessionUUID "userUUID" "a020ba17-a94e-453f-9414-c54aa30caa54"
setSessionUUID :: (?context :: ControllerContext) => ByteString -> UUID -> IO ()
setSessionUUID name value = setSession @UUID name value

-- | Retrives a value from the session, and parses it as an 'Int':
--
-- > action SessionExampleAction = do
-- >     counter <- getSessionInt "counter"
getSessionInt :: (?context :: ControllerContext) => ByteString -> IO (Maybe Int)
getSessionInt = getSession @Int

-- | Retrives a value from the session, and parses it as an 'Integer':
--
-- > action SessionExampleAction = do
-- >     counter <- getSessionInteger "counter"
getSessionInteger :: (?context :: ControllerContext)
                  => ByteString -> IO (Maybe Integer)
getSessionInteger = getSession @Integer

-- | Retrives a value from the session, and parses it as an 'Double':
--
-- > action SessionExampleAction = do
-- >     vDouble <- getSessionDouble "double"
getSessionDouble :: (?context :: ControllerContext)
                 => ByteString -> IO (Maybe Double)
getSessionDouble = getSession @Double

-- | Retrives a value from the session, and parses it as an 'Float':
--
-- > action SessionExampleAction = do
-- >     vFloat <- getSessionFloat "float"
getSessionFloat :: (?context :: ControllerContext)
                => ByteString -> IO (Maybe Float)
getSessionFloat = getSession @Float

-- | Retrives a value from the session, and parses it as an 'Text':
--
-- > action SessionExampleAction = do
-- >     userEmail <- getSessionText "userEmail"
getSessionText :: (?context :: ControllerContext) => ByteString -> IO (Maybe Text)
getSessionText = getSession @Text

-- | Retrives a value from the session, and parses it as an 'String':
--
-- > action SessionExampleAction = do
-- >     userEmail <- getSessionString "userEmail"
getSessionString :: (?context :: ControllerContext)
                 => ByteString -> IO (Maybe String)
getSessionString = getSession @String

-- | Retrives a value from the session, and parses it as an 'ByteString':
--
-- > action SessionExampleAction = do
-- >     userEmail <- getSessionBS "userEmail"
getSessionBS :: (?context :: ControllerContext)
             => ByteString -> IO (Maybe ByteString)
getSessionBS = getSession @ByteString

-- | Retrives a value from the session, and parses it as an 'UUID':
--
-- > action SessionExampleAction = do
-- >     userId <- getSessionUUID "userId"
getSessionUUID :: (?context :: ControllerContext) => ByteString -> IO (Maybe UUID)
getSessionUUID = getSession @UUID

-- | Retrives e.g. an @Id User@ or @Id Project@ from the session:
--
-- > action SessionExampleAction = do
-- >     userId <- getSessionRecordId @User "userId"
getSessionRecordId :: forall record
                    . ( ?context :: ControllerContext
                      , Serialize (PrimaryKey (GetTableName record))
                      )
                   => ByteString -> IO (Maybe (Id record))
getSessionRecordId = getSession @(Id record)

-- | Retrives a value from the session, and parses it as an 'Int':
--
-- > action SessionExampleAction = do
-- >     counter <- getSessionEitherInt "counter"
getSessionEitherInt :: (?context :: ControllerContext)
                    => ByteString -> IO (Either SessionError Int)
getSessionEitherInt = getSessionEither @Int

-- | Retrives a value from the session, and parses it as an 'Integer':
--
-- > action SessionExampleAction = do
-- >     counter <- getSessionEitherInteger "counter"
getSessionEitherInteger :: (?context :: ControllerContext)
                        => ByteString -> IO (Either SessionError Integer)
getSessionEitherInteger = getSessionEither @Integer

-- | Retrives a value from the session, and parses it as an 'Double':
--
-- > action SessionExampleAction = do
-- >     vDouble <- getSessionEitherDouble "double"
getSessionEitherDouble :: (?context :: ControllerContext)
                       => ByteString -> IO (Either SessionError Double)
getSessionEitherDouble = getSessionEither @Double

-- | Retrives a value from the session, and parses it as an 'Float':
--
-- > action SessionExampleAction = do
-- >     vFloat <- getSessionEitherFloat "float"
getSessionEitherFloat :: (?context :: ControllerContext)
                      => ByteString -> IO (Either SessionError Float)
getSessionEitherFloat = getSessionEither @Float

-- | Retrives a value from the session, and parses it as an 'Text':
--
-- > action SessionExampleAction = do
-- >     userEmail <- getSessionEitherText "userEmail"
getSessionEitherText :: (?context :: ControllerContext)
                     => ByteString -> IO (Either SessionError Text)
getSessionEitherText = getSessionEither @Text

-- | Retrives a value from the session, and parses it as an 'String':
--
-- > action SessionExampleAction = do
-- >     userEmail <- getSessionEitherString "userEmail"
getSessionEitherString :: (?context :: ControllerContext)
                       => ByteString -> IO (Either SessionError String)
getSessionEitherString = getSessionEither @String

-- | Retrives a value from the session, and parses it as an 'ByteString':
--
-- > action SessionExampleAction = do
-- >     userEmail <- getSessionEitherBS "userEmail"
getSessionEitherBS :: (?context :: ControllerContext)
                   => ByteString -> IO (Either SessionError ByteString)
getSessionEitherBS = getSessionEither @ByteString

-- | Retrives a value from the session, and parses it as an 'UUID':
--
-- > action SessionExampleAction = do
-- >     userId <- getSessionEitherUUID "userId"
getSessionEitherUUID :: (?context :: ControllerContext)
                     => ByteString -> IO (Either SessionError UUID)
getSessionEitherUUID = getSessionEither @UUID

-- | Retrives e.g. an @Id User@ or @Id Project@ from the session:
--
-- > action SessionExampleAction = do
-- >     userId :: <- getSessionEitherRecordId @User "userId"
getSessionEitherRecordId :: forall record
                          . ( ?context :: ControllerContext
                            , Serialize (PrimaryKey (GetTableName record))
                            )
                         => ByteString -> IO (Either SessionError (Id record))
getSessionEitherRecordId = getSessionEither @(Id record)


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
