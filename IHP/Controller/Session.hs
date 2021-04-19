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
( setSession
, getSession
, getSessionAndClear
, getSessionInt
, getSessionUUID
, getSessionRecordId
, deleteSession
) where

import IHP.Prelude
import IHP.Controller.RequestContext
import IHP.Controller.Context
import IHP.ModelSupport
import qualified Data.Text.Read as Read
import qualified Data.UUID as UUID
import qualified Network.Wai as Wai
import qualified Data.Vault.Lazy as Vault

-- | Stores a value inside the session:
-- 
-- > action SessionExampleAction = do
-- >     setSession "userEmail" "hi@digitallyinduced.com"
--
-- Right now, 'setSession' only accepts Text values.
-- Other types like 'Int' have to be converted to 'Text' using @show theIntValue@.
setSession :: (?context :: ControllerContext) => Text -> Text -> IO ()
setSession name value = case vaultLookup of
    Just (_, sessionInsert) -> sessionInsert (cs name) (cs value)
    Nothing -> pure ()
    where
        RequestContext { request, vault } = get #requestContext ?context
        vaultLookup = Vault.lookup vault (Wai.vault request)

-- | Retrives a value from the session, returns it as a Text:
--
-- > action SessionExampleAction = do
-- >     userEmail <- getSession "userEmail"
--
-- @userEmail@ is set to @Just\ "hi@digitallyinduced.com"@ when the value has been set before. Otherwise, it will be @Nothing@.
--
-- For convenience you can use 'getSessionInt' to retrieve the value as a @Maybe Int@, and 'getSessionUUID' to retrieve the value as a @Maybe UUID@:
--
-- > action SessionExampleAction = do
-- >     counter :: Maybe Int <- getSessionInt "counter"
-- >     userId :: Maybe UUID <- getSessionUUID "userId"
--
getSession :: (?context :: ControllerContext) => Text -> IO (Maybe Text)
getSession name = case vaultLookup of
    Just (sessionLookup, _) -> do
        value <- (sessionLookup (cs name))
        let textValue = fmap cs value
        pure $! if textValue == Just "" then Nothing else textValue
    Nothing -> pure Nothing
    where
        RequestContext { request, vault } = get #requestContext ?context
        vaultLookup = Vault.lookup vault (Wai.vault request)

-- | Retrives a value from the session, and parses it as an 'Int':
--
-- > action SessionExampleAction = do
-- >     counter :: Maybe Int <- getSessionInt "counter"
--
-- Return @Nothing@ if parsing fails.
getSessionInt :: (?context :: ControllerContext) => Text -> IO (Maybe Int)
getSessionInt name = do
    value <- getSession name
    pure $! case fmap (Read.decimal . cs) value of
            Just (Right value) -> Just $ fst value
            _                  -> Nothing

-- | Retrives a value from the session, and parses it as an 'UUID':
--
-- > action SessionExampleAction = do
-- >     userId :: Maybe UUID <- getSessionUUID "userId"
--
-- Return @Nothing@ if parsing fails.
getSessionUUID :: (?context :: ControllerContext) => Text -> IO (Maybe UUID)
getSessionUUID name = do
    value <- getSession name
    pure $! case fmap UUID.fromText value of
            Just (Just value) -> Just value
            _                 -> Nothing

-- | Retrives e.g. an @Id User@ or @Id Project@ from the session:
--
-- > action SessionExampleAction = do
-- >     userId :: Maybe (Id User) <- getSessionRecordId @User "userId"
--
-- Return @Nothing@ if parsing fails.
getSessionRecordId :: forall record. (?context :: ControllerContext, PrimaryKey (GetTableName record) ~ UUID) => Text -> IO (Maybe (Id record))
getSessionRecordId name = fmap Id <$> getSessionUUID name

-- | After deleting a session value, calls to 'getSession' will returns @Nothing@
--
-- __Example:__ Deleting a @userId@ field from the session
--
-- > action LogoutAction = do
-- >     deleteSession "userId"
--
-- __Example:__ Calling 'getSession' after using 'deleteSession' will return @Nothing@
--
-- > setSession "userId" "1337"
-- > userId <- getSession "userId" -- Returns: Just 1337
-- > 
-- > deleteSession "userId"
-- > userId <- getSession "userId" -- Returns: Nothing
deleteSession :: (?context :: ControllerContext) => Text -> IO ()
deleteSession name = setSession name ""

-- | Returns a value from the session, and deletes it after retrieving:
--
-- > action SessionExampleAction = do
-- >     notification :: Maybe Text <- getSessionAndClear "notification"
--
getSessionAndClear :: (?context :: ControllerContext) => Text -> IO (Maybe Text)
getSessionAndClear name = do
    value <- getSession name
    when (isJust value) (deleteSession name)
    pure value
