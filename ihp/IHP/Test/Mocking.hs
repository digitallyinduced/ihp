{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ExistentialQuantification   #-}

module IHP.Test.Mocking where

import           Data.ByteString.Builder                   (toLazyByteString)
import qualified Data.ByteString.Lazy                      as LBS
import qualified Data.Vault.Lazy                           as Vault
import qualified Network.HTTP.Types.Status                 as HTTP
import           Network.Wai
import           Network.Wai.Internal                      (ResponseReceived (..))
import           Network.Wai.Parse                         (Param (..))

import           Wai.Request.Params.Middleware                 (Respond)
import           IHP.ControllerSupport                     (InitControllerContext, Controller, runActionWithNewContext)
import           IHP.FrameworkConfig                       (ConfigBuilder (..), FrameworkConfig (..))
import qualified IHP.FrameworkConfig                       as FrameworkConfig
import           IHP.ModelSupport                          (createModelContext, Id')
import           IHP.Prelude
import           IHP.Log.Types
import           IHP.Job.Types
import Test.Hspec
import qualified Data.Text as Text
import qualified Network.Wai as Wai
import qualified IHP.LoginSupport.Helper.Controller as Session
import qualified Network.Wai.Session
import qualified Data.Serialize as Serialize
import IHP.Controller.Session (sessionVaultKey)
import IHP.RequestVault (RequestBody (..), requestBodyVaultKey)
import IHP.Server (initMiddlewareStack)

type ContextParameters application = (?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext, ?application :: application, InitControllerContext application, ?mocking :: MockContext application)

data MockContext application = InitControllerContext application => MockContext
    { modelContext :: ModelContext
    , frameworkConfig :: FrameworkConfig
    , mockRequest :: Request
    , mockRespond :: Respond
    , application :: application
    }

-- | Run a request through the test middleware stack.
-- This applies the same middlewares that IHP.Server uses (with PGListener disabled).
runTestMiddlewares :: FrameworkConfig -> ModelContext -> [Param] -> Request -> IO Request
runTestMiddlewares frameworkConfig modelContext params baseRequest = do
    -- Capture the modified request after running through middlewares
    resultRef <- newIORef baseRequest
    let captureApp req respond = do
            writeIORef resultRef req
            respond (responseLBS HTTP.status200 [] "")

    -- Use the same middleware stack as production, but without PGListener
    middlewareStack <- initMiddlewareStack frameworkConfig modelContext Nothing

    -- Run request through middleware stack
    _ <- middlewareStack captureApp baseRequest (\_ -> pure ResponseReceived)

    -- Get the modified request, then add our params
    modifiedRequest <- readIORef resultRef
    let requestWithParams = modifiedRequest { Wai.vault = Vault.insert requestBodyVaultKey (FormBody params []) (Wai.vault modifiedRequest) }
    pure requestWithParams

mockContextNoDatabase :: (InitControllerContext application) => application -> ConfigBuilder -> IO (MockContext application)
mockContextNoDatabase application configBuilder = do
   frameworkConfig@(FrameworkConfig {dbPoolMaxConnections, dbPoolIdleTime, databaseUrl}) <- FrameworkConfig.buildFrameworkConfig configBuilder
   logger <- newLogger def { level = Warn } -- don't log queries
   modelContext <- createModelContext dbPoolIdleTime dbPoolMaxConnections databaseUrl logger

   let baseVault = Vault.insert sessionVaultKey mempty Vault.empty
   let baseRequest = defaultRequest { vault = baseVault }
   mockRequest <- runTestMiddlewares frameworkConfig modelContext [] baseRequest
   let mockRespond = const (pure ResponseReceived)

   pure MockContext{..}

-- | Run a IO action, setting implicit params based on supplied mock context
withContext :: (ContextParameters application => IO a) -> MockContext application -> IO a
withContext action mocking@MockContext{..} = let
    ?modelContext = modelContext
    ?request = mockRequest
    ?respond = mockRespond
    ?application = application
    ?mocking = mocking
  in do
    action

setupWithContext :: (ContextParameters application => IO a) -> MockContext application -> IO (MockContext application)
setupWithContext action context = withContext action context >> pure context

-- | Runs a controller action in a mock environment
callAction :: forall application controller. (Controller controller, ContextParameters application, Typeable application, Typeable controller) => controller -> IO Response
callAction controller = callActionWithParams controller []

-- | Runs a controller action in a mock environment
--
-- >>> callActionWithParams CreatePostAction [("title", "Hello World"), ("body", "lorem ipsum")|
-- Response { .. }
--
callActionWithParams :: forall application controller. (Controller controller, ContextParameters application, Typeable application, Typeable controller) => controller -> [Param] -> IO Response
callActionWithParams controller params = do
    -- Add params to the current request (which may have been modified by withUser)
    -- The current ?request already has the vault with modelContext, frameworkConfig, session, and approot
    let requestWithParams = ?request { Wai.vault = Vault.insert requestBodyVaultKey (FormBody params []) (Wai.vault ?request) }

    -- Capture the response
    responseRef <- newIORef Nothing
    let captureRespond response = do
            writeIORef responseRef (Just response)
            pure ResponseReceived

    let ?request = requestWithParams
    let ?respond = captureRespond
    runActionWithNewContext controller

    readIORef responseRef >>= \case
        Just response -> pure response
        Nothing -> error "callActionWithParams: No response was returned by the controller"

-- | Run a Job in a mock environment
--
-- __Example:__
--
-- Let's say you have a Job called @JobPost@ that you would like to process as part of a test.
--
-- >  let postJob <- fetch ...
-- >
-- >  callJob postJob
--
-- Note that 'callJob' doesn't set the Job status that is initially set 'IHP.Job.Types.JobStatusNotStarted', as that is
-- done by the Job queue (see 'IHP.Job.Queue.jobDidSucceed' for example).
--
callJob :: forall application job. (ContextParameters application, Typeable application, Job job) => job -> IO ()
callJob job = do
    let frameworkConfig = ?request.frameworkConfig
    let ?context = frameworkConfig
    perform job


-- | mockAction has been renamed to callAction
mockAction :: forall application controller. (Controller controller, ContextParameters application, Typeable application, Typeable controller) => controller -> IO Response
mockAction = callAction

-- | Get contents of response
mockActionResponse :: forall application controller. (Controller controller, ContextParameters application, Typeable application, Typeable controller) => controller -> IO LBS.ByteString
mockActionResponse = (responseBody =<<) . mockAction

-- | Get HTTP status of the controller
mockActionStatus :: forall application controller. (Controller controller, ContextParameters application, Typeable application, Typeable controller) => controller -> IO HTTP.Status
mockActionStatus = fmap responseStatus . mockAction

responseBody :: Response -> IO LBS.ByteString
responseBody res =
  let (status,headers,body) = responseToStream res in
  body $ \f -> do
    content <- newIORef mempty
    f (\chunk -> modifyIORef' content (<> chunk)) (return ())
    toLazyByteString <$> readIORef content

-- | Asserts that the response body contains the given text.
responseBodyShouldContain :: Response -> Text -> IO ()
responseBodyShouldContain response includedText = do
    body :: Text <- cs <$> responseBody response
    body `shouldSatisfy` (includedText `Text.isInfixOf`)

-- | Asserts that the response body does not contain the given text.
responseBodyShouldNotContain :: Response -> Text -> IO ()
responseBodyShouldNotContain response includedText = do
    body :: Text <- cs <$> responseBody response
    body `shouldNotSatisfy` (includedText `Text.isInfixOf`)

-- | Asserts that the response status is equal to the given status.
responseStatusShouldBe :: Response -> HTTP.Status -> IO ()
responseStatusShouldBe response status = responseStatus response `shouldBe` status

-- | Set's the current user for the application
--
-- Example:
--
-- > user <- newRecord @User
-- >     |> set #email "marc@digitallyinduced.com"
-- >     |> createRecord
-- >
-- > response <- withUser user do
-- >     callAction CreatePostAction
--
-- In this example the 'currentUser' will refer to the newly
-- created user during the execution of CreatePostAction
--
-- Internally this function overrides the session cookie passed to
-- the application.
--
withUser :: forall user application userId result.
    ( ?mocking :: MockContext application
    , ?request :: Request
    , ?respond :: Respond
    , Serialize.Serialize userId
    , HasField "id" user userId
    , KnownSymbol (GetModelName user)
    ) => user -> ((?request :: Request, ?respond :: Respond) => IO result) -> IO result
withUser user callback =
        let ?request = newRequest
        in callback
    where
        newRequest = currentRequest { Wai.vault = newVault }

        newSession :: Network.Wai.Session.Session IO ByteString ByteString
        newSession = (lookupSession, insertSession)

        lookupSession key = if key == sessionKey
            then pure (Just sessionValue)
            else pure Nothing

        insertSession key value = pure ()

        newVault = Vault.insert sessionVaultKey newSession (Wai.vault currentRequest)
        currentRequest = ?request  -- Use current ?request, not ?mocking.mockRequest

        sessionValue = Serialize.encode (user.id)
        sessionKey = cs (Session.sessionKey @user)

-- | Turns a record id into a value that can be used with 'callActionWithParams'
--
-- __Example:__
--
-- Let's say you have a test like this:
--
-- >  let postId = cs $ show $ post.id
-- >
-- >  let params = [ ("postId", postId) ]
--
-- You can replace the @cs $ show $@ with a cleaner 'idToParam':
--
--
-- >  let postId = idToParam (libraryOpening.id)
-- >
-- >  let params = [ ("postId", postId) ]
--
idToParam :: forall table. (Show (Id' table)) => Id' table -> ByteString
idToParam id = id
    |> tshow
    |> cs
