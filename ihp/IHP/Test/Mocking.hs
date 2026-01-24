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

import qualified IHP.AutoRefresh.Types                     as AutoRefresh
import           IHP.RequestBodyMiddleware                 (RequestBody (..), Respond)
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
import qualified IHP.PGListener as PGListener
import IHP.Controller.Session (sessionVaultKey)
import qualified Network.Wai.Middleware.Approot as Approot
import qualified Network.Wai.Test as WaiTest
import IHP.RequestVault (modelContextMiddleware, frameworkConfigMiddleware, RequestBody (..), requestBodyVaultKey)
import IHP.RequestBodyMiddleware (requestBodyMiddleware)

type ContextParameters application = (?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext, ?application :: application, InitControllerContext application, ?mocking :: MockContext application)

data MockContext application = InitControllerContext application => MockContext
    { modelContext :: ModelContext
    , mockRequest :: Request
    , mockRespond :: Respond
    , application :: application
    }

mockContextNoDatabase :: (InitControllerContext application) => application -> ConfigBuilder -> IO (MockContext application)
mockContextNoDatabase application configBuilder = do
   frameworkConfig@(FrameworkConfig {dbPoolMaxConnections, dbPoolIdleTime, databaseUrl}) <- FrameworkConfig.buildFrameworkConfig configBuilder
   let databaseConnection = undefined
   logger <- newLogger def { level = Warn } -- don't log queries
   modelContext <- createModelContext dbPoolIdleTime dbPoolMaxConnections databaseUrl logger

   let sessionVault = Vault.insert sessionVaultKey mempty Vault.empty

   -- Apply middlewares to populate the vault with modelContext, frameworkConfig, and requestBody
   requestRef <- newIORef (error "Internal test error: Request should have been captured by middleware")
   let captureApp req _ = writeIORef requestRef req >> pure ResponseReceived
   let transformedApp = frameworkConfigMiddleware frameworkConfig
                      $ modelContextMiddleware modelContext
                      $ requestBodyMiddleware frameworkConfig.parseRequestBodyOptions captureApp
   _responseReceived <- transformedApp (defaultRequest {vault = sessionVault}) (\_ -> pure ResponseReceived)
   mockRequest <- readIORef requestRef

   let mockRespond = \resp -> pure ResponseReceived

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
    approotMiddleware <- Approot.envFallback
    let ihpWaiApp waiRequest waiRespond = do
            -- Override the request body with the provided params
            let requestWithBody = waiRequest { Wai.vault = Vault.insert requestBodyVaultKey (FormBody params []) waiRequest.vault }
            let ?request = requestWithBody
            let ?respond = waiRespond
            runActionWithNewContext controller

        allMiddlewares app = approotMiddleware app

    simpleResponse <- WaiTest.withSession (allMiddlewares ihpWaiApp) do
        WaiTest.request ?mocking.mockRequest

    pure $ Wai.responseLBS simpleResponse.simpleStatus simpleResponse.simpleHeaders simpleResponse.simpleBody

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
        currentRequest = ?mocking.mockRequest

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
