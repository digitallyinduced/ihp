{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ExistentialQuantification   #-}

module IHP.Test.Mocking where

import           Data.ByteString.Builder                   (toLazyByteString)
import qualified Data.ByteString.Lazy                      as LBS
import qualified Data.Vault.Lazy                           as Vault
import qualified Network.HTTP.Types                        as HTTP
import           Network.Wai
import           Network.Wai.Internal                      (ResponseReceived (..))
import           Network.Wai.Parse                         (Param (..))

import           Wai.Request.Params.Middleware                 (Respond)
import           IHP.ControllerSupport                     (InitControllerContext, Controller, runActionWithNewContext)
import           IHP.FrameworkConfig                       (ConfigBuilder (..), FrameworkConfig (..), RootApplication (..))
import qualified IHP.FrameworkConfig                       as FrameworkConfig
import           IHP.ModelSupport                          (createModelContext, withModelContext, Id', PrimaryKey, GetTableName, unpackId)
import           IHP.Prelude
import           IHP.Log.Types
import           IHP.Job.Types
import qualified Network.Wai as Wai
import IHP.LoginSupport.Types (CurrentUserRecord, currentUserVaultKey, currentUserIdVaultKey)
import IHP.Server (initMiddlewareStack)
import qualified IHP.Server as Server
import IHP.Controller.NotFound (handleNotFound)
import IHP.RouterSupport (FrontController)
import qualified IHP.PGListener as PGListener
import qualified IHP.ErrorController as ErrorController
import System.IO.Unsafe (unsafePerformIO)

type ContextParameters application = (?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext, ?application :: application, InitControllerContext application, ?mocking :: MockContext application)

data MockContext application = InitControllerContext application => MockContext
    { modelContext :: ModelContext
    , frameworkConfig :: FrameworkConfig
    , mockRequest :: Request
    , mockRespond :: Respond
    , application :: application
    , pgListener :: Maybe PGListener.PGListener
    }

-- | Run a request through the test middleware stack.
-- This applies the same middlewares that IHP.Server uses.
-- Used for initial setup only - actual request params are handled in callActionWithParams.
runTestMiddlewares :: FrameworkConfig -> ModelContext -> Maybe PGListener.PGListener -> Request -> IO Request
runTestMiddlewares frameworkConfig modelContext maybePgListener baseRequest = do
    -- Capture the modified request after running through middlewares
    resultRef <- newIORef baseRequest
    let captureApp req respond = do
            writeIORef resultRef req
            respond (responseLBS HTTP.status200 [] "")

    middlewareStack <- initMiddlewareStack frameworkConfig modelContext maybePgListener

    -- Run request through middleware stack
    _ <- middlewareStack captureApp baseRequest (\_ -> pure ResponseReceived)

    readIORef resultRef

{-# DEPRECATED mockContextNoDatabase "Use withMockContext instead for bracket-style resource management" #-}
mockContextNoDatabase :: (InitControllerContext application) => application -> ConfigBuilder -> IO (MockContext application)
mockContextNoDatabase application configBuilder = do
   frameworkConfig@(FrameworkConfig {databaseUrl}) <- FrameworkConfig.buildFrameworkConfig configBuilder
   logger <- newLogger (def :: LoggerSettings) { level = Warn } -- don't log queries
   modelContext <- createModelContext databaseUrl logger

   -- Start with a minimal request - the middleware stack will set up session, etc.
   let baseRequest = defaultRequest
   let pgListener = Nothing
   mockRequest <- runTestMiddlewares frameworkConfig modelContext pgListener baseRequest
   let mockRespond = const (pure ResponseReceived)

   pure MockContext{..}

-- | Bracket-style mock context creation with proper resource cleanup.
--
-- Uses 'withModelContext' to ensure the database pool is released when done.
-- Prefer this over 'mockContextNoDatabase'.
--
-- __Example:__ Use with hspec's 'aroundAll':
--
-- > tests :: Spec
-- > tests = aroundAll (withMockContext WebApplication config) do
-- >     it "should work" $ withContext do
-- >         ...
--
withMockContext :: (InitControllerContext application) => application -> ConfigBuilder -> (MockContext application -> IO a) -> IO a
withMockContext application configBuilder action =
    FrameworkConfig.withFrameworkConfig configBuilder \frameworkConfig -> do
        withModelContext frameworkConfig.databaseUrl frameworkConfig.logger \modelContext -> do
            PGListener.withPGListener frameworkConfig.databaseUrl frameworkConfig.logger \pgListener' -> do
                let baseRequest = defaultRequest
                let pgListener = Just pgListener'
                mockRequest <- runTestMiddlewares frameworkConfig modelContext pgListener baseRequest
                let mockRespond = const (pure ResponseReceived)
                action MockContext{..}

-- | Build a WAI 'Application' from a 'MockContext' for use with @runSession@.
--
-- This mirrors the middleware stack from 'IHP.Server.run':
-- errorHandlerMiddleware wraps the app to catch exceptions and render error pages.
-- EarlyReturnException is caught inside runAction/runActionWithNewContext.
initTestApplication :: (FrontController RootApplication) => MockContext application -> IO Application
initTestApplication MockContext { frameworkConfig, modelContext, pgListener } = do
    middleware <- initMiddlewareStack frameworkConfig modelContext pgListener
    pure $ ErrorController.errorHandlerMiddleware frameworkConfig
         $ middleware
         $ Server.application handleNotFound (\app -> app)

-- | Combines 'withMockContext' and 'initTestApplication' into a single bracket.
--
-- __Example:__ Use with hspec's 'aroundAll':
--
-- > tests :: Spec
-- > tests = aroundAll (withMockContextAndApp WebApplication config) do
-- >     it "should work" $ withContextAndApp \application -> do
-- >         runSession (testGet "/foo") application >>= assertSuccess "bar"
--
withMockContextAndApp :: (InitControllerContext application, FrontController RootApplication) => application -> ConfigBuilder -> ((MockContext application, Application) -> IO a) -> IO a
withMockContextAndApp application configBuilder action =
    withMockContext application configBuilder \ctx -> do
        app <- initTestApplication ctx
        action (ctx, app)

-- | Like 'withContext' but for specs using 'withMockContextAndApp'.
-- The WAI 'Application' is passed to the callback.
withContextAndApp :: (ContextParameters application => Application -> IO a) -> (MockContext application, Application) -> IO a
withContextAndApp action (ctx, app) = withContext (action app) ctx

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
    let MockContext { frameworkConfig, modelContext } = ?mocking

    -- Build request with real form body (let middleware parse it)
    requestBody <- newIORef (HTTP.renderSimpleQuery False params)
    let readBody = atomicModifyIORef requestBody (\body -> ("", body))
    let baseRequest = (Wai.setRequestBodyChunks readBody ?request)
            { Wai.requestMethod = "POST"
            , Wai.requestHeaders = (HTTP.hContentType, "application/x-www-form-urlencoded") : filter ((/= HTTP.hContentType) . fst) (Wai.requestHeaders ?request)
            }

    -- Capture the response
    responseRef <- newIORef Nothing
    let captureRespond response = do
            writeIORef responseRef (Just response)
            pure ResponseReceived

    -- Extract any override middleware stashed in the request vault by helpers
    -- like 'withUser'. We wrap it *innermost* around the controller so it runs
    -- after the full production stack (including 'sessionMiddleware' and
    -- 'authMw'). That makes it last-write-wins on vault keys like
    -- 'currentUserVaultKey', so test helpers can seed a mock user without
    -- fighting 'sessionMiddleware' (from wai-session-maybe), which
    -- unconditionally rewrites 'sessionVaultKey' from the request cookie.
    let overrideMiddleware = fromMaybe id (Vault.lookup mockOverrideVaultKey (Wai.vault ?request))

    -- Create the controller app
    let controllerApp req respond = do
            let ?request = req
            let ?respond = respond
            runActionWithNewContext controller

    -- Run through middleware stack (like the real server does).
    let MockContext { pgListener } = ?mocking
    middlewareStack <- initMiddlewareStack frameworkConfig modelContext pgListener
    _ <- middlewareStack (overrideMiddleware controllerApp) baseRequest captureRespond

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

-- | Vault key holding a 'Wai.Middleware' that test helpers like 'withUser'
-- use to override request vault entries (e.g. 'currentUserVaultKey') *after*
-- the production middleware stack has run. 'callActionWithParams' reads this
-- key and wraps the controller innermost, so the override is guaranteed to
-- be the last writer — sidestepping 'sessionMiddleware'/'authMw', which
-- would otherwise clobber the mock state.
{-# NOINLINE mockOverrideVaultKey #-}
mockOverrideVaultKey :: Vault.Key Wai.Middleware
mockOverrideVaultKey = unsafePerformIO Vault.newKey

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
-- created user during the execution of CreatePostAction.
--
-- Internally this composes a 'Wai.Middleware' into 'mockOverrideVaultKey'
-- that seeds 'currentUserVaultKey' and 'currentUserIdVaultKey' with the
-- mock user. 'callActionWithParams' applies that middleware *innermost*,
-- so it runs after 'sessionMiddleware' and 'authMw' and wins on conflict.
--
withUser :: forall user result.
    ( ?request :: Request
    , ?respond :: Respond
    , user ~ CurrentUserRecord
    , HasField "id" user (Id' (GetTableName user))
    , PrimaryKey (GetTableName user) ~ UUID
    ) => user -> ((?request :: Request, ?respond :: Respond) => IO result) -> IO result
withUser user callback =
        let ?request = newRequest
        in callback
    where
        currentRequest = ?request

        existingOverride :: Wai.Middleware
        existingOverride = fromMaybe id (Vault.lookup mockOverrideVaultKey (Wai.vault currentRequest))

        userMw :: Wai.Middleware
        userMw app req respond =
            let req' = req
                    { Wai.vault
                        = Vault.insert currentUserVaultKey (Just user)
                        . Vault.insert currentUserIdVaultKey (Just (unpackId user.id))
                        $ Wai.vault req
                    }
            in app req' respond

        newVault = Vault.insert mockOverrideVaultKey (existingOverride . userMw) (Wai.vault currentRequest)
        newRequest = currentRequest { Wai.vault = newVault }

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
