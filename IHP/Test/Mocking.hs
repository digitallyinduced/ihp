{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ExistentialQuantification   #-}

module IHP.Test.Mocking where

import           Data.ByteString.Builder                   (toLazyByteString)
import qualified Data.ByteString.Lazy                      as LBS
import qualified Data.Vault.Lazy                           as Vault
import           Database.PostgreSQL.Simple                (connectPostgreSQL)
import           Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Status                 as HTTP
import           Network.Wai
import           Network.Wai.Internal                      (ResponseReceived (..))
import           Network.Wai.Parse                         (Param (..))

import qualified IHP.ApplicationContext                    as ApplicationContext
import           IHP.ApplicationContext                    (ApplicationContext (..))
import qualified IHP.AutoRefresh.Types                     as AutoRefresh
import qualified IHP.Controller.Context                    as Context
import           IHP.Controller.RequestContext             (RequestBody (..), RequestContext (..))
import           IHP.ControllerSupport                     (InitControllerContext, Controller, runActionWithNewContext)
import           IHP.FrameworkConfig                       (ConfigBuilder (..), FrameworkConfig (..))
import qualified IHP.FrameworkConfig                       as FrameworkConfig
import           IHP.ModelSupport                          (createModelContext)
import           IHP.Prelude
import           IHP.Log.Types

type ContextParameters application = (?applicationContext :: ApplicationContext, ?context :: RequestContext, ?modelContext :: ModelContext, ?application :: application, InitControllerContext application, ?mocking :: MockContext application)

data MockContext application = InitControllerContext application => MockContext {
    modelContext :: ModelContext
  , requestContext :: RequestContext
  , applicationContext :: ApplicationContext
  , application :: application
  }

-- | Create contexts that can be used for mocking
mockContext :: (InitControllerContext application) => application -> ConfigBuilder -> IO (MockContext application)
mockContext application configBuilder = do
   frameworkConfig@(FrameworkConfig {dbPoolMaxConnections, dbPoolIdleTime, databaseUrl}) <- FrameworkConfig.buildFrameworkConfig configBuilder
   databaseConnection <- connectPostgreSQL databaseUrl
   logger <- newLogger def { level = Warn } -- don't log queries
   modelContext <- createModelContext dbPoolIdleTime dbPoolMaxConnections databaseUrl logger

   autoRefreshServer <- newIORef AutoRefresh.newAutoRefreshServer
   session <- Vault.newKey
   let sessionVault = Vault.insert session mempty Vault.empty
   let applicationContext = ApplicationContext { modelContext = modelContext, session, autoRefreshServer, frameworkConfig }

   let requestContext = RequestContext
         { request = defaultRequest {vault = sessionVault}
         , requestBody = FormBody [] []
         , respond = \resp -> pure ResponseReceived
         , vault = session
         , frameworkConfig = frameworkConfig }

   pure MockContext{..}

mockContextNoDatabase :: (InitControllerContext application) => application -> ConfigBuilder -> IO (MockContext application)
mockContextNoDatabase application configBuilder = do
   frameworkConfig@(FrameworkConfig {dbPoolMaxConnections, dbPoolIdleTime, databaseUrl}) <- FrameworkConfig.buildFrameworkConfig configBuilder
   let databaseConnection = undefined
   logger <- newLogger def { level = Warn } -- don't log queries
   modelContext <- createModelContext dbPoolIdleTime dbPoolMaxConnections databaseUrl logger

   autoRefreshServer <- newIORef AutoRefresh.newAutoRefreshServer
   session <- Vault.newKey
   let sessionVault = Vault.insert session mempty Vault.empty
   let applicationContext = ApplicationContext { modelContext = modelContext, session, autoRefreshServer, frameworkConfig }

   let requestContext = RequestContext
         { request = defaultRequest {vault = sessionVault}
         , requestBody = FormBody [] []
         , respond = \resp -> pure ResponseReceived
         , vault = session
         , frameworkConfig = frameworkConfig }

   pure MockContext{..}

-- | Run a IO action, setting implicit params based on supplied mock context
withContext :: (ContextParameters application => IO a) -> MockContext application -> IO a
withContext action mocking@MockContext{..} = let
    ?modelContext = modelContext
    ?context = requestContext
    ?applicationContext = applicationContext
    ?application = application
    ?mocking = mocking
  in do
    action

setupWithContext :: (ContextParameters application => IO a) -> MockContext application -> IO (MockContext application)
setupWithContext action context = withContext action context >> pure context

-- | Runs a controller action in a mock environment
mockAction :: forall application controller. (Controller controller, ContextParameters application, Typeable application, Typeable controller) => controller -> IO Response
mockAction controller = do
    responseRef <- newIORef Nothing
    let oldRespond = ?context |> respond
    let customRespond response = do
            writeIORef responseRef (Just response)
            oldRespond response
    let requestContextWithOverridenRespond = ?context { respond = customRespond }
    let ?requestContext = requestContextWithOverridenRespond
    runActionWithNewContext controller
    maybeResponse <- readIORef responseRef
    case maybeResponse of
        Just response -> pure response
        Nothing -> error "mockAction: The action did not render a response"

-- | Get contents of response
mockActionResponse :: forall application controller. (Controller controller, ContextParameters application, Typeable application, Typeable controller) => controller -> IO LBS.ByteString
mockActionResponse = (responseBody =<<) . mockAction

-- | Get HTTP status of the controller
mockActionStatus :: forall application controller. (Controller controller, ContextParameters application, Typeable application, Typeable controller) => controller -> IO HTTP.Status
mockActionStatus = fmap responseStatus . mockAction

-- | Add params to the request context, run the action
withParams :: [Param] -> (ContextParameters application => IO a) -> MockContext application -> IO a
withParams ps action context = withContext action context'
  where
    context' = context{requestContext=(requestContext context){requestBody=FormBody ps []}}

headers :: IO Response -> IO ResponseHeaders
headers = fmap responseHeaders

responseBody :: Response -> IO LBS.ByteString
responseBody res =
  let (status,headers,body) = responseToStream res in
  body $ \f -> do
    content <- newIORef mempty
    f (\chunk -> modifyIORef' content (<> chunk)) (return ())
    toLazyByteString <$> readIORef content
