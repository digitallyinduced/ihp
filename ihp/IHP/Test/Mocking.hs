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
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import           Network.Wai
import           Network.Wai.Internal                      (ResponseReceived (..))
import           Network.Wai.Parse                         (Param (..))

import           IHP.Controller.RequestContext             (RequestBody (..), RequestContext (..))
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
import qualified Control.Exception as Exception
import qualified Network.Wai.Middleware.Approot as Approot
import qualified Network.Wai.Test as WaiTest
import qualified System.Process as Process
import Paths_ihp (getDataFileName)

type ContextParameters application = (?context :: RequestContext, ?modelContext :: ModelContext, ?application :: application, InitControllerContext application, ?mocking :: MockContext application)

data MockContext application = InitControllerContext application => MockContext
    { modelContext :: ModelContext
    , requestContext :: RequestContext
    , application :: application
    }

-- | Create contexts that can be used for mocking. Sets up a temporary database,
-- imports IHPSchema.sql and Application/Schema.sql, and passes a MockContext to the callback.
withIHPApp :: (InitControllerContext application) => application -> ConfigBuilder -> (MockContext application -> IO ()) -> IO ()
withIHPApp application configBuilder hspecAction = do
    FrameworkConfig.withFrameworkConfig configBuilder \frameworkConfig -> do
        let FrameworkConfig { dbPoolMaxConnections, dbPoolIdleTime, databaseUrl } = frameworkConfig

        logger <- newLogger def { level = Warn } -- don't log queries

        withTestDatabase databaseUrl \testDatabaseUrl -> do
            modelContext <- createModelContext dbPoolIdleTime dbPoolMaxConnections testDatabaseUrl logger

            let sessionVault = Vault.insert sessionVaultKey mempty Vault.empty

            let requestContext = RequestContext
                 { request = defaultRequest {vault = sessionVault}
                 , requestBody = FormBody [] []
                 , respond = const (pure ResponseReceived)
                 , frameworkConfig = frameworkConfig }

            hspecAction MockContext { .. }

mockContextNoDatabase :: (InitControllerContext application) => application -> ConfigBuilder -> IO (MockContext application)
mockContextNoDatabase application configBuilder = do
   frameworkConfig@(FrameworkConfig {dbPoolMaxConnections, dbPoolIdleTime, databaseUrl}) <- FrameworkConfig.buildFrameworkConfig configBuilder
   logger <- newLogger def { level = Warn } -- don't log queries
   modelContext <- createModelContext dbPoolIdleTime dbPoolMaxConnections databaseUrl logger

   let sessionVault = Vault.insert sessionVaultKey mempty Vault.empty

   let requestContext = RequestContext
         { request = defaultRequest {vault = sessionVault}
         , requestBody = FormBody [] []
         , respond = \resp -> pure ResponseReceived
         , frameworkConfig = frameworkConfig }

   pure MockContext{..}

-- | Run a IO action, setting implicit params based on supplied mock context
withContext :: (ContextParameters application => IO a) -> MockContext application -> IO a
withContext action mocking@MockContext{..} = let
    ?modelContext = modelContext
    ?context = requestContext
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
    let ihpWaiApp request respond = do
            let requestContextWithOverridenRespond = ?context { respond, request, requestBody = FormBody params [] }
            let ?context = requestContextWithOverridenRespond
            runActionWithNewContext controller

        allMiddlewares app = approotMiddleware app

    simpleResponse <- WaiTest.withSession (allMiddlewares ihpWaiApp) do
        WaiTest.request ?context.request

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
    let frameworkConfig = ?context.frameworkConfig
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
    , ?context :: RequestContext
    , Serialize.Serialize userId
    , HasField "id" user userId
    , KnownSymbol (GetModelName user)
    ) => user -> ((?context :: RequestContext) => IO result) -> IO result
withUser user callback =
        let ?context = newContext
        in callback
    where
        newContext = ?context { request = newRequest }
        newRequest = request { Wai.vault = newVault }

        newSession :: Network.Wai.Session.Session IO ByteString ByteString
        newSession = (lookupSession, insertSession)

        lookupSession key = if key == sessionKey
            then pure (Just sessionValue)
            else pure Nothing

        insertSession key value = pure ()

        newVault = Vault.insert sessionVaultKey newSession (Wai.vault request)
        RequestContext { request } = ?mocking.requestContext

        sessionValue = Serialize.encode (user.id)
        sessionKey = cs (Session.sessionKey @user)

withConnection databaseUrl = Exception.bracket (PG.connectPostgreSQL databaseUrl) PG.close

withTestDatabase masterDatabaseUrl callback = do
    testDatabaseName <- randomDatabaseName

    withConnection masterDatabaseUrl \masterConnection ->
        Exception.bracket_
            (PG.execute masterConnection "CREATE DATABASE ?" [PG.Identifier testDatabaseName])
            (
                -- The WITH FORCE is required to force close open connections
                -- Otherwise the DROP DATABASE takes a few seconds to execute
                PG.execute masterConnection "DROP DATABASE ? WITH (FORCE)" [PG.Identifier testDatabaseName]
            )
            (do
                importSchema (testDatabaseUrl masterDatabaseUrl testDatabaseName)
                callback (testDatabaseUrl masterDatabaseUrl testDatabaseName)
            )

testDatabaseUrl :: ByteString -> Text -> ByteString
testDatabaseUrl masterDatabaseUrl testDatabaseName =
    masterDatabaseUrl
        |> cs
        |> Text.replace "postgresql:///app" ("postgresql:///" <> testDatabaseName)
        |> cs

randomDatabaseName :: IO Text
randomDatabaseName = do
    databaseId <- UUID.nextRandom
    pure ("test-" <> UUID.toText databaseId)

importSchema :: ByteString -> IO ()
importSchema databaseUrl = do
    -- We use the system psql to handle the initial Schema Import as it can handle
    -- complex Schema including variations in formatting, custom types, functions, and table definitions.
    let importSql file = Process.callCommand ("psql " <> (cs databaseUrl) <> " < " <> file)

    ihpSchemaSql <- findIHPSchemaSql
    importSql ihpSchemaSql
    importSql "Application/Schema.sql"

findIHPSchemaSql :: IO FilePath
findIHPSchemaSql = getDataFileName "IHPSchema.sql"

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
