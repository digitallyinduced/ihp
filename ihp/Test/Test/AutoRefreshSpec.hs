{-|
Module: Test.AutoRefreshSpec
Tests that AutoRefresh preserves query parameters when re-rendering
with a bare WebSocket request (no query params).
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.AutoRefreshSpec where
import qualified Data.Aeson as Aeson
import qualified Data.UUID as UUID
import Test.Hspec
import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.ControllerPrelude hiding (get, request)
import IHP.AutoRefresh.View
import Network.Wai
import Network.Wai.Internal (ResponseReceived(..))
import Network.HTTP.Types
import IHP.AutoRefresh (globalAutoRefreshServerVar, autoRefreshStateVaultKey)
import IHP.AutoRefresh.Types
import qualified Control.Concurrent.MVar as MVar
import IHP.Controller.Response (ResponseException(..))
import qualified Control.Exception as Exception
import qualified IHP.PGListener as PGListener
import IHP.Log.Types (Logger(..), LogLevel(..))
import IHP.Server (initMiddlewareStack)
import IHP.Test.Mocking
import qualified Network.Wai as Wai
import qualified Data.Vault.Lazy as Vault
import qualified Text.Blaze.Html.Renderer.Text as BlazeHtml

data WebApplication = WebApplication deriving (Eq, Show, Data)

data TestController
    = ShowItemAction
  deriving (Eq, Show, Data)

instance Controller TestController where
    action ShowItemAction = autoRefresh do
        let marketId = param @Text "marketId"
        renderPlain (cs marketId)

instance AutoRoute TestController

instance FrontController WebApplication where
  controllers = [ parseRoute @TestController ]

instance InitControllerContext WebApplication where
  initContext = pure ()

instance FrontController RootApplication where
    controllers = [ mountFrontController WebApplication ]

config :: ConfigBuilder
config = do
    option Development
    option (AppPort 8000)

-- | Helper that calls a controller action with query parameters (GET-style)
-- and passes a PGListener to the middleware stack so autoRefresh can work.
callActionWithQueryParams
    :: forall application controller
     . ( Controller controller
       , ContextParameters application
       , Typeable application
       , Typeable controller
       )
    => PGListener.PGListener
    -> controller
    -> [(ByteString, ByteString)]
    -> IO Response
callActionWithQueryParams pgListener controller queryParams = do
    let MockContext { frameworkConfig, modelContext } = ?mocking

    -- Build request with query params (GET-style, not POST body)
    let baseRequest = ?request
            { Wai.queryString = map (\(k,v) -> (k, Just v)) queryParams
            , Wai.rawQueryString = renderSimpleQuery True queryParams
            }

    -- Capture the response
    responseRef <- newIORef Nothing
    let captureRespond response = do
            writeIORef responseRef (Just response)
            pure ResponseReceived

    -- Create the controller app
    let controllerApp req respond = do
            let ?request = req
            let ?respond = respond
            runActionWithNewContext controller

    -- Run through middleware stack with PGListener enabled
    middlewareStack <- initMiddlewareStack frameworkConfig modelContext (Just pgListener)
    _ <- middlewareStack controllerApp baseRequest captureRespond

    readIORef responseRef >>= \case
        Just response -> pure response
        Nothing -> error "callActionWithQueryParams: No response was returned by the controller"

testLogger :: Logger
testLogger = Logger
    { write = \_ -> pure ()
    , level = Debug
    , formatter = \_ _ msg -> msg
    , timeCache = pure ""
    , cleanup = pure ()
    }

renderMeta :: (?context :: ControllerContext) => Text
renderMeta = cs (BlazeHtml.renderHtml autoRefreshMeta)

withFreshContextWithRequest :: Request -> (ControllerContext -> IO a) -> IO a
withFreshContextWithRequest request block = do
    let ?request = request
    context <- newControllerContext
    block context

withFreshContext :: (ControllerContext -> IO a) -> IO a
withFreshContext = withFreshContextWithRequest Wai.defaultRequest

tests :: Spec
tests = do
    beforeAll (mockContextNoDatabase WebApplication config) do
        describe "AutoRefresh" do
            describe "renderView" do
                it "should preserve query parameters when re-rendering with a websocket request" $ withContext do
                    -- Clean up any leftover global state from previous tests
                    MVar.modifyMVar_ globalAutoRefreshServerVar (\_ -> pure Nothing)

                    PGListener.withPGListener "" testLogger \pgListener -> do
                        -- 1. Call the action with query params — this triggers autoRefresh
                        --    which stores a session with renderView
                        response <- callActionWithQueryParams pgListener ShowItemAction [("marketId", "abc-123")]
                        body <- responseBody response
                        cs body `shouldBe` ("abc-123" :: Text)

                        -- 2. Extract the stored renderView from the AutoRefreshSession
                        maybeServerRef <- MVar.readMVar globalAutoRefreshServerVar
                        serverRef <- case maybeServerRef of
                            Just ref -> pure ref
                            Nothing -> error "AutoRefreshServer was not created"

                        server <- readIORef serverRef
                        session <- case server.sessions of
                            (s:_) -> pure s
                            [] -> error "No AutoRefresh sessions found"

                        -- 3. Call renderView with a bare request (simulating WebSocket re-render)
                        --    The WebSocket request has NO query params — this is the bug scenario
                        let bareRequest = defaultRequest
                        result <- Exception.try $ session.renderView bareRequest (\_ -> error "respond should not be called")
                        case result of
                            Left (ResponseException reResponse) -> do
                                reBody <- responseBody reResponse
                                -- If query params are NOT preserved, this would throw ParamNotFoundException
                                -- instead of reaching here with the correct value
                                cs reBody `shouldBe` ("abc-123" :: Text)
                            Right _ ->
                                expectationFailure "renderView should have thrown ResponseException"

                        -- Cleanup
                        MVar.modifyMVar_ globalAutoRefreshServerVar (\_ -> pure Nothing)

    describe "AutoRefresh meta tag" do
        it "renders nothing when disabled" do
            withFreshContext \context -> do
                frozen <- freeze context
                let ?context = frozen
                renderMeta `shouldBe` ""

        it "includes the session id when enabled" do
            let requestWithAutoRefresh = Wai.defaultRequest
                    { Wai.vault = Vault.insert autoRefreshStateVaultKey (AutoRefreshEnabled UUID.nil) Wai.defaultRequest.vault
                    }
            withFreshContextWithRequest requestWithAutoRefresh \context -> do
                frozen <- freeze context
                let ?context = frozen
                (cs renderMeta :: String) `shouldContain` "ihp-auto-refresh-id"

    describe "AutoRefresh change set" do
        it "stores row json and allows field access" do
            let userId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a001"
            let row = Aeson.object ["id" Aeson..= userId, "user_id" Aeson..= userId, "name" Aeson..= ("Riley" :: Text)]
            let payload = AutoRefreshRowChangePayload { payloadOperation = AutoRefreshUpdate, payloadOldRow = Nothing, payloadNewRow = Just row, payloadLargePayloadId = Nothing }
            let changeSet = insertRowChange "users" payload mempty
            let [change] = changesForTable "users" changeSet
            change.table `shouldBe` "users"
            rowFieldNew @"userId" change `shouldBe` Just userId

        it "exposes old/new fields" do
            let userId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a005"
            let oldRow = Aeson.object ["id" Aeson..= userId, "name" Aeson..= ("Old" :: Text)]
            let newRow = Aeson.object ["id" Aeson..= userId, "name" Aeson..= ("New" :: Text)]
            let payload = AutoRefreshRowChangePayload
                    { payloadOperation = AutoRefreshUpdate
                    , payloadOldRow = Just oldRow
                    , payloadNewRow = Just newRow
                    , payloadLargePayloadId = Nothing
                    }
            let changeSet = insertRowChangeFromPayload "users" payload mempty
            let [change] = changesForTable "users" changeSet
            rowFieldNew @"name" change `shouldBe` Just ("New" :: Text)
            rowFieldOld @"name" change `shouldBe` Just ("Old" :: Text)

        it "work with only old row" do
            let userId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a006"
            let oldRow = Aeson.object ["id" Aeson..= userId, "name" Aeson..= ("Deleted" :: Text)]
            let payload = AutoRefreshRowChangePayload
                    { payloadOperation = AutoRefreshDelete
                    , payloadOldRow = Just oldRow
                    , payloadNewRow = Nothing
                    , payloadLargePayloadId = Nothing
                    }
            let changeSet = insertRowChangeFromPayload "users" payload mempty
            let [change] = changesForTable "users" changeSet
            rowFieldNew @"name" change `shouldBe` (Nothing :: Maybe Text)
            rowFieldOld @"name" change `shouldBe` Just ("Deleted" :: Text)

        it "routes changes to the matching table slot" do
            let userId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a002"
            let projectId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a003"
            let userRow = Aeson.object ["id" Aeson..= userId, "user_id" Aeson..= userId]
            let projectRow = Aeson.object ["id" Aeson..= projectId, "user_id" Aeson..= userId]
            let userPayload = AutoRefreshRowChangePayload { payloadOperation = AutoRefreshInsert, payloadOldRow = Nothing, payloadNewRow = Just userRow, payloadLargePayloadId = Nothing }
            let projectPayload = AutoRefreshRowChangePayload { payloadOperation = AutoRefreshUpdate, payloadOldRow = Nothing, payloadNewRow = Just projectRow, payloadLargePayloadId = Nothing }
            let changeSet =
                    mempty
                        |> insertRowChange "projects" projectPayload
                        |> insertRowChange "users" userPayload
            length (changesForTable "projects" changeSet) `shouldBe` 1
            length (changesForTable "users" changeSet) `shouldBe` 1

        it "detects table changes" do
            let row = Aeson.object ["id" Aeson..= (1 :: Int)]
            let payload = AutoRefreshRowChangePayload { payloadOperation = AutoRefreshInsert, payloadOldRow = Nothing, payloadNewRow = Just row, payloadLargePayloadId = Nothing }
            let changeSet = insertRowChange "users" payload mempty
            anyChangeOnTable "users" changeSet `shouldBe` True
            anyChangeOnTable "projects" changeSet `shouldBe` False

        it "checks fields across all tables without table filtering" do
            let userId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a004"
            let userRow = Aeson.object ["id" Aeson..= userId, "user_id" Aeson..= userId]
            let projectRow = Aeson.object ["id" Aeson..= ("p-1" :: Text), "user_id" Aeson..= userId]
            let userPayload = AutoRefreshRowChangePayload { payloadOperation = AutoRefreshInsert, payloadOldRow = Nothing, payloadNewRow = Just userRow, payloadLargePayloadId = Nothing }
            let projectPayload = AutoRefreshRowChangePayload { payloadOperation = AutoRefreshUpdate, payloadOldRow = Nothing, payloadNewRow = Just projectRow, payloadLargePayloadId = Nothing }
            let changeSet =
                    mempty
                        |> insertRowChange "users" userPayload
                        |> insertRowChange "projects" projectPayload
            anyChangeWithField @"userId" (== userId) changeSet `shouldBe` True

        it "supports custom field predicates" do
            let userRow = Aeson.object ["id" Aeson..= ("u-1" :: Text), "status" Aeson..= ("archived" :: Text)]
            let payload = AutoRefreshRowChangePayload { payloadOperation = AutoRefreshUpdate, payloadOldRow = Nothing, payloadNewRow = Just userRow, payloadLargePayloadId = Nothing }
            let changeSet = insertRowChange "users" payload mempty
            anyChangeWithField @"status" (`elem` ["active" :: Text, "archived"]) changeSet `shouldBe` True
            anyChangeWithField @"status" (== ("active" :: Text)) changeSet `shouldBe` False
