{-|
Module: Test.AutoRefreshSpec
Tests that AutoRefresh preserves query parameters when re-rendering
with a bare WebSocket request (no query params).
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.AutoRefreshSpec where
import Test.Hspec
import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.ControllerPrelude hiding (get, request)
import IHP.AutoRefresh.View
import Network.Wai
import Network.Wai.Internal (ResponseReceived(..))
import Network.HTTP.Types
import IHP.AutoRefresh (globalAutoRefreshServerVar, autoRefreshStateVaultKey, matchesInsertPayload, shouldRefreshForPayload, jsonValueMatchesText)
import IHP.AutoRefresh.Types
import IHP.QueryBuilder.Types (Condition(..), FilterOperator(..))
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Aeson.Key as AesonKey
import Data.Dynamic (toDyn)
import qualified Data.Set as Set
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
import qualified Data.UUID as UUID

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
    let baseRequest = ?request
            { Wai.queryString = map (\(k,v) -> (k, Just v)) queryParams
            , Wai.rawQueryString = renderSimpleQuery True queryParams
            }
    responseRef <- newIORef Nothing
    let captureRespond response = do
            writeIORef responseRef (Just response)
            pure ResponseReceived
    let controllerApp req respond = do
            let ?request = req
            let ?respond = respond
            runActionWithNewContext controller
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

    describe "matchesInsertPayload" do
        let mkRow pairs = AesonKeyMap.fromList [(AesonKey.fromText k, v) | (k, v) <- pairs]
        let textSnippet val = Snippet.param (val :: Text)
        let uuidSnippet val = Snippet.param val

        it "returns True for a matching EqOp condition" do
            let row = mkRow [("project_id", Aeson.String "abc-123")]
            let condition = ColumnCondition "tasks.project_id" EqOp (textSnippet "abc-123") Nothing Nothing
            matchesInsertPayload condition row `shouldBe` True

        it "returns False for a non-matching EqOp condition" do
            let row = mkRow [("project_id", Aeson.String "other-id")]
            let condition = ColumnCondition "tasks.project_id" EqOp (textSnippet "abc-123") Nothing Nothing
            matchesInsertPayload condition row `shouldBe` False

        it "handles UUID values" do
            let uuid = "a7a37bca-417b-21d5-38fc-7f9000efe79c" :: UUID.UUID
            let row = mkRow [("project_id", Aeson.String "a7a37bca-417b-21d5-38fc-7f9000efe79c")]
            let condition = ColumnCondition "tasks.project_id" EqOp (uuidSnippet uuid) Nothing Nothing
            matchesInsertPayload condition row `shouldBe` True

        it "returns False for AndCondition where one doesn't match" do
            let row = mkRow [("project_id", Aeson.String "abc"), ("status", Aeson.String "active")]
            let cond1 = ColumnCondition "tasks.project_id" EqOp (textSnippet "abc") Nothing Nothing
            let cond2 = ColumnCondition "tasks.status" EqOp (textSnippet "inactive") Nothing Nothing
            matchesInsertPayload (AndCondition cond1 cond2) row `shouldBe` False

        it "returns True for AndCondition where both match" do
            let row = mkRow [("project_id", Aeson.String "abc"), ("status", Aeson.String "active")]
            let cond1 = ColumnCondition "tasks.project_id" EqOp (textSnippet "abc") Nothing Nothing
            let cond2 = ColumnCondition "tasks.status" EqOp (textSnippet "active") Nothing Nothing
            matchesInsertPayload (AndCondition cond1 cond2) row `shouldBe` True

        it "returns True for OrCondition where one matches" do
            let row = mkRow [("status", Aeson.String "active")]
            let cond1 = ColumnCondition "tasks.status" EqOp (textSnippet "active") Nothing Nothing
            let cond2 = ColumnCondition "tasks.status" EqOp (textSnippet "inactive") Nothing Nothing
            matchesInsertPayload (OrCondition cond1 cond2) row `shouldBe` True

        it "returns True for unsupported operator (safe fallback)" do
            let row = mkRow [("name", Aeson.String "hello")]
            let condition = ColumnCondition "tasks.name" (LikeOp CaseSensitive) (textSnippet "%hello%") Nothing Nothing
            matchesInsertPayload condition row `shouldBe` True

        it "returns True when condition has applyLeft (e.g. LOWER)" do
            let row = mkRow [("name", Aeson.String "Hello")]
            let condition = ColumnCondition "tasks.name" EqOp (textSnippet "hello") (Just "LOWER") Nothing
            matchesInsertPayload condition row `shouldBe` True

        it "handles IS NULL condition" do
            let row = mkRow [("deleted_at", Aeson.Null)]
            let condition = ColumnCondition "tasks.deleted_at" IsOp (Snippet.sql "NULL") Nothing Nothing
            matchesInsertPayload condition row `shouldBe` True

        it "rejects IS NULL when value is not null" do
            let row = mkRow [("deleted_at", Aeson.String "2024-01-01")]
            let condition = ColumnCondition "tasks.deleted_at" IsOp (Snippet.sql "NULL") Nothing Nothing
            matchesInsertPayload condition row `shouldBe` False

    describe "shouldRefreshForPayload" do
        let mkInsertPayload row = AutoRefreshRowChangePayload AutoRefreshInsert Nothing (Just (Aeson.Object row)) Nothing
        let mkUpdatePayload rowId = AutoRefreshRowChangePayload AutoRefreshUpdate Nothing (Just (Aeson.Object (AesonKeyMap.fromList [(AesonKey.fromText "id", Aeson.String rowId)]))) Nothing
        let mkRow pairs = AesonKeyMap.fromList [(AesonKey.fromText k, v) | (k, v) <- pairs]
        let textSnippet val = Snippet.param (val :: Text)

        it "INSERT with no conditions tracked → refreshes" do
            let payload = mkInsertPayload (mkRow [("id", Aeson.String "1")])
            shouldRefreshForPayload (Set.fromList ["1"]) Nothing payload `shouldBe` True

        it "INSERT with matching condition → refreshes" do
            let row = mkRow [("project_id", Aeson.String "abc")]
            let condition = ColumnCondition "tasks.project_id" EqOp (textSnippet "abc") Nothing Nothing
            let payload = mkInsertPayload row
            shouldRefreshForPayload (Set.fromList []) (Just [Just (toDyn condition)]) payload `shouldBe` True

        it "INSERT with non-matching condition → does NOT refresh" do
            let row = mkRow [("project_id", Aeson.String "other")]
            let condition = ColumnCondition "tasks.project_id" EqOp (textSnippet "abc") Nothing Nothing
            let payload = mkInsertPayload row
            shouldRefreshForPayload (Set.fromList []) (Just [Just (toDyn condition)]) payload `shouldBe` False

        it "INSERT with unfiltered query (Nothing condition) → refreshes" do
            let row = mkRow [("project_id", Aeson.String "any")]
            let payload = mkInsertPayload row
            shouldRefreshForPayload (Set.fromList []) (Just [Nothing]) payload `shouldBe` True

        it "UPDATE with tracked ID → refreshes" do
            let payload = mkUpdatePayload "abc-123"
            shouldRefreshForPayload (Set.fromList ["abc-123"]) Nothing payload `shouldBe` True

        it "UPDATE with untracked ID → does NOT refresh" do
            let payload = mkUpdatePayload "other-id"
            shouldRefreshForPayload (Set.fromList ["abc-123"]) Nothing payload `shouldBe` False
