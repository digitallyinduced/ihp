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
import Network.Wai
import Network.Wai.Internal (ResponseReceived(..))
import Network.HTTP.Types
import IHP.AutoRefresh (getAvailableSessions, globalAutoRefreshServerVar)
import IHP.AutoRefresh.Types
import qualified Control.Concurrent.MVar as MVar
import IHP.Controller.Response (ResponseException(..))
import qualified Control.Exception as Exception
import qualified IHP.PGListener as PGListener
import IHP.Log.Types (Logger(..), LogLevel(..))
import IHP.Server (initMiddlewareStack)
import IHP.Test.Mocking
import qualified Network.Wai as Wai
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

tests :: Spec
tests = beforeAll (mockContextNoDatabase WebApplication config) do
    describe "AutoRefresh" do
        describe "getAvailableSessions" do
            let activeId = UUID.fromWords 0 0 0 1
            let staleId = UUID.fromWords 0 0 0 2
            let unknownId = UUID.fromWords 0 0 0 3
            let activeText = cs (UUID.toText activeId)
            forM_
                [ ("uses the header snapshot instead of stale cookie entries", [("X-IHP-Auto-Refresh-Sessions", activeText <> cs (UUID.toText unknownId))], [], [activeId, staleId], [activeId])
                , ("uses the websocket query snapshot instead of stale cookie entries", [], [("autoRefreshSessions", Just activeText)], [activeId, staleId], [activeId])
                , ("deduplicates explicit snapshots", [("X-IHP-Auto-Refresh-Sessions", activeText)], [("autoRefreshSessions", Just activeText)], [activeId, staleId], [activeId])
                , ("keeps the cookie fallback for clients without an explicit snapshot", [], [], [activeId, staleId], [activeId, staleId])
                , ("accepts a live session missing from an overwritten cookie", [], [("autoRefreshSessions", Just activeText)], [staleId], [activeId])
                , ("ignores malformed UTF-8 and invalid session IDs", [("X-IHP-Auto-Refresh-Sessions", "\xffinvalid")], [], [activeId, staleId], [])
                ] \(description, headers, queryParams, cookieIds, expected) ->
                    it description $ withContext do
                        event <- MVar.newEmptyMVar
                        now <- getCurrentTime
                        let session id = AutoRefreshSession
                                { id
                                , renderView = \_ _ -> pure ()
                                , event
                                , tables = mempty
                                , lastResponse = ""
                                , lastPing = now
                                }
                        serverRef <- newIORef AutoRefreshServer
                            { subscriptions = []
                            , sessions = map session [activeId, staleId]
                            , subscribedTables = mempty
                            , pgListener = error "pgListener unused in session selection test"
                            }
                        setSession "autoRefreshSessions" (mconcat (map UUID.toText cookieIds))
                        let clientRequest = ?request { requestHeaders = headers, queryString = queryParams }
                        let ?request = clientRequest
                        getAvailableSessions serverRef `shouldReturn` expected

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

        describe "graceful degradation without PGListener" do
            it "should run the action without crashing when PGListener is not available" $ withContext do
                MVar.modifyMVar_ globalAutoRefreshServerVar (\_ -> pure Nothing)

                response <- callActionWithParams ShowItemAction [("marketId", "degraded-ok")]
                body <- responseBody response
                cs body `shouldBe` ("degraded-ok" :: Text)

                -- Verify autoRefresh skipped subscription machinery entirely
                maybeServerRef <- MVar.readMVar globalAutoRefreshServerVar
                case maybeServerRef of
                    Nothing -> pure ()
                    Just _ -> expectationFailure "Expected globalAutoRefreshServerVar to be Nothing"
