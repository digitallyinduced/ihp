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
import Network.HTTP.Types
import IHP.AutoRefresh (getAvailableSessions, globalAutoRefreshServerVar, sessionResponseHasChanged, updateSession)
import IHP.AutoRefresh.Types
import IHP.AutoRefresh.View (autoRefreshMeta)
import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified IHP.PGListener as PGListener
import System.Log.FastLogger (FastLogger)
import IHP.Server (initMiddlewareStack)
import Network.Wai.Test (runSession, request, SResponse(..), simpleBody)
import IHP.Test.Mocking
import qualified Data.UUID as UUID
import qualified Network.Wai as Wai

data WebApplication = WebApplication deriving (Eq, Show, Data)

data TestController
    = ShowItemAction
    | ShowItemHtmlAction
  deriving (Eq, Show, Data)

instance Controller TestController where
    action ShowItemAction = autoRefresh do
        let marketId = param @Text "marketId"
        renderPlain (cs marketId)
    action ShowItemHtmlAction = autoRefresh do
        let marketId = param @Text "marketId"
        let meta = autoRefreshMeta
        respondHtml [hsx|<html><head>{meta}</head><body>{marketId}</body></html>|]

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
    -> IO SResponse
callActionWithQueryParams pgListener controller queryParams = do
    let MockContext { frameworkConfig, modelContext } = ?mocking

    let baseRequest = ?request
            { Wai.queryString = map (\(k,v) -> (k, Just v)) queryParams
            , Wai.rawQueryString = renderSimpleQuery True queryParams
            }

    let controllerApp req respond = do
            let ?request = req
            let ?respond = respond
            runActionWithNewContext controller

    middlewareStack <- initMiddlewareStack frameworkConfig modelContext (Just pgListener)
    runSession (request baseRequest) (middlewareStack controllerApp)

testLogger :: FastLogger
testLogger = noopLogger

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
                                , renderView = \_ respond -> respond (Wai.responseLBS status200 [] "")
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

        describe "autoRefreshMeta" do
            it "renders the ihp-auto-refresh-id meta tag on the initial response" $ withContext do
                MVar.modifyMVar_ globalAutoRefreshServerVar (\_ -> pure Nothing)

                PGListener.withPGListener "" testLogger \pgListener -> do
                    response <- callActionWithQueryParams pgListener ShowItemHtmlAction [("marketId", "abc-123")]
                    let bodyBs = LBS.toStrict (simpleBody response)
                    BS.isInfixOf "ihp-auto-refresh-id" bodyBs `shouldBe` True

                    MVar.modifyMVar_ globalAutoRefreshServerVar (\_ -> pure Nothing)

        describe "renderView" do
            it "should preserve query parameters when re-rendering with a websocket request" $ withContext do
                -- Clean up any leftover global state from previous tests
                MVar.modifyMVar_ globalAutoRefreshServerVar (\_ -> pure Nothing)

                PGListener.withPGListener "" testLogger \pgListener -> do
                    -- 1. Call the action with query params — this triggers autoRefresh
                    --    which stores a session with renderView
                    response <- callActionWithQueryParams pgListener ShowItemAction [("marketId", "abc-123")]
                    cs (simpleBody response) `shouldBe` ("abc-123" :: Text)

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
                    reResponse <- runSession (request defaultRequest) session.renderView
                    -- If query params are NOT preserved, this would throw ParamNotFoundException
                    cs (simpleBody reResponse) `shouldBe` ("abc-123" :: Text)

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

        describe "session state tracking" do
            it "should compare re-rendered html against the latest session response" $ withContext do
                event <- MVar.newEmptyMVar
                now <- getCurrentTime
                let session =
                        AutoRefreshSession
                            { id = UUID.nil
                            , renderView = \_ respond -> respond (Wai.responseLBS status200 [] "")
                            , event
                            , tables = mempty
                            , lastResponse = "resolved"
                            , lastPing = now
                            }
                serverRef <-
                    newIORef
                        AutoRefreshServer
                            { subscriptions = []
                            , sessions = [session]
                            , subscribedTables = mempty
                            , pgListener = error "pgListener unused in session state test"
                            }

                updateSession serverRef UUID.nil (\currentSession -> currentSession { lastResponse = "unresolved" })

                sessionResponseHasChanged serverRef UUID.nil "resolved" `shouldReturn` True
                sessionResponseHasChanged serverRef UUID.nil "unresolved" `shouldReturn` False
