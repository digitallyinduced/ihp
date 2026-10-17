{-|
Module: IDE.ToolServer.MiddlewareSpec
Tests for ToolServer middleware stack.

This test verifies that the ToolServer middleware stack correctly includes
requestBodyMiddleware, which is required for controllers to read form params.

The test uses the actual 'buildToolServerApplication' function from ToolServer,
so if any required middleware is accidentally removed, this test will fail.
-}
module IDE.ToolServer.MiddlewareSpec where

import IHP.Prelude
import Test.Hspec
import Network.Wai
import Network.Wai.Test
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as LBS

import IHP.IDE.ToolServer (withToolServerApplication, ToolServerApplicationWithConfig(..), toolServerSecurityMiddleware)
import IHP.IDE.ToolServer.Types
import qualified System.Environment as Env
import Network.Socket (PortNumber)
import qualified System.Directory as Directory
import qualified Data.Map as Map

-- | Create a new ToolServerApplication with empty IORefs
newToolServerApplication :: PortNumber -> IO ToolServerApplication
newToolServerApplication appPort = do
    appStandardOutput <- newIORef []
    appErrorOutput <- newIORef []
    databaseNeedsMigration <- newIORef False
    pure ToolServerApplication {..}

-- | Build the test application once for all tests
buildTestApp :: IO Application
buildTestApp = do
    Directory.createDirectoryIfMissing True "Config"
    Env.setEnv "IHP_STATIC" "."
    toolServerApp <- newToolServerApplication 8000
    liveReloadClients <- newIORef Map.empty
    withToolServerApplication toolServerApp 8000 liveReloadClients \weightedApp ->
        pure weightedApp.application

tests :: Spec
tests = beforeAll buildTestApp $ do
    describe "ToolServer security boundary" $ do
        it "rejects an unexpected Host before dispatch" $ \_ -> do
            dispatchCount <- newIORef (0 :: Int)
            let protectedApp = toolServerSecurityMiddleware 8000 (countingApp dispatchCount)
            response <- runSession (srequest (requestWith methodGet [(hHost, "attacker.example:8000")])) protectedApp
            simpleStatus response `shouldBe` status403
            readIORef dispatchCount `shouldReturn` 0

        it "rejects a cross-origin mutation before dispatch" $ \_ -> do
            dispatchCount <- newIORef (0 :: Int)
            let protectedApp = toolServerSecurityMiddleware 8000 (countingApp dispatchCount)
            response <- runSession (srequest (requestWith methodPost [("Origin", "http://attacker.example")])) protectedApp
            simpleStatus response `shouldBe` status403
            readIORef dispatchCount `shouldReturn` 0

        it "allows a legitimate local navigation" $ \_ -> do
            dispatchCount <- newIORef (0 :: Int)
            let protectedApp = toolServerSecurityMiddleware 8000 (countingApp dispatchCount)
            response <- runSession (srequest (requestWith methodGet [])) protectedApp
            simpleStatus response `shouldBe` status204
            readIORef dispatchCount `shouldReturn` 1

        it "allows a legitimate same-origin mutation" $ \_ -> do
            dispatchCount <- newIORef (0 :: Int)
            let protectedApp = toolServerSecurityMiddleware 8000 (countingApp dispatchCount)
            response <- runSession (srequest (requestWith methodPost [("Origin", "http://localhost:8000")])) protectedApp
            simpleStatus response `shouldBe` status204
            readIORef dispatchCount `shouldReturn` 1

    describe "ToolServer Middleware Stack" $ do
        it "includes requestBodyMiddleware so controllers can parse form params" $ \app -> do
            response <- runSession (postWithParams "/Migrations/CreateMigration" [("description", "test"), ("createOnly", "true")]) app
            let body = cs (simpleBody response) :: Text
            body `shouldNotSatisfy` ("lookupRequestVault" `isInfixOf`)
            body `shouldNotSatisfy` ("Could not find RequestBody" `isInfixOf`)

countingApp :: IORef Int -> Application
countingApp dispatchCount _ respond = do
    modifyIORef' dispatchCount (+ 1)
    respond (responseLBS status204 [] "")

requestWith :: Method -> RequestHeaders -> SRequest
requestWith method headers = SRequest (defaultRequest
    { requestHeaders = if any ((== hHost) . fst) headers then headers else (hHost, "localhost:8000") : headers
    , requestMethod = method
    , rawPathInfo = "/Generators"
    , pathInfo = ["Generators"]
    }) ""

postWithParams :: ByteString -> [(ByteString, ByteString)] -> Session SResponse
postWithParams path params = srequest $ SRequest req (LBS.fromStrict $ renderSimpleQuery False params)
  where
    req = defaultRequest
        { requestMethod = methodPost
        , pathInfo = filter (/= "") $ decodePathSegments path
        , rawPathInfo = path
        , requestHeaders =
            [ (hHost, "localhost:8000")
            , ("Origin", "http://localhost:8000")
            , (hContentType, "application/x-www-form-urlencoded")
            ]
        }
