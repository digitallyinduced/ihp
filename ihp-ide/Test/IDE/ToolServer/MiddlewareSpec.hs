{-|
Module: Test.IDE.ToolServer.MiddlewareSpec
Tests for ToolServer middleware stack.

This test verifies that the ToolServer middleware stack correctly includes
requestBodyMiddleware, which is required for controllers to read form params.

The test uses the actual 'buildToolServerApplication' function from ToolServer,
so if any required middleware is accidentally removed, this test will fail.
-}
module Test.IDE.ToolServer.MiddlewareSpec where

import IHP.Prelude
import Test.Hspec
import Network.Wai
import Network.Wai.Test
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as LBS
import Data.Text (isInfixOf)

import IHP.IDE.ToolServer (withToolServerApplication, ToolServerApplicationWithConfig(..))
import IHP.IDE.ToolServer.Types
import qualified System.Environment as Env
import Network.Socket (PortNumber)
import qualified System.Directory as Directory
import qualified Data.Map as Map

-- | Create a new ToolServerApplication with empty IORefs
newToolServerApplication :: PortNumber -> IO ToolServerApplication
newToolServerApplication appPort = do
    postgresStandardOutput <- newIORef mempty
    postgresErrorOutput <- newIORef mempty
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
    describe "ToolServer Middleware Stack" $ do
        it "includes requestBodyMiddleware so controllers can parse form params" $ \app -> do
            response <- runSession (postWithParams "/Migrations/CreateMigration" [("description", "test"), ("createOnly", "true")]) app
            let body = cs (simpleBody response) :: Text
            body `shouldNotSatisfy` ("lookupRequestVault" `isInfixOf`)
            body `shouldNotSatisfy` ("Could not find RequestBody" `isInfixOf`)

postWithParams :: ByteString -> [(ByteString, ByteString)] -> Session SResponse
postWithParams path params = srequest $ SRequest req (LBS.fromStrict $ renderSimpleQuery False params)
  where
    req = defaultRequest
        { requestMethod = methodPost
        , pathInfo = filter (/= "") $ decodePathSegments path
        , rawPathInfo = path
        , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
        }
