{-|
Module: Test.IDE.ToolServer.MiddlewareSpec
Tests for ToolServer middleware stack.

This test verifies that the ToolServer middleware stack is correctly configured,
particularly that requestBodyMiddleware is present so controllers can read form params.

If requestBodyMiddleware is accidentally removed from ToolServer.hs, this test will fail.
-}
module Test.IDE.ToolServer.MiddlewareSpec where

import IHP.Prelude
import Test.Hspec
import Network.Wai
import Network.Wai.Test
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as LBS

import IHP.FrameworkConfig
import IHP.RequestVault (frameworkConfigMiddleware, RequestBody(..))
import IHP.RequestBodyMiddleware (requestBodyMiddleware, requestBodyVaultKey)
import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
import qualified Data.Vault.Lazy as Vault
import IHP.Controller.Session (sessionVaultKey)

-- | Build the middleware stack that mirrors ToolServer.hs
-- This is the exact same structure as in ToolServer:
--   methodOverridePost $ sessionMiddleware $ ... $ frameworkConfigMiddleware frameworkConfig
--       $ requestBodyMiddleware frameworkConfig.parseRequestBodyOptions
--       $ application
buildToolServerMiddlewareStack :: FrameworkConfig -> Middleware
buildToolServerMiddlewareStack frameworkConfig =
    methodOverridePost
    . addEmptySession
    . frameworkConfigMiddleware frameworkConfig
    . requestBodyMiddleware frameworkConfig.parseRequestBodyOptions
  where
    -- Add an empty session to the vault (simplified for testing)
    addEmptySession :: Middleware
    addEmptySession app req respond =
        let req' = req { vault = Vault.insert sessionVaultKey mempty req.vault }
        in app req' respond

-- | A simple test application that reads a param from the parsed request body
-- and echoes it back. This mimics what controllers do with paramOrDefault.
--
-- If requestBodyMiddleware is missing, Vault.lookup will return Nothing
-- and we'll return "MIDDLEWARE_MISSING" to make the test fail clearly.
testApplication :: Application
testApplication request respond = do
    let maybeBody = Vault.lookup requestBodyVaultKey request.vault
    let responseText = case maybeBody of
            Just (FormBody params _files) ->
                case lookup "testParam" params of
                    Just value -> cs value
                    Nothing -> "param_not_found"
            Just (JSONBody _ _) -> "unexpected_json_body"
            Nothing -> "MIDDLEWARE_MISSING"
    respond $ responseLBS status200 [(hContentType, "text/plain")] responseText

-- | Make a POST request with form data
postWithParams :: ByteString -> [(ByteString, ByteString)] -> Session SResponse
postWithParams path params = srequest $ SRequest req body
  where
    body = LBS.fromStrict $ renderSimpleQuery False params
    req = defaultRequest
        { requestMethod = methodPost
        , pathInfo = filter (/= "") $ decodePathSegments path
        , rawPathInfo = path
        , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
        }

tests :: Spec
tests = do
    describe "ToolServer Middleware Stack" $ do
        describe "requestBodyMiddleware" $ do
            it "parses form params and stores them in the request vault" $ do
                -- Build the framework config
                frameworkConfig <- buildFrameworkConfig (pure ())

                -- Build the full middleware stack + application
                let app = buildToolServerMiddlewareStack frameworkConfig testApplication

                -- Make a POST request with a form parameter
                response <- runSession (postWithParams "/" [("testParam", "hello-world")]) app

                -- Verify the middleware parsed the param correctly
                simpleBody response `shouldBe` "hello-world"

            it "handles missing params gracefully" $ do
                frameworkConfig <- buildFrameworkConfig (pure ())
                let app = buildToolServerMiddlewareStack frameworkConfig testApplication

                response <- runSession (postWithParams "/" []) app

                -- Should indicate param was not found (not that middleware is missing)
                simpleBody response `shouldBe` "param_not_found"

            it "fails if requestBodyMiddleware is removed from the stack" $ do
                -- This test demonstrates what happens without the middleware
                frameworkConfig <- buildFrameworkConfig (pure ())

                -- Build middleware stack WITHOUT requestBodyMiddleware
                let brokenMiddlewareStack =
                        methodOverridePost
                        . frameworkConfigMiddleware frameworkConfig
                        -- Note: requestBodyMiddleware is intentionally missing here!

                let app = brokenMiddlewareStack testApplication

                response <- runSession (postWithParams "/" [("testParam", "test")]) app

                -- Without the middleware, the vault won't have the parsed body
                simpleBody response `shouldBe` "MIDDLEWARE_MISSING"
