module Test.ServerSpec where

import IHP.Prelude
import Test.Hspec
import Network.Wai
import Network.Wai.Test
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as LBS

import IHP.Static (staticRouteShortcut)
import Network.Wai.Middleware.AssetPath (assetPathMiddleware, assetPath)

-- | A simple WAI app that responds with the pathInfo and rawPathInfo it received
echoApp :: Application
echoApp request respond = respond $ responseLBS status200 []
    (cs (tshow request.pathInfo <> "|" <> cs request.rawPathInfo :: Text))

-- | A fallback app that always responds with "fallback"
fallbackApp :: Application
fallbackApp _request respond = respond $ responseLBS status200 [] "fallback"

makeRequest :: ByteString -> [Text] -> Request
makeRequest rawPath segments = defaultRequest
    { rawPathInfo = rawPath
    , pathInfo = segments
    }

tests :: Spec
tests = do
    describe "staticRouteShortcut" $ do
        it "routes /static/* requests to the static app with prefix stripped" $ do
            let app = staticRouteShortcut echoApp fallbackApp
            let request = makeRequest "/static/app.css" ["static", "app.css"]
            response <- runSession (request' request) app
            let body = cs (simpleBody response) :: Text
            body `shouldBe` "[\"app.css\"]|/app.css"

        it "routes /static/ nested paths with prefix stripped" $ do
            let app = staticRouteShortcut echoApp fallbackApp
            let request = makeRequest "/static/vendor/bootstrap.css" ["static", "vendor", "bootstrap.css"]
            response <- runSession (request' request) app
            let body = cs (simpleBody response) :: Text
            body `shouldBe` "[\"vendor\",\"bootstrap.css\"]|/vendor/bootstrap.css"

        it "routes /static/ root to the static app with empty path" $ do
            let app = staticRouteShortcut echoApp fallbackApp
            let request = makeRequest "/static/" ["static", ""]
            response <- runSession (request' request) app
            let body = cs (simpleBody response) :: Text
            body `shouldBe` "[\"\"]|/"

        it "routes non-static requests to the fallback app" $ do
            let app = staticRouteShortcut echoApp fallbackApp
            let request = makeRequest "/Users" ["Users"]
            response <- runSession (request' request) app
            simpleBody response `shouldBe` "fallback"

        it "routes root requests to the fallback app" $ do
            let app = staticRouteShortcut echoApp fallbackApp
            let request = makeRequest "/" [""]
            response <- runSession (request' request) app
            simpleBody response `shouldBe` "fallback"

        it "does not match /staticx or similar prefixes" $ do
            let app = staticRouteShortcut echoApp fallbackApp
            let request = makeRequest "/staticx/app.css" ["staticx", "app.css"]
            response <- runSession (request' request) app
            simpleBody response `shouldBe` "fallback"

    describe "assetPath with /static/ prefix" $ do
        it "generates /static/-prefixed paths when middleware is applied" $ do
            let middleware = assetPathMiddleware "abc123" Nothing
            let innerApp req respond = do
                    let result = assetPath req "/app.css"
                    respond $ responseLBS status200 [] (cs result)
            let app = middleware innerApp
            response <- runSession (request' defaultRequest) app
            cs (simpleBody response) `shouldBe` ("/static/app.css?v=abc123" :: String)

        it "skips /static/ prefix when base URL is specified" $ do
            let middleware = assetPathMiddleware "abc123" (Just "https://cdn.example.com")
            let innerApp req respond = do
                    let result = assetPath req "/app.js"
                    respond $ responseLBS status200 [] (cs result)
            let app = middleware innerApp
            response <- runSession (request' defaultRequest) app
            cs (simpleBody response) `shouldBe` ("https://cdn.example.com/app.js?v=abc123" :: String)

request' :: Request -> Session SResponse
request' = srequest . flip SRequest ""
