{-# LANGUAGE OverloadedStrings #-}
module Wai.Request.Params.MiddlewareSpec where

import Prelude
import Test.Hspec
import Data.String.Conversions (cs)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Network.Wai
import Network.Wai.Test
import Network.HTTP.Types
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai.Parse as WaiParse

import Wai.Request.Params.Middleware (requestBodyMiddleware, RequestBody(..), requestBodyVaultKey)
import Wai.Request.Params (allParams)

-- | An app that extracts the parsed RequestBody from the vault and returns info about it
inspectBodyApp :: Application
inspectBodyApp req respond = do
    let body = Vault.lookup requestBodyVaultKey (vault req)
    case body of
        Just (FormBody params files _rawPayload) ->
            respond $ responseLBS status200 [] (cs $ "FormBody params=" <> show (length params) <> " files=" <> show (length files))
        Just (JSONBody jsonPayload _) ->
            respond $ responseLBS status200 [] (cs $ "JSONBody payload=" <> show jsonPayload)
        Nothing ->
            respond $ responseLBS status200 [] "no body in vault"

-- | An app that reads the rawPayload from the middleware-parsed body.
-- This simulates what getRequestBody does: it accesses parsedBody.rawPayload.
-- Before the fix, FormBody had no rawPayload field, so getRequestBody fell
-- back to lazyRequestBody which returned empty (stream already consumed).
rereadBodyApp :: Application
rereadBodyApp req respond = do
    let body = Vault.lookup requestBodyVaultKey (vault req)
    case body of
        Just rb -> respond $ responseLBS status200 [] (rawPayload rb)
        Nothing -> respond $ responseLBS status200 [] ""

-- | An app that returns all params (body + query string) via allParams
inspectAllParamsApp :: Application
inspectAllParamsApp req respond = do
    let body = Vault.lookup requestBodyVaultKey (vault req)
    case body of
        Just requestBody ->
            let params = allParams requestBody req
            in respond $ responseLBS status200 [] (cs $ show params)
        Nothing ->
            respond $ responseLBS status200 [] "no body in vault"

app :: Application
app = requestBodyMiddleware WaiParse.defaultParseRequestBodyOptions inspectBodyApp

makeRequest :: ByteString -> [(HeaderName, ByteString)] -> Session SResponse
makeRequest method headers = srequest $ SRequest req ""
  where
    req = defaultRequest
        { requestMethod = method
        , requestHeaders = headers
        }

makeRequestWithBody :: ByteString -> [(HeaderName, ByteString)] -> LBS.ByteString -> Session SResponse
makeRequestWithBody method headers body = srequest $ SRequest req body
  where
    req = defaultRequest
        { requestMethod = method
        , requestHeaders = headers
        }

spec :: Spec
spec = do
    describe "requestBodyMiddleware" $ do
        describe "skips body parsing for bodyless methods" $ do
            it "returns empty FormBody for GET requests" $ do
                response <- runSession (makeRequest "GET" []) app
                cs (simpleBody response) `shouldBe` ("FormBody params=0 files=0" :: String)

            it "returns empty FormBody for HEAD requests" $ do
                response <- runSession (makeRequest "HEAD" []) app
                cs (simpleBody response) `shouldBe` ("FormBody params=0 files=0" :: String)

            it "returns empty FormBody for DELETE requests" $ do
                response <- runSession (makeRequest "DELETE" []) app
                cs (simpleBody response) `shouldBe` ("FormBody params=0 files=0" :: String)

            it "returns empty FormBody for OPTIONS requests" $ do
                response <- runSession (makeRequest "OPTIONS" []) app
                cs (simpleBody response) `shouldBe` ("FormBody params=0 files=0" :: String)

        describe "parses body for methods that have one" $ do
            it "parses form body for POST requests" $ do
                let body = "name=test&value=123"
                response <- runSession (makeRequestWithBody "POST" [(hContentType, "application/x-www-form-urlencoded")] body) app
                cs (simpleBody response) `shouldBe` ("FormBody params=2 files=0" :: String)

            it "parses form body for PUT requests" $ do
                let body = "name=test"
                response <- runSession (makeRequestWithBody "PUT" [(hContentType, "application/x-www-form-urlencoded")] body) app
                cs (simpleBody response) `shouldBe` ("FormBody params=1 files=0" :: String)

            it "parses form body for PATCH requests" $ do
                let body = "name=test"
                response <- runSession (makeRequestWithBody "PATCH" [(hContentType, "application/x-www-form-urlencoded")] body) app
                cs (simpleBody response) `shouldBe` ("FormBody params=1 files=0" :: String)

            it "parses JSON body for POST requests" $ do
                let body = "{\"name\": \"test\"}"
                response <- runSession (makeRequestWithBody "POST" [(hContentType, "application/json")] body) app
                cs (simpleBody response) `shouldBe` ("JSONBody payload=Just (Object (fromList [(\"name\",String \"test\")]))" :: String)

            it "parses JSON body when Content-Type includes charset parameter" $ do
                let body = "{\"name\": \"test\"}"
                response <- runSession (makeRequestWithBody "POST" [(hContentType, "application/json; charset=utf-8")] body) app
                cs (simpleBody response) `shouldBe` ("JSONBody payload=Just (Object (fromList [(\"name\",String \"test\")]))" :: String)

            it "does not parse as JSON when Content-Type is a non-JSON type starting with application/json" $ do
                let body = "{\"name\": \"test\"}"
                response <- runSession (makeRequestWithBody "POST" [(hContentType, "application/jsonFOO")] body) app
                cs (simpleBody response) `shouldBe` ("FormBody params=0 files=0" :: String)

        describe "raw body preservation (getRequestBody)" $ do
            it "preserves raw body for form-encoded POST requests" $ do
                let body = "name=test&value=123"
                let rereadApp = requestBodyMiddleware WaiParse.defaultParseRequestBodyOptions rereadBodyApp
                response <- runSession (makeRequestWithBody "POST" [(hContentType, "application/x-www-form-urlencoded")] body) rereadApp
                simpleBody response `shouldBe` body

            it "preserves raw body for POST with non-JSON Content-Type" $ do
                let body = "key1=val1"
                let rereadApp = requestBodyMiddleware WaiParse.defaultParseRequestBodyOptions rereadBodyApp
                response <- runSession (makeRequestWithBody "POST" [(hContentType, "application/x-www-form-urlencoded")] body) rereadApp
                simpleBody response `shouldBe` body

        describe "query string params" $ do
            it "preserves query params on GET requests even though body parsing is skipped" $ do
                let allParamsApp = requestBodyMiddleware WaiParse.defaultParseRequestBodyOptions inspectAllParamsApp
                let req = defaultRequest
                        { requestMethod = "GET"
                        , queryString = [("foo", Just "bar"), ("baz", Just "qux")]
                        }
                response <- runSession (srequest $ SRequest req "") allParamsApp
                cs (simpleBody response) `shouldBe` ("[(\"foo\",Just \"bar\"),(\"baz\",Just \"qux\")]" :: String)
