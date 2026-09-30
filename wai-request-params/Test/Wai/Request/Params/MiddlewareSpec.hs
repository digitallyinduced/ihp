{-# LANGUAGE OverloadedStrings #-}
module Wai.Request.Params.MiddlewareSpec where

import Prelude
import Test.Hspec
import Data.String.Conversions (cs)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Network.Wai
import Network.Wai.Test
import Network.Wai.Internal (ResponseReceived (..))
import Network.HTTP.Types
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai.Parse as WaiParse
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.IORef
import Control.Exception (evaluate, try, SomeException)
import System.Directory (doesFileExist)

import Wai.Request.Params.Middleware (requestBodyMiddleware, requestBodyMiddlewareWith, RequestBody(..), requestBodyVaultKey, readRawRequestBody, FileUploadBackend (..), tempFilesVaultKey)
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

            it "returns JSONBody with Nothing payload for invalid JSON POST" $ do
                let body = "not valid json{{"
                response <- runSession (makeRequestWithBody "POST" [(hContentType, "application/json")] body) app
                cs (simpleBody response) `shouldBe` ("JSONBody payload=Nothing" :: String)

            it "returns FormBody for POST without Content-Type" $ do
                let body = "{\"name\": \"test\"}"
                response <- runSession (makeRequestWithBody "POST" [] body) app
                cs (simpleBody response) `shouldBe` ("FormBody params=0 files=0" :: String)

            it "returns JSONBody with Nothing payload for empty body with application/json" $ do
                let body = ""
                response <- runSession (makeRequestWithBody "POST" [(hContentType, "application/json")] body) app
                cs (simpleBody response) `shouldBe` ("JSONBody payload=Nothing" :: String)

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

            it "returns the full body for a raw POST via readRawRequestBody" $ do
                let body = "raw webhook payload"
                let readRawApp req respond = do
                        first <- readRawRequestBody req
                        second <- readRawRequestBody req
                        respond $ responseLBS status200 [] (first <> "|" <> second)
                response <- runSession (makeRequestWithBody "POST" [(hContentType, "application/octet-stream")] body) (requestBodyMiddleware WaiParse.defaultParseRequestBodyOptions readRawApp)
                simpleBody response `shouldBe` (body <> "|" <> body)

            it "returns the full body for a POST without Content-Type via readRawRequestBody" $ do
                let body = "{\"name\": \"test\"}"
                let readRawApp req respond = readRawRequestBody req >>= respond . responseLBS status200 []
                response <- runSession (makeRequestWithBody "POST" [] body) (requestBodyMiddleware WaiParse.defaultParseRequestBodyOptions readRawApp)
                simpleBody response `shouldBe` body

            it "returns the full body for JSON via readRawRequestBody" $ do
                let body = "{\"name\": \"test\"}"
                let readRawApp req respond = readRawRequestBody req >>= respond . responseLBS status200 []
                response <- runSession (makeRequestWithBody "POST" [(hContentType, "application/json")] body) (requestBodyMiddleware WaiParse.defaultParseRequestBodyOptions readRawApp)
                simpleBody response `shouldBe` body

        describe "raw bodies (non-form, non-JSON content types)" $ do
            it "leaves a 50 MB application/octet-stream body unread so the app can stream it" $ do
                let chunk = BS.replicate (64 * 1024) 42
                let chunkCount = 800 -- 800 * 64 KiB = 50 MiB
                chunksLeft <- newIORef (chunkCount :: Int)
                let bodyReader = atomicModifyIORef' chunksLeft $ \n -> if n <= 0 then (0, BS.empty) else (n - 1, chunk)
                let req = setRequestBodyChunks bodyReader defaultRequest
                        { requestMethod = "POST"
                        , requestHeaders = [(hContentType, "application/octet-stream")]
                        }
                resultRef <- newIORef Nothing
                let streamingApp req' respond = do
                        -- Nothing has been read before the app runs
                        left <- readIORef chunksLeft
                        let rawPayloadLength = maybe (-1) (LBS.length . rawPayload) (Vault.lookup requestBodyVaultKey (vault req'))
                        let streamBody total = do
                                c <- getRequestBodyChunk req'
                                if BS.null c then pure total else streamBody (total + BS.length c)
                        total <- streamBody 0
                        writeIORef resultRef (Just (chunkCount - left, rawPayloadLength, total))
                        respond $ responseLBS status200 [] ""
                _ <- requestBodyMiddleware WaiParse.defaultParseRequestBodyOptions streamingApp req (const (pure ResponseReceived))
                -- No chunk was read by the middleware, rawPayload is empty,
                -- and the app streamed all 50 MiB itself
                readIORef resultRef `shouldReturn` Just (0, 0, 50 * 1024 * 1024)

        describe "multipart bodies" $ do
            let multipartBody = LBS.concat
                    [ "--BOUNDARY\r\n"
                    , "Content-Disposition: form-data; name=\"title\"\r\n\r\n"
                    , "Hello\r\n"
                    , "--BOUNDARY\r\n"
                    , "Content-Disposition: form-data; name=\"video\"; filename=\"video.mp4\"\r\n"
                    , "Content-Type: video/mp4\r\n\r\n"
                    , "file-content\r\n"
                    , "--BOUNDARY--\r\n"
                    ]
            let multipartHeaders = [(hContentType, "multipart/form-data; boundary=BOUNDARY")]

            it "parses params and files with the in-memory backend, leaving rawPayload empty" $ do
                let inspectApp req respond = case Vault.lookup requestBodyVaultKey (vault req) of
                        Just FormBody { params, files, rawPayload } ->
                            respond $ responseLBS status200 [] (cs (show (params, map (\(name, info) -> (name, WaiParse.fileName info, WaiParse.fileContent info)) files, rawPayload)))
                        _ -> respond $ responseLBS status200 [] "unexpected body"
                response <- runSession (makeRequestWithBody "POST" multipartHeaders multipartBody) (requestBodyMiddleware WaiParse.defaultParseRequestBodyOptions inspectApp)
                cs (simpleBody response) `shouldBe` ("([(\"title\",\"Hello\")],[(\"video\",\"video.mp4\",\"file-content\")],\"\")" :: String)

            it "stores files on disk with the temp file backend and removes them after the request" $ do
                tempPathRef <- newIORef Nothing
                let inspectApp req respond = case (Vault.lookup requestBodyVaultKey (vault req), Vault.lookup tempFilesVaultKey (vault req)) of
                        (Just FormBody { params, files, rawPayload }, Just [(_, tempFile)]) -> do
                            let path = WaiParse.fileContent tempFile
                            writeIORef tempPathRef (Just path)
                            existsDuringRequest <- doesFileExist path
                            onDisk <- LBS.readFile path
                            let contents = map (\(name, info) -> (name, WaiParse.fileContent info)) files
                            respond $ responseLBS status200 [] (cs (show (params, existsDuringRequest, onDisk, contents, rawPayload)))
                        _ -> respond $ responseLBS status200 [] "unexpected body"
                response <- runSession (makeRequestWithBody "POST" multipartHeaders multipartBody) (requestBodyMiddlewareWith TempFileUploads WaiParse.defaultParseRequestBodyOptions inspectApp)
                cs (simpleBody response) `shouldBe` ("([(\"title\",\"Hello\")],True,\"file-content\",[(\"video\",\"file-content\")],\"\")" :: String)
                Just path <- readIORef tempPathRef
                doesFileExist path >>= (`shouldBe` False)

            it "applies setMaxRequestFileSize while streaming" $ do
                let options = WaiParse.setMaxRequestFileSize 4 WaiParse.defaultParseRequestBodyOptions
                result <- try (runSession (makeRequestWithBody "POST" multipartHeaders multipartBody) (requestBodyMiddlewareWith TempFileUploads options inspectBodyApp) >>= evaluate . simpleBody)
                case result of
                    Left (_ :: SomeException) -> pure ()
                    Right body -> expectationFailure ("expected the upload to be rejected, got: " <> Char8.unpack (LBS.toStrict body))

        describe "query string params" $ do
            it "preserves query params on GET requests even though body parsing is skipped" $ do
                let allParamsApp = requestBodyMiddleware WaiParse.defaultParseRequestBodyOptions inspectAllParamsApp
                let req = defaultRequest
                        { requestMethod = "GET"
                        , queryString = [("foo", Just "bar"), ("baz", Just "qux")]
                        }
                response <- runSession (srequest $ SRequest req "") allParamsApp
                cs (simpleBody response) `shouldBe` ("[(\"foo\",Just \"bar\"),(\"baz\",Just \"qux\")]" :: String)
