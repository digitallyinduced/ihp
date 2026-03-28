{-|
Module: Test.ControllerSupportSpec
-}
module Test.ControllerSupportSpec where

import IHP.Prelude
import Test.Hspec
import IHP.ControllerSupport (requestBodyJSON)
import IHP.Controller.Response (responseHeadersVaultKey)
import IHP.Environment (Environment (..))
import qualified IHP.FrameworkConfig as FrameworkConfig
import qualified IHP.RequestVault as RequestVault
import Wai.Request.Params.Middleware (RequestBody (..), requestBodyVaultKey, Respond)
import Network.Wai.Middleware.EarlyReturn (earlyReturnMiddleware)
import qualified Data.Vault.Lazy as Vault
import qualified Data.Aeson as Aeson
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai.Internal
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Network.HTTP.Types (status400)
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString.Builder (toLazyByteString)

tests = do
    describe "IHP.ControllerSupport" do
        describe "requestBodyJSON" do
            it "should return parsed JSON value for valid JSONBody" do
                let jsonValue = Aeson.object [("name", Aeson.String "test")]
                let requestBody = JSONBody { jsonPayload = Just jsonValue, rawPayload = "{\"name\":\"test\"}" }
                request <- buildRequest requestBody Development
                responseRef <- newIORef (error "no response captured")
                let ?request = request
                let ?respond = captureRespond responseRef
                result <- requestBodyJSON
                result `shouldBe` jsonValue

            it "should return 400 for FormBody" do
                let requestBody = FormBody { params = [], files = [], rawPayload = "" }
                response <- runWithEarlyReturn requestBody Development requestBodyJSON
                Wai.responseStatus response `shouldBe` status400
                let body = responseBody response
                body `shouldSatisfy` bodyContains "form content type"

            it "should return 400 for JSONBody with empty body" do
                let requestBody = JSONBody { jsonPayload = Nothing, rawPayload = "" }
                response <- runWithEarlyReturn requestBody Development requestBodyJSON
                Wai.responseStatus response `shouldBe` status400
                let body = responseBody response
                body `shouldSatisfy` bodyContains "request body is empty"

            it "should return 400 for JSONBody with invalid JSON" do
                let requestBody = JSONBody { jsonPayload = Nothing, rawPayload = "not valid json" }
                response <- runWithEarlyReturn requestBody Development requestBodyJSON
                Wai.responseStatus response `shouldBe` status400
                let body = responseBody response
                body `shouldSatisfy` bodyContains "not valid json"

            it "should truncate long payloads in dev mode" do
                let longPayload = LBS.pack (replicate 500 65) -- 500 bytes of 'A'
                let requestBody = JSONBody { jsonPayload = Nothing, rawPayload = longPayload }
                response <- runWithEarlyReturn requestBody Development requestBodyJSON
                let body = responseBody response
                body `shouldSatisfy` bodyContains "truncated"
                body `shouldSatisfy` bodyContains "raw request body was"

            it "should omit raw payload in production mode" do
                let requestBody = JSONBody { jsonPayload = Nothing, rawPayload = "not valid json" }
                response <- runWithEarlyReturn requestBody Production requestBodyJSON
                Wai.responseStatus response `shouldBe` status400
                let body = responseBody response
                body `shouldSatisfy` (not . bodyContains "not valid json")
                body `shouldSatisfy` (not . bodyContains "raw request body was")

bodyContains :: BS.ByteString -> LBS.ByteString -> Bool
bodyContains needle haystack = BS.isInfixOf needle (LBS.toStrict haystack)

-- | Run an action that calls earlyReturn through the earlyReturnMiddleware,
-- capturing the response.
runWithEarlyReturn :: RequestBody -> Environment -> ((?request :: Wai.Request, ?respond :: Respond) => IO a) -> IO Wai.Response
runWithEarlyReturn requestBody environment action = do
    request <- buildRequest requestBody environment
    responseRef <- newIORef (error "no response captured")
    let app req respond = do
            let ?request = req
            let ?respond = respond
            _ <- action
            error "requestBodyJSON should have called earlyReturn"
    earlyReturnMiddleware app request (captureRespond responseRef)
    readIORef responseRef

-- | Creates a Respond callback that captures the Response in an IORef
captureRespond :: IORef Wai.Response -> Respond
captureRespond ref response = do
    writeIORef ref response
    pure Wai.Internal.ResponseReceived

buildRequest :: RequestBody -> Environment -> IO Wai.Request
buildRequest requestBody environment = do
    frameworkConfig <- FrameworkConfig.buildFrameworkConfig (FrameworkConfig.option environment)
    headersRef <- newIORef []
    pure Wai.defaultRequest
        { Wai.vault = Vault.insert RequestVault.frameworkConfigVaultKey frameworkConfig
                    $ Vault.insert requestBodyVaultKey requestBody
                    $ Vault.insert responseHeadersVaultKey headersRef Vault.empty
        }

-- | Extract the body from a WAI Response (works for responseLBS responses)
responseBody :: Wai.Response -> LBS.ByteString
responseBody response =
    let (_, _, withBody) = Wai.responseToStream response
    in unsafePerformIO $ withBody $ \streamingBody -> do
        ref <- newIORef mempty
        streamingBody
            (\chunk -> modifyIORef ref (<> chunk))
            (pure ())
        builder <- readIORef ref
        pure (toLazyByteString builder)
