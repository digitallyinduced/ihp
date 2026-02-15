{-|
Module: Test.ControllerSupportSpec
-}
module Test.ControllerSupportSpec where

import IHP.Prelude
import Test.Hspec
import IHP.ControllerSupport (requestBodyJSON)
import IHP.Controller.Response (ResponseException(..))
import IHP.Environment (Environment (..))
import qualified IHP.FrameworkConfig as FrameworkConfig
import qualified IHP.RequestVault as RequestVault
import Wai.Request.Params.Middleware (RequestBody (..), requestBodyVaultKey)
import qualified Data.Vault.Lazy as Vault
import qualified Data.Aeson as Aeson
import qualified Network.Wai as Wai
import qualified Control.Exception as Exception
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
                let ?request = request
                result <- requestBodyJSON
                result `shouldBe` jsonValue

            it "should return 400 for FormBody" do
                let requestBody = FormBody { params = [], files = [], rawPayload = "" }
                request <- buildRequest requestBody Development
                let ?request = request
                result <- Exception.try requestBodyJSON
                case result of
                    Left (ResponseException response) -> do
                        Wai.responseStatus response `shouldBe` status400
                        let body = responseBody response
                        body `shouldSatisfy` bodyContains "form content type"
                    Right _ -> expectationFailure "Expected ResponseException"

            it "should return 400 for JSONBody with empty body" do
                let requestBody = JSONBody { jsonPayload = Nothing, rawPayload = "" }
                request <- buildRequest requestBody Development
                let ?request = request
                result <- Exception.try requestBodyJSON
                case result of
                    Left (ResponseException response) -> do
                        Wai.responseStatus response `shouldBe` status400
                        let body = responseBody response
                        body `shouldSatisfy` bodyContains "request body is empty"
                    Right _ -> expectationFailure "Expected ResponseException"

            it "should return 400 for JSONBody with invalid JSON" do
                let requestBody = JSONBody { jsonPayload = Nothing, rawPayload = "not valid json" }
                request <- buildRequest requestBody Development
                let ?request = request
                result <- Exception.try requestBodyJSON
                case result of
                    Left (ResponseException response) -> do
                        Wai.responseStatus response `shouldBe` status400
                        let body = responseBody response
                        body `shouldSatisfy` bodyContains "not valid json"
                    Right _ -> expectationFailure "Expected ResponseException"

            it "should truncate long payloads in dev mode" do
                let longPayload = LBS.pack (replicate 500 65) -- 500 bytes of 'A'
                let requestBody = JSONBody { jsonPayload = Nothing, rawPayload = longPayload }
                request <- buildRequest requestBody Development
                let ?request = request
                result <- Exception.try requestBodyJSON
                case result of
                    Left (ResponseException response) -> do
                        let body = responseBody response
                        body `shouldSatisfy` bodyContains "truncated"
                        body `shouldSatisfy` bodyContains "raw request body was"
                    Right _ -> expectationFailure "Expected ResponseException"

            it "should omit raw payload in production mode" do
                let requestBody = JSONBody { jsonPayload = Nothing, rawPayload = "not valid json" }
                request <- buildRequest requestBody Production
                let ?request = request
                result <- Exception.try requestBodyJSON
                case result of
                    Left (ResponseException response) -> do
                        Wai.responseStatus response `shouldBe` status400
                        let body = responseBody response
                        body `shouldSatisfy` (not . bodyContains "not valid json")
                        body `shouldSatisfy` (not . bodyContains "raw request body was")
                    Right _ -> expectationFailure "Expected ResponseException"

bodyContains :: BS.ByteString -> LBS.ByteString -> Bool
bodyContains needle haystack = BS.isInfixOf needle (LBS.toStrict haystack)

buildRequest :: RequestBody -> Environment -> IO Wai.Request
buildRequest requestBody environment = do
    frameworkConfig <- FrameworkConfig.buildFrameworkConfig (FrameworkConfig.option environment)
    pure Wai.defaultRequest
        { Wai.vault = Vault.insert RequestVault.frameworkConfigVaultKey frameworkConfig
                    $ Vault.insert requestBodyVaultKey requestBody Vault.empty
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
