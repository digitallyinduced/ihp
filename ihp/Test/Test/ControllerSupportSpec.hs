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
import Wai.Request.Params.Middleware (RequestBody (..), requestBodyVaultKey)
import Network.Wai.Middleware.EarlyReturn (earlyReturnMiddleware)
import Network.Wai.Test (runSession, request, SResponse(..))
import qualified Data.Vault.Lazy as Vault
import qualified Data.Aeson as Aeson
import qualified Network.Wai as Wai
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Network.HTTP.Types (status400)

tests = do
    describe "IHP.ControllerSupport" do
        describe "requestBodyJSON" do
            it "should return parsed JSON value for valid JSONBody" do
                let jsonValue = Aeson.object [("name", Aeson.String "test")]
                let requestBody = JSONBody { jsonPayload = Just jsonValue, rawPayload = "{\"name\":\"test\"}" }
                req <- buildRequest requestBody Development
                let ?request = req
                let ?respond = error "respond should not be called for valid JSON"
                result <- requestBodyJSON
                result `shouldBe` jsonValue

            it "should return 400 for FormBody" do
                let requestBody = FormBody { params = [], files = [], rawPayload = "" }
                SResponse {..} <- runRequestBodyJSON requestBody Development
                simpleStatus `shouldBe` status400
                simpleBody `shouldSatisfy` bodyContains "form content type"

            it "should return 400 for JSONBody with empty body" do
                let requestBody = JSONBody { jsonPayload = Nothing, rawPayload = "" }
                SResponse {..} <- runRequestBodyJSON requestBody Development
                simpleStatus `shouldBe` status400
                simpleBody `shouldSatisfy` bodyContains "request body is empty"

            it "should return 400 for JSONBody with invalid JSON" do
                let requestBody = JSONBody { jsonPayload = Nothing, rawPayload = "not valid json" }
                SResponse {..} <- runRequestBodyJSON requestBody Development
                simpleStatus `shouldBe` status400
                simpleBody `shouldSatisfy` bodyContains "not valid json"

            it "should truncate long payloads in dev mode" do
                let longPayload = LBS.pack (replicate 500 65) -- 500 bytes of 'A'
                let requestBody = JSONBody { jsonPayload = Nothing, rawPayload = longPayload }
                SResponse {..} <- runRequestBodyJSON requestBody Development
                simpleBody `shouldSatisfy` bodyContains "truncated"
                simpleBody `shouldSatisfy` bodyContains "raw request body was"

            it "should omit raw payload in production mode" do
                let requestBody = JSONBody { jsonPayload = Nothing, rawPayload = "not valid json" }
                SResponse {..} <- runRequestBodyJSON requestBody Production
                simpleStatus `shouldBe` status400
                simpleBody `shouldSatisfy` (not . bodyContains "not valid json")
                simpleBody `shouldSatisfy` (not . bodyContains "raw request body was")

bodyContains :: BS.ByteString -> LBS.ByteString -> Bool
bodyContains needle haystack = BS.isInfixOf needle (LBS.toStrict haystack)

-- | Run requestBodyJSON through earlyReturnMiddleware, returning the SResponse
runRequestBodyJSON :: RequestBody -> Environment -> IO SResponse
runRequestBodyJSON requestBody environment = do
    req <- buildRequest requestBody environment
    let app r respond = do
            let ?request = r
            let ?respond = respond
            _ <- requestBodyJSON
            error "requestBodyJSON should have called earlyReturn"
    runSession (request req) (earlyReturnMiddleware app)

buildRequest :: RequestBody -> Environment -> IO Wai.Request
buildRequest requestBody environment = do
    frameworkConfig <- FrameworkConfig.buildFrameworkConfig (FrameworkConfig.option environment)
    headersRef <- newIORef []
    pure Wai.defaultRequest
        { Wai.vault = Vault.insert RequestVault.frameworkConfigVaultKey frameworkConfig
                    $ Vault.insert requestBodyVaultKey requestBody
                    $ Vault.insert responseHeadersVaultKey headersRef Vault.empty
        }
