{-|
Module: Test.ControllerSupportSpec
-}
module Test.ControllerSupportSpec where

import IHP.Prelude
import Test.Hspec
import IHP.ControllerSupport (requestBodyJSON, InitControllerContext (..), startWebSocketAppAndFailOnHTTP)
import IHP.Controller.Response (responseHeadersVaultKey)
import IHP.Environment (Environment (..))
import qualified IHP.FrameworkConfig as FrameworkConfig
import IHP.ModelSupport (notConnectedModelContext)
import qualified IHP.RequestVault as RequestVault
import qualified IHP.WebSocket as WS
import Wai.Request.Params.Middleware (RequestBody (..), requestBodyVaultKey)
import Network.Wai.Middleware.EarlyReturn (earlyReturnMiddleware)
import Network.Wai.Test (runSession, request, Session, SResponse(..), assertStatus, assertBodyContains)
import Test.Util (assertBodyNotContains)
import qualified Data.Vault.Lazy as Vault
import qualified Data.Aeson as Aeson
import qualified Network.Wai as Wai
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types (status101)

-- | Minimal application fixture for 'startWebSocketApp' — just enough to
-- satisfy the 'InitControllerContext' constraint without any real initialisation.
data TestApp = TestApp
    deriving (Eq, Show)

instance InitControllerContext TestApp where
    initContext = pure ()

-- | Minimal 'WSApp' fixture for 'startWebSocketApp'. The run/onPing/onClose
-- defaults from the class are sufficient — the regression test below never
-- completes the handshake, so none of them actually fire.
data TestWSApp = TestWSApp

instance WS.WSApp TestWSApp where
    initialState = TestWSApp

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
                runRequestBodyJSON requestBody Development \response -> do
                    assertStatus 400 response
                    assertBodyContains "form content type" response

            it "should return 400 for JSONBody with empty body" do
                let requestBody = JSONBody { jsonPayload = Nothing, rawPayload = "" }
                runRequestBodyJSON requestBody Development \response -> do
                    assertStatus 400 response
                    assertBodyContains "request body is empty" response

            it "should return 400 for JSONBody with invalid JSON" do
                let requestBody = JSONBody { jsonPayload = Nothing, rawPayload = "not valid json" }
                runRequestBodyJSON requestBody Development \response -> do
                    assertStatus 400 response
                    assertBodyContains "not valid json" response

            it "should truncate long payloads in dev mode" do
                let longPayload = LBS.pack (replicate 500 65) -- 500 bytes of 'A'
                let requestBody = JSONBody { jsonPayload = Nothing, rawPayload = longPayload }
                runRequestBodyJSON requestBody Development \response -> do
                    assertBodyContains "truncated" response
                    assertBodyContains "raw request body was" response

            it "should omit raw payload in production mode" do
                let requestBody = JSONBody { jsonPayload = Nothing, rawPayload = "not valid json" }
                runRequestBodyJSON requestBody Production \response -> do
                    assertStatus 400 response
                    assertBodyNotContains "not valid json" response
                    assertBodyNotContains "raw request body was" response

        describe "startWebSocketApp" do
            -- Regression for digitallyinduced/ihp#2625: 'wai-websockets'
            -- builds a successful WebSocket upgrade as a 'ResponseRaw' whose
            -- fallback 'Response' carries 'status500'. Warp runs the raw
            -- streaming handler and the client correctly receives 101, but
            -- request-logger middlewares read 'responseStatus' on the
            -- fallback (because 'responseStatus (ResponseRaw _ res) =
            -- responseStatus res') and log 500 for every successful upgrade.
            -- 'startWebSocketApp' must rewrite the fallback status so the
            -- view-from-middleware agrees with the on-the-wire status.
            it "presents successful WebSocket upgrades as status 101 to middleware" do
                frameworkConfig <- FrameworkConfig.buildFrameworkConfig (FrameworkConfig.option Development)
                let modelContext = notConnectedModelContext frameworkConfig.logger
                let baseRequest = Wai.defaultRequest
                        { Wai.requestHeaders =
                            [ ("Host", "localhost")
                            , ("Upgrade", "websocket")
                            , ("Connection", "Upgrade")
                            , ("Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ==")
                            , ("Sec-WebSocket-Version", "13")
                            ]
                        , Wai.vault =
                            Vault.insert RequestVault.frameworkConfigVaultKey frameworkConfig
                            $ Vault.insert RequestVault.modelContextVaultKey modelContext Vault.empty
                        }
                let app r respond = do
                        let ?request = r
                        let ?respond = respond
                        let ?application = TestApp
                        startWebSocketAppAndFailOnHTTP @TestWSApp @TestApp TestWSApp r respond
                SResponse { simpleStatus } <- runSession (request baseRequest) app
                simpleStatus `shouldBe` status101

-- | Run requestBodyJSON through earlyReturnMiddleware, asserting on the SResponse
runRequestBodyJSON :: RequestBody -> Environment -> (SResponse -> Session ()) -> IO ()
runRequestBodyJSON requestBody environment check = do
    req <- buildRequest requestBody environment
    let app r respond = do
            let ?request = r
            let ?respond = respond
            _ <- requestBodyJSON
            error "requestBodyJSON should have called earlyReturn"
    runSession (request req >>= check) (earlyReturnMiddleware app)

buildRequest :: RequestBody -> Environment -> IO Wai.Request
buildRequest requestBody environment = do
    frameworkConfig <- FrameworkConfig.buildFrameworkConfig (FrameworkConfig.option environment)
    headersRef <- newIORef []
    pure Wai.defaultRequest
        { Wai.vault = Vault.insert RequestVault.frameworkConfigVaultKey frameworkConfig
                    $ Vault.insert requestBodyVaultKey requestBody
                    $ Vault.insert responseHeadersVaultKey headersRef Vault.empty
        }
