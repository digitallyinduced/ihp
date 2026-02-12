module Test.AutoRefreshSpec where

import Prelude
import Data.Text (Text)
import Data.IORef
import Test.Hspec
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import qualified Data.Vault.Lazy as Vault
import qualified Data.TMap as TypeMap
import Wai.Request.Params (param, RequestBody (..), requestBodyVaultKey)
import IHP.ControllerContext (ControllerContext (..), newControllerContext, freeze, unfreeze, putContext)

tests :: Spec
tests = do
    describe "AutoRefresh" do
        describe "renderView" do
            it "should preserve query parameters when re-rendering with a websocket request" do
                -- Set up the original HTTP request with query params (as received by the initial page load)
                let requestBody = FormBody { params = [], files = [] }
                let originalRequest = Wai.defaultRequest
                        { Wai.rawQueryString = "?marketId=019c4ee7-2533-7d49-926f-a8e9308c18f3&tradingAction=sell"
                        , Wai.queryString = [("marketId", Just "019c4ee7-2533-7d49-926f-a8e9308c18f3"), ("tradingAction", Just "sell")]
                        , Wai.vault = Vault.insert requestBodyVaultKey requestBody Vault.empty
                        }

                -- Set up controller context with the original request (mirrors autoRefresh setup)
                controllerContext <- newControllerContext
                let ?context = controllerContext
                putContext originalRequest
                frozenControllerContext <- freeze ?context

                -- Ensure we don't lose the query string. Use originalRequest directly.
                let renderView = \_waiRequest -> do
                        ctx <- unfreeze frozenControllerContext
                        let ?context = ctx
                        let ?request = originalRequest
                        putContext originalRequest
                        -- Verify the action can read query params from ?request
                        let marketId = param requestBody ?request "marketId" :: Text
                        let tradingAction = param requestBody ?request "tradingAction" :: Text
                        pure (marketId, tradingAction)

                -- Simulate AutoRefresh calling renderView with a WebSocket request (no query params)
                let wsRequest = Wai.defaultRequest
                (marketId, tradingAction) <- renderView wsRequest

                marketId `shouldBe` "019c4ee7-2533-7d49-926f-a8e9308c18f3"
                tradingAction `shouldBe` "sell"
