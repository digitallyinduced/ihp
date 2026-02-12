{-|
Module: Test.ControllerSupportSpec
-}
module Test.ControllerSupportSpec where

import IHP.Prelude
import Test.Hspec
import IHP.ControllerSupport (requestBodyJSON)
import Wai.Request.Params.Middleware (RequestBody (..), requestBodyVaultKey)
import qualified Data.Vault.Lazy as Vault
import qualified Data.Aeson as Aeson
import qualified Network.Wai as Wai
import qualified GHC.IO as IO

tests = do
    describe "IHP.ControllerSupport" do
        describe "requestBodyJSON" do
            it "should return parsed JSON value for valid JSONBody" do
                let jsonValue = Aeson.object [("name", Aeson.String "test")]
                let requestBody = JSONBody { jsonPayload = Just jsonValue, rawPayload = "{\"name\":\"test\"}" }
                let request = Wai.defaultRequest { Wai.vault = Vault.insert requestBodyVaultKey requestBody Vault.empty }
                let ?request = request
                requestBodyJSON `shouldBe` jsonValue

            it "should throw descriptive error for FormBody" do
                let requestBody = FormBody { params = [], files = [] }
                let request = Wai.defaultRequest { Wai.vault = Vault.insert requestBodyVaultKey requestBody Vault.empty }
                let ?request = request
                (IO.evaluate requestBodyJSON) `shouldThrow` errorCall "Expected JSON body, but the request has a form content type. Make sure to set 'Content-Type: application/json' in the request header."

            it "should throw descriptive error for JSONBody with empty body" do
                let requestBody = JSONBody { jsonPayload = Nothing, rawPayload = "" }
                let request = Wai.defaultRequest { Wai.vault = Vault.insert requestBodyVaultKey requestBody Vault.empty }
                let ?request = request
                (IO.evaluate requestBodyJSON) `shouldThrow` errorCall "Expected JSON body, but could not decode the request body. The request body is empty."

            it "should throw descriptive error for JSONBody with invalid JSON" do
                let requestBody = JSONBody { jsonPayload = Nothing, rawPayload = "not valid json" }
                let request = Wai.defaultRequest { Wai.vault = Vault.insert requestBodyVaultKey requestBody Vault.empty }
                let ?request = request
                (IO.evaluate requestBodyJSON) `shouldThrow` errorCall "Expected JSON body, but could not decode the request body. The raw request body was: \"not valid json\""
