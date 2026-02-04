{-|
Module: Test.Controller.ContextSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.Controller.ContextSpec where

import Test.Hspec
import IHP.Prelude
import IHP.Controller.Context
import Wai.Request.Params.Middleware (RequestBody (..), Respond)
import Control.Exception
import Network.Wai.Internal (ResponseReceived(..))
import Network.Wai.Test (defaultRequest)
import qualified Data.Vault.Lazy as Vault
import IHP.RequestVault (requestBodyVaultKey)
import Network.Wai (Request, vault)
import qualified Data.List as List

tests = do
    let requestBody = FormBody [] []
    let mockRequest = defaultRequest { vault = Vault.insert requestBodyVaultKey requestBody (vault defaultRequest) }
    let mockRespond :: Respond
        mockRespond = \_ -> pure ResponseReceived
    let ?request = mockRequest
    let ?respond = mockRespond
    describe "IHP.Controller.Context" do
        describe "putContext" do
            it "store a value" do
                context <- newControllerContext
                let ?context = context
                putContext ("hello" :: Text)

            it "fails if called on a frozen context" do
                context <- newControllerContext >>= freeze
                let ?context = context
                putContext ("hello" :: Text) `shouldThrow` anyException

        describe "fromContext" do
            it "return a stored value" do
                context <- newControllerContext
                let ?context = context

                putContext ("hello" :: Text)

                result <- fromContext @Text
                result `shouldBe` "hello"

            it "should fail if type not in container" do
                context <- newControllerContext
                let ?context = context

                (fromContext @Text) `shouldThrow` (\e -> case e of
                    ErrorCall msg -> "Unable to find Text in controller context:" `List.isPrefixOf` msg
                    _ -> False)

            it "return a stored value if frozen" do
                context <- newControllerContext
                let ?context = context

                putContext ("hello" :: Text)
                context <- freeze ?context
                let ?context = context

                result <- fromContext @Text
                result `shouldBe` "hello"

        describe "fromFrozenContext" do
            it "sohuld fail if not frozen" do
                context <- newControllerContext
                let ?context = context

                putContext ("hello" :: Text)

                let result = fromFrozenContext @Text
                (evaluate result) `shouldThrow` (errorCall "maybeFromFrozenContext called on a non frozen context while trying to access Text")

            it "return a stored value" do
                context <- newControllerContext
                let ?context = context

                putContext ("hello" :: Text)

                context <- freeze ?context

                let ?context = context

                (fromFrozenContext @Text) `shouldBe` "hello"

            it "should provide helpful error message for known types" do
                context <- newControllerContext
                let ?context = context

                -- Test that error message does not include a hint for unknown types
                (fromContext @Int) `shouldThrow` (\e -> case e of
                    ErrorCall msg ->
                        "Unable to find" `List.isPrefixOf` msg &&
                        not ("Hint:" `List.isInfixOf` msg)  -- Int is not a known type, so no hint
                    _ -> False)
