{-|
Module: Test.Controller.ContextSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.Controller.ContextSpec where

import Test.Hspec
import IHP.Prelude
import IHP.Controller.Context
import Control.Exception

tests = do
    let ?requestContext = undefined
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

                (fromContext @Text) `shouldThrow` (errorCall "Unable to find Text in controller context: TypeRepMap []")

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
                (evaluate result) `shouldThrow` (errorCall "maybeFromFrozenContext called on a non frozen context")

            it "return a stored value" do
                context <- newControllerContext
                let ?context = context

                putContext ("hello" :: Text)

                context <- freeze ?context

                let ?context = context

                (fromFrozenContext @Text) `shouldBe` "hello"
