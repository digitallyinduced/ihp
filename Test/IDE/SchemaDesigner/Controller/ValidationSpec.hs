module Test.IDE.SchemaDesigner.Controller.ValidationSpec where

import Test.Hspec
import IHP.Prelude

import IHP.IDE.SchemaDesigner.Controller.Validation
import IHP.ValidationSupport

tests :: SpecWith ()
tests = do
    describe "IHP.IDE.SchemaDesigner.Controller.Validation" do
        describe "the isUniqueInList validator" do
            it "should handle cases wihout old value" do
                isUniqueInList [] Nothing "hello" `shouldBe` Success
                isUniqueInList ["hi", "bye"] Nothing "hello" `shouldBe` Success
                isUniqueInList ["hi"] Nothing "hi" `shouldSatisfy` isFailure
            it "should handle cases with old value" do
                isUniqueInList ["bye"] (Just "bye") "hello" `shouldBe` Success
                isUniqueInList ["bye", "hi"] (Just "hi") "hi" `shouldBe` Success
                isUniqueInList ["bye", "hi"] (Just "bye") "hi" `shouldSatisfy` isFailure

        describe "the validateNameInSchema validator" do
            it "should reject an empty name" do
                validateNameInSchema "" [] Nothing "" `shouldSatisfy` isFailure
            it "should reject a reserved keyword" do
                validateNameInSchema "" [] Nothing "bigint" `shouldSatisfy` isFailure
                validateNameInSchema "" [] Nothing "where" `shouldSatisfy` isFailure
            it "should reject a new name already in use" do
                validateNameInSchema "" ["used"] Nothing "used" `shouldSatisfy` isFailure
            it "should allow a name already in used if it is the old name" do
                validateNameInSchema "" ["used"] (Just "used") "used" `shouldBe` Success
            it "should allow legal names not already in use" do
                validateNameInSchema "" [] Nothing "new_name" `shouldBe` Success