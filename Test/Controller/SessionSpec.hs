{-|
Module: Test.Controller.ParamSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.Controller.SessionSpec where

import Data.Either (isLeft)
import IHP.Controller.Session
import IHP.ModelSupport
import IHP.Prelude
import Test.Hspec


tests = do
    describe "IHP.Controller.Session" do
        sessionValue

sessionValue = describe "SessionValue" do
    sessionValueInt
    sessionValueInteger
    sessionValueDouble
    sessionValueFloat
    sessionValueText
    sessionValueString
    sessionValueByteString
    sessionValueUUID
    sessionValueRecordId

sessionValueInt = describe "Int" do
    describe "toSessionValue" do
        it "should handle numeric input" do
            toSessionValue @Int 1984 `shouldBe` "1984"
        it "should handle negative numbers" do
            toSessionValue @Int (-1984) `shouldBe` "-1984"
        it "should handle zero value" do
            toSessionValue @Int 0 `shouldBe` "0"
    describe "fromSessionValue" do
        it "should handle numeric input" do
            fromSessionValue @Int "1984" `shouldBe` Right 1984
        it "should handle negative numbers" do
            fromSessionValue @Int "-1984" `shouldBe` Right (-1984)
        it "should handle zero value" do
            fromSessionValue @Int "0" `shouldBe` Right 0
        it "should fail on floating number" do
            fromSessionValue @Int "0.23" `shouldSatisfy` isLeft
        it "should fail on text input" do
            fromSessionValue @Int "error" `shouldSatisfy` isLeft
        it "should fail on text input with leading digit" do
            fromSessionValue @Int "0error" `shouldSatisfy` isLeft
    describe "fromSessionValue equel toSessionValue on correct input" do
        it "with numeric input" do
            conversion @Int 1984 `shouldBe` Right 1984
        it "with negative numbers" do
            conversion @Int (-1984) `shouldBe` Right (-1984)
        it "with zero" do
            conversion @Int 0 `shouldBe` Right 0

sessionValueInteger = describe "Integer" do
    describe "toSessionValue" do
        it "should handle numeric input" do
            toSessionValue @Integer 1984 `shouldBe` "1984"
        it "should handle negative numbers" do
            toSessionValue @Integer (-1984) `shouldBe` "-1984"
        it "should handle zero value" do
            toSessionValue @Integer 0 `shouldBe` "0"
    describe "fromSessionValue" do
        it "should handle numeric input" do
            fromSessionValue @Integer "1984" `shouldBe` Right 1984
        it "should handle negative numbers" do
            fromSessionValue @Integer "-1984" `shouldBe` Right (-1984)
        it "should handle zero value" do
            fromSessionValue @Integer "0" `shouldBe` Right 0
        it "should fail on floating number" do
            fromSessionValue @Integer "0.23" `shouldSatisfy` isLeft
        it "should fail on text input" do
            fromSessionValue @Integer "error" `shouldSatisfy` isLeft
        it "should fail on text input with leading digit" do
            fromSessionValue @Integer "0error" `shouldSatisfy` isLeft
    describe "fromSessionValue equel toSessionValue on correct input" do
        it "with numeric input" do
            conversion @Integer 1984 `shouldBe` Right 1984
        it "with negative numbers" do
            conversion @Integer (-1984) `shouldBe` Right (-1984)
        it "with zero" do
            conversion @Integer 0 `shouldBe` Right 0

sessionValueDouble = describe "Double" do
    describe "toSessionValue" do
        it "should handle integer" do
            toSessionValue @Double 1984 `shouldBe` "1984.0"
        it "should handle negative integer" do
            toSessionValue @Double (-1984) `shouldBe` "-1984.0"
        it "should handle floating number" do
            toSessionValue @Double 1984.123 `shouldBe` "1984.123"
        it "should handle negative floating number" do
            toSessionValue @Double (-1984.123) `shouldBe` "-1984.123"
        it "should handle zero value" do
            toSessionValue @Double 0 `shouldBe` "0.0"
    describe "fromSessionValue" do
        it "should handle integer" do
            fromSessionValue @Double "1984" `shouldBe` Right 1984
        it "should handle negative integer" do
            fromSessionValue @Double "-1984" `shouldBe` Right (-1984)
        it "should handle floating number" do
            fromSessionValue @Double "1984.123" `shouldBe` Right 1984.123
        it "should handle negative floating number" do
            fromSessionValue @Double "-1984.123" `shouldBe` Right (-1984.123)
        it "should handle zero value" do
            fromSessionValue @Double "0" `shouldBe` Right 0
        it "should fail on text input" do
            fromSessionValue @Double "error" `shouldSatisfy` isLeft
        it "should fail on text input with leading digit" do
            fromSessionValue @Double "0error" `shouldSatisfy` isLeft
        it "should fail on text input with leading floating number" do
            fromSessionValue @Double "123.456.error" `shouldSatisfy` isLeft
    describe "fromSessionValue equel toSessionValue on correct input" do
        it "with integer" do
            conversion @Double 1984 `shouldBe` Right 1984
        it "with negative integer" do
            conversion @Double (-1984) `shouldBe` Right (-1984)
        it "with floating number" do
            conversion @Double 1984.123 `shouldBe` Right 1984.123
        it "with negative floating number" do
            conversion @Double (-1984.123) `shouldBe` Right (-1984.123)
        it "with zero" do
            conversion @Double 0 `shouldBe` Right 0

sessionValueFloat = describe "Float" do
    describe "toSessionValue" do
        it "should handle integer" do
            toSessionValue @Float 1984 `shouldBe` "1984.0"
        it "should handle negative integer" do
            toSessionValue @Float (-1984) `shouldBe` "-1984.0"
        it "should handle floating number" do
            toSessionValue @Float 1984.123 `shouldBe` "1984.123"
        it "should handle negative floating number" do
            toSessionValue @Float (-1984.123) `shouldBe` "-1984.123"
        it "should handle zero value" do
            toSessionValue @Float 0 `shouldBe` "0.0"
    describe "fromSessionValue" do
        it "should handle integer" do
            fromSessionValue @Float "1984" `shouldBe` Right 1984
        it "should handle negative integer" do
            fromSessionValue @Float "-1984" `shouldBe` Right (-1984)
        it "should handle floating number" do
            fromSessionValue @Float "1984.123" `shouldBe` Right 1984.123
        it "should handle negative floating number" do
            fromSessionValue @Float "-1984.123" `shouldBe` Right (-1984.123)
        it "should handle zero value" do
            fromSessionValue @Float "0" `shouldBe` Right 0
        it "should fail on text input" do
            fromSessionValue @Float "error" `shouldSatisfy` isLeft
        it "should fail on text input with leading digit" do
            fromSessionValue @Float "0error" `shouldSatisfy` isLeft
        it "should fail on text input with leading floating number" do
            fromSessionValue @Float "123.456.error" `shouldSatisfy` isLeft
    describe "fromSessionValue equel toSessionValue on correct input" do
        it "with integer" do
            conversion @Float 1984 `shouldBe` Right 1984
        it "with negative integer" do
            conversion @Float (-1984) `shouldBe` Right (-1984)
        it "with floating number" do
            conversion @Float 1984.123 `shouldBe` Right 1984.123
        it "with negative floating number" do
            conversion @Float (-1984.123) `shouldBe` Right (-1984.123)
        it "with zero" do
            conversion @Float 0 `shouldBe` Right 0

sessionValueText = describe "Text" do
    describe "toSessionValue" do
        it "should handle text input" do
            toSessionValue @Text "test" `shouldBe` "test"
    describe "fromSessionValue" do
        it "should handle text input" do
            fromSessionValue @Text "test" `shouldBe` Right "test"
    describe "fromSessionValue equel toSessionValue on correct input" do
        it "with text input" do
            conversion @Text "test" `shouldBe` Right "test"

sessionValueString = describe "String" do
    describe "toSessionValue" do
        it "should handle text input" do
            toSessionValue @String "test" `shouldBe` "test"
    describe "fromSessionValue" do
        it "should handle text input" do
            fromSessionValue @String "test" `shouldBe` Right "test"
    describe "fromSessionValue equel toSessionValue on correct input" do
        it "with text input" do
            conversion @String "test" `shouldBe` Right "test"

sessionValueByteString = describe "ByteString" do
    describe "toSessionValue" do
        it "should handle text input" do
            toSessionValue @ByteString "test" `shouldBe` "test"
    describe "fromSessionValue" do
        it "should handle text input" do
            fromSessionValue @ByteString "test" `shouldBe` Right "test"
    describe "fromSessionValue equel toSessionValue on correct input" do
        it "with text input" do
            conversion @ByteString "test" `shouldBe` Right "test"

sessionValueUUID = describe "UUID" do
    let uuid = "6188329c-6bad-47f6-800c-2fd19ce0b2df" :: UUID
    let uuidText = "6188329c-6bad-47f6-800c-2fd19ce0b2df" :: Text
    describe "toSessionValue" do
        it "should handle correct uuid" do
            toSessionValue @UUID uuid `shouldBe` uuidText
    describe "fromSessionValue" do
        it "should handle correct uuid" do
            fromSessionValue @UUID uuidText `shouldBe` Right uuid
        it "should fail on invalid input" do
            fromSessionValue @UUID "not a uuid" `shouldSatisfy` isLeft
    describe "fromSessionValue equel toSessionValue on correct input" do
        it "with correct uuid" do
            conversion @UUID uuid `shouldBe` Right uuid

sessionValueRecordId = describe "RecordId" do
    let intId = 100 :: Int
    let intText = "100" :: Text
    let uuidId = "a020ba17-a94e-453f-9414-c54aa30caa54" :: UUID
    let uuidText = "a020ba17-a94e-453f-9414-c54aa30caa54" :: Text
    let post = Post { id = Id intId }
    let user = User { id = Id uuidId }
    describe "toSessionValue" do
        it "should handle records with int id" do
            toSessionValue @(Id Post) (get #id post) `shouldBe` intText
        it "should handle records with uuid id" do
            toSessionValue @(Id User) (get #id user) `shouldBe` uuidText
    describe "fromSessionValue" do
        it "should handle records with int id" do
            fromSessionValue @(Id Post) intText `shouldBe` Right (get #id post)
        it "should handle records with uuid id" do
            fromSessionValue @(Id User) uuidText `shouldBe` Right (get #id user)
    describe "fromSessionValue equel toSessionValue on correct input" do
        it "with int id" do
            conversion @(Id Post) (get #id post) `shouldBe` Right (get #id post)
        it "with uuid id" do
            conversion @(Id User) (get #id user) `shouldBe` Right (get #id user)

conversion :: forall value . SessionValue value => value -> Either Text value
conversion = fromSessionValue @value . toSessionValue @value

data Post = Post { id :: (Id' "posts") }
type instance PrimaryKey "posts" = Int
type instance GetTableName Post = "posts"

data User = User { id :: (Id' "users") }
type instance PrimaryKey "users" = UUID
type instance GetTableName User = "users"
