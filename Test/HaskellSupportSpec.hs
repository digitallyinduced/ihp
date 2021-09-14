{-|
Module: Test.HaskellSupportSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.HaskellSupportSpec where

import Test.Hspec
import IHP.Prelude
import IHP.HaskellSupport

tests = do
    describe "HaskellSupport" do
        describe "stripTags" do
            it "should deal with empty input" do
                stripTags "" `shouldBe` ""

            it "should strip html tags and return the plain text" do
                stripTags "This is <b>Bold</b>" `shouldBe` "This is Bold"

        describe "copyFields" do
            it "should copy fields" do
                let a = RecordA { fieldA = 1, fieldB = "test" }
                let b = RecordB { fieldA = 0, fieldB = "" }

                let b' = b |> copyFields @["fieldA", "fieldB"] a

                b' `shouldBe` RecordB { fieldA = 1, fieldB = "test" }

        describe "allEnumValues" do
            it "should return all enum values" do
                (allEnumValues @Color) `shouldBe` [Yellow, Red, Blue]

        describe "setIfJust" do
            it "should set a Just value" do
                let c = RecordC { field = Nothing }
                let c' = c |> setIfJust #field (Just 1)

                c' `shouldBe` RecordC { field = Just 1 }

            it "should not set a Nothing value" do
                let c = RecordC { field = Just 1 }
                let c' = c |> setIfJust #field Nothing

                c' `shouldBe` RecordC { field = Just 1 }


data RecordA = RecordA { fieldA :: Int, fieldB :: Text } deriving (Eq, Show)
data RecordB = RecordB { fieldA :: Int, fieldB :: Text } deriving (Eq, Show)
data RecordC = RecordC { field :: Maybe Int } deriving (Eq, Show)

instance SetField "fieldA" RecordB Int where
    setField value record = record { fieldA = value }

instance SetField "fieldB" RecordB Text where
    setField value record = record { fieldB = value }

instance SetField "field" RecordC (Maybe Int) where
    setField value record = record { field = value }

data Color = Yellow | Red | Blue deriving (Enum, Show, Eq)
