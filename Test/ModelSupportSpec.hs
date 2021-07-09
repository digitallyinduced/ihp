{-|
Module: Test.HaskellSupportSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.ModelSupportSpec where

import Test.Hspec
import IHP.Prelude
import IHP.ModelSupport
import qualified Data.Aeson as Aeson
import qualified Data.Dynamic as Dynamic
import Text.Read (read)

tests = do
    describe "ModelSupport" do
        describe "InputValue" do
            describe "Text" do
                it "should return the text" do
                    let text :: Text = "Lorem Ipsum"
                    (inputValue text) `shouldBe` text
            
            describe "Int" do
                it "should return numeric representation" do
                    (inputValue (1 :: Int)) `shouldBe` "1"
            
            describe "Integer" do
                it "should return numeric representation" do
                    (inputValue (1 :: Int)) `shouldBe` "1"
            
            describe "Double" do
                it "should return numeric representation" do
                    (inputValue (1.337 :: Double)) `shouldBe` "1.337"
            
            describe "Float" do
                it "should return numeric representation" do
                    (inputValue (1.337 :: Float)) `shouldBe` "1.337"
            
            describe "Bool" do
                it "should deal with True and False" do
                    (inputValue True) `shouldBe` "on"
                    (inputValue False) `shouldBe` "off"
            
            describe "UUID" do
                it "should return text representation" do
                    let uuid :: UUID = "b9a1129e-e53a-4370-b778-0d1c28dc6ecc"
                    (inputValue uuid) `shouldBe` "b9a1129e-e53a-4370-b778-0d1c28dc6ecc"
            
            describe "UTCTime" do
                it "should return text representation" do
                    let (Just utctime) :: Maybe UTCTime = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" "2020-11-08T12:03:35Z"
                    (inputValue utctime) `shouldBe` "2020-11-08T12:03:35Z"
            
            describe "Day" do
                it "should return text representation" do
                    let (Just utctime) :: Maybe UTCTime = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" "2020-11-08T12:03:35Z"
                    let day :: Day = utctDay utctime
                    (inputValue day) `shouldBe` "2020-11-08"

            describe "TimeOfDay" do
                it "should return text representation" do
                    let timeOfDay :: TimeOfDay = read "12:00:00"
                    (inputValue timeOfDay) `shouldBe` "12:00:00"
            
            describe "Maybe" do
                it "should return empty string on Nothing" do
                    let value :: Maybe Int = Nothing
                    (inputValue value) `shouldBe` ""
                
                it "should return the value if Just" do
                    let value :: Maybe Int = Just 1
                    (inputValue value) `shouldBe` "1"
            
            describe "[a]" do
                it "should return CSV" do
                    let value :: [Int] = [1, 2, 3]
                    (inputValue value) `shouldBe` "1,2,3"
                
                it "should return empty string on empty list" do
                    let value :: [Int] = []
                    (inputValue value) `shouldBe` ""
            
            describe "JSON" do
                it "should return a json string for a Aeson.Value" do
                    let (Just value) :: Maybe Aeson.Value = Aeson.decode "{\"hello\":true}"
                    (inputValue value) `shouldBe` "{\"hello\":true}"

        describe "didChange" do
            let project = Project { id = 1337, name = "Test", meta = def { originalDatabaseRecord = Just (Dynamic.toDyn project) } }
            
            it "should return False for a new record" do
                (project |> didChange #name) `shouldBe` False
                (project |> didChange #id) `shouldBe` False

            it "should return True for a changed field" do
                (project |> set #name "Changed" |> didChange #name) `shouldBe` True
                (project |> didChange #id) `shouldBe` False
            
            it "should return false for a changed field when set with the same value again" do
                (project |> set #name "Test" |> didChange #name) `shouldBe` False
                (project |> didChange #id) `shouldBe` False

data Project = Project { id :: Int, name :: Text, meta :: MetaBag }
instance SetField "id" Project Int where
    setField value project@(Project { id, name, meta }) = project { Test.ModelSupportSpec.id = value, meta = meta { touchedFields = "id":(touchedFields meta) } }
instance SetField "name" Project Text where
    setField value project@(Project { id, name, meta }) = project { name = value, meta = meta { touchedFields = "name":(touchedFields meta) } }