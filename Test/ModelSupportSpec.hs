{-|
Module: Test.HaskellSupportSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.ModelSupportSpec where

import Test.Hspec
import IHP.Prelude
import IHP.ModelSupport
import qualified Data.Aeson as Aeson

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

