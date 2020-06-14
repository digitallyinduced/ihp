{-|
Module: Test.ValidationSupport.ValidateFieldSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.ValidationSupport.ValidateFieldSpec where

import Test.Hspec
import IHP.Prelude
import IHP.ValidationSupport.ValidateField
import IHP.ValidationSupport.Types
import IHP.ViewPrelude (cs, plain)
import Data.Text (Text, length)

tests = do
    describe "The nonEmpty validator" do
        it "should handle trivial cases" do
            nonEmpty ("hi" :: Text) `shouldBe` Success
            nonEmpty (Just "hi") `shouldBe` Success

            nonEmpty ("" :: Text) `shouldSatisfy` isFailure
            nonEmpty Nothing `shouldSatisfy` isFailure

    describe "The isEmptyValue validator" do
        it "should handle trivial cases" do
            isEmptyValue ("" :: Text) `shouldBe` Success
            isEmptyValue Nothing `shouldBe` Success
            
            isEmptyValue ("hi" :: Text) `shouldSatisfy` isFailure
            isEmptyValue (Just "hi") `shouldSatisfy` isFailure

    describe "The isPhoneNumber validator" do
        it "should handle trivial cases" do
            isPhoneNumber "1337" `shouldSatisfy` isFailure
            isPhoneNumber "+1337" `shouldSatisfy` isFailure
            isPhoneNumber "0049123456789" `shouldSatisfy` isFailure
            isPhoneNumber "+49123456789" `shouldBe` Success
            
    describe "The isEmail validator" do
        it "should handle trivial cases" do
            isEmail "foo@example.com" `shouldBe` Success
            isEmail "foobar" `shouldSatisfy` isFailure

        it "should allow subdomains" do
            isEmail "foo@bar.example.com" `shouldBe` Success

        it "should allow + chars in the user part" do
            isEmail "foo+bar@example.com" `shouldBe` Success

        it "should reject + chars in the server part" do
            isEmail "foo@bar+example.com" `shouldSatisfy` isFailure
            isEmail "foo@example.bar+com" `shouldSatisfy` isFailure

        it "should allow Unicode characters" do
            isEmail "ॐ@मणिपद्मे.हूँ" `shouldBe` Success
            
        it "should reject servers consisting of just TLD" do
            isEmail "foo@localhost" `shouldSatisfy` isFailure

    describe "The isInRange validator" do
        it "should handle trivial cases" do
            isInRange (10, 20) 15 `shouldBe` Success
            isInRange (0, 10) 0 `shouldBe` Success
            isInRange (0, 10) 10 `shouldBe` Success
            isInRange ((-10), 0) (-10) `shouldBe` Success

            isInRange (10, 20) 21 `shouldSatisfy` isFailure
            isInRange (10, 20) 9 `shouldSatisfy` isFailure

    describe "The isLessThan validator" do
        it "should handle trivial cases" do
            isLessThan 10 9 `shouldBe` Success
            isLessThan 10 (-10) `shouldBe` Success

            isLessThan 10 10 `shouldSatisfy` isFailure
            isLessThan 10 11 `shouldSatisfy` isFailure

    describe "The isGreaterThan validator" do
        it "should handle trivial cases" do
            isGreaterThan 10 11 `shouldBe` Success
            isGreaterThan (-10) 0 `shouldBe` Success

            isGreaterThan 10 10 `shouldSatisfy` isFailure
            isGreaterThan 10 9 `shouldSatisfy` isFailure

    describe "The hasMaxLength validator" do
        it "should handle trivial cases" do
            hasMaxLength 10 "foo" `shouldBe` Success
            hasMaxLength 3 "foo" `shouldBe` Success

            hasMaxLength 2 "foo" `shouldSatisfy` isFailure

        it "should handle Umlauts" do
            hasMaxLength 7 "Rösrath" `shouldBe` Success
            hasMaxLength 6 "Rösrath" `shouldSatisfy` isFailure

        {-    
        it "should handle Unicode" do
            -- haha, not possible :-)

            -- What's the length of "ॐमणिपद्मेहूँ"?
            -- Okay, that was easy... what's the length of "🤦🏼‍♂️"?
        -}          
          
    describe "The hasMinLength validator" do
        it "should handle trivial cases" do
            hasMinLength 3 "foo" `shouldBe` Success
            hasMinLength 1 "foo" `shouldBe` Success

            hasMinLength 4 "foo" `shouldSatisfy` isFailure

        it "should handle Umlauts" do
            hasMinLength 7 "Rösrath" `shouldBe` Success
            hasMinLength 8 "Rösrath" `shouldSatisfy` isFailure
            
    describe "The isRgbHexColor validator" do
        it "should handle trivial cases" do
            isRgbHexColor "#ff0000" `shouldBe` Success
            isRgbHexColor "huh?" `shouldSatisfy` isFailure

        it "should handle shorthand syntax" do
            isRgbHexColor "#fff" `shouldBe` Success

        it "should reject rgba values" do
            isRgbHexColor "#ff000000" `shouldSatisfy` isFailure
            isRgbHexColor "#f000" `shouldSatisfy` isFailure

    describe "The isRgbaHexColor validator" do
        it "should handle trivial cases" do
            isRgbaHexColor "#ff000000" `shouldBe` Success
            isRgbaHexColor "huh?" `shouldSatisfy` isFailure

        it "should handle shorthand syntax" do
            isRgbaHexColor "#ffff" `shouldBe` Success

        it "should reject rgb values" do
            isRgbaHexColor "#ff0000" `shouldSatisfy` isFailure
            isRgbaHexColor "#f00" `shouldSatisfy` isFailure

    describe "The isRgbColor validator" do
        it "should handle trivial cases" do
            isRgbColor "rgb(255, 0, 0)" `shouldBe` Success
            isRgbColor "huh?" `shouldSatisfy` isFailure

        it "should accept decimal numbers" do
            isRgbColor "rgb(255, 100.5, .4)" `shouldBe` Success

            isRgbColor "rgb(255, 100., .4)" `shouldSatisfy` isFailure
            isRgbColor "rgb(255, 100.5, .)" `shouldSatisfy` isFailure

    describe "The isRgbaColor validator" do
        it "should handle trivial cases" do
            isRgbaColor "rgba(255, 0, 0, 0)" `shouldBe` Success
            isRgbaColor "huh?" `shouldSatisfy` isFailure

        it "should accept decimal numbers" do
            isRgbaColor "rgba(255, 100.5, .4, 0)" `shouldBe` Success

            isRgbaColor "rgba(255, 100., .4, 0)" `shouldSatisfy` isFailure
            isRgbaColor "rgba(255, 100.5, ., 0)" `shouldSatisfy` isFailure

    describe "The isColor validator" do
        it "should accept hex colors" do
            isColor "#f00" `shouldBe` Success
            isColor "#ff000000" `shouldBe` Success

        it "should accept rgb(a) colors" do
            isColor "rgb(255, 0, 0)" `shouldBe` Success
            isColor "rgba(255, 0, 0, .5)" `shouldBe` Success

    describe "The isUrl validator" do
        it "should handle trivial cases" do
            isUrl "http://ihp.digitallyinduced.com/" `shouldBe` Success
            isUrl "https://ihp.digitallyinduced.com/" `shouldBe` Success

            isUrl "ihp.digitallyinduced.com" `shouldSatisfy` isFailure
            isUrl "gopher://ihp.digitallyinduced.com/" `shouldSatisfy` isFailure

    describe "The validateAny validator" do
        it "should handle trivial cases" do
            validateAny [isEmptyValue, isEmail] "" `shouldBe` Success
            validateAny [isGreaterThan(10), isLessThan(5)] 1 `shouldBe` Success
            validateAny [isGreaterThan(10), isLessThan(5)] 11 `shouldBe` Success
            validateAny [isGreaterThan(10), isLessThan(5)] 7 `shouldSatisfy` isFailure

    describe "The validateAll validator" do
        it "should handle trivial cases" do
            validateAll [isGreaterThan(1900), isLessThan(2020)] 2016 `shouldBe` Success
            validateAll [isGreaterThan(1900), isLessThan(2020)] 2050 `shouldSatisfy` isFailure

    describe "The isInList validator" do
        it "should handle trivial cases" do
            isInList [1954, 1974, 1990, 2014] 2014 `shouldBe` Success
            isInList [1954, 1974, 1990, 2014] 2018 `shouldSatisfy` isFailure

            isInList ["C", "Haskell", "Rust"] "Haskell" `shouldBe` Success
            isInList ["C", "Haskell", "Rust"] "JavaScript" `shouldSatisfy` isFailure -- rightly!
