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
