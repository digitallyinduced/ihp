{-|
Module: Test.FileStorage.MimeTypesSpec
Copyright: (c) digitally induced GmbH, 2021
-}
module Test.FileStorage.MimeTypesSpec where

import Test.Hspec
import IHP.Prelude
import IHP.FileStorage.MimeTypes

tests = do
    describe "IHP.FileStorage.MimeTypes" do
        describe "guessMimeType" do
            it "return common mime types" do
                guessMimeType "test.jpg" `shouldBe` "image/jpeg"
                guessMimeType "test.txt" `shouldBe` "text/plain"
            it "returns application/octet-stream for unknown files" do
                guessMimeType "unknown" `shouldBe` "application/octet-stream"
                guessMimeType "test.hztr43ed" `shouldBe` "application/octet-stream"
                guessMimeType "" `shouldBe` "application/octet-stream"