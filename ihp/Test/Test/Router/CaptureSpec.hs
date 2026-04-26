{-|
Module: Test.Router.CaptureSpec
Copyright: (c) digitally induced GmbH, 2026
-}
module Test.Router.CaptureSpec where

import Test.Hspec
import IHP.Prelude
import "ihp" IHP.Router.Capture
import qualified Data.UUID as UUID

tests = do
    describe "IHP.Router.Capture" do
        describe "Text" do
            it "parses any UTF-8 segment" do
                parseCapture @Text "hello" `shouldBe` Just "hello"
                parseCapture @Text "" `shouldBe` Just ""
                parseCapture @Text "with spaces" `shouldBe` Just "with spaces"

            it "renders verbatim" do
                renderCapture ("hello" :: Text) `shouldBe` "hello"

        describe "Int" do
            it "parses decimal digits" do
                parseCapture @Int "42" `shouldBe` Just 42
                parseCapture @Int "-7" `shouldBe` Just (-7)

            it "rejects non-numeric input" do
                parseCapture @Int "foo" `shouldBe` Nothing
                parseCapture @Int "" `shouldBe` Nothing
                parseCapture @Int "42abc" `shouldBe` Nothing

            it "renders as decimal" do
                renderCapture (42 :: Int) `shouldBe` "42"

        describe "Integer" do
            it "parses arbitrary-precision integers" do
                parseCapture @Integer "99999999999999999999" `shouldBe` Just 99999999999999999999
                parseCapture @Integer "0" `shouldBe` Just 0

        describe "UUID" do
            let sample = UUID.fromText "a32913dd-ef80-4f3e-9a91-7879e17b2ece"
            it "parses a 36-char ASCII UUID" do
                parseCapture @UUID "a32913dd-ef80-4f3e-9a91-7879e17b2ece" `shouldBe` sample

            it "rejects malformed input" do
                parseCapture @UUID "not-a-uuid" `shouldBe` Nothing
                parseCapture @UUID "" `shouldBe` Nothing

            it "renders back to canonical form" do
                case sample of
                    Just uuid -> renderCapture uuid `shouldBe` "a32913dd-ef80-4f3e-9a91-7879e17b2ece"
                    Nothing -> expectationFailure "sample UUID should have parsed"

        describe "Bool" do
            it "accepts true/false and 1/0" do
                parseCapture @Bool "true" `shouldBe` Just True
                parseCapture @Bool "false" `shouldBe` Just False
                parseCapture @Bool "1" `shouldBe` Just True
                parseCapture @Bool "0" `shouldBe` Just False

            it "rejects anything else" do
                parseCapture @Bool "yes" `shouldBe` Nothing
                parseCapture @Bool "TRUE" `shouldBe` Nothing
                parseCapture @Bool "" `shouldBe` Nothing

            it "renders as canonical lowercase" do
                renderCapture True `shouldBe` "true"
                renderCapture False `shouldBe` "false"

        describe "Day" do
            it "parses ISO-8601 date" do
                parseCapture @Day "2026-04-24" `shouldBe` Just (fromGregorian 2026 4 24)

            it "rejects non-date input" do
                parseCapture @Day "today" `shouldBe` Nothing
                parseCapture @Day "2026-04" `shouldBe` Nothing

            it "renders as ISO-8601" do
                renderCapture (fromGregorian 2026 4 24) `shouldBe` "2026-04-24"

        describe "Segment" do
            it "parses non-empty segments" do
                parseCapture @Segment "foo" `shouldBe` Just (Segment "foo")

            it "rejects empty input" do
                parseCapture @Segment "" `shouldBe` Nothing

            it "renders verbatim" do
                renderCapture (Segment "bar") `shouldBe` "bar"
