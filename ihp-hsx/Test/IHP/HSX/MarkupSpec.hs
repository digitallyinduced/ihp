{-|
Module: IHP.HSX.MarkupSpec
Copyright: (c) digitally induced GmbH, 2024
-}
module IHP.HSX.MarkupSpec where

import Test.Hspec
import Prelude
import Data.Text (Text)
import Data.String.Conversions (cs)
import IHP.HSX.Markup

tests :: SpecWith ()
tests = do
    describe "IHP.HSX.Markup" do
        describe "isEmpty" do
            it "should return True for mempty" do
                isEmpty (mempty :: Html) `shouldBe` True

            it "should return True for empty string literal" do
                isEmpty ("" :: Html) `shouldBe` True

            it "should return False for non-empty markup" do
                isEmpty (escapeHtml "hello") `shouldBe` False

            it "should return False for raw bytestring content" do
                isEmpty (rawByteString "<div></div>") `shouldBe` False

            it "should return True for concatenation of empty values" do
                isEmpty (mempty <> mempty :: Html) `shouldBe` True

            it "should return False for concatenation with non-empty value" do
                isEmpty (mempty <> escapeHtml "x") `shouldBe` False

        describe "ConvertibleStrings" do
            it "converts Text to Html via cs" do
                renderMarkupText (cs ("hello" :: Text) :: Html) `shouldBe` "hello"

            it "HTML-escapes Text when converted via cs" do
                renderMarkupText (cs ("<script>alert(1)</script>" :: Text) :: Html)
                    `shouldBe` "&lt;script&gt;alert(1)&lt;/script&gt;"

            it "converts String to Html via cs" do
                renderMarkupText (cs ("hello" :: String) :: Html) `shouldBe` "hello"

            it "HTML-escapes String when converted via cs" do
                renderMarkupText (cs ("a & b" :: String) :: Html) `shouldBe` "a &amp; b"
