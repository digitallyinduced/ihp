module Test.IDE.SourceInfoSpec where

import Test.Hspec
import IHP.Prelude
import IHP.IDE.SourceInfo

tests :: Spec
tests = do
    describe "formatSourceInfo" do
        it "formats NixStore source info" do
            let info = NixStore "/nix/store/abc123-ihp-ide-1.3.0"
            formatSourceInfo info `shouldBe` "IHP Source: Nix store (/nix/store/abc123-ihp-ide-1.3.0)"

        it "formats LocalCheckout with git info" do
            let info = LocalCheckout "/home/user/IHP" (Just "feature-x") (Just "abc1234")
            formatSourceInfo info `shouldBe` "IHP Source: Local checkout (/home/user/IHP) on branch feature-x at commit abc1234"

        it "formats LocalCheckout without git info" do
            let info = LocalCheckout "/home/user/IHP" Nothing Nothing
            formatSourceInfo info `shouldBe` "IHP Source: Local checkout (/home/user/IHP)"

        it "formats LocalCheckout with branch but no commit" do
            let info = LocalCheckout "/home/user/IHP" (Just "main") Nothing
            formatSourceInfo info `shouldBe` "IHP Source: Local checkout (/home/user/IHP) on branch main"

        it "formats LocalCheckout with commit but no branch" do
            let info = LocalCheckout "/home/user/IHP" Nothing (Just "abc1234")
            formatSourceInfo info `shouldBe` "IHP Source: Local checkout (/home/user/IHP) at commit abc1234"
