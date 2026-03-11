module Test.IDE.SourceInfoSpec where

import Test.Hspec
import IHP.Prelude
import IHP.IDE.SourceInfo
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS

tests :: Spec
tests = do
    describe "formatSourceInfo" do
        it "formats NixStore with branch and revision" do
            let info = NixStore "/nix/store/abc123-ihp-env-var-backwards-compat" (Just "ihp-source-info") (Just "49b5daf226bf74d106247269e1270f16e14090e8")
            formatSourceInfo info `shouldBe` "IHP Source: Nix store (/nix/store/abc123-ihp-env-var-backwards-compat) on branch ihp-source-info at revision 49b5daf226bf74d106247269e1270f16e14090e8"

        it "formats NixStore without flake.lock info" do
            let info = NixStore "/nix/store/abc123-ihp-env-var-backwards-compat" Nothing Nothing
            formatSourceInfo info `shouldBe` "IHP Source: Nix store (/nix/store/abc123-ihp-env-var-backwards-compat)"

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

    describe "parseFlakeLockJson" do
        it "extracts branch and revision from valid flake.lock" do
            parseFlakeLockJson sampleFlakeLock `shouldBe` (Just "master", Just "abc123def456")

        it "returns Nothing for missing ihp input" do
            parseFlakeLockJson noIhpFlakeLock `shouldBe` (Nothing, Nothing)

        it "returns Nothing for invalid JSON" do
            parseFlakeLockJson "not json" `shouldBe` (Nothing, Nothing)

        it "handles missing ref in original" do
            parseFlakeLockJson noRefFlakeLock `shouldBe` (Nothing, Just "abc123def456")

sampleFlakeLock :: LBS.ByteString
sampleFlakeLock = Aeson.encode $ Aeson.object
    [ "nodes" .= Aeson.object
        [ "ihp" .= Aeson.object
            [ "original" .= Aeson.object ["ref" .= ("master" :: Text), "type" .= ("github" :: Text)]
            , "locked" .= Aeson.object ["rev" .= ("abc123def456" :: Text), "type" .= ("github" :: Text)]
            ]
        ]
    ]

noIhpFlakeLock :: LBS.ByteString
noIhpFlakeLock = Aeson.encode $ Aeson.object
    [ "nodes" .= Aeson.object
        [ "nixpkgs" .= Aeson.object
            [ "locked" .= Aeson.object ["rev" .= ("xyz" :: Text)]
            ]
        ]
    ]

noRefFlakeLock :: LBS.ByteString
noRefFlakeLock = Aeson.encode $ Aeson.object
    [ "nodes" .= Aeson.object
        [ "ihp" .= Aeson.object
            [ "original" .= Aeson.object ["type" .= ("github" :: Text)]
            , "locked" .= Aeson.object ["rev" .= ("abc123def456" :: Text), "type" .= ("github" :: Text)]
            ]
        ]
    ]
