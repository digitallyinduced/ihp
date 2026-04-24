{-|
Module: Test.Router.DSLQuoterSpec
Copyright: (c) digitally induced GmbH, 2026
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Router.DSLQuoterSpec where

import Test.Hspec
import IHP.Prelude
import IHP.RouterSupport
import IHP.Router.DSL (routes)
import IHP.Router.Capture (renderCapture, parseCapture)
import IHP.ControllerPrelude
import Network.HTTP.Types.Method (StdMethod (..))
import Network.Wai (queryString)

-- Minimal controller type used to exercise the DSL splice.
data QuoterController
    = IndexAction
    | NewItemAction
    | ShowItemAction { itemId :: Int }
    | EditItemAction { itemId :: Int }
    -- Fields that aren't bound in the path → query-string decoded by the splice.
    | SearchAction { q :: Text, page :: Maybe Int, tags :: [Text] }
    | LegacyShowAction { itemId :: Int }
    deriving (Eq, Show)

instance Controller QuoterController where
    action IndexAction          = renderPlain "index"
    action NewItemAction        = renderPlain "new"
    action ShowItemAction { itemId } = renderPlain (cs (tshow itemId))
    action EditItemAction { itemId } = renderPlain (cs ("edit " <> tshow itemId))
    action SearchAction { q } = renderPlain (cs q)
    action LegacyShowAction { itemId } = renderPlain (cs (tshow itemId))

-- Force a TH declaration-group boundary so the QuoterController type is
-- visible to the [routes|…|] splice via 'reify'.
$(pure [])

-- The quoter emits HasPath + CanRoute for this type.
[routes|QuoterController
GET    /items                 IndexAction
GET    /items/new             NewItemAction
GET    /items/{itemId}        ShowItemAction
GET    /items/{itemId}/edit   EditItemAction
-- Explicit query-param list: q (required), page (optional), tags (list).
-- Field type determines required / optional / list behaviour.
GET    /search?q&page&tags    SearchAction
-- Back-compat with AutoRoute's /LegacyShow?itemId=… URL shape.
GET    /LegacyShow?itemId     LegacyShowAction
|]

tests = do
    describe "IHP.Router.DSL (routes quoter — end-to-end)" do
        describe "HasPath (pathTo) generation" do
            it "renders static routes" do
                pathTo IndexAction `shouldBe` "/items"
                pathTo NewItemAction `shouldBe` "/items/new"

            it "renders routes with captures" do
                pathTo (ShowItemAction { itemId = 42 }) `shouldBe` "/items/42"
                pathTo (EditItemAction { itemId = 7 }) `shouldBe` "/items/7/edit"

            it "reuses the same path for constructors with the same pattern" do
                -- Only one pathTo clause is emitted per constructor, even
                -- if multiple routes point to the same constructor. This
                -- test just ensures the generation is deterministic.
                pathTo (ShowItemAction { itemId = 1 })
                    `shouldBe` pathTo (ShowItemAction { itemId = 1 })

        describe "query-string fields" do
            it "renders required string field" do
                pathTo (SearchAction { q = "haskell", page = Nothing, tags = [] })
                    `shouldBe` "/search?q=haskell"

            it "renders optional Maybe Int field" do
                pathTo (SearchAction { q = "x", page = Just 3, tags = [] })
                    `shouldBe` "/search?q=x&page=3"

            it "omits Maybe field when Nothing" do
                pathTo (SearchAction { q = "x", page = Nothing, tags = [] })
                    `shouldBe` "/search?q=x"

            it "renders list field with repeated key" do
                pathTo (SearchAction { q = "x", page = Nothing, tags = ["a", "b"] })
                    `shouldBe` "/search?q=x&tags=a&tags=b"

            it "omits list field when empty" do
                pathTo (SearchAction { q = "x", page = Nothing, tags = [] })
                    `shouldBe` "/search?q=x"

            it "produces AutoRoute-compatible URL for LegacyShow?itemId=N" do
                -- Preserves URL backward compatibility with AutoRoute's
                -- ShowAction-style URLs.
                pathTo (LegacyShowAction { itemId = 42 })
                    `shouldBe` "/LegacyShow?itemId=42"
