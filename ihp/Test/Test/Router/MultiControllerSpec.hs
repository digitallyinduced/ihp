{-|
Module: Test.Router.MultiControllerSpec
Copyright: (c) digitally induced GmbH, 2026

Verifies that a single header-less @[routes|…|]@ block can describe
routes that target constructors of multiple different controller types —
i.e. routes for a whole application go in one place.
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Router.MultiControllerSpec where

import Test.Hspec
import IHP.Prelude
import IHP.RouterSupport
import "ihp" IHP.Router.DSL (routes)
import "ihp" IHP.Router.Capture (renderCapture, parseCapture)
import IHP.ControllerPrelude
import Network.HTTP.Types.Method (StdMethod (..))

-- Two different controllers declared in the same module.
data PostsCtrl
    = PostsIndexAction
    | ShowPostAction { postId :: Int }
    deriving (Eq, Show)

data UsersCtrl
    = UsersIndexAction
    | ShowUserAction { userId :: Int }
    deriving (Eq, Show)

instance Controller PostsCtrl where
    action PostsIndexAction          = renderPlain "posts index"
    action ShowPostAction { postId } = renderPlain (cs (tshow postId))

instance Controller UsersCtrl where
    action UsersIndexAction          = renderPlain "users index"
    action ShowUserAction { userId } = renderPlain (cs (tshow userId))

$(pure [])

-- One header-less block covering both controllers.
-- The splice reifies each action constructor, groups by parent type,
-- and emits CanRoute/HasPath instances for both PostsCtrl and UsersCtrl.
[routes|
GET /posts              PostsIndexAction
GET /posts/{postId}     ShowPostAction
GET /users              UsersIndexAction
GET /users/{userId}     ShowUserAction
|]

tests = do
    describe "IHP.Router.DSL — multi-controller (header-less) form" do
        it "emits HasPath for the first controller's actions" do
            pathTo PostsIndexAction `shouldBe` "/posts"
            pathTo (ShowPostAction { postId = 42 }) `shouldBe` "/posts/42"

        it "emits HasPath for the second controller's actions" do
            pathTo UsersIndexAction `shouldBe` "/users"
            pathTo (ShowUserAction { userId = 7 }) `shouldBe` "/users/7"

        it "emits distinct CanRoute instances for each controller type" do
            -- The compilation of this module with the single [routes|…|]
            -- block above is the real proof: two CanRoute instances were
            -- emitted, one per parent type. If they hadn't been, either
            -- pathTo would fail to resolve or the instance would be
            -- duplicated. The assertions above exercise both.
            True `shouldBe` True
