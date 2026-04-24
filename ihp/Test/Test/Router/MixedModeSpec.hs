{-|
Module: Test.Router.MixedModeSpec
Copyright: (c) digitally induced GmbH, 2026

Proves a single IHP app can mix controllers using the legacy 'AutoRoute'
and controllers using the new @[routes|…|]@ DSL.
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Router.MixedModeSpec where

import Test.Hspec
import IHP.Prelude
import IHP.RouterSupport
import IHP.Router.DSL (routes)
import IHP.Router.Capture (renderCapture)
import IHP.Router.UrlGenerator (HasPath (..))
import IHP.ControllerPrelude
import Network.HTTP.Types.Method (StdMethod (..))
import Control.Applicative (empty)

---------------------------------------------------------------------------
-- A controller that uses the legacy AutoRoute path
---------------------------------------------------------------------------

data OldController
    = OldIndexAction
    | OldShowAction { thingId :: Int }
    deriving (Eq, Show, Data)

instance Controller OldController where
    action OldIndexAction = renderPlain "old index"
    action OldShowAction { thingId } = renderPlain (cs (tshow thingId))

instance AutoRoute OldController

---------------------------------------------------------------------------
-- A controller that uses the new DSL
---------------------------------------------------------------------------

data NewController
    = NewIndexAction
    | NewShowAction { itemId :: Int }
    deriving (Eq, Show)

instance Controller NewController where
    action NewIndexAction = renderPlain "new index"
    action NewShowAction { itemId } = renderPlain (cs (tshow itemId))

$(pure [])

[routes|NewController
GET /new-items          NewIndexAction
GET /new-items/{itemId} NewShowAction
|]

---------------------------------------------------------------------------
-- Wire both into one FrontController
---------------------------------------------------------------------------

data MixedApplication = MixedApplication deriving (Eq, Show, Data)

instance FrontController MixedApplication where
    controllers =
        [ parseRoute @OldController   -- AutoRoute
        , parseRoute @NewController   -- [routes|…|]
        ]

instance InitControllerContext MixedApplication where
    initContext = pure ()

tests = do
    describe "Mixed routing styles in one application" do
        describe "pathTo" do
            it "works for AutoRoute controllers" do
                pathTo OldIndexAction `shouldBe` "/OldIndex"

            it "works for DSL controllers" do
                pathTo NewIndexAction `shouldBe` "/new-items"
                pathTo (NewShowAction { itemId = 99 }) `shouldBe` "/new-items/99"

        describe "FrontController wiring" do
            it "type-checks with both routing styles in the controllers list" do
                -- If this module compiles, the types are compatible.
                -- The runtime integration (WAI dispatch of both styles in
                -- one app) is proven by Phase 3's mixed test apps.
                True `shouldBe` True
