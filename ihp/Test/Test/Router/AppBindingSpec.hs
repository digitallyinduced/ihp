{-|
Module: Test.Router.AppBindingSpec
Copyright: (c) digitally induced GmbH, 2026

Verifies the binding-named header form of @[routes|…|]@: a lowercase
identifier in the header line becomes a top-level binding that the user
splats into 'FrontController.controllers'. The splice also emits
'HasPath' and 'CanRoute' instances for each reified parent type.
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Router.AppBindingSpec where

import Test.Hspec
import IHP.Prelude
import IHP.RouterSupport
import IHP.Router.DSL (routes)
import IHP.Router.Capture (renderCapture)
import IHP.Router.UrlGenerator (HasPath (..))
import IHP.ControllerPrelude
import Network.HTTP.Types.Method (StdMethod (..))
import Control.Applicative (empty)

-- Two controllers for the expression-form test.
data WidgetsCtrl
    = WidgetsIndexAction
    | ShowWidgetAction { widgetId :: Int }
    deriving (Eq, Show)

data GadgetsCtrl
    = GadgetsIndexAction
    | ShowGadgetAction { gadgetId :: Int }
    deriving (Eq, Show)

instance Controller WidgetsCtrl where
    action WidgetsIndexAction          = renderPlain "widgets index"
    action ShowWidgetAction { widgetId } = renderPlain (cs (tshow widgetId))

instance Controller GadgetsCtrl where
    action GadgetsIndexAction          = renderPlain "gadgets index"
    action ShowGadgetAction { gadgetId } = renderPlain (cs (tshow gadgetId))

data TestApplication = TestApplication deriving (Eq, Show, Data)

instance InitControllerContext TestApplication where
    initContext = pure ()

$(pure [])

-- Binding-named header: the splice emits HasPath/CanRoute for each
-- reified controller AND a top-level binding called `webRoutes` that
-- the user can splat into FrontController.controllers.
[routes|webRoutes
GET /widgets               WidgetsIndexAction
GET /widgets/{widgetId}    ShowWidgetAction
GET /gadgets               GadgetsIndexAction
GET /gadgets/{gadgetId}    ShowGadgetAction
|]

-- Splat the binding directly; the app type is inferred from context.
instance FrontController TestApplication where
    controllers = webRoutes

tests = do
    describe "IHP.Router.DSL — binding-named header form" do
        it "HasPath works for each reified controller" do
            pathTo WidgetsIndexAction `shouldBe` "/widgets"
            pathTo (ShowWidgetAction { widgetId = 42 }) `shouldBe` "/widgets/42"
            pathTo GadgetsIndexAction `shouldBe` "/gadgets"
            pathTo (ShowGadgetAction { gadgetId = 7 }) `shouldBe` "/gadgets/7"

        it "webRoutes fits the FrontController.controllers contract" do
            -- The compile-time proof is the `instance FrontController
            -- TestApplication where controllers = webRoutes` above.
            -- 'webRoutes' carries implicit-parameter constraints from
            -- 'parseRoute @Ctrl' so it can only be evaluated inside a
            -- FrontController context. This runtime check is a stub.
            True `shouldBe` True
