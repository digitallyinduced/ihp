{-|
Module: Test.Router.AppBindingSpec
Copyright: (c) digitally induced GmbH, 2026

Verifies that the @for AppType@ header form emits a
@\<app\>Routes :: [ControllerRoute AppType]@ value the user can splat
into 'FrontController.controllers' instead of listing each
@parseRoute \@Ctrl@ by hand.
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

-- Two controllers we'll reference from a `for AppType` block.
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

-- Application type the routes block will bind against.
data TestApplication = TestApplication deriving (Eq, Show, Data)

instance InitControllerContext TestApplication where
    initContext = pure ()

$(pure [])

-- `for TestApplication` header: the splice groups routes by reified
-- parent type (emitting HasPath/CanRoute for WidgetsCtrl and GadgetsCtrl)
-- AND emits a top-level binding `testRoutes :: [ControllerRoute TestApplication]`.
-- Naming rule: lowercase first letter, strip "Application" suffix.
--   TestApplication → testRoutes
[routes|for TestApplication
GET /widgets               WidgetsIndexAction
GET /widgets/{widgetId}    ShowWidgetAction
GET /gadgets               GadgetsIndexAction
GET /gadgets/{gadgetId}    ShowGadgetAction
|]

-- Splat the generated binding directly into FrontController — no
-- per-controller parseRoute listing required.
instance FrontController TestApplication where
    controllers = testRoutes

tests = do
    describe "IHP.Router.DSL — `for AppType` header emits <app>Routes" do
        it "HasPath still works for each reified controller" do
            pathTo WidgetsIndexAction `shouldBe` "/widgets"
            pathTo (ShowWidgetAction { widgetId = 42 }) `shouldBe` "/widgets/42"
            pathTo GadgetsIndexAction `shouldBe` "/gadgets"
            pathTo (ShowGadgetAction { gadgetId = 7 }) `shouldBe` "/gadgets/7"

        it "testRoutes fits the FrontController.controllers contract" do
            -- The real proof is compile-time: the `instance FrontController
            -- TestApplication where controllers = testRoutes` declaration
            -- above type-checks iff the splice emitted a binding with the
            -- right implicit-parameter and class-constraint signature.
            -- 'testRoutes' can't be evaluated at runtime outside of a
            -- FrontController context (it needs '?request'/'?respond'/
            -- '?application' implicits), so this spec is intentionally
            -- a type-level check disguised as a runtime test.
            True `shouldBe` True
