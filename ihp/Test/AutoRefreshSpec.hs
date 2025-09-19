{-|
Module: Test.AutoRefreshSpec

Unit tests for Auto Refresh helpers.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.AutoRefreshSpec where

import Test.Hspec
import IHP.Prelude
import qualified Data.UUID as UUID
import IHP.AutoRefresh
import IHP.AutoRefresh.View
import IHP.AutoRefresh.Types
import IHP.Controller.Context
import Text.Blaze.Html.Renderer.Text (renderHtml)

import IHP.HSX.QQ (hsx)
import qualified Text.Blaze.Html5 as Html5

renderMeta :: (?context :: ControllerContext) => Text
renderMeta = cs (renderHtml autoRefreshMeta)

withFreshContext :: (ControllerContext -> IO a) -> IO a
withFreshContext block = do
    let ?requestContext = undefined
    context <- newControllerContext
    block context

freezeContext :: ControllerContext -> IO ControllerContext
freezeContext = freeze

tests :: Spec
tests = describe "AutoRefresh" do
    it "stores AutoRefreshTarget in the controller context" do
        withFreshContext \context -> do
            let ?context = context
            setAutoRefreshTarget "#chat-pane"
            fromContext @AutoRefreshTarget `shouldReturn` AutoRefreshTarget "#chat-pane"

    it "renders meta tag with target attribute when target is set" do
        withFreshContext \context -> do
            let ?context = context
            putContext AutoRefreshDisabled
            putContext (AutoRefreshEnabled UUID.nil)
            setAutoRefreshTarget "#chat-pane"
            frozen <- freezeContext ?context
            let ?context = frozen
            renderMeta `shouldBe` "<meta property=\"ihp-auto-refresh-id\" content=\"00000000-0000-0000-0000-000000000000\" data-ihp-auto-refresh-target=\"#chat-pane\">"

    it "renders meta tag without target attribute when no target is set" do
        withFreshContext \context -> do
            let ?context = context
            putContext AutoRefreshDisabled
            putContext (AutoRefreshEnabled UUID.nil)
            frozen <- freezeContext ?context
            let ?context = frozen
            renderMeta `shouldBe` "<meta property=\"ihp-auto-refresh-id\" content=\"00000000-0000-0000-0000-000000000000\">"

    it "renders nothing when auto refresh is disabled" do
        withFreshContext \context -> do
            let ?context = context
            putContext AutoRefreshDisabled
            frozen <- freezeContext ?context
            let ?context = frozen
            renderMeta `shouldBe` ""
