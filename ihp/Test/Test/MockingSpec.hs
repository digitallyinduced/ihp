{-|
Module: Test.MockingSpec
Tests for IHP.Test.Mocking, ensuring params are correctly delivered
through the middleware stack and that redirect responses preserve
their status codes.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.MockingSpec where
import ClassyPrelude
import Test.Hspec
import IHP.Test.Mocking
import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.ControllerPrelude hiding (get, request)
import IHP.HSX.Markup (Html)
import Network.Wai.Test
import Test.Util (testGet, testPostForm)

import qualified Data.UUID as UUID
import qualified Network.Wai as Wai
import Network.HTTP.Types.Status (status200)
import IHP.LoginSupport.Middleware (authMiddlewareWith)
import IHP.LoginSupport.Types (currentUserVaultKey)
import qualified IHP.LoginSupport.Helper.Controller as LoginController
import Test.LoginSupport.AuthVaultSpec (TestUser (..))

data WebApplication = WebApplication deriving (Eq, Show, Data)

data TestController
    = EchoParamAction
    | RedirectAction
    | RequireUserAction
  deriving (Eq, Show, Data)

instance Controller TestController where
    action EchoParamAction = do
        let value = param @Text "message"
        renderPlain (cs value)
    action RedirectAction = do
        redirectToPath "/target"
    action RequireUserAction = do
        -- Reads 'currentUserVaultKey' via the standard login helper.
        -- The configured 'AuthMiddleware' below always writes 'Nothing',
        -- so the only way this returns a user is if 'withUser' seeded
        -- one via 'mockOverrideVaultKey'.
        case LoginController.currentUserOrNothing @TestUser of
            Just (TestUser { id = Id uuid }) -> renderPlain (cs (UUID.toASCIIBytes uuid))
            Nothing -> renderPlain "anonymous"

instance AutoRoute TestController

instance FrontController WebApplication where
  controllers = [ parseRoute @TestController ]

defaultLayout :: Html -> Html
defaultLayout inner = inner

instance InitControllerContext WebApplication where
  initContext = do
    setLayout defaultLayout

instance FrontController RootApplication where
    controllers = [ mountFrontController WebApplication ]

config = do
    option Development
    option (AppPort 8000)
    -- Install an 'AuthMiddleware' that always writes 'Nothing' to
    -- 'currentUserVaultKey'. This simulates the real @authMiddleware \@User@
    -- running but finding no session — which is exactly what happens in
    -- tests because 'sessionMiddleware' (from wai-session-maybe) rewrites
    -- 'sessionVaultKey' from the request cookie. The regression test below
    -- verifies that 'withUser'\'s override still wins, because
    -- 'callActionWithParams' wraps the controller innermost.
    option $ AuthMiddleware (authMiddlewareWith currentUserVaultKey (\_ -> pure Nothing))

tests :: Spec
tests = aroundAll (withMockContextAndApp WebApplication config) do
    describe "IHP.Test.Mocking" do
        describe "form params" do
            it "should deliver params to the controller action" $ withContextAndApp \application -> do
                runSession (do
                    response <- testPostForm "test/EchoParam" [("message", "hello world")]
                    assertStatus 200 response
                    assertBody "hello world" response
                    ) application

            it "should return status 200 for rendered responses" $ withContextAndApp \application -> do
                runSession (do
                    testPostForm "test/EchoParam" [("message", "test")] >>= assertStatus 200
                    ) application

        describe "redirectTo" do
            it "should return status 302" $ withContextAndApp \application -> do
                runSession (do
                    testGet "test/Redirect" >>= assertStatus 302
                    ) application

        describe "withUser" do
            -- Regression test for a timing bug between 'withUser' and the
            -- WAI-based 'authMiddleware'. Previously 'withUser' seeded the
            -- mock user into 'sessionVaultKey', but 'sessionMiddleware'
            -- (from wai-session-maybe) unconditionally overwrote that key
            -- with a fresh session built from the request cookie, so
            -- 'authMiddleware' saw no user and every protected action
            -- rendered "anonymous".
            --
            -- The fix: 'withUser' now stashes a middleware in
            -- 'mockOverrideVaultKey' that seeds 'currentUserVaultKey'
            -- directly. 'callActionWithParams' applies that middleware
            -- innermost (wrapping the controller), so it runs *after*
            -- 'authMiddleware' and wins on conflict.
            it "seeds currentUser even when authMiddleware would clear it" $ withContextAndApp \_ -> do
                let Just uuid = UUID.fromString "00000000-0000-0000-0000-000000000001"
                let user = TestUser { id = Id uuid }
                response <- withUser user do
                    callAction RequireUserAction
                bodyBytes <- responseBody response
                Wai.responseStatus response `shouldBe` status200
                bodyBytes `shouldBe` cs (UUID.toASCIIBytes uuid)

            it "returns anonymous when withUser is not used" $ withContextAndApp \_ -> do
                response <- callAction RequireUserAction
                bodyBytes <- responseBody response
                Wai.responseStatus response `shouldBe` status200
                bodyBytes `shouldBe` "anonymous"
