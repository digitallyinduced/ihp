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

data WebApplication = WebApplication deriving (Eq, Show, Data)

data TestController
    = EchoParamAction
    | RedirectAction
  deriving (Eq, Show, Data)

instance Controller TestController where
    action EchoParamAction = do
        let value = param @Text "message"
        renderPlain (cs value)
    action RedirectAction = do
        redirectToPath "/target"

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
