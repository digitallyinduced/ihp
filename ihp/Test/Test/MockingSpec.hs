{-|
Module: Test.MockingSpec
Tests for IHP.Test.Mocking, ensuring callActionWithParams correctly
delivers params through the middleware stack and that redirect responses
preserve their status codes.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.MockingSpec where
import qualified Prelude
import ClassyPrelude
import Test.Hspec
import IHP.Test.Mocking
import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.Job.Types
import IHP.ControllerPrelude hiding (get, request)
import Text.Blaze.Html (Html)
import Network.Wai
import Network.HTTP.Types

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
tests = beforeAll (mockContextNoDatabase WebApplication config) do
    describe "IHP.Test.Mocking" do
        describe "callActionWithParams" do
            it "should deliver params to the controller action" $ withContext do
                response <- callActionWithParams EchoParamAction [("message", "hello world")]
                body <- responseBody response
                cs body `shouldBe` ("hello world" :: Text)

            it "should return status 200 for rendered responses" $ withContext do
                response <- callActionWithParams EchoParamAction [("message", "test")]
                responseStatus response `shouldBe` status200

        describe "redirectTo" do
            it "should return status 302" $ withContext do
                response <- callAction RedirectAction
                responseStatus response `shouldBe` status302
