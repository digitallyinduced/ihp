{-|
Module: Test.Controller.EarlyReturnSpec
Tests for early return functionality.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Controller.EarlyReturnSpec where

import Test.Hspec
import IHP.Prelude
import IHP.Test.Mocking hiding (application)
import IHP.FrameworkConfig
import IHP.Environment (Environment(..))
import IHP.ControllerPrelude hiding (get, request)
import IHP.ViewPrelude (Html, hsx)
import IHP.Controller.EarlyReturn (handleEarlyReturn, EarlyReturnException(..))
import IHP.RouterSupport hiding (get)
import qualified IHP.Server as Server
import Network.Wai
import Network.Wai.Test
import Network.HTTP.Types
import qualified Control.Exception as Exception

data WebApplication = WebApplication deriving (Eq, Show, Data)

data TestController
    = TestActionWithEarlyReturn
    | TestActionWithoutEarlyReturn
    | TestActionEarlyReturnSkipsRest
  deriving (Eq, Show, Data)

instance Controller TestController where
    action TestActionWithEarlyReturn = do
        earlyReturn (renderPlain "Early")
        renderPlain "Normal"

    action TestActionWithoutEarlyReturn = do
        renderPlain "Normal"

    action TestActionEarlyReturnSkipsRest = do
        when True do
            earlyReturn (renderPlain "Early")
        renderPlain "Should not reach"

instance AutoRoute TestController

instance FrontController WebApplication where
  controllers = [ parseRoute @TestController ]


defaultLayout :: Html -> Html
defaultLayout inner = [hsx|{inner}|]

instance InitControllerContext WebApplication where
  initContext = do
    setLayout defaultLayout

instance FrontController RootApplication where
    controllers = [ mountFrontController WebApplication ]

testGet :: ByteString -> Session SResponse
testGet url = request $ setPath defaultRequest { requestMethod = methodGet } url


config = do
    option Development
    option (AppPort 8000)

application :: Application
application = Server.application handleNotFound (\app -> app)

tests :: Spec
tests = do
    describe "handleEarlyReturn" $ do
        it "should return ResponseReceived from normal action" $ do
            let mockResponse = error "mock ResponseReceived" :: ResponseReceived
            let action = pure mockResponse
            result <- handleEarlyReturn action
            pure ()

        it "should catch EarlyReturnException and return its ResponseReceived" $ do
            let mockResponse = error "mock ResponseReceived" :: ResponseReceived
            let action = do
                    Exception.throwIO (EarlyReturnException mockResponse)
                    pure mockResponse
            result <- handleEarlyReturn action
            pure ()

    beforeAll (mockContextNoDatabase WebApplication config) do
        describe "Early Return in Controllers" $ do
            it "should return early response when earlyReturn is called" $ withContext do
                response <- runSession (testGet "test/TestActionWithEarlyReturn") application
                response.simpleStatus `shouldBe` status200
                response.simpleBody `shouldBe` "Early"

            it "should return normal response when earlyReturn is not called" $ withContext do
                response <- runSession (testGet "test/TestActionWithoutEarlyReturn") application
                response.simpleStatus `shouldBe` status200
                response.simpleBody `shouldBe` "Normal"

            it "should skip remaining code after earlyReturn" $ withContext do
                response <- runSession (testGet "test/TestActionEarlyReturnSkipsRest") application
                response.simpleStatus `shouldBe` status200
                response.simpleBody `shouldBe` "Early"
