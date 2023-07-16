{-|
Module: Test.ViewSupportSpec

Tests for view support functions.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.ViewSupportSpec where
import qualified Prelude
import ClassyPrelude
import Test.Hspec
import IHP.Test.Mocking hiding (application)
import IHP.Prelude
import IHP.QueryBuilder
import IHP.Environment
import IHP.FrameworkConfig
import IHP.HaskellSupport
import IHP.RouterSupport hiding (get)
import IHP.FrameworkConfig
import IHP.Job.Types
import IHP.Controller.RequestContext hiding (request)
import IHP.ViewPrelude
import IHP.ControllerPrelude hiding (get, request)
import qualified IHP.Server as Server
import Data.Attoparsec.ByteString.Char8 (string, Parser, (<?>), parseOnly, take, endOfInput, choice, takeTill, takeByteString)
import Network.Wai
import Network.Wai.Test
import Network.HTTP.Types
import qualified IHP.ErrorController as ErrorController
import Data.String.Conversions
import Data.Text as Text
import Unsafe.Coerce
import IHP.ApplicationContext

import qualified Network.Wai.Session as Session
import qualified Network.Wai.Session.Map as Session

data WebApplication = WebApplication deriving (Eq, Show, Data)

data TestController
    = TestAction
    | TestWithParamAction { param :: Text }
  deriving (Eq, Show, Data)

data AnotherTestController
    = AnotherTestAction
  deriving (Eq, Show, Data)

instance Controller TestController where
    action TestAction = do
        renderPlain "TestAction"
    action TestWithParamAction { .. } = do
        render ShowView { .. }

instance Controller AnotherTestController where
    action AnotherTestAction = do
        renderPlain "AnotherTestAction"

instance AutoRoute TestController
instance AutoRoute AnotherTestController

instance FrontController WebApplication where
  controllers = [ parseRoute @TestController, parseRoute @AnotherTestController ]

data ShowView = ShowView { param :: Text}

instance View ShowView where
    html ShowView { .. }= [hsx|
        isActiveAction {param}: {isActiveAction $ TestWithParamAction param}
        isActiveAction bar: {isActiveAction $ TestWithParamAction "bar"}

        isActivePath {param}: {isActivePath $ "/test/TestWithParam?param=" <> param}
        isActivePath bar: {isActivePath ("/test/TestWithParam?param=bar" :: Text)}
    |]

defaultLayout :: Html -> Html
defaultLayout inner =  [hsx|{inner}|]

instance InitControllerContext WebApplication where
  initContext = do
    setLayout defaultLayout

instance FrontController RootApplication where
    controllers = [ mountFrontController WebApplication ]

testGet :: ByteString -> Session SResponse
testGet url = request $ setPath defaultRequest { requestMethod = methodGet } url

assertTextExists :: Text -> SResponse -> IO ()
assertTextExists body response = do
    response.simpleStatus `shouldBe` status200
    Text.isInfixOf body (cs response.simpleBody) `shouldBe` True

config = do
    option Development
    option (AppPort 8000)

makeApplication :: (?applicationContext :: ApplicationContext) => IO Application
makeApplication = do
    store <- Session.mapStore_
    let sessionMiddleware :: Middleware = Session.withSession store "SESSION" ?applicationContext.frameworkConfig.sessionCookie ?applicationContext.session
    pure (sessionMiddleware (Server.application ErrorController.handleNotFound))

tests :: Spec
tests = beforeAll (mockContextNoDatabase WebApplication config) do
    describe "isActiveAction" $ do
        it "should return True on the same route" $ withContext do
            application <- makeApplication
            runSession (testGet "test/TestWithParam?param=foo") application >>= assertTextExists "isActiveAction foo: True"
        it "should return False on a different route" $ withContext do
            application <- makeApplication
            runSession (testGet "test/TestWithParam?param=foo") application >>= assertTextExists "isActiveAction bar: False"
    describe "isActivePath" $ do
        it "should return True on the same route" $ withContext do
            application <- makeApplication
            runSession (testGet "test/TestWithParam?param=foo") application >>= assertTextExists "isActivePath foo: True"
        it "should return False on a different route" $ withContext do
            application <- makeApplication
            runSession (testGet "test/TestWithParam?param=foo") application >>= assertTextExists "isActivePath bar: False"
    describe "isActiveController" $ do
        it "should return True on the same route" $ withContext do
            application <- makeApplication
            runSession (testGet "test/TestWithParam?param=foo") application >>= assertTextExists "isActiveController TestController: True"
        it "should return False on a different route" $ withContext do
            application <- makeApplication
            runSession (testGet "test/TestWithParam?param=foo") application >>= assertTextExists "isActiveController AnotherTestAction: False"