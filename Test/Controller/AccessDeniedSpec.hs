{-|
Module: Test.Controller.AccessDeniedSpec
Tests for Access denied functions.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Controller.AccessDeniedSpec where
import qualified Prelude
import ClassyPrelude
import Test.Hspec
import IHP.Test.Mocking hiding (application)
import IHP.Prelude
import IHP.QueryBuilder
import IHP.Environment
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
import Data.String.Conversions
import Data.Text as Text
import Unsafe.Coerce
import IHP.ApplicationContext

import qualified Network.Wai.Session as Session
import qualified Network.Wai.Session.Map as Session

data WebApplication = WebApplication deriving (Eq, Show, Data)

data TestController
    = TestActionAccessDeniedWhen
    | TestActionAccessDeniedUnless
  deriving (Eq, Show, Data)

instance Controller TestController where
    action TestActionAccessDeniedWhen = do
        accessDeniedWhen True
        renderPlain "Test"
    action TestActionAccessDeniedUnless = do
        accessDeniedUnless False
        renderPlain "Test"

instance AutoRoute TestController

instance FrontController WebApplication where
  controllers = [ parseRoute @TestController ]


defaultLayout :: Html -> Html
defaultLayout inner =  [hsx|{inner}|]

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

makeApplication :: (?applicationContext :: ApplicationContext) => IO Application
makeApplication = do
    store <- Session.mapStore_
    let sessionMiddleware :: Middleware = Session.withSession store "SESSION" ?applicationContext.frameworkConfig.sessionCookie ?applicationContext.session
    pure (sessionMiddleware (Server.application handleNotFound))

assertAccessDenied :: SResponse -> IO ()
assertAccessDenied response = do
    response.simpleStatus `shouldBe` status403
    response.simpleBody `shouldNotBe` "Test"

tests :: Spec
tests = beforeAll (mockContextNoDatabase WebApplication config) do
    describe "Not found" $ do
        it "should return show 404 page when notFoundWhen is True" $ withContext do
            application <- makeApplication
            runSession (testGet "test/TestActionAccessDeniedWhen") application >>= assertAccessDenied
        it "should return False on a different route" $ withContext do
            application <- makeApplication
            runSession (testGet "test/TestActionAccessDeniedUnless") application >>= assertAccessDenied