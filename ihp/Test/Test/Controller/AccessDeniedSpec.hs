{-|
Module: Test.Controller.AccessDeniedSpec
Tests for Access denied functions.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Controller.AccessDeniedSpec where
import ClassyPrelude
import Test.Hspec
import IHP.Test.Mocking
import IHP.Prelude
import IHP.Environment
import IHP.RouterSupport hiding (get)
import IHP.FrameworkConfig
import IHP.ViewPrelude
import IHP.ControllerPrelude hiding (get, request)
import Network.Wai.Test
import Network.HTTP.Types

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

assertAccessDenied :: SResponse -> IO ()
assertAccessDenied response = do
    response.simpleStatus `shouldBe` status403
    response.simpleBody `shouldNotBe` "Test"

tests :: Spec
tests = aroundAll (withMockContextAndApp WebApplication config) do
    describe "Access denied" $ do
        it "should return show 403 page when acessDeniedWhen is True" $ withContextAndApp \application -> do
            runSession (testGet "test/TestActionAccessDeniedWhen") application >>= assertAccessDenied
        it "should return show 403 page when acessDeniedUnless is False" $ withContextAndApp \application -> do
            runSession (testGet "test/TestActionAccessDeniedUnless") application >>= assertAccessDenied