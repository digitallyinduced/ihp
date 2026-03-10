{-|
Module: Test.ViewSupportSpec

Tests for view support functions.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.ViewSupportSpec where
import ClassyPrelude
import Test.Hspec
import IHP.Test.Mocking
import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.RouterSupport hiding (get)
import IHP.ViewPrelude
import IHP.ControllerPrelude hiding (get, request)
import Network.Wai.Test
import Network.HTTP.Types
import Data.Text as Text
import Test.ModelFixtures ()

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

        isActiveController TestController: {isActiveController @TestController}
        isActiveController AnotherTestAction: {isActiveController @AnotherTestController}
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

tests :: Spec
tests = aroundAll (withMockContextAndApp WebApplication config) do
    describe "isActiveAction" $ do
        it "should return True on the same route" $ withContextAndApp \application -> do
            runSession (testGet "test/TestWithParam?param=foo") application >>= assertTextExists "isActiveAction foo: True"
        it "should return False on a different route" $ withContextAndApp \application -> do
            runSession (testGet "test/TestWithParam?param=foo") application >>= assertTextExists "isActiveAction bar: False"
    describe "isActivePath" $ do
        it "should return True on the same route" $ withContextAndApp \application -> do
            runSession (testGet "test/TestWithParam?param=foo") application >>= assertTextExists "isActivePath foo: True"
        it "should return False on a different route" $ withContextAndApp \application -> do
            runSession (testGet "test/TestWithParam?param=foo") application >>= assertTextExists "isActivePath bar: False"
    describe "isActiveController" $ do
        it "should return True on the same route" $ withContextAndApp \application -> do
            runSession (testGet "test/TestWithParam?param=foo") application >>= assertTextExists "isActiveController TestController: True"
        it "should return False on a different route" $ withContextAndApp \application -> do
            runSession (testGet "test/TestWithParam?param=foo") application >>= assertTextExists "isActiveController AnotherTestAction: False"

    describe "HSX" $ do
        it "allow using Id's in HSX attributes without explicitly calling inputValue" $ withContextAndApp \_ -> do
            let
                id :: Id' "users"
                id = Id ("70a10b53-a776-470a-91a8-900cdda06aa2" :: UUID)

            (ClassyPrelude.tshow [hsx|<input value={id} />|]) `shouldBe` "<input value=\"70a10b53-a776-470a-91a8-900cdda06aa2\">"

type instance PrimaryKey "users" = UUID