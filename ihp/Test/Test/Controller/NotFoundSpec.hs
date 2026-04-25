{-|
Module: Test.Controller.NotFoundSpec
Tests for Not found functions.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Controller.NotFoundSpec where
import ClassyPrelude
import Test.Hspec
import IHP.Test.Mocking
import IHP.Prelude
import IHP.Environment
import IHP.RouterSupport hiding (get)
import IHP.Router.DSL (routes)
import IHP.FrameworkConfig
import IHP.ViewPrelude
import IHP.ControllerPrelude hiding (get, request)
import Network.HTTP.Types.Method (StdMethod (..))
import Network.Wai.Test
import Test.Util (testGet)

data WebApplication = WebApplication deriving (Eq, Show, Data)

data TestController
    = TestActionNotFoundWhen
    | TestActionNotFoundUnless
  deriving (Eq, Show, Data)

instance Controller TestController where
    action TestActionNotFoundWhen = do
        notFoundWhen True
        renderPlain "Test"
    action TestActionNotFoundUnless = do
        notFoundUnless False
        renderPlain "Test"

$(pure [])

[routes|TestController
GET /test/TestActionNotFoundWhen   TestActionNotFoundWhen
GET /test/TestActionNotFoundUnless TestActionNotFoundUnless
|]

instance FrontController WebApplication where
  controllers = [ parseRoute @TestController ]


defaultLayout :: Html -> Html
defaultLayout inner =  [hsx|{inner}|]

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
    describe "Not found" $ do
        it "should return show 404 page when notFoundWhen is True" $ withContextAndApp \application -> do
            runSession (testGet "test/TestActionNotFoundWhen" >>= assertStatus 404) application
        it "should return show 404 page when notFoundUnless is False" $ withContextAndApp \application -> do
            runSession (testGet "test/TestActionNotFoundUnless" >>= assertStatus 404) application