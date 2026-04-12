{-|
Module: Test.Controller.CookieSpec
Copyright: (c) digitally induced GmbH, 2022
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Controller.CookieSpec where

import IHP.Prelude
import Test.Hspec
import IHP.Test.Mocking
import IHP.Environment
import IHP.FrameworkConfig
import IHP.ControllerPrelude hiding (get, request)
import IHP.Controller.Cookie
import Web.Cookie
import Network.Wai.Test
import Test.Util (testGet)

data WebApplication = WebApplication deriving (Eq, Show, Data)

data CookieTestController
    = SetCookieAction
  deriving (Eq, Show, Data)

instance Controller CookieTestController where
    action SetCookieAction = do
        setCookie defaultSetCookie
            { setCookieName = "exampleCookie"
            , setCookieValue = "exampleValue"
            }
        renderPlain "ok"

instance AutoRoute CookieTestController

instance FrontController WebApplication where
    controllers = [parseRoute @CookieTestController]

instance InitControllerContext WebApplication where
    initContext = pure ()

instance FrontController RootApplication where
    controllers = [mountFrontController WebApplication]

config = do
    option Development
    option (AppPort 8000)

tests :: Spec
tests = aroundAll (withMockContextAndApp WebApplication config) do
    describe "IHP.Controller.Cookie" do
        describe "setCookie" do
            it "set a 'Set-Cookie' header" $ withContextAndApp \application -> do
                runSession (do
                    response <- testGet "test/SetCookie"
                    assertStatus 200 response
                    assertHeader "Set-Cookie" "exampleCookie=exampleValue" response
                    ) application
