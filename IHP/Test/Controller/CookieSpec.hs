{-|
Module: Test.Controller.CookieSpec
Copyright: (c) digitally induced GmbH, 2022
-}
module Test.Controller.CookieSpec where

import IHP.Prelude
import Test.Hspec

import IHP.Controller.RequestContext
import qualified IHP.ControllerSupport as ControllerSupport
import IHP.Controller.Cookie
import IHP.Controller.Context

import qualified Network.Wai as Wai
import Web.Cookie
import Network.HTTP.Types.Status

tests = do
    describe "IHP.Controller.Cookie" do
        describe "setCookie" do
            it "set a 'Set-Cookie' header" do
                context <- createControllerContext
                let ?context = context

                setCookie defaultSetCookie
                        { setCookieName = "exampleCookie"
                        , setCookieValue = "exampleValue"
                        }

                let response = Wai.responseLBS status200 [] "Hello World"
                responseWithHeaders <- ControllerSupport.addResponseHeadersFromContext response

                Wai.responseHeaders responseWithHeaders `shouldBe` [("Set-Cookie", "exampleCookie=exampleValue")]


createControllerContext = do
    let
        requestBody = FormBody { params = [], files = [] }
        request = Wai.defaultRequest
        requestContext = RequestContext { request, respond = error "respond", requestBody, vault = error "vault", frameworkConfig = error "frameworkConfig" }
    let ?requestContext = requestContext
    newControllerContext
