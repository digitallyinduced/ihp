{-|
Module: Test.Controller.CookieSpec
Copyright: (c) digitally induced GmbH, 2022
-}
module Test.Controller.CookieSpec where

import IHP.Prelude
import Test.Hspec

import Wai.Request.Params.Middleware (RequestBody (..), requestBodyVaultKey)
import qualified Data.Vault.Lazy as Vault
import IHP.Controller.Response (addResponseHeadersFromContext, responseHeadersVaultKey)
import IHP.Controller.Cookie
import IHP.Controller.Context
import qualified Network.Wai as Wai
import Web.Cookie
import Network.HTTP.Types.Status
import qualified Data.TMap as TypeMap

tests = do
    describe "IHP.Controller.Cookie" do
        describe "setCookie" do
            it "set a 'Set-Cookie' header" do
                (context, request) <- createControllerContext
                let ?context = context
                let ?request = request

                setCookie defaultSetCookie
                        { setCookieName = "exampleCookie"
                        , setCookieValue = "exampleValue"
                        }

                let response = Wai.responseLBS status200 [] "Hello World"
                responseWithHeaders <- addResponseHeadersFromContext response

                Wai.responseHeaders responseWithHeaders `shouldBe` [("Set-Cookie", "exampleCookie=exampleValue")]


createControllerContext = do
    headersRef <- newIORef []
    let
        requestBody = FormBody { params = [], files = [] }
        request = Wai.defaultRequest { Wai.vault = Vault.insert requestBodyVaultKey requestBody
                                                  . Vault.insert responseHeadersVaultKey headersRef
                                                  $ Vault.empty }
    let ?request = request
    context <- newControllerContext
    pure (context, request)
