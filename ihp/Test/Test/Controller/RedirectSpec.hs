{-|
Module: Test.Controller.RedirectSpec
Tests for the redirect helpers in IHP.Controller.Redirect.
-}
module Test.Controller.RedirectSpec where

import IHP.Prelude
import Test.Hspec
import IHP.Controller.Redirect
import IHP.Controller.Response (responseHeadersVaultKey)
import Wai.Request.Params.Middleware (Respond)
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import qualified Network.Wai.Middleware.Approot as Approot
import qualified Data.Vault.Lazy as Vault
import Network.HTTP.Types (hLocation)

tests :: Spec
tests = describe "IHP.Controller.Redirect" do
    describe "isLocalPath" do
        it "accepts a path on this site" do
            let paths = ["/", "/posts", "/posts/1?details=on", "/posts#comments", "/a:b"]
            filter isLocalPath paths `shouldBe` paths

        it "refuses anything a browser could read as another host or scheme" do
            let paths =
                    [ "https://evil.example"
                    , "//evil.example"
                    , "/\\evil.example"
                    , "evil.example"
                    , "@evil.example"
                    , ".evil.example"
                    , "javascript:alert(1)"
                    , "?next=1"
                    , ""
                    ]
            filter isLocalPath paths `shouldBe` []

        it "refuses whitespace and control characters" do
            let paths = ["/\t/evil.example", "/sched ule", "/a\nb", "/a\rb", "/a\0b", "/a\DELb"]
            filter isLocalPath paths `shouldBe` []

    describe "redirectToPath" do
        it "redirects to the path on this site" do
            location <- redirectLocation (redirectToPath "/posts/1?details=on")
            location `shouldBe` Just "https://app.example.com/posts/1?details=on"

        it "refuses a path that would send the browser to another host" do
            redirectLocation (redirectToPath "@evil.example") `shouldThrow` anyErrorCall
            redirectLocation (redirectToPath ".evil.example") `shouldThrow` anyErrorCall
            redirectLocation (redirectToPath "//evil.example") `shouldThrow` anyErrorCall
            redirectLocation (redirectToPath "https://evil.example") `shouldThrow` anyErrorCall

    describe "redirectToPathSeeOther" do
        it "redirects to the path on this site" do
            location <- redirectLocation (redirectToPathSeeOther "/posts")
            location `shouldBe` Just "https://app.example.com/posts"

        it "refuses a path that would send the browser to another host" do
            redirectLocation (redirectToPathSeeOther "@evil.example") `shouldThrow` anyErrorCall

-- | Runs a redirect inside a request whose approot is @https://app.example.com@
-- and returns the @Location@ header of the response.
redirectLocation :: ((?request :: Wai.Request, ?respond :: Respond) => IO Wai.ResponseReceived) -> IO (Maybe ByteString)
redirectLocation redirect = do
    location <- newIORef Nothing
    responseHeaders <- newIORef []
    let app request respond =
            let ?request = request { Wai.vault = Vault.insert responseHeadersVaultKey responseHeaders request.vault }
                ?respond = respond
            in redirect
    _ <- Approot.hardcoded "https://app.example.com" app Wai.defaultRequest \response -> do
        writeIORef location (lookup hLocation (Wai.responseHeaders response))
        pure Wai.ResponseReceived
    readIORef location
