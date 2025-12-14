module Main where

import Test.Hspec
import Network.Wai.Middleware.ContentSecurityPolicy
import qualified Data.Text as Text

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "CSP Source Rendering" do
        it "renders 'self' correctly" do
            renderCSPSource self `shouldBe` "'self'"

        it "renders 'none' correctly" do
            renderCSPSource none `shouldBe` "'none'"

        it "renders 'unsafe-inline' correctly" do
            renderCSPSource unsafeInline `shouldBe` "'unsafe-inline'"

        it "renders 'unsafe-eval' correctly" do
            renderCSPSource unsafeEval `shouldBe` "'unsafe-eval'"

        it "renders 'strict-dynamic' correctly" do
            renderCSPSource strictDynamic `shouldBe` "'strict-dynamic'"

        it "renders nonce correctly" do
            renderCSPSource (nonce "abc123") `shouldBe` "'nonce-abc123'"

        it "renders data: correctly" do
            renderCSPSource data' `shouldBe` "data:"

        it "renders https: correctly" do
            renderCSPSource https `shouldBe` "https:"

        it "renders host correctly" do
            renderCSPSource (host "example.com") `shouldBe` "example.com"

        it "renders scheme correctly" do
            renderCSPSource (scheme "wss:") `shouldBe` "wss:"

    describe "CSP Directive Rendering" do
        it "renders default-src correctly" do
            renderCSPDirective DefaultSrc `shouldBe` "default-src"

        it "renders script-src correctly" do
            renderCSPDirective ScriptSrc `shouldBe` "script-src"

        it "renders frame-ancestors correctly" do
            renderCSPDirective FrameAncestors `shouldBe` "frame-ancestors"

    describe "Default CSP" do
        it "has default-src 'self'" do
            let rendered = renderCSP defaultCSP
            rendered `shouldSatisfy` Text.isInfixOf "default-src 'self'"

        it "has object-src 'none'" do
            let rendered = renderCSP defaultCSP
            rendered `shouldSatisfy` Text.isInfixOf "object-src 'none'"

        it "has base-uri 'none'" do
            let rendered = renderCSP defaultCSP
            rendered `shouldSatisfy` Text.isInfixOf "base-uri 'none'"

    describe "Strict CSP" do
        it "includes nonce in script-src" do
            let csp = strictCSP "test-nonce"
            let rendered = renderCSP csp
            rendered `shouldSatisfy` Text.isInfixOf "script-src 'nonce-test-nonce'"

        it "includes strict-dynamic in script-src" do
            let csp = strictCSP "test-nonce"
            let rendered = renderCSP csp
            rendered `shouldSatisfy` Text.isInfixOf "'strict-dynamic'"

        it "includes nonce in style-src" do
            let csp = strictCSP "test-nonce"
            let rendered = renderCSP csp
            rendered `shouldSatisfy` Text.isInfixOf "style-src 'nonce-test-nonce'"

        it "allows data: images" do
            let csp = strictCSP "test-nonce"
            let rendered = renderCSP csp
            rendered `shouldSatisfy` Text.isInfixOf "img-src 'self' data:"

    describe "Custom CSP" do
        it "renders custom source list correctly" do
            let csp = defaultCSP 
                    { scriptSrc = Just [self, host "cdn.example.com", nonce "abc"]
                    }
            let rendered = renderCSP csp
            rendered `shouldSatisfy` Text.isInfixOf "script-src 'self' cdn.example.com 'nonce-abc'"

        it "omits directives with Nothing value" do
            let csp = defaultCSP { scriptSrc = Nothing }
            let rendered = renderCSP csp
            rendered `shouldNotSatisfy` Text.isInfixOf "script-src"

        it "includes upgrade-insecure-requests when enabled" do
            let csp = defaultCSP { upgradeInsecureRequests = True }
            let rendered = renderCSP csp
            rendered `shouldSatisfy` Text.isInfixOf "upgrade-insecure-requests"

        it "includes block-all-mixed-content when enabled" do
            let csp = defaultCSP { blockAllMixedContent = True }
            let rendered = renderCSP csp
            rendered `shouldSatisfy` Text.isInfixOf "block-all-mixed-content"

        it "handles multiple directives correctly" do
            let csp = defaultCSP
                    { scriptSrc = Just [nonce "test"]
                    , styleSrc = Just [self]
                    , imgSrc = Just [self, data']
                    }
            let rendered = renderCSP csp
            rendered `shouldSatisfy` Text.isInfixOf "script-src 'nonce-test'"
            rendered `shouldSatisfy` Text.isInfixOf "style-src 'self'"
            rendered `shouldSatisfy` Text.isInfixOf "img-src 'self' data:"

    describe "CSP Rendering Format" do
        it "separates directives with semicolons and spaces" do
            let csp = defaultCSP
                    { scriptSrc = Just [self]
                    , styleSrc = Just [self]
                    }
            let rendered = renderCSP csp
            -- Should contain "; " between directives
            (Text.splitOn "; " rendered |> length) `shouldSatisfy` (>= 2)

        it "renders a complete valid CSP header" do
            let csp = strictCSP "nonce123"
            let rendered = renderCSP csp
            -- Should be a non-empty string
            rendered `shouldNotBe` ""
            -- Should contain multiple directives
            (Text.splitOn "; " rendered |> length) `shouldSatisfy` (> 5)
