{-|
Module: IHP.HSX.QQSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.HSX.QQSpec where

import Test.Hspec
import Prelude
import Control.Monad.State.Strict (State, get, put, evalState)
import qualified IHP.HSX.QQ as Blaze
import qualified IHP.HSX.Lucid2.QQ as Lucid2
import qualified IHP.HSX.Lucid2.Attribute as Lucid2
import qualified Text.Blaze.Renderer.Text as Blaze
import qualified Text.Blaze as Blaze
import Text.Blaze (preEscapedTextValue)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import IHP.HSX.CustomHsxCases
import qualified "lucid2" Lucid.Base as Lucid2 (Html, HtmlT, renderText, renderTextT)

tests :: SpecWith ()
tests = do
    describe "HSX" do
        it "should work with static html" do
            [hsx|<strong>hello world</strong>|] `shouldBeSameHtml` "<strong>hello world</strong>"
            [hsx|<h1>hello world</h1>|] `shouldBeSameHtml` "<h1>hello world</h1>"
            [hsx|<blink>web scale</blink>|] `shouldBeSameHtml` "<blink>web scale</blink>"

        it "should work with an empty input" do
            [hsx||] `shouldBeSameHtml` ""

        it "should support multiple root nodes" do
            [hsx|<u>underlined</u><i>italic</i>|] `shouldBeSameHtml` "<u>underlined</u><i>italic</i>"

        it "should work with text variables" do
            let myString :: Text = "World!"
            [hsx|Hello {myString}|] `shouldBeSameHtml` "Hello World!"

        it "should work with complex haskell expressions" do
            let project = Project { name = "Testproject" }
            [hsx|<h1>Project: {project.name}</h1>|] `shouldBeSameHtml` "<h1>Project: Testproject</h1>"

        it "should support lambdas and pattern matching on constructors" do
            let placeData = PlaceId "Punches Cross"
            [hsx|<h1>{(\(PlaceId x) -> x)(placeData)}</h1>|] `shouldBeSameHtml` "<h1>Punches Cross</h1>"

        it "should support infix notation for standard constructors e.g. (:):" do
            [hsx| <h1>{show $ (:) 1  [2,3,42]}</h1> |] `shouldBeSameHtml` "<h1>[1,2,3,42]</h1>"

        it "should support infix notation for standard constructors e.g. (,):" do
            [hsx|<h1>{show $ ((,) 1 2)}</h1>|] `shouldBeSameHtml` "<h1>(1,2)</h1>"

        it "should support self closing tags" do
            [hsx|<input>|] `shouldBeSameHtml` "<input>"
            [hsx|<br><br/>|] `shouldBeSameHtml` "<br><br>"

        it "should support boolean attributes" do
            [hsx|<input disabled>|] `shouldBeSameHtml` "<input disabled=\"disabled\">"
            [hsx|<input disabled={True}>|] `shouldBeSameHtml` "<input disabled=\"disabled\">"
            [hsx|<input disabled={False}>|] `shouldBeSameHtml` "<input>"

        it "should correctly output True and False values in data attributes" do
            [hsx|<form data-disabled-javascript-submission></form>|] `shouldBeSameHtml` "<form data-disabled-javascript-submission=\"true\"></form>"
            [hsx|<form data-disabled-javascript-submission={True}></form>|] `shouldBeSameHtml` "<form data-disabled-javascript-submission=\"true\"></form>"
            [hsx|<form data-disabled-javascript-submission={False}></form>|] `shouldBeSameHtml` "<form data-disabled-javascript-submission=\"false\"></form>"

        it "should work with inline JS" do
            [hsx|<script>var a = { hello: true }; alert('hello world');</script>|] `shouldBeSameHtml` "<script>var a = { hello: true }; alert('hello world');</script>"
            [hsx|<script>{haskellCodeNotWorkingHere}</script>|] `shouldBeSameHtml` "<script>{haskellCodeNotWorkingHere}</script>"

        it "should work with inline CSS" do
            [hsx|<style>.blue { background: blue; }</style>|] `shouldBeSameHtml` "<style>.blue { background: blue; }</style>"
            [hsx|<style>{haskellCodeNotWorkingHere}</style>|] `shouldBeSameHtml` "<style>{haskellCodeNotWorkingHere}</style>"

        it "should strip whitespace around script tags" do
            [hsx|
                <script>
                    var apiKey = document.currentScript.dataset.apiKey;
                </script>
            |] `shouldBeSameHtml` "<script>var apiKey = document.currentScript.dataset.apiKey;</script>"

        it "should strip whitespace around text nodes" do
            [hsx| <strong> Hello World </strong> |] `shouldBeSameHtml` "<strong>Hello World</strong>"
            [hsx|<i> a</i>|] `shouldBeSameHtml` "<i>a</i>"
            [hsx|<i> a b</i>|] `shouldBeSameHtml` "<i>a b</i>"
            [hsx|<i> a b c </i>|] `shouldBeSameHtml` "<i>a b c</i>"

        it "should collapse spaces" do
            [hsx|
                Hello
                World
                !
            |] `shouldBeSameHtml` "Hello World !"

        it "should not strip whitespace around variables near to text nodes" do
            let name :: Text = "Tester"
            [hsx| <strong> Hello {name} ! </strong> |] `shouldBeSameHtml` "<strong>Hello Tester !</strong>"
            [hsx| <strong> Hello {name}{name} ! </strong> |] `shouldBeSameHtml` "<strong>Hello TesterTester !</strong>"
            [hsx| <strong> Hello {name} {name} ! </strong> |] `shouldBeSameHtml` "<strong>Hello Tester Tester !</strong>"
            [hsx|{name}|] `shouldBeSameHtml` "Tester"

            let question :: Text = "Q"
            let answer :: Text = "A"
            [hsx|<td>{question} → {answer}</td>|] `shouldBeSameHtml` "<td>Q → A</td>"

        it "should work with html comments" do
            [Blaze.hsx|<div><!--my comment--></div>|] `shouldBeBlazeHtml` "<div><!-- my comment --></div>"
            [Lucid2.hsx|<div><!--my comment--></div>|] `shouldBeLucid2Html` "<div><!--my comment--></div>"

        it "should work with no render comments" do
            [hsx|<div>{- my comment -}</div>|] `shouldBeSameHtml` "<div></div>"

        it "should escape variables to avoid XSS" do
            let variableContent :: Text = "<script>alert(1);</script>"
            [hsx|{variableContent}|] `shouldBeSameHtml` "&lt;script&gt;alert(1);&lt;/script&gt;"

        it "should parse custom web component tags" do
            [hsx|<confetti-effect></confetti-effect>|] `shouldBeSameHtml` "<confetti-effect></confetti-effect>"
            [hsx|<confetti-effect/>|] `shouldBeSameHtml` "<confetti-effect></confetti-effect>" -- Currently we cannot deal with self closing tags as expected
            [hsx|<div is="confetti-effect"></div>|] `shouldBeSameHtml` "<div is=\"confetti-effect\"></div>"

        it "should parse a small hsx document" do
            let metaTags = [hsx|
                <meta charset="utf-8">
            |]
            let stylesheets = [hsx|
                <link rel="stylesheet" href="/vendor/bootstrap.min.css"/>
                <link rel="stylesheet" href="/vendor/flatpickr.min.css"/>
                <link rel="stylesheet" href="/app.css"/>
            |]
            let scripts = [hsx|
                <script src="/prod.js"></script>
            |]
            [hsx|
                <html>
                    <head>
                        {metaTags}

                        {stylesheets}
                        {scripts}

                        <title>IHP Forum</title>
                    </head>
                </html>
            |] `shouldBeSameHtml` "<html><head><meta charset=\"utf-8\"> <link rel=\"stylesheet\" href=\"/vendor/bootstrap.min.css\"><link rel=\"stylesheet\" href=\"/vendor/flatpickr.min.css\"><link rel=\"stylesheet\" href=\"/app.css\"> <script src=\"/prod.js\"></script><title>IHP Forum</title></head></html>"

        it "should parse an example hsx document" do
            let metaTags = [hsx|
                <meta charset="utf-8">
            |]
            metaTags `shouldBeSameHtml` "<meta charset=\"utf-8\">"
            let stylesheets = [hsx|
                <link rel="stylesheet" href="/vendor/bootstrap.min.css"/>
                <link rel="stylesheet" href="/vendor/flatpickr.min.css"/>
                <link rel="stylesheet" href="/app.css"/>
            |]
            let scripts = [hsx|
                <script src="/prod.js"></script>
            |]
            [hsx|
                <html>
                    <head>
                        {metaTags}

                        {stylesheets}
                        {scripts}

                        <title>IHP Forum</title>
                    </head>
                    <body>
                        <div class="container mt-4">
                            <nav class="navbar navbar-expand-lg navbar-light mb-4">
                                <a class="navbar-brand" href="/">λ IHP Forum</a>
                            </nav>
                        </div>
                    </body>
                </html>
            |] `shouldBeSameHtml` "<html><head><meta charset=\"utf-8\"> <link rel=\"stylesheet\" href=\"/vendor/bootstrap.min.css\"><link rel=\"stylesheet\" href=\"/vendor/flatpickr.min.css\"><link rel=\"stylesheet\" href=\"/app.css\"> <script src=\"/prod.js\"></script><title>IHP Forum</title></head><body><div class=\"container mt-4\"><nav class=\"navbar navbar-expand-lg navbar-light mb-4\"><a class=\"navbar-brand\" href=\"/\">\955 IHP Forum</a></nav></div></body></html>"

        it "should handle spread attributes with a variable" do
            let customAttributes :: [(Text, Text)] = [
                    ("hello", "world")
                    ]
            [hsx|<div {...customAttributes}></div>|] `shouldBeSameHtml` "<div hello=\"world\"></div>"

        it "should handle spread attributes with a list" do
            -- See https://github.com/digitallyinduced/ihp/issues/1226

            [hsx|<div {...[ ("data-hoge" :: Text, "Hello World!" :: Text) ]}></div>|] `shouldBeSameHtml` "<div data-hoge=\"Hello World!\"></div>"

        it "should support pre escaped class names for Blaze" do
            -- See https://github.com/digitallyinduced/ihp/issues/1527

            let className = preEscapedTextValue "a&"
            [Blaze.hsx|<div class={className}></div>|] `shouldBeBlazeHtml` "<div class=\"a&\"></div>"

        it "should support pre escaped class names for Lucid2" do
            -- See https://github.com/digitallyinduced/ihp/issues/1527

            let className = Lucid2.MkLucidAttributeRaw "a&"
            [Lucid2.hsx|<div class={className}></div>|] `shouldBeLucid2Html` "<div class=\"a&\"></div>"

        it "should support support doctype" do
            -- See https://github.com/digitallyinduced/ihp/issues/1717

            [Blaze.hsx|<!DOCTYPE html><html lang="en"><body>hello</body></html>|] `shouldBeBlazeHtml` "<!DOCTYPE HTML>\n<html lang=\"en\"><body>hello</body></html>"
            [Lucid2.hsx|<!DOCTYPE html><html lang="en"><body>hello</body></html>|] `shouldBeLucid2Html` "<!DOCTYPE HTML><html lang=\"en\"><body>hello</body></html>"

        it "should support non-Identity effects for Lucid2" do
            let increment :: State Int String = do
                  x <- get
                  put $! (x + 1)
                  pure $! show x
                monadFragment :: Lucid2.HtmlT (State Int) ()
                monadFragment = [Lucid2.hsxM|<div>{increment}</div><div>{increment}</div><div>{increment}</div>|]
                textFragment :: TL.Text
                textFragment = evalState (Lucid2.renderTextT monadFragment) 1
                insertMonadFragment :: Lucid2.HtmlT (State Int) ()
                insertMonadFragment = [Lucid2.hsxM|<div>{monadFragment}</div><div>{monadFragment}</div>|]
                doubleTextFragment :: TL.Text
                doubleTextFragment = evalState (Lucid2.renderTextT insertMonadFragment) 1
            textFragment `shouldBe` "<div>1</div><div>2</div><div>3</div>"
            doubleTextFragment `shouldBe` "<div><div>1</div><div>2</div><div>3</div></div><div><div>4</div><div>5</div><div>6</div></div>"

    describe "customHsx" do
        it "should allow specified custom tags" do
            [myTagsOnlyHsx|<mycustomtag>hello</mycustomtag>|] `shouldBeSameHtml` "<mycustomtag>hello</mycustomtag>"
            [myTagsOnlyHsx|<anothercustomtag>world</anothercustomtag>|] `shouldBeSameHtml` "<anothercustomtag>world</anothercustomtag>"

        it "should allow specified custom attributes" do
            [myAttrsOnlyHsx|<div my-custom-attr="hello">test</div>|] `shouldBeSameHtml` "<div my-custom-attr=\"hello\">test</div>"
            [myAttrsOnlyHsx|<div anothercustomattr="world">test</div>|] `shouldBeSameHtml` "<div anothercustomattr=\"world\">test</div>"

        it "should allow combining custom tags and attributes" do
            [myCustomHsx|<mycustomtag my-custom-attr="hello">test</mycustomtag>|] `shouldBeSameHtml` "<mycustomtag my-custom-attr=\"hello\">test</mycustomtag>"

        it "should work with regular HTML tags and attributes too" do
           [myCustomHsx|<div class="hello" my-custom-attr="test">world</div>|] `shouldBeSameHtml` "<div class=\"hello\" my-custom-attr=\"test\">world</div>"

data Project = Project { name :: Text }

data PlaceId  = PlaceId Text
data LocationId  = LocationId Int PlaceId
newtype NewPlaceId = NewPlaceId Text

newPlaceData = NewPlaceId "New Punches Cross"
locationId = LocationId 17 (PlaceId "Punches Cross")

shouldBeSameHtml :: HasCallStack => AllBackends -> TL.Text -> Expectation
shouldBeSameHtml MkAllBackends {..} expectedHtml = do
  Blaze.renderMarkup blazeMarkup `shouldBe` expectedHtml
  Lucid2.renderText lucid2Html `shouldBe` expectedHtml
  Lucid2.renderText lucid2HtmlM `shouldBe` expectedHtml

shouldBeBlazeHtml :: HasCallStack => Blaze.Markup -> TL.Text -> Expectation
shouldBeBlazeHtml blazeMarkup expectedHtml =
  Blaze.renderMarkup blazeMarkup `shouldBe` expectedHtml

shouldBeLucid2Html :: HasCallStack => Lucid2.Html () -> TL.Text -> Expectation
shouldBeLucid2Html lucid2Html expectedHtml =
  Lucid2.renderText lucid2Html `shouldBe` expectedHtml
