{-|
Module: Test.HtmlSupport.QQSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.HtmlSupport.QQSpec where

import Test.Hspec
import IHP.Prelude
import IHP.HtmlSupport.QQ
import qualified Text.Blaze.Renderer.Text as Blaze

tests = do
    describe "HSX" do
        it "should work with static html" do 
            [hsx|<strong>hello world</strong>|] `shouldBeHtml` "<strong>hello world</strong>"
            [hsx|<h1>hello world</h1>|] `shouldBeHtml` "<h1>hello world</h1>"
            [hsx|<blink>web scale</blink>|] `shouldBeHtml` "<blink>web scale</blink>"

        it "should work with an empty input" do
            [hsx||] `shouldBeHtml` ""
        
        it "should support multiple root nodes" do
            [hsx|<u>underlined</u><i>italic</i>|] `shouldBeHtml` "<u>underlined</u><i>italic</i>"
        
        it "should work with text variables" do
            let myString :: Text = "World!"
            [hsx|Hello {myString}|] `shouldBeHtml` "Hello World!"
        
        it "should work with complex haskell expressions" do
            let project = Project { name = "Testproject" }
            [hsx|<h1>Project: {get #name project}</h1>|] `shouldBeHtml` "<h1>Project: Testproject</h1>"

        it "should support self closing tags" do
            [hsx|<input>|] `shouldBeHtml` "<input>"
            [hsx|<br><br/>|] `shouldBeHtml` "<br><br>"

        it "should support boolean attributes" do
            [hsx|<input disabled>|] `shouldBeHtml` "<input disabled=\"disabled\">"
            [hsx|<input disabled={True}>|] `shouldBeHtml` "<input disabled=\"disabled\">"
            [hsx|<input disabled={False}>|] `shouldBeHtml` "<input>"
        
        it "should work with inline JS" do
            [hsx|<script>var a = { hello: true }; alert('hello world');</script>|] `shouldBeHtml` "<script>var a = { hello: true }; alert('hello world');</script>"
            [hsx|<script>{haskellCodeNotWorkingHere}</script>|] `shouldBeHtml` "<script>{haskellCodeNotWorkingHere}</script>"
        
        it "should work with inline CSS" do
            [hsx|<style>.blue { background: blue; }</style>|] `shouldBeHtml` "<style>.blue { background: blue; }</style>"
            [hsx|<style>{haskellCodeNotWorkingHere}</style>|] `shouldBeHtml` "<style>{haskellCodeNotWorkingHere}</style>"

        it "should strip whitespace around script tags" do
            [hsx|
                <script>
                    var apiKey = document.currentScript.dataset.apiKey;
                </script>
            |] `shouldBeHtml` "<script>var apiKey = document.currentScript.dataset.apiKey;</script>"
        
        it "should strip whitespace around text nodes" do
            [hsx| <strong> Hello World </strong> |] `shouldBeHtml` "<strong>Hello World</strong>"
            [hsx|<i> a</i>|] `shouldBeHtml` "<i>a</i>"
            [hsx|<i> a b</i>|] `shouldBeHtml` "<i>a b</i>"
            [hsx|<i> a b c </i>|] `shouldBeHtml` "<i>a b c</i>"
        
        it "should collapse spaces" do
            [hsx|
                Hello
                World
                !
            |] `shouldBeHtml` "Hello World !"
        
        it "should not strip whitespace around variables near to text nodes" do
            let name :: Text = "Tester"
            [hsx| <strong> Hello {name} ! </strong> |] `shouldBeHtml` "<strong>Hello Tester !</strong>"
            [hsx| <strong> Hello {name}{name} ! </strong> |] `shouldBeHtml` "<strong>Hello TesterTester !</strong>"
            [hsx| <strong> Hello {name} {name} ! </strong> |] `shouldBeHtml` "<strong>Hello Tester Tester !</strong>"
            [hsx|{name}|] `shouldBeHtml` "Tester"

            let question :: Text = "Q"
            let answer :: Text = "A"
            [hsx|<td>{question} → {answer}</td>|] `shouldBeHtml` "<td>Q → A</td>"

        it "should work with html comments" do
            [hsx|<div><!--my comment--></div>|] `shouldBeHtml` "<div><!-- my comment --></div>"
        
        it "should escape variables to avoid XSS" do
            let variableContent :: Text = "<script>alert(1);</script>"
            [hsx|{variableContent}|] `shouldBeHtml` "&lt;script&gt;alert(1);&lt;/script&gt;"

        it "should parse custom web component tags" do
            [hsx|<confetti-effect></confetti-effect>|] `shouldBeHtml` "<confetti-effect></confetti-effect>"
            [hsx|<confetti-effect/>|] `shouldBeHtml` "<confetti-effect></confetti-effect>" -- Currently we cannot deal with self closing tags as expected
            [hsx|<div is="confetti-effect"></div>|] `shouldBeHtml` "<div is=\"confetti-effect\"></div>"

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
            |] `shouldBeHtml` "<html><head><meta charset=\"utf-8\"> <link rel=\"stylesheet\" href=\"/vendor/bootstrap.min.css\"><link rel=\"stylesheet\" href=\"/vendor/flatpickr.min.css\"><link rel=\"stylesheet\" href=\"/app.css\"> <script src=\"/prod.js\"></script><title>IHP Forum</title></head></html>"

        it "should parse an example hsx document" do
            let metaTags = [hsx|
                <meta charset="utf-8">
            |]
            metaTags `shouldBeHtml` "<meta charset=\"utf-8\">"
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
            |] `shouldBeHtml` "<html><head><meta charset=\"utf-8\"> <link rel=\"stylesheet\" href=\"/vendor/bootstrap.min.css\"><link rel=\"stylesheet\" href=\"/vendor/flatpickr.min.css\"><link rel=\"stylesheet\" href=\"/app.css\"> <script src=\"/prod.js\"></script><title>IHP Forum</title></head><body><div class=\"container mt-4\"><nav class=\"navbar navbar-expand-lg navbar-light mb-4\"><a class=\"navbar-brand\" href=\"/\">\955 IHP Forum</a></nav></div></body></html>"

data Project = Project { name :: Text }

shouldBeHtml hsx expectedHtml = (Blaze.renderMarkup hsx) `shouldBe` expectedHtml