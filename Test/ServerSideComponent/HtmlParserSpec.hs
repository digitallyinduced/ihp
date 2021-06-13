{-|
Module: Test.ServerSideComponent.HtmlParserSpec
Copyright: (c) digitally induced GmbH, 2021
-}
module Test.ServerSideComponent.HtmlParserSpec where

import Test.Hspec
import IHP.Prelude
import IHP.ServerSideComponent.HtmlParser

tests = do
    describe "ServerSideComponent" do
        describe "HtmlParser" do
            it "should parse a closing tag with spaces" do
                let p = parseHtml "<div></div >"
                p `shouldBe` (Right (Children [Node "div" [] [] 0 12]))

            it "should parse html nodes with attributes" do
                let p = parseHtml "<div class=\"hello\"> <span id=\"1\"> </span> </div>"
                p `shouldBe` Right (Children {children = [Node {tagName = "div", attributes = [Attribute {attributeName = "class", attributeValue = "hello"}], children = [Node {tagName = "span", attributes = [Attribute {attributeName = "id", attributeValue = "1"}], children = [], startOffset = 20, endOffset = 41}], startOffset = 0, endOffset = 48}]})
            

            it "should parse self closing tags with spaces around it" do
                let p = parseHtml " <div/> "
                p `shouldBe`  (Right (Children [Node "div" [] [] 1 7]))
            
            it "should not loose whitespace text nodes" do
                let p = parseHtml "<table></table> <input/>"
                p `shouldBe`  (Right (Children {children =
                        [ Node {tagName = "table", attributes = [], children = [], startOffset = 0, endOffset = 15}
                        , TextNode " "
                        , Node {tagName = "input", attributes = [], children = [], startOffset = 16, endOffset = 24}
                        ]}))

            it "should parse nested html" do
                let p = parseHtml "<div class=\"loading\">The app is <i>loading</i></div>"
                p `shouldBe` Right (Children {children = [Node {tagName = "div", attributes = [Attribute {attributeName = "class", attributeValue = "loading"}], children = [TextNode {textContent = "The app is "},Node {tagName = "i", attributes = [], children = [TextNode {textContent = "loading"}], startOffset = 32, endOffset = 46}], startOffset = 0, endOffset = 52}]})
