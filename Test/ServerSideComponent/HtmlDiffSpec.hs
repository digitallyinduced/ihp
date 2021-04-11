{-|
Module: Test.ServerSideComponent.HtmlParserSpec
Copyright: (c) digitally induced GmbH, 2021
-}
module Test.ServerSideComponent.HtmlDiffSpec where

import Test.Hspec
import IHP.Prelude
import IHP.ServerSideComponent.HtmlParser
import IHP.ServerSideComponent.HtmlDiff

tests = do
    describe "ServerSideComponent" do
        describe "HtmlDiff" do
            describe "diffHtml" do
                it "should work on trivial cases" do
                    let a = "<div>hello</div>"
                    let b = "<div>world</div>"
                    let result = diffHtml a b
                    let expected = [UpdateTextContent {textContent = "world", path = [0,0]}]
                    result `shouldBe` (Right expected)

                it "should work on complex cases" do
                    let a = "<div class=\"loading\">The app is <i>loading</i></div>"
                    let b = "<div class=\"loaded\"><p>loaded</p></div>"
                    let result = diffHtml a b
                    let expected =
                            [ UpdateNode {attributeOperations = [UpdateAttribute {attributeName = "class", attributeValue = "loaded"}], path = [0]}
                            , ReplaceNode {oldNode = TextNode {textContent = "The app is "}, newNode = Node {tagName = "p", attributes = [], children = [TextNode {textContent = "loaded"}], startOffset = 20, endOffset = 33}, path = [0,0]}
                            , DeleteNode {node = Node {tagName = "i", attributes = [], children = [TextNode {textContent = "loading"}], startOffset = 32, endOffset = 46}, path = [1,0]}
                            ]
                    result `shouldBe` (Right expected)

            describe "diffNodes" do
                let ?oldHtml = ""
                let ?newHtml = ""
                it "should work on text nodes" do
                    let a = TextNode { textContent = "old" }
                    let b = TextNode { textContent = "new" }
                    let result = diffNodes a b
                    let expected = [UpdateTextContent { textContent = "new", path = [] }]
                    result `shouldBe` expected

                it "should patch comment nodes" do
                    let a = CommentNode { comment = "old" }
                    let b = CommentNode { comment = "new" }
                    let result = diffNodes a b
                    let expected = [UpdateComment { comment = "new", path = [] }]
                    result `shouldBe` expected

                it "should patch PreEscapedText nodes" do
                    let a = PreEscapedTextNode { textContent = "alert(1)" }
                    let b = PreEscapedTextNode { textContent = "alert(2)" }
                    let result = diffNodes a b
                    let expected = [UpdatePreEscapedTextNode { textContent = "alert(2)", path = [] }]
                    result `shouldBe` expected

                it "should replace nodes if the tag name is different" do
                    let a = Node { tagName = "a", attributes = [Attribute { attributeName = "href", attributeValue = "#" }], children = [ TextNode { textContent = "hello" } ], startOffset = 0, endOffset = 0 }
                    let b = Node { tagName = "div", attributes = [Attribute { attributeName = "class", attributeValue = "d-block" }], children = [], startOffset = 0, endOffset = 0 }
                    let result = diffNodes a b
                    let expected = [ReplaceNode { oldNode = a, newNode = b, path = [] }]
                    result `shouldBe` expected

                it "should patch attributes if the tag name is the same" do
                    let a = Node { tagName = "a", attributes = [Attribute { attributeName = "href", attributeValue = "#" }], children = [], startOffset = 0, endOffset = 0 }
                    let b = Node { tagName = "a", attributes = [Attribute { attributeName = "href", attributeValue = "http://example.com/" }], children = [], startOffset = 0, endOffset = 0 }
                    let result = diffNodes a b
                    let expected = [UpdateNode { attributeOperations = [UpdateAttribute { attributeName = "href", attributeValue = "http://example.com/"}], path = [] }]
                    result `shouldBe` expected

                it "should patch children" do
                    let a = Children { children = [
                                Node { tagName = "a", attributes = [Attribute { attributeName = "href", attributeValue = "#" }], children = [], startOffset = 0, endOffset = 0 }
                            ] }
                    let b = Children { children = [
                                Node { tagName = "a", attributes = [Attribute { attributeName = "href", attributeValue = "http://example.com/" }], children = [], startOffset = 0, endOffset = 0 }
                            ] }
                    let result = diffNodes a b
                    let expected = [UpdateNode { attributeOperations = [UpdateAttribute { attributeName = "href", attributeValue = "http://example.com/"}], path = [0] }]
                    result `shouldBe` expected

                it "should patch children of nodes if the tag name is the same" do
                    let a = Node { tagName = "a", attributes = [Attribute { attributeName = "href", attributeValue = "#" }], children = [ Node { tagName = "strong", attributes = [], children = [TextNode "hello"], startOffset = 0, endOffset = 0 } ], startOffset = 0, endOffset = 0 }
                    let b = Node { tagName = "a", attributes = [Attribute { attributeName = "href", attributeValue = "http://example.com/" }], children = [ Node { tagName = "strong", attributes = [], children = [TextNode "world"], startOffset = 0, endOffset = 0 } ], startOffset = 0, endOffset = 0 }
                    let result = diffNodes a b
                    let expected =
                            [ UpdateNode { attributeOperations = [UpdateAttribute { attributeName = "href", attributeValue = "http://example.com/"}], path = [] }
                            , UpdateTextContent { textContent = "world", path = [0,0]}
                            ]
                    result `shouldBe` expected


                it "should delete children nodes" do
                    let a = Node { tagName = "a", attributes = [], children = [ TextNode { textContent = "a" }, TextNode { textContent = "b" } ], startOffset = 0, endOffset = 0 }
                    let b = Node { tagName = "a", attributes = [], children = [ TextNode { textContent = "a" } ], startOffset = 0, endOffset = 0 }
                    let result = diffNodes a b
                    let expected =
                            [ DeleteNode { node = TextNode { textContent = "b" }, path = [1] }
                            ]
                    result `shouldBe` expected



            describe "diffAttributes" do
                it "detect new attributes" do
                    let old = [Attribute "value" ""]
                    let new = [Attribute "value" "", Attribute "disabled" ""]

                    let patch = diffAttributes old new
                    patch  `shouldBe` [AddAttribute { attributeName = "disabled", attributeValue = "" }]

                it "detect updated attributes" do
                    let old = [Attribute "value" ""]
                    let new = [Attribute "value" "new value"]

                    let patch = diffAttributes old new
                    patch  `shouldBe` [UpdateAttribute { attributeName = "value", attributeValue = "new value" }]

                it "detect deleted attributes" do
                    let old = [Attribute "value" ""]
                    let new = []

                    let patch = diffAttributes old new
                    patch  `shouldBe` [DeleteAttribute { attributeName = "value" }]