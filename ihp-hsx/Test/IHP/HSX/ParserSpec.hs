{-|
Module: IHP.HSX.QQSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.HSX.ParserSpec where

import Test.Hspec
import Prelude
import IHP.HSX.Parser
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Error as Megaparsec
import qualified "template-haskell" Language.Haskell.TH as TH
import qualified "template-haskell" Language.Haskell.TH.Syntax as TH
import qualified Data.Set as Set


tests = do
    let position = Megaparsec.SourcePos "" (Megaparsec.mkPos 1) (Megaparsec.mkPos 1)
    let extensions = []
    
    describe "HSX Parser" do
        let settings = HsxSettings True Set.empty Set.empty
        it "should fail on invalid html tags" do
            let errorText = "1:13:\n  |\n1 | <myinvalidel>\n  |             ^\nInvalid tag name: myinvalidel\n"
            let (Left error) = parseHsx settings position extensions "<myinvalidel>"
            (Megaparsec.errorBundlePretty error) `shouldBe` errorText

        it "should fail on invalid attribute names" do
            let errorText = "1:23:\n  |\n1 | <div invalid-attribute=\"test\">\n  |                       ^\nInvalid attribute name: invalid-attribute\n"
            let (Left error) = parseHsx settings position extensions "<div invalid-attribute=\"test\">"
            (Megaparsec.errorBundlePretty error) `shouldBe` errorText

        it "should fail on unmatched tags" do
            let errorText = "1:7:\n  |\n1 | <div></span>\n  |       ^\nunexpected '/'\nexpecting \"</div>\", identifier, or white space\n"
            let (Left error) = parseHsx settings position extensions "<div></span>"
            (Megaparsec.errorBundlePretty error) `shouldBe` errorText

        it "should parse a closing tag with spaces" do
            let p = parseHsx settings position extensions "<div></div >"
            p `shouldBe` (Right (Children [Node "div" [] [] False]))

        it "should strip spaces around nodes" do
            let p = parseHsx settings position extensions "<div> <span> </span> </div>"
            p `shouldBe` (Right (Children [Node "div" [] [Node "span" [] [] False] False]))

        it "should strip spaces after self closing tags" do
            let p = parseHsx settings position extensions "<head>{\"meta\"}\n\n                        <link rel=\"stylesheet\" href=\"/vendor/bootstrap.min.css\"></head>"
            p `shouldBe` (Right (Children [Node "head" [] [SplicedNode (TH.LitE (TH.StringL "meta")),Node "link" [StaticAttribute "rel" (TextValue "stylesheet"),StaticAttribute "href" (TextValue "/vendor/bootstrap.min.css")] [] True] False]))

        it "should not strip spaces in a text node" do
            let p = parseHsx settings position extensions " Hello World "
            p `shouldBe` (Right (Children [TextNode "Hello World"]))

        it "should deal with variables in text nodes" do
            let p = parseHsx settings position extensions "<div>\n    Hello {\"name\"}! \n</div>"
            p `shouldBe`  (Right (Children [Node "div" [] [TextNode "Hello ",SplicedNode (TH.LitE (TH.StringL "name")),TextNode "!"] False]))

        it "should parse self closing tags with spaces around it" do
            let p = parseHsx settings position extensions " <div/> "
            p `shouldBe`  (Right (Children [Node "div" [] [] False]))

        it "should collapse spaces" do
            let p = parseHsx settings position extensions "\n    Hello\n    World\n    !    "
            p `shouldBe`  (Right (Children [TextNode "Hello World !"]))
        
        it "should parse spread values" do
            let p = parseHsx settings position extensions "<div {...variables}/>"
            -- We cannot easily construct the @VarE variables@ expression, therefore we use show here for comparison
            show p `shouldBe` "Right (Children [Node \"div\" [SpreadAttributes (VarE variables)] [] False])"

        it "should parse spread values with a space" do
            -- See https://github.com/digitallyinduced/ihp/issues/1588
            let p = parseHsx settings position extensions "<div { ...variables }/>"
            show p `shouldBe` "Right (Children [Node \"div\" [SpreadAttributes (VarE variables)] [] False])"

        it "should accept underscores in data attributes" do
            let p = parseHsx settings position extensions "<div data-client_id=\"test\"/>"
            p `shouldBe` (Right (Children [Node "div" [StaticAttribute "data-client_id" (TextValue "test")] [] False]))

        it "should accept doctype" do
            let p = parseHsx settings position extensions "<!DOCTYPE html><html lang=\"en\"><body>hello</body></html>"
            p `shouldBe` (Right (Children [Node "!DOCTYPE" [StaticAttribute "html" (TextValue "html")] [] True, Node "html" [StaticAttribute "lang" (TextValue "en")] [Node "body" [] [TextNode "hello"] False] False]))

    describe "uncheckedHsx" do
        let settings = HsxSettings False Set.empty Set.empty
        it "should not check markup" do
            let p = parseHsx settings position extensions "<invalid-tag invalid-attribute=\"invalid\"/>"
            p `shouldBe` (Right (Children [Node "invalid-tag" [StaticAttribute "invalid-attribute" (TextValue "invalid")] [] False]))
        
        it "should not check attribute names" do
            let p = parseHsx settings position extensions "<div invalid-attribute=\"invalid\"/>"
            p `shouldBe` (Right (Children [Node "div" [StaticAttribute "invalid-attribute" (TextValue "invalid")] [] False]))

        it "should fail on unmatched tags" do
            let errorText = "1:7:\n  |\n1 | <div></span>\n  |       ^\nunexpected '/'\nexpecting \"</div>\", identifier, or white space\n"
            let (Left error) = parseHsx settings position extensions "<div></span>"
            (Megaparsec.errorBundlePretty error) `shouldBe` errorText

        it "should parse a closing tag with spaces" do
            let p = parseHsx settings position extensions "<div></div >"
            p `shouldBe` (Right (Children [Node "div" [] [] False]))

        it "should strip spaces around nodes" do
            let p = parseHsx settings position extensions "<div> <span> </span> </div>"
            p `shouldBe` (Right (Children [Node "div" [] [Node "span" [] [] False] False]))

        it "should strip spaces after self closing tags" do
            let p = parseHsx settings position extensions "<head>{\"meta\"}\n\n                        <link rel=\"stylesheet\" href=\"/vendor/bootstrap.min.css\"></head>"
            p `shouldBe` (Right (Children [Node "head" [] [SplicedNode (TH.LitE (TH.StringL "meta")),Node "link" [StaticAttribute "rel" (TextValue "stylesheet"),StaticAttribute "href" (TextValue "/vendor/bootstrap.min.css")] [] True] False]))

        it "should not strip spaces in a text node" do
            let p = parseHsx settings position extensions " Hello World "
            p `shouldBe` (Right (Children [TextNode "Hello World"]))

        it "should deal with variables in text nodes" do
            let p = parseHsx settings position extensions "<div>\n    Hello {\"name\"}! \n</div>"
            p `shouldBe`  (Right (Children [Node "div" [] [TextNode "Hello ",SplicedNode (TH.LitE (TH.StringL "name")),TextNode "!"] False]))

        it "should parse self closing tags with spaces around it" do
            let p = parseHsx settings position extensions " <div/> "
            p `shouldBe`  (Right (Children [Node "div" [] [] False]))

        it "should collapse spaces" do
            let p = parseHsx settings position extensions "\n    Hello\n    World\n    !    "
            p `shouldBe`  (Right (Children [TextNode "Hello World !"]))
        
        it "should parse spread values" do
            let p = parseHsx settings position extensions "<div {...variables}/>"
            -- We cannot easily construct the @VarE variables@ expression, therefore we use show here for comparison
            show p `shouldBe` "Right (Children [Node \"div\" [SpreadAttributes (VarE variables)] [] False])"

        it "should parse spread values with a space" do
            -- See https://github.com/digitallyinduced/ihp/issues/1588
            let p = parseHsx settings position extensions "<div { ...variables }/>"
            show p `shouldBe` "Right (Children [Node \"div\" [SpreadAttributes (VarE variables)] [] False])"

        it "should accept underscores in data attributes" do
            let p = parseHsx settings position extensions "<div data-client_id=\"test\"/>"
            p `shouldBe` (Right (Children [Node "div" [StaticAttribute "data-client_id" (TextValue "test")] [] False]))

        it "should accept doctype" do
            let p = parseHsx settings position extensions "<!DOCTYPE html><html lang=\"en\"><body>hello</body></html>"
            p `shouldBe` (Right (Children [Node "!DOCTYPE" [StaticAttribute "html" (TextValue "html")] [] True, Node "html" [StaticAttribute "lang" (TextValue "en")] [Node "body" [] [TextNode "hello"] False] False]))

    describe "customHsx" do
        let customSettings = HsxSettings True 
                (Set.fromList ["mycustomtag"])
                (Set.fromList ["my-custom-attr"])

        it "should allow specified custom tags" do
            let p = parseHsx customSettings position extensions "<mycustomtag>hello</mycustomtag>"
            p `shouldBe` (Right (Children [Node "mycustomtag" [] [TextNode "hello"] False]))

        it "should reject non-specified custom tags" do
            let errorText = "1:15:\n  |\n1 | <notallowedtag>hello</notallowedtag>\n  |               ^\nInvalid tag name: notallowedtag\n"
            case parseHsx customSettings position extensions "<notallowedtag>hello</notallowedtag>" of
                Left error -> (Megaparsec.errorBundlePretty error) `shouldBe` errorText
                Right _ -> fail "Expected parser to fail with invalid tag name"

        it "should allow specified custom attributes" do
            let p = parseHsx customSettings position extensions "<div my-custom-attr=\"hello\">test</div>"
            p `shouldBe` (Right (Children [Node "div" [StaticAttribute "my-custom-attr" (TextValue "hello")] [TextNode "test"] False]))

        it "should reject non-specified custom attributes" do
            let errorText = "1:22:\n  |\n1 | <div not-allowed-attr=\"test\">\n  |                      ^\nInvalid attribute name: not-allowed-attr\n"
            case parseHsx customSettings position extensions "<div not-allowed-attr=\"test\">" of
                Left error -> (Megaparsec.errorBundlePretty error) `shouldBe` errorText
                Right _ -> fail "Expected parser to fail with invalid attribute name"

        it "should allow mixing custom and standard elements" do
            let p = parseHsx customSettings position extensions "<mycustomtag class=\"hello\" my-custom-attr=\"world\">test</mycustomtag>"
            p `shouldBe` (Right (Children [Node "mycustomtag" [StaticAttribute "class" (TextValue "hello"), StaticAttribute "my-custom-attr" (TextValue "world")] [TextNode "test"] False]))