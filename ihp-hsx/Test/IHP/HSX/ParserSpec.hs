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
import qualified Data.List as List


tests = do
    let position = Megaparsec.SourcePos "" (Megaparsec.mkPos 1) (Megaparsec.mkPos 1)
    let extensions = []
    
    describe "HSX Parser" do
        let settings = HsxSettings True Set.empty Set.empty
        it "should fail on invalid html tags" do
            case parseHsx settings position extensions "<myinvalidel>" of
                Left error -> (Megaparsec.errorBundlePretty error) `shouldSatisfy` \e ->
                    "Invalid tag name: myinvalidel" `List.isInfixOf` e
                Right _ -> fail "Expected parser to fail with invalid tag name"

        it "should fail on invalid attribute names" do
            case parseHsx settings position extensions "<div invalid-attribute=\"test\">" of
                Left error -> (Megaparsec.errorBundlePretty error) `shouldSatisfy` \e ->
                    "Invalid attribute name: invalid-attribute" `List.isInfixOf` e
                Right _ -> fail "Expected parser to fail with invalid attribute name"

        it "should fail on unmatched tags" do
            let errorText = "1:6:\n  |\n1 | <div></span>\n  |      ^^^^^\nunexpected \"</spa\"\nexpecting closing tag </div> (to match opening <div> tag) or white space\n"
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

        it "should accept autocapitalize attribute" do
            let p = parseHsx settings position extensions "<input autocapitalize=\"off\"/>"
            p `shouldBe` (Right (Children [Node "input" [StaticAttribute "autocapitalize" (TextValue "off")] [] True]))

        it "should accept autocapitalize attribute on textarea" do
            let p = parseHsx settings position extensions "<textarea autocapitalize=\"none\"></textarea>"
            p `shouldBe` (Right (Children [Node "textarea" [StaticAttribute "autocapitalize" (TextValue "none")] [] False]))

        it "should accept doctype" do
            let p = parseHsx settings position extensions "<!DOCTYPE html><html lang=\"en\"><body>hello</body></html>"
            p `shouldBe` (Right (Children [Node "!DOCTYPE" [StaticAttribute "html" (TextValue "html")] [] True, Node "html" [StaticAttribute "lang" (TextValue "en")] [Node "body" [] [TextNode "hello"] False] False]))

        it "should suggest similar tag names for typos" do
            case parseHsx settings position extensions "<dvi>" of
                Left error -> (Megaparsec.errorBundlePretty error) `shouldSatisfy` \e ->
                    "Invalid tag name: dvi" `List.isInfixOf` e && "div" `List.isInfixOf` e
                Right _ -> fail "Expected parser to fail with invalid tag name"

        it "should suggest similar attribute names for typos" do
            case parseHsx settings position extensions "<div classs=\"test\">" of
                Left error -> (Megaparsec.errorBundlePretty error) `shouldSatisfy` \e ->
                    "Invalid attribute name: classs" `List.isInfixOf` e && "class" `List.isInfixOf` e
                Right _ -> fail "Expected parser to fail with invalid attribute name"

        it "should not suggest anything for completely wrong names" do
            case parseHsx settings position extensions "<zzzzz>" of
                Left error -> (Megaparsec.errorBundlePretty error) `shouldSatisfy` \e ->
                    "Invalid tag name: zzzzz" `List.isInfixOf` e && not ("Did you mean" `List.isInfixOf` e)
                Right _ -> fail "Expected parser to fail with invalid tag name"

    describe "uncheckedHsx" do
        let settings = HsxSettings False Set.empty Set.empty
        it "should not check markup" do
            let p = parseHsx settings position extensions "<invalid-tag invalid-attribute=\"invalid\"/>"
            p `shouldBe` (Right (Children [Node "invalid-tag" [StaticAttribute "invalid-attribute" (TextValue "invalid")] [] False]))
        
        it "should not check attribute names" do
            let p = parseHsx settings position extensions "<div invalid-attribute=\"invalid\"/>"
            p `shouldBe` (Right (Children [Node "div" [StaticAttribute "invalid-attribute" (TextValue "invalid")] [] False]))

        it "should fail on unmatched tags" do
            let errorText = "1:6:\n  |\n1 | <div></span>\n  |      ^^^^^\nunexpected \"</spa\"\nexpecting closing tag </div> (to match opening <div> tag) or white space\n"
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
            case parseHsx customSettings position extensions "<notallowedtag>hello</notallowedtag>" of
                Left error -> (Megaparsec.errorBundlePretty error) `shouldSatisfy` \e ->
                    "Invalid tag name: notallowedtag" `List.isInfixOf` e
                Right _ -> fail "Expected parser to fail with invalid tag name"

        it "should allow specified custom attributes" do
            let p = parseHsx customSettings position extensions "<div my-custom-attr=\"hello\">test</div>"
            p `shouldBe` (Right (Children [Node "div" [StaticAttribute "my-custom-attr" (TextValue "hello")] [TextNode "test"] False]))

        it "should reject non-specified custom attributes" do
            case parseHsx customSettings position extensions "<div not-allowed-attr=\"test\">" of
                Left error -> (Megaparsec.errorBundlePretty error) `shouldSatisfy` \e ->
                    "Invalid attribute name: not-allowed-attr" `List.isInfixOf` e
                Right _ -> fail "Expected parser to fail with invalid attribute name"

        it "should allow mixing custom and standard elements" do
            let p = parseHsx customSettings position extensions "<mycustomtag class=\"hello\" my-custom-attr=\"world\">test</mycustomtag>"
            p `shouldBe` (Right (Children [Node "mycustomtag" [StaticAttribute "class" (TextValue "hello"), StaticAttribute "my-custom-attr" (TextValue "world")] [TextNode "test"] False]))

    describe "SVG" do
        let settings = HsxSettings True Set.empty Set.empty

        it "should parse a basic svg element" do
            let p = parseHsx settings position extensions "<svg viewBox=\"0 0 100 100\"></svg>"
            p `shouldBe` (Right (Children [Node "svg" [StaticAttribute "viewBox" (TextValue "0 0 100 100")] [] False]))

        it "should parse svg shapes" do
            let p = parseHsx settings position extensions "<circle cx=\"50\" cy=\"50\" r=\"40\"/>"
            p `shouldBe` (Right (Children [Node "circle" [StaticAttribute "cx" (TextValue "50"), StaticAttribute "cy" (TextValue "50"), StaticAttribute "r" (TextValue "40")] [] False]))

        it "should parse self-closing svg elements" do
            let p = parseHsx settings position extensions "<path d=\"M10,10 L90,90\"/>"
            p `shouldBe` (Right (Children [Node "path" [StaticAttribute "d" (TextValue "M10,10 L90,90")] [] False]))

        it "should parse rect with presentation attributes" do
            let p = parseHsx settings position extensions "<rect x=\"10\" y=\"10\" width=\"80\" height=\"80\" fill=\"red\" stroke=\"black\" stroke-width=\"2\"/>"
            p `shouldBe` (Right (Children [Node "rect" [StaticAttribute "x" (TextValue "10"), StaticAttribute "y" (TextValue "10"), StaticAttribute "width" (TextValue "80"), StaticAttribute "height" (TextValue "80"), StaticAttribute "fill" (TextValue "red"), StaticAttribute "stroke" (TextValue "black"), StaticAttribute "stroke-width" (TextValue "2")] [] False]))

        it "should parse nested svg groups" do
            let p = parseHsx settings position extensions "<svg><g transform=\"translate(10,10)\"><rect width=\"50\" height=\"50\"/></g></svg>"
            p `shouldBe` (Right (Children [Node "svg" [] [Node "g" [StaticAttribute "transform" (TextValue "translate(10,10)")] [Node "rect" [StaticAttribute "width" (TextValue "50"), StaticAttribute "height" (TextValue "50")] [] False] False] False]))

        it "should parse svg text element" do
            let p = parseHsx settings position extensions "<text x=\"10\" y=\"30\" font-size=\"20\">Hello</text>"
            p `shouldBe` (Right (Children [Node "text" [StaticAttribute "x" (TextValue "10"), StaticAttribute "y" (TextValue "30"), StaticAttribute "font-size" (TextValue "20")] [TextNode "Hello"] False]))

        it "should parse linearGradient with stops" do
            let p = parseHsx settings position extensions "<linearGradient id=\"grad1\"><stop offset=\"0%\" stop-color=\"red\"/><stop offset=\"100%\" stop-color=\"blue\"/></linearGradient>"
            p `shouldBe` (Right (Children [Node "linearGradient" [StaticAttribute "id" (TextValue "grad1")] [Node "stop" [StaticAttribute "offset" (TextValue "0%"), StaticAttribute "stop-color" (TextValue "red")] [] False, Node "stop" [StaticAttribute "offset" (TextValue "100%"), StaticAttribute "stop-color" (TextValue "blue")] [] False] False]))

        it "should parse svg image element" do
            let p = parseHsx settings position extensions "<image href=\"photo.png\" width=\"100\" height=\"100\"/>"
            p `shouldBe` (Right (Children [Node "image" [StaticAttribute "href" (TextValue "photo.png"), StaticAttribute "width" (TextValue "100"), StaticAttribute "height" (TextValue "100")] [] False]))

        it "should parse camelCase svg element names" do
            let p = parseHsx settings position extensions "<clipPath id=\"clip\"><rect width=\"100\" height=\"100\"/></clipPath>"
            p `shouldBe` (Right (Children [Node "clipPath" [StaticAttribute "id" (TextValue "clip")] [Node "rect" [StaticAttribute "width" (TextValue "100"), StaticAttribute "height" (TextValue "100")] [] False] False]))

        it "should parse filter elements" do
            let p = parseHsx settings position extensions "<filter id=\"blur\"><feGaussianBlur stdDeviation=\"5\"/></filter>"
            p `shouldBe` (Right (Children [Node "filter" [StaticAttribute "id" (TextValue "blur")] [Node "feGaussianBlur" [StaticAttribute "stdDeviation" (TextValue "5")] [] False] False]))

        it "should parse defs/use pattern" do
            let p = parseHsx settings position extensions "<svg><defs><circle id=\"dot\" r=\"5\"/></defs><use href=\"#dot\" x=\"10\" y=\"10\"/></svg>"
            p `shouldBe` (Right (Children [Node "svg" [] [Node "defs" [] [Node "circle" [StaticAttribute "id" (TextValue "dot"), StaticAttribute "r" (TextValue "5")] [] False] False, Node "use" [StaticAttribute "href" (TextValue "#dot"), StaticAttribute "x" (TextValue "10"), StaticAttribute "y" (TextValue "10")] [] False] False]))

        it "should parse svg with spliced attribute" do
            let p = parseHsx settings position extensions "<circle cx=\"50\" cy=\"50\" r={\"40\"}/>"
            p `shouldBe` (Right (Children [Node "circle" [StaticAttribute "cx" (TextValue "50"), StaticAttribute "cy" (TextValue "50"), StaticAttribute "r" (ExpressionValue (TH.LitE (TH.StringL "40")))] [] False]))

        it "should parse new SVG attributes (fr, paint-order, vector-effect, transform-origin)" do
            let p = parseHsx settings position extensions "<circle paint-order=\"stroke\" vector-effect=\"non-scaling-stroke\" transform-origin=\"center\"/>"
            p `shouldBe` (Right (Children [Node "circle" [StaticAttribute "paint-order" (TextValue "stroke"), StaticAttribute "vector-effect" (TextValue "non-scaling-stroke"), StaticAttribute "transform-origin" (TextValue "center")] [] False]))

        it "should parse radialGradient with fr attribute" do
            let p = parseHsx settings position extensions "<radialGradient fr=\"20%\"></radialGradient>"
            p `shouldBe` (Right (Children [Node "radialGradient" [StaticAttribute "fr" (TextValue "20%")] [] False]))