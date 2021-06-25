{-|
Module: Test.Controller.ParamSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.Controller.ParamSpec where

import IHP.Prelude
import IHP.HaskellSupport
import Test.Hspec
import IHP.Controller.Param
import IHP.Controller.Context
import IHP.Controller.RequestContext
import IHP.ModelSupport
import qualified Data.Aeson as Aeson
import qualified Data.UUID as UUID
import qualified Data.TMap as TypeMap
import qualified Network.Wai as Wai
import qualified GHC.IO as IO

tests = do
    describe "IHP.Controller.Param" do
        describe "param" do
            it "should parse valid input" do
                let ?context = createControllerContextWithParams [("page", "1")]
                (param @Int "page") `shouldBe` 1
            
            it "should fail on empty input" do
                let ?context = createControllerContextWithParams [("page", "")]
                (IO.evaluate (param @Int "page")) `shouldThrow` (== ParamCouldNotBeParsedException { name = "page", parserError = "ParamReader Int: not enough input" })
            
            it "should fail if param not provided" do
                let ?context = createControllerContextWithParams []
                (IO.evaluate (param @Int "page")) `shouldThrow` (== ParamNotFoundException { name = "page" })
            
            it "should fail with a parser error on invalid input" do
                let ?context = createControllerContextWithParams [("page", "NaN")]
                (IO.evaluate (param @Int "page")) `shouldThrow` (== ParamCouldNotBeParsedException { name = "page", parserError = "ParamReader Int: Failed reading: takeWhile1" })

        describe "paramOrNothing" do
            it "should parse valid input" do
                let ?context = createControllerContextWithParams [("referredBy", "776ab71d-327f-41b3-90a8-7b5a251c4b88")]
                (paramOrNothing @UUID "referredBy") `shouldBe` (Just "776ab71d-327f-41b3-90a8-7b5a251c4b88")
            
            it "should return Nothing on empty input" do
                let ?context = createControllerContextWithParams [("referredBy", "")]
                (paramOrNothing @UUID "referredBy") `shouldBe` Nothing
            
            it "should return Nothing if param not provided" do
                let ?context = createControllerContextWithParams []
                (paramOrNothing @UUID "referredBy") `shouldBe` Nothing
            
            it "should fail with a parser error on invalid input" do
                let ?context = createControllerContextWithParams [("referredBy", "not a uuid")]
                (IO.evaluate (paramOrNothing @UUID "referredBy")) `shouldThrow` (== ParamCouldNotBeParsedException { name = "referredBy", parserError = "FromParameter UUID: Parse error" })

        describe "paramOrDefault" do
            it "should parse valid input" do
                let ?context = createControllerContextWithParams [("page", "1")]
                (paramOrDefault @Int 0 "page") `shouldBe` 1
            
            it "should return default value on empty input" do
                let ?context = createControllerContextWithParams [("page", "")]
                (paramOrDefault @Int 10 "page") `shouldBe` 10
            
            it "should return default value if param not provided" do
                let ?context = createControllerContextWithParams []
                (paramOrDefault @Int 10 "page") `shouldBe` 10
            
            it "should fail with a parser error on invalid input" do
                let ?context = createControllerContextWithParams [("page", "NaN")]
                (IO.evaluate (paramOrDefault @Int 10 "page")) `shouldThrow` (== ParamCouldNotBeParsedException { name = "page", parserError = "ParamReader Int: Failed reading: takeWhile1" })


        describe "paramList" do
            it "should parse valid input" do
                let ?context = createControllerContextWithParams [("ingredients", "milk"), ("ingredients", "egg")]
                (paramList @Text "ingredients") `shouldBe` ["milk", "egg"]
            
            it "should fail on invalid input" do
                let ?context = createControllerContextWithParams [("numbers", "1"), ("numbers", "NaN")]
                (IO.evaluate (paramList @Int "numbers")) `shouldThrow` (errorCall "param: Parameter 'numbers' is invalid")
            
            it "should deal with empty input" do
                let ?context = createControllerContextWithParams []
                (paramList @Int "numbers") `shouldBe` []

        describe "hasParam" do
            it "returns True if param given" do
                let ?context = createControllerContextWithParams [("a", "test")]
                hasParam "a" `shouldBe` True
            
            it "returns True if param given but empty" do
                let ?context = createControllerContextWithParams [("a", "")]
                hasParam "a" `shouldBe` True
            
            it "returns False if param missing" do
                let ?context = createControllerContextWithParams []
                hasParam "a" `shouldBe` False

        describe "ParamReader" do
            describe "ByteString" do
                it "should handle text input" do
                    (readParameter @ByteString "test") `shouldBe` (Right "test")
                it "should handle JSON strings" do
                    (readParameterJSON @ByteString (json "\"test\"")) `shouldBe` (Right ("test" :: ByteString))
                
                it "should fail on other JSON input" do
                    (readParameterJSON @ByteString (json "1")) `shouldBe` (Left ("ParamReader ByteString: Expected String" :: ByteString))
            
            describe "Int" do
                it "should accept numeric input" do
                    (readParameter @Int "1337") `shouldBe` (Right 1337)
                
                it "should accept negative numbers" do
                    (readParameter @Int "-1337") `shouldBe` (Right (-1337))

                it "should accept JSON numerics " do
                    (readParameterJSON @Int (json "1337")) `shouldBe` (Right 1337)
                
                it "should fail on other JSON input " do
                    (readParameterJSON @Int (json "true")) `shouldBe` (Left "ParamReader Int: Expected Int")

            describe "Integer" do
                it "should accept numeric input" do
                    (readParameter @Integer "1337") `shouldBe` (Right 1337)
                
                it "should accept negative numbers" do
                    (readParameter @Integer "-1337") `shouldBe` (Right (-1337))

                it "should accept JSON numerics " do
                    (readParameterJSON @Integer (json "1337")) `shouldBe` (Right 1337)
                
                it "should fail on other JSON input " do
                    (readParameterJSON @Integer (json "true")) `shouldBe` (Left "ParamReader Integer: Expected Integer")
                    (readParameterJSON @Integer (json "\"1\"")) `shouldBe` (Left "ParamReader Integer: Expected Integer")


            describe "Double" do
                it "should accept integer input" do
                    (readParameter @Double "1337") `shouldBe` (Right 1337)

                it "should accept floating point input" do
                    (readParameter @Double "1.2") `shouldBe` (Right 1.2)
                    (readParameter @Float "1.2345679") `shouldBe` (Right 1.2345679)
                
                it "should accept JSON integer input" do
                    (readParameterJSON @Double (json "1337")) `shouldBe` (Right 1337)

                it "should accept JSON floating point input" do
                    (readParameterJSON @Double (json "1.2")) `shouldBe` (Right 1.2)
                
                it "should fail on other JSON input " do
                    (readParameterJSON @Double (json "true")) `shouldBe` (Left "ParamReader Double: Expected Double")
                    (readParameterJSON @Double (json "\"1\"")) `shouldBe` (Left "ParamReader Double: Expected Double")

            describe "Float" do
                it "should accept integer input" do
                    (readParameter @Float "1337") `shouldBe` (Right 1337)

                it "should accept floating point input" do
                    (readParameter @Float "1.2") `shouldBe` (Right 1.2)
                    (readParameter @Float "1.2345679") `shouldBe` (Right 1.2345679)
                
                it "should accept JSON integer input" do
                    (readParameterJSON @Float (json "1337")) `shouldBe` (Right 1337)

                it "should accept JSON floating point input" do
                    (readParameterJSON @Float (json "1.2")) `shouldBe` (Right 1.2)
                
                it "should fail on other JSON input " do
                    (readParameterJSON @Float (json "true")) `shouldBe` (Left "ParamReader Float: Expected Float")
                    (readParameterJSON @Float (json "\"1\"")) `shouldBe` (Left "ParamReader Float: Expected Float")

            describe "Point" do
                it "should accept integer input" do
                    (readParameter @Point "1337,1338") `shouldBe` (Right Point { x = 1337, y = 1338 })

                it "should accept floating point input" do
                    (readParameter @Point "1.2,1.3") `shouldBe` (Right Point { x = 1.2, y = 1.3 })
                
                it "should accept JSON integer input" do
                    (readParameterJSON @Point (json "\"1337,1338\"")) `shouldBe` (Right Point { x = 1337, y = 1338 })

                it "should accept JSON floating point input" do
                    (readParameterJSON @Point (json "\"1.2,1.3\"")) `shouldBe` (Right Point { x = 1.2, y = 1.3 })
                
                it "should fail on other JSON input " do
                    (readParameterJSON @Point (json "true")) `shouldBe` (Left "ParamReader Point: Expected Point")
                    (readParameterJSON @Point (json "\"1\"")) `shouldBe` (Left "ParamReader Point: ,: not enough input")
                    (readParameterJSON @Point (json "\"1.2\"")) `shouldBe` (Left "ParamReader Point: ,: not enough input")

            describe "Text" do
                it "should handle text input" do
                    (readParameter @Text "test") `shouldBe` (Right "test")
                
                it "should handle JSON strings" do
                    (readParameterJSON @Text (json "\"test\"")) `shouldBe` (Right ("test"))
                
                it "should fail on other JSON input" do
                    (readParameterJSON @Text (json "1")) `shouldBe` (Left ("ParamReader Text: Expected String"))

            describe "CSV" do
                it "should handle empty input" do
                    (readParameter @[Int] "") `shouldBe` (Right [])

                it "should handle a single value" do
                    (readParameter @[Int] "1") `shouldBe` (Right [1])
                
                it "should handle comma separated values" do
                    (readParameter @[Int] "1,2,3") `shouldBe` (Right [1,2,3])

                it "should fail if a single value is invalid" do
                    (readParameter @[Int] "1,a,3") `shouldBe` (Left "ParamReader Int: Failed reading: takeWhile1")
                
                it "should handle JSON arrays" do
                    (readParameterJSON @[Int] (json "[1,2,3]")) `shouldBe` (Right [1,2,3])
                
                it "should fail on JSON input that is not an array" do
                    (readParameterJSON @[Int] (json "true")) `shouldBe` (Left "ParamReader Text: Expected Array")

            describe "Bool" do
                it "should accept 'on' as True" do
                    (readParameter @Bool "on") `shouldBe` (Right True)

                it "should accept 'true' as True" do
                    (readParameter @Bool "true") `shouldBe` (Right True)
                    (readParameter @Bool "TruE") `shouldBe` (Right True)

                it "should accept everything else as false input" do
                    (readParameter @Bool "off") `shouldBe` (Right False)
                    (readParameter @Bool "false") `shouldBe` (Right False)
                    (readParameter @Bool "invalid") `shouldBe` (Right False)

            describe "UUID" do
                it "should accept UUID values" do
                    (readParameter @UUID "6188329c-6bad-47f6-800c-2fd19ce0b2df") `shouldBe` (Right "6188329c-6bad-47f6-800c-2fd19ce0b2df")
                    (readParameter @UUID "a020ba17-a94e-453f-9414-c54aa30caa54") `shouldBe` (Right "a020ba17-a94e-453f-9414-c54aa30caa54")

                it "should fail on invalid values" do
                    (readParameter @UUID "not a uuid") `shouldBe` (Left "FromParameter UUID: Parse error")

                it "should accept JSON UUIDs" do
                    (readParameterJSON @UUID (json "\"6188329c-6bad-47f6-800c-2fd19ce0b2df\"")) `shouldBe` (Right "6188329c-6bad-47f6-800c-2fd19ce0b2df")
                
                it "should fail on invalid JSON input" do
                    (readParameterJSON @UUID (json "\"not a uuid\"")) `shouldBe` (Left "FromParameter UUID: Parse error")
                    (readParameterJSON @UUID (json "false")) `shouldBe` (Left "ParamReader UUID: Expected String")

            describe "UTCTime" do
                it "should accept timestamps" do
                    (tshow (readParameter @UTCTime "2020-11-08T12:03:35Z")) `shouldBe` ("Right 2020-11-08 12:03:35 UTC")
                
                it "should accept dates" do
                    (tshow (readParameter @UTCTime "2020-11-08")) `shouldBe` ("Right 2020-11-08 00:00:00 UTC")
                
                it "should fail on invalid inputs" do
                    (readParameter @UTCTime "not a timestamp") `shouldBe` (Left "ParamReader UTCTime: Failed parsing")
                
                it "should accept JSON strings" do
                    (tshow (readParameterJSON @UTCTime (json "\"2020-11-08T12:03:35Z\""))) `shouldBe` ("Right 2020-11-08 12:03:35 UTC")

            describe "LocalTime" do
                it "should accept timestamps" do
                    (tshow (readParameter @LocalTime "2020-11-08T12:03:35Z")) `shouldBe` ("Right 2020-11-08 12:03:35")
                
                it "should accept dates" do
                    (tshow (readParameter @LocalTime "2020-11-08")) `shouldBe` ("Right 2020-11-08 00:00:00")
                
                it "should fail on invalid inputs" do
                    (readParameter @LocalTime "not a timestamp") `shouldBe` (Left "ParamReader LocalTime: Failed parsing")
                
                it "should accept JSON strings" do
                    (tshow (readParameterJSON @LocalTime (json "\"2020-11-08T12:03:35Z\""))) `shouldBe` ("Right 2020-11-08 12:03:35")

            describe "Day" do
                it "should accept dates" do
                    (tshow (readParameter @Day "2020-11-08")) `shouldBe` ("Right 2020-11-08")
                
                it "should fail on invalid inputs" do
                    (readParameter @Day "not a timestamp") `shouldBe` (Left "ParamReader Day: Failed parsing")
                
                it "should accept JSON strings" do
                    (tshow (readParameterJSON @Day (json "\"2020-11-08\""))) `shouldBe` ("Right 2020-11-08")

            describe "TimeOfDay" do
                it "should accept time values" do
                    (tshow (readParameter @TimeOfDay "12:00:00")) `shouldBe` ("Right 12:00:00")
                
                it "should fail on invalid inputs" do
                    (readParameter @TimeOfDay "not a time") `shouldBe` (Left "ParamReader TimeOfDay: Please enter a valid time in the format hh:mm:ss")
                    (readParameter @TimeOfDay "25:00:00") `shouldBe` (Left "ParamReader TimeOfDay: Please enter a valid time in the format hh:mm:ss")
                
                it "should accept JSON strings" do
                    (tshow (readParameterJSON @TimeOfDay (json "\"13:37:00\""))) `shouldBe` ("Right 13:37:00")

            describe "Maybe" do
                it "should accept values" do
                    (readParameter @(Maybe Int) "1") `shouldBe` (Right (Just 1))
                    (readParameter @(Maybe Text) "hello") `shouldBe` (Right (Just "hello"))

                it "should handle empty input as Nothing" do
                    (readParameter @(Maybe Int) "") `shouldBe` (Right Nothing)
                    (readParameter @(Maybe UUID) "") `shouldBe` (Right Nothing)
                
                it "should handle empty Text as Just" do
                    (readParameter @(Maybe Text) "") `shouldBe` (Right (Just ""))
                    (readParameter @(Maybe ByteString) "") `shouldBe` (Right (Just ""))

                it "should handle empty Bool as False" do
                    (readParameter @(Maybe Bool) "") `shouldBe` (Right (Just False))

                it "should deal with parser errors" do
                    (readParameter @(Maybe Int) "not a number") `shouldBe` (Left "ParamReader Int: Failed reading: takeWhile1")

            describe "Enum" do
                it "should accept values" do
                    (readParameter "Yellow") `shouldBe` (Right Yellow)
                    (readParameter "Red") `shouldBe` (Right Red)
                    (readParameter "Blue") `shouldBe` (Right Blue)

                it "should fail on invalid values" do
                    (readParameter @Color "black") `shouldBe` (Left "Invalid value")
                    (readParameter @Color "") `shouldBe` (Left "Invalid value")

                it "should deal with JSON" do
                    (readParameterJSON (json "\"Yellow\"")) `shouldBe` (Right Yellow)
                    (readParameterJSON (json "\"Red\"")) `shouldBe` (Right Red)
                    (readParameterJSON (json "\"Blue\"")) `shouldBe` (Right Blue)

                it "should fail on invalid JSON" do
                    (readParameterJSON @Color (json "\"\"")) `shouldBe` (Left "Invalid value")
                    (readParameterJSON @Color (json "1337")) `shouldBe` (Left "enumParamReaderJSON: Invalid value, expected a string but got something else")

createControllerContextWithParams params =
        let
            requestBody = FormBody { params, files = [] }
            request = Wai.defaultRequest
            requestContext = RequestContext { request, respond = error "respond", requestBody, vault = error "vault", frameworkConfig = error "frameworkConfig" }
        in FrozenControllerContext { requestContext, customFields = TypeMap.empty }

json :: Text -> Aeson.Value
json string =
    let (Just value) :: Maybe Aeson.Value = Aeson.decode (cs string)
    in value

data Color = Yellow | Red | Blue deriving (Enum, Show, Eq)
instance ParamReader Color where
    readParameter = enumParamReader
    readParameterJSON = enumParamReaderJSON
instance InputValue Color where inputValue = tshow