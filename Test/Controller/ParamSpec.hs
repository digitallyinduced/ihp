{-|
Module: Test.Controller.ParamSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.Controller.ParamSpec where

import IHP.Prelude
import IHP.HaskellSupport
import Test.Hspec
import IHP.Controller.Param
import qualified Data.Aeson as Aeson
import qualified Data.UUID as UUID

tests = do
    describe "IHP.Controller.Param" do
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
            
            describe "Day" do
                it "should accept dates" do
                    (tshow (readParameter @Day "2020-11-08")) `shouldBe` ("Right 2020-11-08")
                
                it "should fail on invalid inputs" do
                    (readParameter @Day "not a timestamp") `shouldBe` (Left "ParamReader Day: Failed parsing")
                
                it "should accept JSON strings" do
                    (tshow (readParameterJSON @Day (json "\"2020-11-08\""))) `shouldBe` ("Right 2020-11-08")

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

json :: Text -> Aeson.Value
json string =
    let (Just value) :: Maybe Aeson.Value = Aeson.decode (cs string)
    in value