{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Test.Wai.Request.ParamsSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Wai.Request.ParamsSpec where

import Prelude
import Test.Hspec
import Wai.Request.Params
import qualified Data.Vault.Lazy as Vault
import qualified Data.Aeson as Aeson
import qualified Data.UUID as UUID
import qualified Network.Wai as Wai
import qualified GHC.IO as IO
import Data.Scientific (Scientific)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (LocalTime, TimeOfDay)
import Data.Time.Calendar (Day)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (UUID)
import Data.String.Conversions (cs)
import Data.Maybe (fromJust)

spec :: Spec
spec = do
    describe "Wai.Request.Params" $ do
        describe "param" $ do
            it "should parse valid input" $ do
                let (requestBody, request) = createRequestWithParams [("page", "1")]
                (param @Int requestBody request "page") `shouldBe` 1

            it "should fail on empty input" $ do
                let (requestBody, request) = createRequestWithParams [("page", "")]
                (IO.evaluate (param @Int requestBody request "page")) `shouldThrow` (== ParamCouldNotBeParsedException { name = "page", parserError = "has to be an integer" })

            it "should fail if param not provided" $ do
                let (requestBody, request) = createRequestWithParams []
                (IO.evaluate (param @Int requestBody request "page")) `shouldThrow` (== ParamNotFoundException { name = "page" })

            it "should fail with a parser error on invalid input" $ do
                let (requestBody, request) = createRequestWithParams [("page", "NaN")]
                (IO.evaluate (param @Int requestBody request "page")) `shouldThrow` (== ParamCouldNotBeParsedException { name = "page", parserError = "has to be an integer" })

        describe "paramOrNothing" $ do
            it "should parse valid input" $ do
                let (requestBody, request) = createRequestWithParams [("referredBy", "776ab71d-327f-41b3-90a8-7b5a251c4b88")]
                (paramOrNothing @UUID requestBody request "referredBy") `shouldBe` (Just (uuid "776ab71d-327f-41b3-90a8-7b5a251c4b88"))

            it "should return Nothing on empty input" $ do
                let (requestBody, request) = createRequestWithParams [("referredBy", "")]
                (paramOrNothing @UUID requestBody request "referredBy") `shouldBe` Nothing

            it "should return Nothing if param not provided" $ do
                let (requestBody, request) = createRequestWithParams []
                (paramOrNothing @UUID requestBody request "referredBy") `shouldBe` Nothing

            it "should fail with a parser error on invalid input" $ do
                let (requestBody, request) = createRequestWithParams [("referredBy", "not a uuid")]
                (IO.evaluate (paramOrNothing @UUID requestBody request "referredBy")) `shouldThrow` (== ParamCouldNotBeParsedException { name = "referredBy", parserError = "has to be an UUID" })

        describe "paramOrDefault" $ do
            it "should parse valid input" $ do
                let (requestBody, request) = createRequestWithParams [("page", "1")]
                (paramOrDefault @Int requestBody request 0 "page") `shouldBe` 1

            it "should return default value on empty input" $ do
                let (requestBody, request) = createRequestWithParams [("page", "")]
                (paramOrDefault @Int requestBody request 10 "page") `shouldBe` 10

            it "should return default value if param not provided" $ do
                let (requestBody, request) = createRequestWithParams []
                (paramOrDefault @Int requestBody request 10 "page") `shouldBe` 10

            it "should fail with a parser error on invalid input" $ do
                let (requestBody, request) = createRequestWithParams [("page", "NaN")]
                (IO.evaluate (paramOrDefault @Int requestBody request 10 "page")) `shouldThrow` (== ParamCouldNotBeParsedException { name = "page", parserError = "has to be an integer" })


        describe "paramList" $ do
            it "should parse valid input" $ do
                let (requestBody, request) = createRequestWithParams [("ingredients", "milk"), ("ingredients", "egg")]
                (paramList @Text requestBody request "ingredients") `shouldBe` ["milk", "egg"]

            it "should fail on invalid input" $ do
                let (requestBody, request) = createRequestWithParams [("numbers", "1"), ("numbers", "NaN")]
                (IO.evaluate (paramList @Int requestBody request "numbers")) `shouldThrow` (errorCall "param: Parameter 'numbers' is invalid")

            it "should deal with empty input" $ do
                let (requestBody, request) = createRequestWithParams []
                (paramList @Int requestBody request "numbers") `shouldBe` []

        describe "paramListOrNothing" $ do
            it "should parse valid input" $ do
                let (requestBody, request) = createRequestWithParams [("ingredients", "milk"), ("ingredients", ""), ("ingredients", "egg")]
                (paramListOrNothing @Text requestBody request "ingredients") `shouldBe` [Just "milk", Nothing, Just "egg"]

            it "should not fail on invalid input" $ do
                let (requestBody, request) = createRequestWithParams [("numbers", "1"), ("numbers", "NaN")]
                (paramListOrNothing @Int requestBody request "numbers") `shouldBe` [Just 1, Nothing]

            it "should deal with empty input" $ do
                let (requestBody, request) = createRequestWithParams []
                (paramListOrNothing @Int requestBody request "numbers") `shouldBe` []

        describe "hasParam" $ do
            it "returns True if param given" $ do
                let (requestBody, request) = createRequestWithParams [("a", "test")]
                hasParam requestBody request "a" `shouldBe` True

            it "returns True if param given but empty" $ do
                let (requestBody, request) = createRequestWithParams [("a", "")]
                hasParam requestBody request "a" `shouldBe` True

            it "returns False if param missing" $ do
                let (requestBody, request) = createRequestWithParams []
                hasParam requestBody request "a" `shouldBe` False

        describe "ParamReader" $ do
            describe "ByteString" $ do
                it "should handle text input" $ do
                    (readParameter @ByteString "test") `shouldBe` (Right "test")
                it "should handle JSON strings" $ do
                    (readParameterJSON @ByteString (json "\"test\"")) `shouldBe` (Right ("test" :: ByteString))

                it "should fail on other JSON input" $ do
                    (readParameterJSON @ByteString (json "1")) `shouldBe` (Left ("Expected String" :: ByteString))

            describe "Int" $ do
                it "should accept numeric input" $ do
                    (readParameter @Int "1337") `shouldBe` (Right 1337)

                it "should accept negative numbers" $ do
                    (readParameter @Int "-1337") `shouldBe` (Right (-1337))

                it "should accept JSON numerics " $ do
                    (readParameterJSON @Int (json "1337")) `shouldBe` (Right 1337)

                it "should fail on other JSON input " $ do
                    (readParameterJSON @Int (json "true")) `shouldBe` (Left "Expected Int")

            describe "Integer" $ do
                it "should accept numeric input" $ do
                    (readParameter @Integer "1337") `shouldBe` (Right 1337)

                it "should accept negative numbers" $ do
                    (readParameter @Integer "-1337") `shouldBe` (Right (-1337))

                it "should accept JSON numerics " $ do
                    (readParameterJSON @Integer (json "1337")) `shouldBe` (Right 1337)

                it "should fail on other JSON input " $ do
                    (readParameterJSON @Integer (json "true")) `shouldBe` (Left "Expected Integer")
                    (readParameterJSON @Integer (json "\"1\"")) `shouldBe` (Left "Expected Integer")


            describe "Double" $ do
                it "should accept integer input" $ do
                    (readParameter @Double "1337") `shouldBe` (Right 1337)

                it "should accept floating point input" $ do
                    (readParameter @Double "1.2") `shouldBe` (Right 1.2)
                    (readParameter @Double "1.2345679") `shouldBe` (Right 1.2345679)

                it "should accept JSON integer input" $ do
                    (readParameterJSON @Double (json "1337")) `shouldBe` (Right 1337)

                it "should accept JSON floating point input" $ do
                    (readParameterJSON @Double (json "1.2")) `shouldBe` (Right 1.2)

                it "should fail on other JSON input " $ do
                    (readParameterJSON @Double (json "true")) `shouldBe` (Left "Expected Double")
                    (readParameterJSON @Double (json "\"1\"")) `shouldBe` (Left "Expected Double")

            describe "Scientific" $ do
                it "should accept integer input" $ do
                    (readParameter @Scientific "1337") `shouldBe` (Right 1337)

                it "should accept floating point input" $ do
                    (readParameter @Scientific "1.2") `shouldBe` (Right 1.2)
                    (readParameter @Scientific "1.2345679") `shouldBe` (Right 1.2345679)
                    let x = "1e-1024" -- -1024 is smaller than minimal Double exponent of -1021
                        y = "1.0e-1024"
                    (show <$> readParameter @Scientific x) `shouldBe` (Right y)

                it "should accept JSON integer input" $ do
                    (readParameterJSON @Scientific (json "1337")) `shouldBe` (Right 1337)

                it "should accept JSON floating point input" $ do
                    (readParameterJSON @Scientific (json "1.2")) `shouldBe` (Right 1.2)

                it "should fail on other JSON input " $ do
                    (readParameterJSON @Scientific (json "true")) `shouldBe` (Left "Expected Scientific")
                    (readParameterJSON @Scientific (json "\"1\"")) `shouldBe` (Left "Expected Scientific")

            describe "Float" $ do
                it "should accept integer input" $ do
                    (readParameter @Float "1337") `shouldBe` (Right 1337)

                it "should accept floating point input" $ do
                    (readParameter @Float "1.2") `shouldBe` (Right 1.2)
                    (readParameter @Float "1.2345679") `shouldBe` (Right 1.2345679)

                it "should accept JSON integer input" $ do
                    (readParameterJSON @Float (json "1337")) `shouldBe` (Right 1337)

                it "should accept JSON floating point input" $ do
                    (readParameterJSON @Float (json "1.2")) `shouldBe` (Right 1.2)

                it "should fail on other JSON input " $ do
                    (readParameterJSON @Float (json "true")) `shouldBe` (Left "Expected Float")
                    (readParameterJSON @Float (json "\"1\"")) `shouldBe` (Left "Expected Float")

            describe "Text" $ do
                it "should handle text input" $ do
                    (readParameter @Text "test") `shouldBe` (Right "test")

                it "should handle JSON strings" $ do
                    (readParameterJSON @Text (json "\"test\"")) `shouldBe` (Right ("test"))

                it "should fail on other JSON input" $ do
                    (readParameterJSON @Text (json "1")) `shouldBe` (Left ("Expected String"))

            describe "CSV" $ do
                it "should handle empty input" $ do
                    (readParameter @[Int] "") `shouldBe` (Right [])

                it "should handle a single value" $ do
                    (readParameter @[Int] "1") `shouldBe` (Right [1])

                it "should handle comma separated values" $ do
                    (readParameter @[Int] "1,2,3") `shouldBe` (Right [1,2,3])

                it "should fail if a single value is invalid" $ do
                    (readParameter @[Int] "1,a,3") `shouldBe` (Left "has to be an integer")

                it "should handle JSON arrays" $ do
                    (readParameterJSON @[Int] (json "[1,2,3]")) `shouldBe` (Right [1,2,3])

                it "should fail on JSON input that is not an array" $ do
                    (readParameterJSON @[Int] (json "true")) `shouldBe` (Left "Expected Array")

            describe "Bool" $ do
                it "should accept 'on' as True" $ do
                    (readParameter @Bool "on") `shouldBe` (Right True)

                it "should accept 'true' as True" $ do
                    (readParameter @Bool "true") `shouldBe` (Right True)
                    (readParameter @Bool "TruE") `shouldBe` (Right True)

                it "should accept everything else as false input" $ do
                    (readParameter @Bool "off") `shouldBe` (Right False)
                    (readParameter @Bool "false") `shouldBe` (Right False)
                    (readParameter @Bool "invalid") `shouldBe` (Right False)

            describe "UUID" $ do
                it "should accept UUID values" $ do
                    (readParameter @UUID "6188329c-6bad-47f6-800c-2fd19ce0b2df") `shouldBe` (Right (uuid "6188329c-6bad-47f6-800c-2fd19ce0b2df"))
                    (readParameter @UUID "a020ba17-a94e-453f-9414-c54aa30caa54") `shouldBe` (Right (uuid "a020ba17-a94e-453f-9414-c54aa30caa54"))

                it "should fail on invalid values" $ do
                    (readParameter @UUID "not a uuid") `shouldBe` (Left "has to be an UUID")

                it "should accept JSON UUIDs" $ do
                    (readParameterJSON @UUID (json "\"6188329c-6bad-47f6-800c-2fd19ce0b2df\"")) `shouldBe` (Right (uuid "6188329c-6bad-47f6-800c-2fd19ce0b2df"))

                it "should fail on invalid JSON input" $ do
                    (readParameterJSON @UUID (json "\"not a uuid\"")) `shouldBe` (Left "Invalid UUID")
                    (readParameterJSON @UUID (json "false")) `shouldBe` (Left "Expected String with an UUID")

            describe "UTCTime" $ do
                it "should accept timestamps" $ do
                    (tshow (readParameter @UTCTime "2020-11-08T12:03:35Z")) `shouldBe` ("Right 2020-11-08 12:03:35 UTC")

                it "should accept dates" $ do
                    (tshow (readParameter @UTCTime "2020-11-08")) `shouldBe` ("Right 2020-11-08 00:00:00 UTC")

                it "should fail on invalid inputs" $ do
                    (readParameter @UTCTime "not a timestamp") `shouldBe` (Left "has to be a valid date and time, e.g. 2020-11-08T12:03:35Z")

                it "should accept JSON strings" $ do
                    (tshow (readParameterJSON @UTCTime (json "\"2020-11-08T12:03:35Z\""))) `shouldBe` ("Right 2020-11-08 12:03:35 UTC")

            describe "LocalTime" $ do
                it "should accept timestamps" $ do
                    (tshow (readParameter @LocalTime "2020-11-08T12:03:35Z")) `shouldBe` ("Right 2020-11-08 12:03:35")

                it "should accept dates" $ do
                    (tshow (readParameter @LocalTime "2020-11-08")) `shouldBe` ("Right 2020-11-08 00:00:00")

                it "should fail on invalid inputs" $ do
                    (readParameter @LocalTime "not a timestamp") `shouldBe` (Left "has to be a valid date and time, e.g. 2020-11-08T12:03:35Z")

                it "should accept JSON strings" $ do
                    (tshow (readParameterJSON @LocalTime (json "\"2020-11-08T12:03:35Z\""))) `shouldBe` ("Right 2020-11-08 12:03:35")

            describe "Day" $ do
                it "should accept dates" $ do
                    (tshow (readParameter @Day "2020-11-08")) `shouldBe` ("Right 2020-11-08")

                it "should fail on invalid inputs" $ do
                    (readParameter @Day "not a timestamp") `shouldBe` (Left "has to be a date, e.g. 2020-11-08")

                it "should accept JSON strings" $ do
                    (tshow (readParameterJSON @Day (json "\"2020-11-08\""))) `shouldBe` ("Right 2020-11-08")

            describe "TimeOfDay" $ do
                it "should accept time values" $ do
                    (tshow (readParameter @TimeOfDay "12:00:00")) `shouldBe` ("Right 12:00:00")

                it "should fail on invalid inputs" $ do
                    (readParameter @TimeOfDay "not a time") `shouldBe` (Left "has to be time in the format hh:mm:ss")
                    (readParameter @TimeOfDay "25:00:00") `shouldBe` (Left "has to be time in the format hh:mm:ss")

                it "should accept JSON strings" $ do
                    (tshow (readParameterJSON @TimeOfDay (json "\"13:37:00\""))) `shouldBe` ("Right 13:37:00")

            describe "Maybe" $ do
                it "should accept values" $ do
                    (readParameter @(Maybe Int) "1") `shouldBe` (Right (Just 1))
                    (readParameter @(Maybe Text) "hello") `shouldBe` (Right (Just "hello"))

                it "should handle empty input as Nothing" $ do
                    (readParameter @(Maybe Int) "") `shouldBe` (Right Nothing)
                    (readParameter @(Maybe UUID) "") `shouldBe` (Right Nothing)
                    (readParameterJSON @(Maybe Bool) "") `shouldBe` (Right Nothing)

                it "should handle empty Text as Just" $ do
                    (readParameter @(Maybe Text) "") `shouldBe` (Right (Just ""))
                    (readParameter @(Maybe ByteString) "") `shouldBe` (Right (Just ""))

                it "should handle empty Bool as False" $ do
                    (readParameter @(Maybe Bool) "") `shouldBe` (Right (Just False))

                it "should deal with parser errors" $ do
                    (readParameter @(Maybe Int) "not a number") `shouldBe` (Left "has to be an integer")

createRequestWithParams :: [(ByteString, ByteString)] -> (RequestBody, Wai.Request)
createRequestWithParams params =
    let
        requestBody = FormBody { params, files = [] }
        request = Wai.defaultRequest { Wai.vault = Vault.insert requestBodyVaultKey requestBody Vault.empty }
    in (requestBody, request)

createRequestWithJson :: Text -> (RequestBody, Wai.Request)
createRequestWithJson params =
    let
        requestBody = JSONBody { jsonPayload = Just (json params), rawPayload = cs params }
        request = Wai.defaultRequest { Wai.vault = Vault.insert requestBodyVaultKey requestBody Vault.empty }
    in (requestBody, request)

json :: Text -> Aeson.Value
json string =
    let (Just value) :: Maybe Aeson.Value = Aeson.decode (cs string)
    in value

tshow :: Show a => a -> Text
tshow = Text.pack . show

uuid :: Text -> UUID
uuid = fromJust . UUID.fromText
