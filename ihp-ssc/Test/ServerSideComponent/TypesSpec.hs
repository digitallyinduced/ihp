{-|
Module: Test.ServerSideComponent.TypesSpec
Copyright: (c) digitally induced GmbH, 2021
Description: Tests for SSC error types and JSON encoding
-}
module Test.ServerSideComponent.TypesSpec where

import Test.Hspec
import IHP.Prelude
import IHP.ServerSideComponent.Types
import IHP.ServerSideComponent.ControllerFunctions ()
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap

tests :: Spec
tests = do
    describe "ServerSideComponent" do
        describe "Types" do
            describe "SSCError" do
                it "should encode SSCDiffError to JSON" do
                    let error = SSCDiffError { errorMessage = "Parse error at line 1" }
                    let encoded = Aeson.encode error
                    let decoded = Aeson.decode encoded :: Maybe Aeson.Value

                    decoded `shouldSatisfy` isJust

                    case decoded of
                        Just (Aeson.Object obj) -> do
                            KeyMap.lookup "type" obj `shouldBe` Just (Aeson.String "SSCDiffError")
                            KeyMap.lookup "errorMessage" obj `shouldBe` Just (Aeson.String "Parse error at line 1")
                        _ -> expectationFailure "Expected JSON object"

                it "should encode SSCActionError to JSON" do
                    let error = SSCActionError { errorMessage = "Action handler threw exception" }
                    let encoded = Aeson.encode error
                    let decoded = Aeson.decode encoded :: Maybe Aeson.Value

                    decoded `shouldSatisfy` isJust

                    case decoded of
                        Just (Aeson.Object obj) -> do
                            KeyMap.lookup "type" obj `shouldBe` Just (Aeson.String "SSCActionError")
                            KeyMap.lookup "errorMessage" obj `shouldBe` Just (Aeson.String "Action handler threw exception")
                        _ -> expectationFailure "Expected JSON object"

                it "should encode SSCParseError to JSON" do
                    let error = SSCParseError { errorMessage = "Invalid JSON payload" }
                    let encoded = Aeson.encode error
                    let decoded = Aeson.decode encoded :: Maybe Aeson.Value

                    decoded `shouldSatisfy` isJust

                    case decoded of
                        Just (Aeson.Object obj) -> do
                            KeyMap.lookup "type" obj `shouldBe` Just (Aeson.String "SSCParseError")
                            KeyMap.lookup "errorMessage" obj `shouldBe` Just (Aeson.String "Invalid JSON payload")
                        _ -> expectationFailure "Expected JSON object"

                it "should round-trip SSCError through JSON" do
                    let errors =
                            [ SSCDiffError { errorMessage = "diff error" }
                            , SSCActionError { errorMessage = "action error" }
                            , SSCParseError { errorMessage = "parse error" }
                            ]

                    forM_ errors $ \error -> do
                        let encoded = Aeson.encode error
                        let decoded = Aeson.decode encoded :: Maybe SSCError
                        decoded `shouldBe` Just error

                it "error type should end with Error suffix for client detection" do
                    -- The JavaScript client checks if payload.type.endsWith('Error')
                    let diffError = SSCDiffError { errorMessage = "test" }
                    let actionError = SSCActionError { errorMessage = "test" }
                    let parseError = SSCParseError { errorMessage = "test" }

                    let checkErrorType error = do
                            let encoded = Aeson.encode error
                            case Aeson.decode encoded :: Maybe Aeson.Value of
                                Just (Aeson.Object obj) ->
                                    case KeyMap.lookup "type" obj of
                                        Just (Aeson.String typeStr) ->
                                            typeStr `shouldSatisfy` (\t -> "Error" `isSuffixOf` t)
                                        _ -> expectationFailure "Expected type field to be a string"
                                _ -> expectationFailure "Expected JSON object"

                    checkErrorType diffError
                    checkErrorType actionError
                    checkErrorType parseError
