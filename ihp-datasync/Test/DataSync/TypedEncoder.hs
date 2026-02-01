{-|
Copyright: (c) digitally induced GmbH, 2025
-}
module Test.DataSync.TypedEncoder where

import Test.Hspec
import IHP.Prelude
import IHP.DataSync.DynamicQuery
import IHP.DataSync.TypedEncoder
import IHP.Postgres.TimeParser (PGInterval(..))
import IHP.Postgres.Point (Point(..))
import Hasql.Statement (Statement(..))
import qualified Hasql.DynamicStatements.Statement as DynStatement
import qualified Hasql.Decoders as Decoders
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Control.Exception as Exception

-- | Convert a Snippet to its SQL text representation for testing purposes.
snippetToSql :: Snippet -> ByteString
snippetToSql snippet = case DynStatement.dynamicallyParameterized snippet Decoders.noResult False of
    Statement sql _ _ _ -> sql

tests = do
    describe "IHP.DataSync.TypedEncoder" do
        describe "typedDynamicValueParam" do
            it "encodes NULL regardless of type" do
                snippetToSql (typedDynamicValueParam Nothing Null) `shouldBe` "NULL"
                snippetToSql (typedDynamicValueParam (Just "uuid") Null) `shouldBe` "NULL"
                snippetToSql (typedDynamicValueParam (Just "text") Null) `shouldBe` "NULL"

            it "encodes UUID with typed encoder" do
                snippetToSql (typedDynamicValueParam (Just "uuid") (UUIDValue "a5d7772f-c63f-4444-be69-dd9afd902e9b"))
                    `shouldBe` "$1"

            it "encodes text with typed encoder" do
                snippetToSql (typedDynamicValueParam (Just "text") (TextValue "hello"))
                    `shouldBe` "$1"

            it "encodes int4 with typed encoder" do
                snippetToSql (typedDynamicValueParam (Just "int4") (IntValue 42))
                    `shouldBe` "$1"

            it "encodes int8 with typed encoder" do
                snippetToSql (typedDynamicValueParam (Just "int8") (IntValue 42))
                    `shouldBe` "$1"

            it "encodes bool with typed encoder" do
                snippetToSql (typedDynamicValueParam (Just "bool") (BoolValue True))
                    `shouldBe` "$1"

            it "encodes float8 with typed encoder" do
                snippetToSql (typedDynamicValueParam (Just "float8") (DoubleValue 3.14))
                    `shouldBe` "$1"

            it "encodes custom enum with explicit cast" do
                snippetToSql (typedDynamicValueParam (Just "my_enum") (TextValue "active"))
                    `shouldBe` "$1::\"my_enum\""

            it "errors when no type info is available" do
                Exception.evaluate (snippetToSql (typedDynamicValueParam Nothing (TextValue "hello")))
                    `shouldThrow` anyErrorCall

            it "encodes Point with SQL syntax regardless of type" do
                snippetToSql (typedDynamicValueParam (Just "point") (PointValue (Point 1.0 2.0)))
                    `shouldBe` "point(1.0,2.0)"

            it "encodes Array with typed element encoding" do
                snippetToSql (typedDynamicValueParam (Just "_int4") (ArrayValue [IntValue 1, IntValue 2]))
                    `shouldBe` "ARRAY[$1, $2]"

            it "propagates array element type by stripping _ prefix" do
                snippetToSql (typedDynamicValueParam (Just "_uuid") (ArrayValue [UUIDValue "a5d7772f-c63f-4444-be69-dd9afd902e9b"]))
                    `shouldBe` "ARRAY[$1]"

        describe "typedAesonValueToSnippet" do
            it "encodes JSON null as NULL" do
                snippetToSql (typedAesonValueToSnippet Nothing Aeson.Null) `shouldBe` "NULL"
                snippetToSql (typedAesonValueToSnippet (Just "text") Aeson.Null) `shouldBe` "NULL"

            it "preserves JSONB values as-is" do
                snippetToSql (typedAesonValueToSnippet (Just "jsonb") (Aeson.object ["key" Aeson..= ("value" :: Text)]))
                    `shouldBe` "$1"

            it "preserves JSON values as-is" do
                snippetToSql (typedAesonValueToSnippet (Just "json") (Aeson.object ["key" Aeson..= ("value" :: Text)]))
                    `shouldBe` "$1"

            it "decodes {x, y} object as Point only when column type is point" do
                let pointJson = Aeson.object ["x" Aeson..= (1.0 :: Double), "y" Aeson..= (2.0 :: Double)]
                snippetToSql (typedAesonValueToSnippet (Just "point") pointJson)
                    `shouldBe` "point(1.0,2.0)"

            it "does not decode {x, y} object as Point when column type is not point" do
                let pointJson = Aeson.object ["x" Aeson..= (1.0 :: Double), "y" Aeson..= (2.0 :: Double)]
                -- When column type is jsonb, the {x,y} object should be preserved as JSON
                snippetToSql (typedAesonValueToSnippet (Just "jsonb") pointJson)
                    `shouldBe` "$1"

            it "encodes string values with typed encoder" do
                snippetToSql (typedAesonValueToSnippet (Just "text") (Aeson.String "hello"))
                    `shouldBe` "$1"

            it "encodes number values with typed encoder" do
                snippetToSql (typedAesonValueToSnippet (Just "int4") (Aeson.Number 42))
                    `shouldBe` "$1"

            it "encodes boolean values with typed encoder" do
                snippetToSql (typedAesonValueToSnippet (Just "bool") (Aeson.Bool True))
                    `shouldBe` "$1"

        describe "dynamicValueParam" do
            it "encodes IntValue as native int parameter" do
                snippetToSql (dynamicValueParam (IntValue 42))
                    `shouldBe` "$1"

            it "encodes DoubleValue as native float parameter" do
                snippetToSql (dynamicValueParam (DoubleValue 3.14))
                    `shouldBe` "$1"

            it "encodes TextValue as native text parameter" do
                snippetToSql (dynamicValueParam (TextValue "hello"))
                    `shouldBe` "$1"

            it "encodes BoolValue as native bool parameter" do
                snippetToSql (dynamicValueParam (BoolValue True))
                    `shouldBe` "$1"

            it "encodes UUIDValue as native uuid parameter" do
                snippetToSql (dynamicValueParam (UUIDValue "a5d7772f-c63f-4444-be69-dd9afd902e9b"))
                    `shouldBe` "$1"

            it "encodes DateTimeValue as native timestamptz parameter" do
                let epoch = UTCTime (toEnum 0) 0
                snippetToSql (dynamicValueParam (DateTimeValue epoch))
                    `shouldBe` "$1"

            it "encodes PointValue as SQL literal" do
                snippetToSql (dynamicValueParam (PointValue (Point 1.0 2.0)))
                    `shouldBe` "point(1.0,2.0)"

            it "encodes IntervalValue as text parameter" do
                snippetToSql (dynamicValueParam (IntervalValue (PGInterval "1 day")))
                    `shouldBe` "$1"

            it "encodes ArrayValue with native element parameters" do
                snippetToSql (dynamicValueParam (ArrayValue [IntValue 1, IntValue 2]))
                    `shouldBe` "ARRAY[$1, $2]"

            it "encodes Null as SQL NULL" do
                snippetToSql (dynamicValueParam Null)
                    `shouldBe` "NULL"
