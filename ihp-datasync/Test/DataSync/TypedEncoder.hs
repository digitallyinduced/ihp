{-|
Copyright: (c) digitally induced GmbH, 2025
-}
module Test.DataSync.TypedEncoder where

import Test.Hspec
import IHP.Prelude
import IHP.DataSync.DynamicQuery
import IHP.DataSync.TypedEncoder
import Hasql.Statement (Statement(..))
import qualified Hasql.DynamicStatements.Statement as DynStatement
import qualified Hasql.Decoders as Decoders
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Vector as Vector
import qualified Control.Exception as Exception

-- | Convert a Snippet to its SQL text representation for testing purposes.
snippetToSql :: Snippet -> ByteString
snippetToSql snippet = case DynStatement.dynamicallyParameterized snippet Decoders.noResult False of
    Statement sql _ _ _ -> sql

tests = do
    describe "IHP.DataSync.TypedEncoder" do
        describe "typedValueParam" do
            it "encodes NULL regardless of type" do
                snippetToSql (typedValueParam Nothing Aeson.Null) `shouldBe` "NULL"
                snippetToSql (typedValueParam (Just "uuid") Aeson.Null) `shouldBe` "NULL"
                snippetToSql (typedValueParam (Just "text") Aeson.Null) `shouldBe` "NULL"

            it "encodes UUID with typed encoder" do
                snippetToSql (typedValueParam (Just "uuid") (Aeson.String "a5d7772f-c63f-4444-be69-dd9afd902e9b"))
                    `shouldBe` "$1"

            it "encodes text with typed encoder" do
                snippetToSql (typedValueParam (Just "text") (Aeson.String "hello"))
                    `shouldBe` "$1"

            it "encodes int4 with typed encoder" do
                snippetToSql (typedValueParam (Just "int4") (Aeson.Number 42))
                    `shouldBe` "$1"

            it "encodes int8 with typed encoder" do
                snippetToSql (typedValueParam (Just "int8") (Aeson.Number 42))
                    `shouldBe` "$1"

            it "encodes bool with typed encoder" do
                snippetToSql (typedValueParam (Just "bool") (Aeson.Bool True))
                    `shouldBe` "$1"

            it "encodes float8 with typed encoder" do
                snippetToSql (typedValueParam (Just "float8") (Aeson.Number 3.14))
                    `shouldBe` "$1"

            it "encodes custom enum with explicit cast" do
                snippetToSql (typedValueParam (Just "my_enum") (Aeson.String "active"))
                    `shouldBe` "$1::\"my_enum\""

            it "errors when no type info is available" do
                Exception.evaluate (snippetToSql (typedValueParam Nothing (Aeson.String "hello")))
                    `shouldThrow` anyErrorCall

            it "encodes Point with SQL syntax regardless of type" do
                let pointJson = Aeson.object ["x" Aeson..= (1.0 :: Double), "y" Aeson..= (2.0 :: Double)]
                snippetToSql (typedValueParam (Just "point") pointJson)
                    `shouldBe` "point(1.0,2.0)"

            it "encodes Array with typed element encoding" do
                snippetToSql (typedValueParam (Just "_int4") (Aeson.Array (Vector.fromList [Aeson.Number 1, Aeson.Number 2])))
                    `shouldBe` "ARRAY[$1, $2]"

            it "propagates array element type by stripping _ prefix" do
                snippetToSql (typedValueParam (Just "_uuid") (Aeson.Array (Vector.fromList [Aeson.String "a5d7772f-c63f-4444-be69-dd9afd902e9b"])))
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
            it "encodes Number (integer) as native int parameter" do
                snippetToSql (dynamicValueParam (Aeson.Number 42))
                    `shouldBe` "$1"

            it "encodes Number (double) as native float parameter" do
                snippetToSql (dynamicValueParam (Aeson.Number 3.14))
                    `shouldBe` "$1"

            it "encodes String as native text parameter" do
                snippetToSql (dynamicValueParam (Aeson.String "hello"))
                    `shouldBe` "$1"

            it "encodes Bool as native bool parameter" do
                snippetToSql (dynamicValueParam (Aeson.Bool True))
                    `shouldBe` "$1"

            it "encodes Array with native element parameters" do
                snippetToSql (dynamicValueParam (Aeson.Array (Vector.fromList [Aeson.Number 1, Aeson.Number 2])))
                    `shouldBe` "ARRAY[$1, $2]"

            it "encodes Null as SQL NULL" do
                snippetToSql (dynamicValueParam Aeson.Null)
                    `shouldBe` "NULL"
