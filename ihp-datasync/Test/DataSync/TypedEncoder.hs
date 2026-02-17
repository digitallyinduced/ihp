{-|
Copyright: (c) digitally induced GmbH, 2025
-}
module Test.DataSync.TypedEncoder where

import Test.Hspec
import IHP.Prelude
import IHP.DataSync.DynamicQuery
import IHP.DataSync.TypedEncoder
import IHP.QueryBuilder.HasqlCompiler (CompilerState(..), emptyCompilerState)
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Control.Exception as Exception

-- | Run a state-threaded param function and return just the SQL text.
-- Starts from emptyCompilerState so $1, $2, etc.
paramToSql :: (CompilerState -> (Text, CompilerState)) -> Text
paramToSql f = fst (f emptyCompilerState)

tests = do
    describe "IHP.DataSync.TypedEncoder" do
        describe "typedValueParam" do
            it "encodes NULL regardless of type" do
                paramToSql (typedValueParam Nothing Aeson.Null) `shouldBe` "NULL"
                paramToSql (typedValueParam (Just "uuid") Aeson.Null) `shouldBe` "NULL"
                paramToSql (typedValueParam (Just "text") Aeson.Null) `shouldBe` "NULL"

            it "encodes UUID with typed encoder" do
                paramToSql (typedValueParam (Just "uuid") (Aeson.String "a5d7772f-c63f-4444-be69-dd9afd902e9b"))
                    `shouldBe` "$1"

            it "encodes text with typed encoder" do
                paramToSql (typedValueParam (Just "text") (Aeson.String "hello"))
                    `shouldBe` "$1"

            it "encodes int4 with typed encoder" do
                paramToSql (typedValueParam (Just "int4") (Aeson.Number 42))
                    `shouldBe` "$1"

            it "encodes int8 with typed encoder" do
                paramToSql (typedValueParam (Just "int8") (Aeson.Number 42))
                    `shouldBe` "$1"

            it "encodes bool with typed encoder" do
                paramToSql (typedValueParam (Just "bool") (Aeson.Bool True))
                    `shouldBe` "$1"

            it "encodes float8 with typed encoder" do
                paramToSql (typedValueParam (Just "float8") (Aeson.Number 3.14))
                    `shouldBe` "$1"

            it "encodes custom enum with explicit cast" do
                paramToSql (typedValueParam (Just "my_enum") (Aeson.String "active"))
                    `shouldBe` "$1::\"my_enum\""

            it "errors when no type info is available" do
                Exception.evaluate (paramToSql (typedValueParam Nothing (Aeson.String "hello")))
                    `shouldThrow` anyErrorCall

            it "encodes Point with binary encoder" do
                let pointJson = Aeson.object ["x" Aeson..= (1.0 :: Double), "y" Aeson..= (2.0 :: Double)]
                paramToSql (typedValueParam (Just "point") pointJson)
                    `shouldBe` "$1"

            it "encodes Array with typed element encoding" do
                paramToSql (typedValueParam (Just "_int4") (Aeson.Array (Vector.fromList [Aeson.Number 1, Aeson.Number 2])))
                    `shouldBe` "ARRAY[$1, $2]"

            it "propagates array element type by stripping _ prefix" do
                paramToSql (typedValueParam (Just "_uuid") (Aeson.Array (Vector.fromList [Aeson.String "a5d7772f-c63f-4444-be69-dd9afd902e9b"])))
                    `shouldBe` "ARRAY[$1]"

        describe "typedAesonValueToSnippet" do
            it "encodes JSON null as NULL" do
                paramToSql (typedAesonValueToSnippet Nothing Aeson.Null) `shouldBe` "NULL"
                paramToSql (typedAesonValueToSnippet (Just "text") Aeson.Null) `shouldBe` "NULL"

            it "preserves JSONB values as-is" do
                paramToSql (typedAesonValueToSnippet (Just "jsonb") (Aeson.object ["key" Aeson..= ("value" :: Text)]))
                    `shouldBe` "$1"

            it "preserves JSON values as-is" do
                paramToSql (typedAesonValueToSnippet (Just "json") (Aeson.object ["key" Aeson..= ("value" :: Text)]))
                    `shouldBe` "$1"

            it "decodes {x, y} object as Point only when column type is point" do
                let pointJson = Aeson.object ["x" Aeson..= (1.0 :: Double), "y" Aeson..= (2.0 :: Double)]
                paramToSql (typedAesonValueToSnippet (Just "point") pointJson)
                    `shouldBe` "$1"

            it "does not decode {x, y} object as Point when column type is not point" do
                let pointJson = Aeson.object ["x" Aeson..= (1.0 :: Double), "y" Aeson..= (2.0 :: Double)]
                -- When column type is jsonb, the {x,y} object should be preserved as JSON
                paramToSql (typedAesonValueToSnippet (Just "jsonb") pointJson)
                    `shouldBe` "$1"

            it "encodes string values with typed encoder" do
                paramToSql (typedAesonValueToSnippet (Just "text") (Aeson.String "hello"))
                    `shouldBe` "$1"

            it "encodes number values with typed encoder" do
                paramToSql (typedAesonValueToSnippet (Just "int4") (Aeson.Number 42))
                    `shouldBe` "$1"

            it "encodes boolean values with typed encoder" do
                paramToSql (typedAesonValueToSnippet (Just "bool") (Aeson.Bool True))
                    `shouldBe` "$1"

        describe "dynamicValueParam" do
            it "encodes Number (integer) as native int parameter" do
                paramToSql (dynamicValueParam (Aeson.Number 42))
                    `shouldBe` "$1"

            it "encodes Number (double) as native float parameter" do
                paramToSql (dynamicValueParam (Aeson.Number 3.14))
                    `shouldBe` "$1"

            it "encodes String as native text parameter" do
                paramToSql (dynamicValueParam (Aeson.String "hello"))
                    `shouldBe` "$1"

            it "encodes Bool as native bool parameter" do
                paramToSql (dynamicValueParam (Aeson.Bool True))
                    `shouldBe` "$1"

            it "encodes Array with native element parameters" do
                paramToSql (dynamicValueParam (Aeson.Array (Vector.fromList [Aeson.Number 1, Aeson.Number 2])))
                    `shouldBe` "ARRAY[$1, $2]"

            it "encodes Null as SQL NULL" do
                paramToSql (dynamicValueParam Aeson.Null)
                    `shouldBe` "NULL"
