{-|
Module: Test.SchemaCompilerSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.SchemaCompilerSpec where

import Test.Hspec
import IHP.Prelude
import IHP.SchemaCompiler
import IHP.IDE.SchemaDesigner.Types
import NeatInterpolation
import qualified Data.Text as Text

tests = do
    describe "SchemaCompiler" do
        describe "compileEnumDataDefinitions" do
            it "should deal with enum values that have spaces" do
                let statement = CreateEnumType { name = "mood", values = ["happy", "very happy", "sad", "very sad"] }
                let output = compileStatementPreview [statement] statement |> Text.strip

                output `shouldBe` [text|
                    data Mood = Happy | VeryHappy | Sad | VerySad deriving (Eq, Show, Read, Enum)
                    instance FromField Mood where
                        fromField field (Just value) | value == (Data.Text.Encoding.encodeUtf8 "happy") = pure Happy
                        fromField field (Just value) | value == (Data.Text.Encoding.encodeUtf8 "very happy") = pure VeryHappy
                        fromField field (Just value) | value == (Data.Text.Encoding.encodeUtf8 "sad") = pure Sad
                        fromField field (Just value) | value == (Data.Text.Encoding.encodeUtf8 "very sad") = pure VerySad
                        fromField field (Just value) = returnError ConversionFailed field ("Unexpected value for enum value. Got: " <> Data.String.Conversions.cs value)
                        fromField field Nothing = returnError UnexpectedNull field "Unexpected null for enum value"
                    instance Default Mood where def = Happy
                    instance ToField Mood where
                        toField Happy = toField ("happy" :: Text)
                        toField VeryHappy = toField ("very happy" :: Text)
                        toField Sad = toField ("sad" :: Text)
                        toField VerySad = toField ("very sad" :: Text)
                    instance InputValue Mood where
                        inputValue Happy = "happy" :: Text
                        inputValue VeryHappy = "very happy" :: Text
                        inputValue Sad = "sad" :: Text
                        inputValue VerySad = "very sad" :: Text

                    instance IHP.Controller.Param.ParamReader Mood where readParameter = IHP.Controller.Param.enumParamReader
                |]
        describe "compileCreate" do
            let statement = StatementCreateTable $ CreateTable {
                    name = "users",
                    columns = [ Column "id" PUUID Nothing False False ],
                    primaryKeyConstraint = PrimaryKeyConstraint ["id"],
                    constraints = []
                }
            let compileOutput = compileStatementPreview [statement] statement |> Text.strip

            it "should compile CanCreate instance with sqlQuery" $ \statement -> do
                getInstanceDecl "CanCreate" compileOutput `shouldBe` [text|
                    instance CanCreate User where
                        create :: (?modelContext :: ModelContext) => User -> IO User
                        create model = do
                            List.head <$> sqlQuery "INSERT INTO users (id) VALUES (?) RETURNING *" (Only (get #id model))
                        createMany [] = pure []
                        createMany models = do
                            sqlQuery (Query $ "INSERT INTO users (id) VALUES " <> (ByteString.intercalate ", " (List.map (\_ -> "(?)") models)) <> " RETURNING *") (List.concat $ List.map (\model -> [toField (get #id model)]) models)
                    |]
            it "should compile CanUpdate instance with sqlQuery" $ \statement -> do
                getInstanceDecl "CanUpdate" compileOutput `shouldBe` [text|
                    instance CanUpdate User where
                        updateRecord model = do
                            List.head <$> sqlQuery "UPDATE users SET id = ? WHERE id = ? RETURNING *" ((fieldWithUpdate #id model, get #id model))
                    |]



getInstanceDecl :: Text -> Text -> Text
getInstanceDecl instanceName full =
    Text.splitOn "\n" full
        |> findInstanceDecl
        |> takeInstanceDecl
        |> Text.unlines
        |> Text.strip
    where
        findInstanceDecl (line:rest)
            | ("instance " <> instanceName) `isPrefixOf` line = line : rest
            | otherwise = findInstanceDecl rest
        findInstnaceDecl [] = error "didn't find instance declaration of " <> instanceName

        takeInstanceDecl (line:rest)
            | isEmpty line = []
            | otherwise = line : takeInstanceDecl rest
        takeInstanceDecl [] = error "never encountered newline?"

