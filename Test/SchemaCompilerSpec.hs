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

                    instance IHP.Controller.Param.ParamReader Mood where readParameter = IHP.Controller.Param.enumParamReader; readParameterJSON = IHP.Controller.Param.enumParamReaderJSON
                |]
            it "should not pluralize values" do
                -- See https://github.com/digitallyinduced/ihp/issues/767
                let statement = CreateEnumType { name = "Province", values = ["Alberta", "BritishColumbia", "Saskatchewan", "Manitoba", "Ontario", "Quebec", "NovaScotia", "NewBrunswick", "PrinceEdwardIsland", "NewfoundlandAndLabrador"] }
                let output = compileStatementPreview [statement] statement |> Text.strip

                output `shouldBe` [text|
                    data Province = Alberta | Britishcolumbia | Saskatchewan | Manitoba | Ontario | Quebec | Novascotia | Newbrunswick | Princeedwardisland | Newfoundlandandlabrador deriving (Eq, Show, Read, Enum)
                    instance FromField Province where
                        fromField field (Just value) | value == (Data.Text.Encoding.encodeUtf8 "Alberta") = pure Alberta
                        fromField field (Just value) | value == (Data.Text.Encoding.encodeUtf8 "BritishColumbia") = pure Britishcolumbia
                        fromField field (Just value) | value == (Data.Text.Encoding.encodeUtf8 "Saskatchewan") = pure Saskatchewan
                        fromField field (Just value) | value == (Data.Text.Encoding.encodeUtf8 "Manitoba") = pure Manitoba
                        fromField field (Just value) | value == (Data.Text.Encoding.encodeUtf8 "Ontario") = pure Ontario
                        fromField field (Just value) | value == (Data.Text.Encoding.encodeUtf8 "Quebec") = pure Quebec
                        fromField field (Just value) | value == (Data.Text.Encoding.encodeUtf8 "NovaScotia") = pure Novascotia
                        fromField field (Just value) | value == (Data.Text.Encoding.encodeUtf8 "NewBrunswick") = pure Newbrunswick
                        fromField field (Just value) | value == (Data.Text.Encoding.encodeUtf8 "PrinceEdwardIsland") = pure Princeedwardisland
                        fromField field (Just value) | value == (Data.Text.Encoding.encodeUtf8 "NewfoundlandAndLabrador") = pure Newfoundlandandlabrador
                        fromField field (Just value) = returnError ConversionFailed field ("Unexpected value for enum value. Got: " <> Data.String.Conversions.cs value)
                        fromField field Nothing = returnError UnexpectedNull field "Unexpected null for enum value"
                    instance Default Province where def = Alberta
                    instance ToField Province where
                        toField Alberta = toField ("Alberta" :: Text)
                        toField Britishcolumbia = toField ("BritishColumbia" :: Text)
                        toField Saskatchewan = toField ("Saskatchewan" :: Text)
                        toField Manitoba = toField ("Manitoba" :: Text)
                        toField Ontario = toField ("Ontario" :: Text)
                        toField Quebec = toField ("Quebec" :: Text)
                        toField Novascotia = toField ("NovaScotia" :: Text)
                        toField Newbrunswick = toField ("NewBrunswick" :: Text)
                        toField Princeedwardisland = toField ("PrinceEdwardIsland" :: Text)
                        toField Newfoundlandandlabrador = toField ("NewfoundlandAndLabrador" :: Text)
                    instance InputValue Province where
                        inputValue Alberta = "Alberta" :: Text
                        inputValue Britishcolumbia = "BritishColumbia" :: Text
                        inputValue Saskatchewan = "Saskatchewan" :: Text
                        inputValue Manitoba = "Manitoba" :: Text
                        inputValue Ontario = "Ontario" :: Text
                        inputValue Quebec = "Quebec" :: Text
                        inputValue Novascotia = "NovaScotia" :: Text
                        inputValue Newbrunswick = "NewBrunswick" :: Text
                        inputValue Princeedwardisland = "PrinceEdwardIsland" :: Text
                        inputValue Newfoundlandandlabrador = "NewfoundlandAndLabrador" :: Text

                    instance IHP.Controller.Param.ParamReader Province where readParameter = IHP.Controller.Param.enumParamReader; readParameterJSON = IHP.Controller.Param.enumParamReaderJSON
                |]
            it "should deal with duplicate enum values" do
                let enum1 = CreateEnumType { name = "property_type", values = ["APARTMENT", "HOUSE"] }
                let enum2 = CreateEnumType { name = "apartment_type", values = ["LOFT", "APARTMENT"] }
                let output = compileStatementPreview [enum1, enum2] enum1 |> Text.strip

                output `shouldBe` [text|
                    data PropertyType = PropertyTypeApartment | House deriving (Eq, Show, Read, Enum)
                    instance FromField PropertyType where
                        fromField field (Just value) | value == (Data.Text.Encoding.encodeUtf8 "APARTMENT") = pure PropertyTypeApartment
                        fromField field (Just value) | value == (Data.Text.Encoding.encodeUtf8 "HOUSE") = pure House
                        fromField field (Just value) = returnError ConversionFailed field ("Unexpected value for enum value. Got: " <> Data.String.Conversions.cs value)
                        fromField field Nothing = returnError UnexpectedNull field "Unexpected null for enum value"
                    instance Default PropertyType where def = PropertyTypeApartment
                    instance ToField PropertyType where
                        toField PropertyTypeApartment = toField ("APARTMENT" :: Text)
                        toField House = toField ("HOUSE" :: Text)
                    instance InputValue PropertyType where
                        inputValue PropertyTypeApartment = "APARTMENT" :: Text
                        inputValue House = "HOUSE" :: Text

                    instance IHP.Controller.Param.ParamReader PropertyType where readParameter = IHP.Controller.Param.enumParamReader; readParameterJSON = IHP.Controller.Param.enumParamReaderJSON
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

            it "should compile CanUpdate instance with an array type with an explicit cast" do
                let statement = StatementCreateTable $ CreateTable {
                    name = "users",
                    columns = [ Column "id" PUUID Nothing False True, Column "ids" (PArray PUUID) Nothing False False ],
                    primaryKeyConstraint = PrimaryKeyConstraint ["id"],
                    constraints = []
                }
                let compileOutput = compileStatementPreview [statement] statement |> Text.strip

                getInstanceDecl "CanUpdate" compileOutput `shouldBe` [text|
                    instance CanUpdate User where
                        updateRecord model = do
                            List.head <$> sqlQuery "UPDATE users SET id = ?, ids = ? :: UUID[] WHERE id = ? RETURNING *" ((fieldWithUpdate #id model, fieldWithUpdate #ids model, get #id model))
                    |]
            it "should deal with double default values" do
                let statement = StatementCreateTable CreateTable
                        { name = "users"
                        , columns =
                            [ Column "id" PUUID Nothing False True, Column "ids" (PArray PUUID) Nothing False False
                            , Column {name = "electricity_unit_price", columnType = PDouble, defaultValue = Just (TypeCastExpression (DoubleExpression 0.17) PDouble), notNull = True, isUnique = False}
                            ]
                        , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        , constraints = []
                        }
                let compileOutput = compileStatementPreview [statement] statement |> Text.strip

                putStrLn compileOutput

                compileOutput `shouldBe` [text|
                    data User'  = User {id :: (Id' "users"), ids :: (Maybe [UUID]), electricityUnitPrice :: Double, meta :: MetaBag} deriving (Eq, Show)
                    instance InputValue User where inputValue = IHP.ModelSupport.recordToInputValue
                    type User = User' 

                    instance FromRow User where
                        fromRow = do
                            id <- field
                            ids <- field
                            electricityUnitPrice <- field
                            let theRecord = User id ids electricityUnitPrice def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }
                            pure theRecord

                    type instance GetTableName (User' ) = "users"
                    type instance GetModelByTableName "users" = User
                    type instance GetModelName (User' ) = "User"

                    type instance PrimaryKey "users" = UUID

                    instance QueryBuilder.FilterPrimaryKey "users" where
                        filterWhereId id builder =
                            builder |> QueryBuilder.filterWhere (#id, id)
                        {-# INLINE filterWhereId #-}

                    instance CanCreate User where
                        create :: (?modelContext :: ModelContext) => User -> IO User
                        create model = do
                            List.head <$> sqlQuery "INSERT INTO users (id, ids, electricity_unit_price) VALUES (?, ? :: UUID[], ?) RETURNING *" ((get #id model, get #ids model, fieldWithDefault #electricityUnitPrice model))
                        createMany [] = pure []
                        createMany models = do
                            sqlQuery (Query $ "INSERT INTO users (id, ids, electricity_unit_price) VALUES " <> (ByteString.intercalate ", " (List.map (\_ -> "(?, ? :: UUID[], ?)") models)) <> " RETURNING *") (List.concat $ List.map (\model -> [toField (get #id model), toField (get #ids model), toField (fieldWithDefault #electricityUnitPrice model)]) models)

                    instance CanUpdate User where
                        updateRecord model = do
                            List.head <$> sqlQuery "UPDATE users SET id = ?, ids = ? :: UUID[], electricity_unit_price = ? WHERE id = ? RETURNING *" ((fieldWithUpdate #id model, fieldWithUpdate #ids model, fieldWithUpdate #electricityUnitPrice model, get #id model))

                    instance Record User where
                        {-# INLINE newRecord #-}
                        newRecord = User def def 0.17  def
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

