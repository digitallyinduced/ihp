{-|
Module: Test.SchemaCompilerSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.SchemaCompilerSpec where

import Test.Hspec
import IHP.Prelude
import IHP.SchemaCompiler
import IHP.IDE.SchemaDesigner.Types
import qualified Data.Text as Text
import Test.IDE.SchemaDesigner.ParserSpec (parseSqlStatements)

tests = do
    describe "SchemaCompiler" do
        describe "compileEnumDataDefinitions" do
            it "should deal with enum values that have spaces" do
                let statement = CreateEnumType { name = "mood", values = ["happy", "very happy", "sad", "very sad"] }
                let output = compileStatementPreview [statement] statement |> Text.strip

                output `shouldBe` [trimming|
                    data Mood = Happy | VeryHappy | Sad | VerySad deriving (Eq, Show, Read, Enum, Bounded, Ord)
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
                    instance DeepSeq.NFData Mood where rnf a = seq a ()
                    instance IHP.Controller.Param.ParamReader Mood where readParameter = IHP.Controller.Param.enumParamReader; readParameterJSON = IHP.Controller.Param.enumParamReaderJSON
                |]
            it "should deal with enums that have no values" do
                -- https://github.com/digitallyinduced/ihp/issues/1026
                -- Empty enums typically happen when an enum was just created in the schema designer and no value has been added yet by the user
                let statement = CreateEnumType { name = "mood", values = [] }
                let output = compileStatementPreview [statement] statement |> Text.strip

                -- We don't generate anything when no values are defined as there's nothing you could do with the enum yet
                -- An empty data declaration is not really useful in this case
                output `shouldBe` mempty
            it "should not pluralize values" do
                -- See https://github.com/digitallyinduced/ihp/issues/767
                let statement = CreateEnumType { name = "Province", values = ["Alberta", "BritishColumbia", "Saskatchewan", "Manitoba", "Ontario", "Quebec", "NovaScotia", "NewBrunswick", "PrinceEdwardIsland", "NewfoundlandAndLabrador"] }
                let output = compileStatementPreview [statement] statement |> Text.strip

                output `shouldBe` [trimming|
                    data Province = Alberta | Britishcolumbia | Saskatchewan | Manitoba | Ontario | Quebec | Novascotia | Newbrunswick | Princeedwardisland | Newfoundlandandlabrador deriving (Eq, Show, Read, Enum, Bounded, Ord)
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
                    instance DeepSeq.NFData Province where rnf a = seq a ()
                    instance IHP.Controller.Param.ParamReader Province where readParameter = IHP.Controller.Param.enumParamReader; readParameterJSON = IHP.Controller.Param.enumParamReaderJSON
                |]
            it "should deal with duplicate enum values" do
                let enum1 = CreateEnumType { name = "property_type", values = ["APARTMENT", "HOUSE"] }
                let enum2 = CreateEnumType { name = "apartment_type", values = ["LOFT", "APARTMENT"] }
                let output = compileStatementPreview [enum1, enum2] enum1 |> Text.strip

                output `shouldBe` [trimming|
                    data PropertyType = PropertyTypeApartment | House deriving (Eq, Show, Read, Enum, Bounded, Ord)
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
                    instance DeepSeq.NFData PropertyType where rnf a = seq a ()
                    instance IHP.Controller.Param.ParamReader PropertyType where readParameter = IHP.Controller.Param.enumParamReader; readParameterJSON = IHP.Controller.Param.enumParamReaderJSON
                |]
        describe "compileCreate" do
            let statement = StatementCreateTable $ CreateTable {
                    name = "users",
                    columns = [ Column "id" PUUID Nothing False False Nothing ],
                    primaryKeyConstraint = PrimaryKeyConstraint ["id"],
                    constraints = [],
                    unlogged = False
                }
            let compileOutput = compileStatementPreview [statement] statement |> Text.strip

            it "should compile CanCreate instance with sqlQuery" $ \statement -> do
                getInstanceDecl "CanCreate" compileOutput `shouldBe` [trimming|
                    instance CanCreate User where
                        create :: (?modelContext :: ModelContext) => User -> IO User
                        create model = do
                            List.head <$> sqlQuery "INSERT INTO users (id) VALUES (?) RETURNING id" (Only (model.id))
                        createMany [] = pure []
                        createMany models = do
                            sqlQuery (Query $ "INSERT INTO users (id) VALUES " <> (ByteString.intercalate ", " (List.map (\_ -> "(?)") models)) <> " RETURNING id") (List.concat $ List.map (\model -> [toField (model.id)]) models)
                    |]
            it "should compile CanUpdate instance with sqlQuery" $ \statement -> do
                getInstanceDecl "CanUpdate" compileOutput `shouldBe` [trimming|
                    instance CanUpdate User where
                        updateRecord model = do
                            List.head <$> sqlQuery "UPDATE users SET id = ? WHERE id = ? RETURNING id" ((fieldWithUpdate #id model, model.id))
                    |]

            it "should compile CanUpdate instance with an array type with an explicit cast" do
                let statement = StatementCreateTable $ CreateTable {
                    name = "users",
                    columns = [ Column "id" PUUID Nothing False True Nothing, Column "ids" (PArray PUUID) Nothing False False Nothing],
                    primaryKeyConstraint = PrimaryKeyConstraint ["id"],
                    constraints = []
                    , unlogged = False
                }
                let compileOutput = compileStatementPreview [statement] statement |> Text.strip

                getInstanceDecl "CanUpdate" compileOutput `shouldBe` [trimming|
                    instance CanUpdate User where
                        updateRecord model = do
                            List.head <$> sqlQuery "UPDATE users SET id = ?, ids = ? :: UUID[] WHERE id = ? RETURNING id, ids" ((fieldWithUpdate #id model, fieldWithUpdate #ids model, model.id))
                    |]
            it "should deal with double default values" do
                let statement = StatementCreateTable CreateTable
                        { name = "users"
                        , columns =
                            [ Column "id" PUUID Nothing False True Nothing, Column "ids" (PArray PUUID) Nothing False False Nothing
                            , Column {name = "electricity_unit_price", columnType = PDouble, defaultValue = Just (TypeCastExpression (DoubleExpression 0.17) PDouble), notNull = True, isUnique = False, generator = Nothing}
                            ]
                        , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        , constraints = []
                        , unlogged = False
                        }
                let compileOutput = compileStatementPreview [statement] statement |> Text.strip

                compileOutput `shouldBe` [trimming|
                    data User'  = User {id :: (Id' "users"), ids :: (Maybe [UUID]), electricityUnitPrice :: Double, meta :: MetaBag} deriving (Eq, Show)

                    type instance PrimaryKey "users" = UUID

                    type User = User' 

                    type instance GetTableName (User' ) = "users"
                    type instance GetModelByTableName "users" = User

                    instance Default (Id' "users") where def = Id def

                    instance () => Table (User' ) where
                        tableName = "users"
                        tableNameByteString = Data.Text.Encoding.encodeUtf8 "users"
                        columnNames = ["id","ids","electricity_unit_price"]
                        primaryKeyCondition User { id } = [("id", toField id)]
                        {-# INLINABLE primaryKeyCondition #-}


                    instance InputValue User where inputValue = IHP.ModelSupport.recordToInputValue


                    instance FromRow User where
                        fromRow = do
                            id <- field
                            ids <- field
                            electricityUnitPrice <- field
                            let theRecord = User id ids electricityUnitPrice def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }
                            pure theRecord


                    type instance GetModelName (User' ) = "User"

                    instance CanCreate User where
                        create :: (?modelContext :: ModelContext) => User -> IO User
                        create model = do
                            List.head <$> sqlQuery "INSERT INTO users (id, ids, electricity_unit_price) VALUES (?, ? :: UUID[], ?) RETURNING id, ids, electricity_unit_price" ((model.id, model.ids, fieldWithDefault #electricityUnitPrice model))
                        createMany [] = pure []
                        createMany models = do
                            sqlQuery (Query $ "INSERT INTO users (id, ids, electricity_unit_price) VALUES " <> (ByteString.intercalate ", " (List.map (\_ -> "(?, ? :: UUID[], ?)") models)) <> " RETURNING id, ids, electricity_unit_price") (List.concat $ List.map (\model -> [toField (model.id), toField (model.ids), toField (fieldWithDefault #electricityUnitPrice model)]) models)

                    instance CanUpdate User where
                        updateRecord model = do
                            List.head <$> sqlQuery "UPDATE users SET id = ?, ids = ? :: UUID[], electricity_unit_price = ? WHERE id = ? RETURNING id, ids, electricity_unit_price" ((fieldWithUpdate #id model, fieldWithUpdate #ids model, fieldWithUpdate #electricityUnitPrice model, model.id))

                    instance Record User where
                        {-# INLINE newRecord #-}
                        newRecord = User def def 0.17  def


                    instance QueryBuilder.FilterPrimaryKey "users" where
                        filterWhereId id builder =
                            builder |> QueryBuilder.filterWhere (#id, id)
                        {-# INLINE filterWhereId #-}
                |]
            it "should deal with integer default values for double columns" do
                let statement = StatementCreateTable CreateTable
                        { name = "users"
                        , columns =
                            [ Column "id" PUUID Nothing False True Nothing, Column "ids" (PArray PUUID) Nothing False False Nothing
                            , Column {name = "electricity_unit_price", columnType = PDouble, defaultValue = Just (IntExpression 0), notNull = True, isUnique = False, generator = Nothing}
                            ]
                        , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        , constraints = []
                        , unlogged = False
                        }
                let compileOutput = compileStatementPreview [statement] statement |> Text.strip

                compileOutput `shouldBe` [trimming|
                    data User'  = User {id :: (Id' "users"), ids :: (Maybe [UUID]), electricityUnitPrice :: Double, meta :: MetaBag} deriving (Eq, Show)

                    type instance PrimaryKey "users" = UUID

                    type User = User' 

                    type instance GetTableName (User' ) = "users"
                    type instance GetModelByTableName "users" = User

                    instance Default (Id' "users") where def = Id def

                    instance () => Table (User' ) where
                        tableName = "users"
                        tableNameByteString = Data.Text.Encoding.encodeUtf8 "users"
                        columnNames = ["id","ids","electricity_unit_price"]
                        primaryKeyCondition User { id } = [("id", toField id)]
                        {-# INLINABLE primaryKeyCondition #-}


                    instance InputValue User where inputValue = IHP.ModelSupport.recordToInputValue


                    instance FromRow User where
                        fromRow = do
                            id <- field
                            ids <- field
                            electricityUnitPrice <- field
                            let theRecord = User id ids electricityUnitPrice def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }
                            pure theRecord


                    type instance GetModelName (User' ) = "User"

                    instance CanCreate User where
                        create :: (?modelContext :: ModelContext) => User -> IO User
                        create model = do
                            List.head <$> sqlQuery "INSERT INTO users (id, ids, electricity_unit_price) VALUES (?, ? :: UUID[], ?) RETURNING id, ids, electricity_unit_price" ((model.id, model.ids, fieldWithDefault #electricityUnitPrice model))
                        createMany [] = pure []
                        createMany models = do
                            sqlQuery (Query $ "INSERT INTO users (id, ids, electricity_unit_price) VALUES " <> (ByteString.intercalate ", " (List.map (\_ -> "(?, ? :: UUID[], ?)") models)) <> " RETURNING id, ids, electricity_unit_price") (List.concat $ List.map (\model -> [toField (model.id), toField (model.ids), toField (fieldWithDefault #electricityUnitPrice model)]) models)

                    instance CanUpdate User where
                        updateRecord model = do
                            List.head <$> sqlQuery "UPDATE users SET id = ?, ids = ? :: UUID[], electricity_unit_price = ? WHERE id = ? RETURNING id, ids, electricity_unit_price" ((fieldWithUpdate #id model, fieldWithUpdate #ids model, fieldWithUpdate #electricityUnitPrice model, model.id))

                    instance Record User where
                        {-# INLINE newRecord #-}
                        newRecord = User def def 0  def


                    instance QueryBuilder.FilterPrimaryKey "users" where
                        filterWhereId id builder =
                            builder |> QueryBuilder.filterWhere (#id, id)
                        {-# INLINE filterWhereId #-}
                |]
            it "should not touch GENERATED columns" do
                let statement = StatementCreateTable CreateTable
                        { name = "users"
                        , columns =
                            [ Column "id" PUUID Nothing False True Nothing
                            , Column {name = "ts", columnType = PTSVector, defaultValue = Nothing, notNull = True, isUnique = False, generator = Just (ColumnGenerator { generate = VarExpression "someResult", stored = False }) }
                            ]
                        , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        , constraints = []
                        , unlogged = False
                        }
                let compileOutput = compileStatementPreview [statement] statement |> Text.strip

                compileOutput `shouldBe` [trimming|
                    data User'  = User {id :: (Id' "users"), ts :: (Maybe TSVector), meta :: MetaBag} deriving (Eq, Show)

                    type instance PrimaryKey "users" = UUID

                    type User = User' 

                    type instance GetTableName (User' ) = "users"
                    type instance GetModelByTableName "users" = User

                    instance Default (Id' "users") where def = Id def

                    instance () => Table (User' ) where
                        tableName = "users"
                        tableNameByteString = Data.Text.Encoding.encodeUtf8 "users"
                        columnNames = ["id","ts"]
                        primaryKeyCondition User { id } = [("id", toField id)]
                        {-# INLINABLE primaryKeyCondition #-}


                    instance InputValue User where inputValue = IHP.ModelSupport.recordToInputValue


                    instance FromRow User where
                        fromRow = do
                            id <- field
                            ts <- field
                            let theRecord = User id ts def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }
                            pure theRecord


                    type instance GetModelName (User' ) = "User"

                    instance CanCreate User where
                        create :: (?modelContext :: ModelContext) => User -> IO User
                        create model = do
                            List.head <$> sqlQuery "INSERT INTO users (id) VALUES (?) RETURNING id" (Only (model.id))
                        createMany [] = pure []
                        createMany models = do
                            sqlQuery (Query $ "INSERT INTO users (id) VALUES " <> (ByteString.intercalate ", " (List.map (\_ -> "(?)") models)) <> " RETURNING id") (List.concat $ List.map (\model -> [toField (model.id)]) models)

                    instance CanUpdate User where
                        updateRecord model = do
                            List.head <$> sqlQuery "UPDATE users SET id = ? WHERE id = ? RETURNING id" ((fieldWithUpdate #id model, model.id))

                    instance Record User where
                        {-# INLINE newRecord #-}
                        newRecord = User def def  def


                    instance QueryBuilder.FilterPrimaryKey "users" where
                        filterWhereId id builder =
                            builder |> QueryBuilder.filterWhere (#id, id)
                        {-# INLINE filterWhereId #-}
                |]
            it "should deal with multiple has many relationships to the same table" do
                let statements = parseSqlStatements [trimming|
                    CREATE TABLE landing_pages (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL
                    );
                    CREATE TABLE paragraph_ctas (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        landing_page_id UUID NOT NULL,
                        to_landing_page_id UUID NOT NULL
                    );
                    ALTER TABLE paragraph_ctas ADD CONSTRAINT paragraph_ctas_ref_landing_page_id FOREIGN KEY (landing_page_id) REFERENCES landing_pages (id) ON DELETE NO ACTION;
                    ALTER TABLE paragraph_ctas ADD CONSTRAINT paragraph_ctas_ref_to_landing_page_id FOREIGN KEY (to_landing_page_id) REFERENCES landing_pages (id) ON DELETE NO ACTION;
                |]
                let
                    isTargetTable :: Statement -> Bool
                    isTargetTable (StatementCreateTable CreateTable { name }) = name == "landing_pages"
                    isTargetTable otherwise = False
                let (Just statement) = find isTargetTable statements
                let compileOutput = compileStatementPreview statements statement |> Text.strip

                compileOutput `shouldBe` [trimming|
                    data LandingPage' paragraphCtasLandingPages paragraphCtasToLandingPages = LandingPage {id :: (Id' "landing_pages"), paragraphCtasLandingPages :: paragraphCtasLandingPages, paragraphCtasToLandingPages :: paragraphCtasToLandingPages, meta :: MetaBag} deriving (Eq, Show)

                    type instance PrimaryKey "landing_pages" = UUID
                    type instance Include "paragraphCtasLandingPages" (LandingPage' paragraphCtasLandingPages paragraphCtasToLandingPages) = LandingPage' [ParagraphCta] paragraphCtasToLandingPages
                    type instance Include "paragraphCtasToLandingPages" (LandingPage' paragraphCtasLandingPages paragraphCtasToLandingPages) = LandingPage' paragraphCtasLandingPages [ParagraphCta]

                    type LandingPage = LandingPage' (QueryBuilder.QueryBuilder "paragraph_ctas") (QueryBuilder.QueryBuilder "paragraph_ctas")

                    type instance GetTableName (LandingPage' _ _) = "landing_pages"
                    type instance GetModelByTableName "landing_pages" = LandingPage

                    instance Default (Id' "landing_pages") where def = Id def

                    instance () => Table (LandingPage' paragraphCtasLandingPages paragraphCtasToLandingPages) where
                        tableName = "landing_pages"
                        tableNameByteString = Data.Text.Encoding.encodeUtf8 "landing_pages"
                        columnNames = ["id"]
                        primaryKeyCondition LandingPage { id } = [("id", toField id)]
                        {-# INLINABLE primaryKeyCondition #-}


                    instance InputValue LandingPage where inputValue = IHP.ModelSupport.recordToInputValue


                    instance FromRow LandingPage where
                        fromRow = do
                            id <- field
                            let theRecord = LandingPage id def def def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }
                            pure theRecord


                    type instance GetModelName (LandingPage' _ _) = "LandingPage"

                    instance CanCreate LandingPage where
                        create :: (?modelContext :: ModelContext) => LandingPage -> IO LandingPage
                        create model = do
                            List.head <$> sqlQuery "INSERT INTO landing_pages (id) VALUES (?) RETURNING id" (Only (fieldWithDefault #id model))
                        createMany [] = pure []
                        createMany models = do
                            sqlQuery (Query $ "INSERT INTO landing_pages (id) VALUES " <> (ByteString.intercalate ", " (List.map (\_ -> "(?)") models)) <> " RETURNING id") (List.concat $ List.map (\model -> [toField (fieldWithDefault #id model)]) models)

                    instance CanUpdate LandingPage where
                        updateRecord model = do
                            List.head <$> sqlQuery "UPDATE landing_pages SET id = ? WHERE id = ? RETURNING id" ((fieldWithUpdate #id model, model.id))

                    instance Record LandingPage where
                        {-# INLINE newRecord #-}
                        newRecord = LandingPage def def def def


                    instance QueryBuilder.FilterPrimaryKey "landing_pages" where
                        filterWhereId id builder =
                            builder |> QueryBuilder.filterWhere (#id, id)
                        {-# INLINE filterWhereId #-}
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
        findInstanceDecl [] = error ("didn't find instance declaration of " <> instanceName)

        takeInstanceDecl (line:rest)
            | isEmpty line = []
            | otherwise = line : takeInstanceDecl rest
        takeInstanceDecl [] = error "never encountered newline?"
