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
                    instance CanCreate Generated.ActualTypes.User where
                        create :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO Generated.ActualTypes.User
                        create model = do
                            sqlQuerySingleRow "INSERT INTO users (id) VALUES (?) RETURNING id" (Only (model.id))
                        createMany [] = pure []
                        createMany models = do
                            sqlQuery (Query $ "INSERT INTO users (id) VALUES " <> (ByteString.intercalate ", " (List.map (\_ -> "(?)") models)) <> " RETURNING id") (List.concat $ List.map (\model -> [toField (model.id)]) models)
                        createRecordDiscardResult :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO ()
                        createRecordDiscardResult model = do
                            sqlExecDiscardResult "INSERT INTO users (id) VALUES (?)" (Only (model.id))
                    |]
            it "should compile CanUpdate instance with sqlQuery" $ \statement -> do
                getInstanceDecl "CanUpdate" compileOutput `shouldBe` [trimming|
                    instance CanUpdate Generated.ActualTypes.User where
                        updateRecord model = do
                            sqlQuerySingleRow "UPDATE users SET id = ? WHERE id = ? RETURNING id" ((fieldWithUpdate #id model, model.id))
                        updateRecordDiscardResult model = do
                            sqlExecDiscardResult "UPDATE users SET id = ? WHERE id = ?" ((fieldWithUpdate #id model, model.id))
                    |]

            it "should compile CanUpdate instance with an array type with an explicit cast" do
                let statement = StatementCreateTable $ CreateTable {
                    name = "users",
                    columns = [ Column "id" PUUID Nothing True True Nothing, Column "ids" (PArray PUUID) Nothing False False Nothing],
                    primaryKeyConstraint = PrimaryKeyConstraint ["id"],
                    constraints = []
                    , unlogged = False
                }
                let compileOutput = compileStatementPreview [statement] statement |> Text.strip

                getInstanceDecl "CanUpdate" compileOutput `shouldBe` [trimming|
                    instance CanUpdate Generated.ActualTypes.User where
                        updateRecord model = do
                            sqlQuerySingleRow "UPDATE users SET id = ?, ids = ? :: UUID[] WHERE id = ? RETURNING id, ids" ((fieldWithUpdate #id model, fieldWithUpdate #ids model, model.id))
                        updateRecordDiscardResult model = do
                            sqlExecDiscardResult "UPDATE users SET id = ?, ids = ? :: UUID[] WHERE id = ?" ((fieldWithUpdate #id model, fieldWithUpdate #ids model, model.id))
                    |]
            it "should deal with double default values" do
                let statement = StatementCreateTable CreateTable
                        { name = "users"
                        , columns =
                            [ Column "id" PUUID Nothing True True Nothing, Column "ids" (PArray PUUID) Nothing False False Nothing
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
                    type instance GetModelByTableName "users" = Generated.ActualTypes.User

                    instance Default (Id' "users") where def = Id def

                    instance () => IHP.ModelSupport.Table (User' ) where
                        tableName = "users"
                        tableNameByteString = Data.Text.Encoding.encodeUtf8 "users"
                        columnNames = ["id","ids","electricity_unit_price"]
                        primaryKeyColumnNames = ["id"]
                        primaryKeyConditionForId (Id (id)) = toField id
                        {-# INLINABLE primaryKeyConditionForId #-}


                    instance InputValue Generated.ActualTypes.User where inputValue = IHP.ModelSupport.recordToInputValue


                    instance FromRow Generated.ActualTypes.User where
                        fromRow = do
                            id <- field
                            ids <- field
                            electricityUnitPrice <- field
                            let theRecord = Generated.ActualTypes.User id ids electricityUnitPrice def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }
                            pure theRecord


                    type instance GetModelName (User' ) = "User"

                    instance CanCreate Generated.ActualTypes.User where
                        create :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO Generated.ActualTypes.User
                        create model = do
                            sqlQuerySingleRow "INSERT INTO users (id, ids, electricity_unit_price) VALUES (?, ? :: UUID[], ?) RETURNING id, ids, electricity_unit_price" ((model.id, model.ids, fieldWithDefault #electricityUnitPrice model))
                        createMany [] = pure []
                        createMany models = do
                            sqlQuery (Query $ "INSERT INTO users (id, ids, electricity_unit_price) VALUES " <> (ByteString.intercalate ", " (List.map (\_ -> "(?, ? :: UUID[], ?)") models)) <> " RETURNING id, ids, electricity_unit_price") (List.concat $ List.map (\model -> [toField (model.id), toField (model.ids), toField (fieldWithDefault #electricityUnitPrice model)]) models)
                        createRecordDiscardResult :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO ()
                        createRecordDiscardResult model = do
                            sqlExecDiscardResult "INSERT INTO users (id, ids, electricity_unit_price) VALUES (?, ? :: UUID[], ?)" ((model.id, model.ids, fieldWithDefault #electricityUnitPrice model))

                    instance CanUpdate Generated.ActualTypes.User where
                        updateRecord model = do
                            sqlQuerySingleRow "UPDATE users SET id = ?, ids = ? :: UUID[], electricity_unit_price = ? WHERE id = ? RETURNING id, ids, electricity_unit_price" ((fieldWithUpdate #id model, fieldWithUpdate #ids model, fieldWithUpdate #electricityUnitPrice model, model.id))
                        updateRecordDiscardResult model = do
                            sqlExecDiscardResult "UPDATE users SET id = ?, ids = ? :: UUID[], electricity_unit_price = ? WHERE id = ?" ((fieldWithUpdate #id model, fieldWithUpdate #ids model, fieldWithUpdate #electricityUnitPrice model, model.id))

                    instance Record Generated.ActualTypes.User where
                        {-# INLINE newRecord #-}
                        newRecord = Generated.ActualTypes.User def def 0.17  def


                    instance QueryBuilder.FilterPrimaryKey "users" where
                        filterWhereId id builder =
                            builder |> QueryBuilder.filterWhere (#id, id)
                        {-# INLINE filterWhereId #-}
                |]
            it "should deal with integer default values for double columns" do
                let statement = StatementCreateTable CreateTable
                        { name = "users"
                        , columns =
                            [ Column "id" PUUID Nothing True True Nothing, Column "ids" (PArray PUUID) Nothing False False Nothing
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
                    type instance GetModelByTableName "users" = Generated.ActualTypes.User

                    instance Default (Id' "users") where def = Id def

                    instance () => IHP.ModelSupport.Table (User' ) where
                        tableName = "users"
                        tableNameByteString = Data.Text.Encoding.encodeUtf8 "users"
                        columnNames = ["id","ids","electricity_unit_price"]
                        primaryKeyColumnNames = ["id"]
                        primaryKeyConditionForId (Id (id)) = toField id
                        {-# INLINABLE primaryKeyConditionForId #-}


                    instance InputValue Generated.ActualTypes.User where inputValue = IHP.ModelSupport.recordToInputValue


                    instance FromRow Generated.ActualTypes.User where
                        fromRow = do
                            id <- field
                            ids <- field
                            electricityUnitPrice <- field
                            let theRecord = Generated.ActualTypes.User id ids electricityUnitPrice def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }
                            pure theRecord


                    type instance GetModelName (User' ) = "User"

                    instance CanCreate Generated.ActualTypes.User where
                        create :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO Generated.ActualTypes.User
                        create model = do
                            sqlQuerySingleRow "INSERT INTO users (id, ids, electricity_unit_price) VALUES (?, ? :: UUID[], ?) RETURNING id, ids, electricity_unit_price" ((model.id, model.ids, fieldWithDefault #electricityUnitPrice model))
                        createMany [] = pure []
                        createMany models = do
                            sqlQuery (Query $ "INSERT INTO users (id, ids, electricity_unit_price) VALUES " <> (ByteString.intercalate ", " (List.map (\_ -> "(?, ? :: UUID[], ?)") models)) <> " RETURNING id, ids, electricity_unit_price") (List.concat $ List.map (\model -> [toField (model.id), toField (model.ids), toField (fieldWithDefault #electricityUnitPrice model)]) models)
                        createRecordDiscardResult :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO ()
                        createRecordDiscardResult model = do
                            sqlExecDiscardResult "INSERT INTO users (id, ids, electricity_unit_price) VALUES (?, ? :: UUID[], ?)" ((model.id, model.ids, fieldWithDefault #electricityUnitPrice model))

                    instance CanUpdate Generated.ActualTypes.User where
                        updateRecord model = do
                            sqlQuerySingleRow "UPDATE users SET id = ?, ids = ? :: UUID[], electricity_unit_price = ? WHERE id = ? RETURNING id, ids, electricity_unit_price" ((fieldWithUpdate #id model, fieldWithUpdate #ids model, fieldWithUpdate #electricityUnitPrice model, model.id))
                        updateRecordDiscardResult model = do
                            sqlExecDiscardResult "UPDATE users SET id = ?, ids = ? :: UUID[], electricity_unit_price = ? WHERE id = ?" ((fieldWithUpdate #id model, fieldWithUpdate #ids model, fieldWithUpdate #electricityUnitPrice model, model.id))

                    instance Record Generated.ActualTypes.User where
                        {-# INLINE newRecord #-}
                        newRecord = Generated.ActualTypes.User def def 0  def


                    instance QueryBuilder.FilterPrimaryKey "users" where
                        filterWhereId id builder =
                            builder |> QueryBuilder.filterWhere (#id, id)
                        {-# INLINE filterWhereId #-}
                |]
            it "should not touch GENERATED columns" do
                let statement = StatementCreateTable CreateTable
                        { name = "users"
                        , columns =
                            [ Column "id" PUUID Nothing True True Nothing
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
                    type instance GetModelByTableName "users" = Generated.ActualTypes.User

                    instance Default (Id' "users") where def = Id def

                    instance () => IHP.ModelSupport.Table (User' ) where
                        tableName = "users"
                        tableNameByteString = Data.Text.Encoding.encodeUtf8 "users"
                        columnNames = ["id","ts"]
                        primaryKeyColumnNames = ["id"]
                        primaryKeyConditionForId (Id (id)) = toField id
                        {-# INLINABLE primaryKeyConditionForId #-}


                    instance InputValue Generated.ActualTypes.User where inputValue = IHP.ModelSupport.recordToInputValue


                    instance FromRow Generated.ActualTypes.User where
                        fromRow = do
                            id <- field
                            ts <- field
                            let theRecord = Generated.ActualTypes.User id ts def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }
                            pure theRecord


                    type instance GetModelName (User' ) = "User"

                    instance CanCreate Generated.ActualTypes.User where
                        create :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO Generated.ActualTypes.User
                        create model = do
                            sqlQuerySingleRow "INSERT INTO users (id) VALUES (?) RETURNING id" (Only (model.id))
                        createMany [] = pure []
                        createMany models = do
                            sqlQuery (Query $ "INSERT INTO users (id) VALUES " <> (ByteString.intercalate ", " (List.map (\_ -> "(?)") models)) <> " RETURNING id") (List.concat $ List.map (\model -> [toField (model.id)]) models)
                        createRecordDiscardResult :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO ()
                        createRecordDiscardResult model = do
                            sqlExecDiscardResult "INSERT INTO users (id) VALUES (?)" (Only (model.id))

                    instance CanUpdate Generated.ActualTypes.User where
                        updateRecord model = do
                            sqlQuerySingleRow "UPDATE users SET id = ? WHERE id = ? RETURNING id" ((fieldWithUpdate #id model, model.id))
                        updateRecordDiscardResult model = do
                            sqlExecDiscardResult "UPDATE users SET id = ? WHERE id = ?" ((fieldWithUpdate #id model, model.id))

                    instance Record Generated.ActualTypes.User where
                        {-# INLINE newRecord #-}
                        newRecord = Generated.ActualTypes.User def def  def


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
                    
                    type LandingPage = LandingPage' (QueryBuilder.QueryBuilder "paragraph_ctas") (QueryBuilder.QueryBuilder "paragraph_ctas")

                    type instance GetTableName (LandingPage' _ _) = "landing_pages"
                    type instance GetModelByTableName "landing_pages" = Generated.ActualTypes.LandingPage

                    instance Default (Id' "landing_pages") where def = Id def

                    instance () => IHP.ModelSupport.Table (LandingPage' paragraphCtasLandingPages paragraphCtasToLandingPages) where
                        tableName = "landing_pages"
                        tableNameByteString = Data.Text.Encoding.encodeUtf8 "landing_pages"
                        columnNames = ["id"]
                        primaryKeyColumnNames = ["id"]
                        primaryKeyConditionForId (Id (id)) = toField id
                        {-# INLINABLE primaryKeyConditionForId #-}


                    instance InputValue Generated.ActualTypes.LandingPage where inputValue = IHP.ModelSupport.recordToInputValue


                    instance FromRow Generated.ActualTypes.LandingPage where
                        fromRow = do
                            id <- field
                            let theRecord = Generated.ActualTypes.LandingPage id def def def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }
                            pure theRecord


                    type instance GetModelName (LandingPage' _ _) = "LandingPage"

                    instance CanCreate Generated.ActualTypes.LandingPage where
                        create :: (?modelContext :: ModelContext) => Generated.ActualTypes.LandingPage -> IO Generated.ActualTypes.LandingPage
                        create model = do
                            sqlQuerySingleRow "INSERT INTO landing_pages (id) VALUES (?) RETURNING id" (Only (fieldWithDefault #id model))
                        createMany [] = pure []
                        createMany models = do
                            sqlQuery (Query $ "INSERT INTO landing_pages (id) VALUES " <> (ByteString.intercalate ", " (List.map (\_ -> "(?)") models)) <> " RETURNING id") (List.concat $ List.map (\model -> [toField (fieldWithDefault #id model)]) models)
                        createRecordDiscardResult :: (?modelContext :: ModelContext) => Generated.ActualTypes.LandingPage -> IO ()
                        createRecordDiscardResult model = do
                            sqlExecDiscardResult "INSERT INTO landing_pages (id) VALUES (?)" (Only (fieldWithDefault #id model))

                    instance CanUpdate Generated.ActualTypes.LandingPage where
                        updateRecord model = do
                            sqlQuerySingleRow "UPDATE landing_pages SET id = ? WHERE id = ? RETURNING id" ((fieldWithUpdate #id model, model.id))
                        updateRecordDiscardResult model = do
                            sqlExecDiscardResult "UPDATE landing_pages SET id = ? WHERE id = ?" ((fieldWithUpdate #id model, model.id))

                    instance Record Generated.ActualTypes.LandingPage where
                        {-# INLINE newRecord #-}
                        newRecord = Generated.ActualTypes.LandingPage def def def def


                    instance QueryBuilder.FilterPrimaryKey "landing_pages" where
                        filterWhereId id builder =
                            builder |> QueryBuilder.filterWhere (#id, id)
                        {-# INLINE filterWhereId #-}
                |]
            it "should not use DEFAULT for array columns" do
                let statement = StatementCreateTable CreateTable
                        { name = "users"
                        , columns =
                            [ Column "id" PUUID Nothing True True Nothing
                            , Column {name = "keywords", columnType = PArray PText, defaultValue = Just (VarExpression "NULL"), notNull = False, isUnique = False, generator = Nothing}
                            ]
                        , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        , constraints = []
                        , unlogged = False
                        }
                let compileOutput = compileStatementPreview [statement] statement |> Text.strip

                getInstanceDecl "CanCreate" compileOutput `shouldBe` [trimming|
                    instance CanCreate Generated.ActualTypes.User where
                        create :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO Generated.ActualTypes.User
                        create model = do
                            sqlQuerySingleRow "INSERT INTO users (id, keywords) VALUES (?, ? :: TEXT[]) RETURNING id, keywords" ((model.id, model.keywords))
                        createMany [] = pure []
                        createMany models = do
                            sqlQuery (Query $ "INSERT INTO users (id, keywords) VALUES " <> (ByteString.intercalate ", " (List.map (\_ -> "(?, ? :: TEXT[])") models)) <> " RETURNING id, keywords") (List.concat $ List.map (\model -> [toField (model.id), toField (model.keywords)]) models)
                        createRecordDiscardResult :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO ()
                        createRecordDiscardResult model = do
                            sqlExecDiscardResult "INSERT INTO users (id, keywords) VALUES (?, ? :: TEXT[])" ((model.id, model.keywords))
                    |]
        describe "compileStatementPreview for table with arbitrarily named primary key" do
            let statements = parseSqlStatements [trimming|
                CREATE TABLE things (
                    thing_arbitrary_ident UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL
                );
                CREATE TABLE others (
                    other_arbitrary_ident UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                    thing_ref UUID NOT NULL
                );
                ALTER TABLE others ADD CONSTRAINT other_thing_refs FOREIGN KEY (thing_ref) REFERENCES things (thing_arbitrary_ident) ON DELETE NO ACTION;
            |]
            let
                isTargetTable :: Statement -> Bool
                isTargetTable (StatementCreateTable CreateTable { name }) = name == "things"
                isTargetTable otherwise = False
            let (Just statement) = find isTargetTable statements
            let compileOutput = compileStatementPreview statements statement |> Text.strip
        
            it "should compile CanCreate instance with sqlQuery" $ \statement -> do
                getInstanceDecl "CanCreate" compileOutput `shouldBe` [trimming|
                    instance CanCreate Generated.ActualTypes.Thing where
                        create :: (?modelContext :: ModelContext) => Generated.ActualTypes.Thing -> IO Generated.ActualTypes.Thing
                        create model = do
                            sqlQuerySingleRow "INSERT INTO things (thing_arbitrary_ident) VALUES (?) RETURNING thing_arbitrary_ident" (Only (fieldWithDefault #thingArbitraryIdent model))
                        createMany [] = pure []
                        createMany models = do
                            sqlQuery (Query $ "INSERT INTO things (thing_arbitrary_ident) VALUES " <> (ByteString.intercalate ", " (List.map (\_ -> "(?)") models)) <> " RETURNING thing_arbitrary_ident") (List.concat $ List.map (\model -> [toField (fieldWithDefault #thingArbitraryIdent model)]) models)
                        createRecordDiscardResult :: (?modelContext :: ModelContext) => Generated.ActualTypes.Thing -> IO ()
                        createRecordDiscardResult model = do
                            sqlExecDiscardResult "INSERT INTO things (thing_arbitrary_ident) VALUES (?)" (Only (fieldWithDefault #thingArbitraryIdent model))
                    |]
            it "should compile CanUpdate instance with sqlQuery" $ \statement -> do
                getInstanceDecl "CanUpdate" compileOutput `shouldBe` [trimming|
                    instance CanUpdate Generated.ActualTypes.Thing where
                        updateRecord model = do
                            sqlQuerySingleRow "UPDATE things SET thing_arbitrary_ident = ? WHERE thing_arbitrary_ident = ? RETURNING thing_arbitrary_ident" ((fieldWithUpdate #thingArbitraryIdent model, model.thingArbitraryIdent))
                        updateRecordDiscardResult model = do
                            sqlExecDiscardResult "UPDATE things SET thing_arbitrary_ident = ? WHERE thing_arbitrary_ident = ?" ((fieldWithUpdate #thingArbitraryIdent model, model.thingArbitraryIdent))
                    |]
            it "should compile FromRow instance" $ \statement -> do
                getInstanceDecl "FromRow" compileOutput `shouldBe` [trimming|
                    instance FromRow Generated.ActualTypes.Thing where
                        fromRow = do
                            thingArbitraryIdent <- field
                            let theRecord = Generated.ActualTypes.Thing thingArbitraryIdent (QueryBuilder.filterWhere (#thingRef, thingArbitraryIdent) (QueryBuilder.query @Other)) def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }
                            pure theRecord
                    |]
            it "should compile Table instance" $ \statement -> do
                getInstanceDecl "() => IHP.ModelSupport.Table" compileOutput `shouldBe` [trimming|
                    instance () => IHP.ModelSupport.Table (Thing' others) where
                        tableName = "things"
                        tableNameByteString = Data.Text.Encoding.encodeUtf8 "things"
                        columnNames = ["thing_arbitrary_ident"]
                        primaryKeyColumnNames = ["thing_arbitrary_ident"]
                        primaryKeyConditionForId (Id (thingArbitraryIdent)) = toField thingArbitraryIdent
                        {-# INLINABLE primaryKeyConditionForId #-}
                    |]
            it "should compile QueryBuilder.FilterPrimaryKey instance" $ \statement -> do
                getInstanceDecl "QueryBuilder.FilterPrimaryKey" compileOutput `shouldBe` [trimming|
                    instance QueryBuilder.FilterPrimaryKey "things" where
                        filterWhereId thingArbitraryIdent builder =
                            builder |> QueryBuilder.filterWhere (#thingArbitraryIdent, thingArbitraryIdent)
                        {-# INLINE filterWhereId #-}
                    |]
        describe "compileStatementPreview for table with composite primary key" do
            let statements = parseSqlStatements [trimming|
                CREATE TABLE bits (
                    bit_arbitrary_ident UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL
                );
                CREATE TABLE parts (
                    part_arbitrary_ident UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL
                );
                CREATE TABLE bit_part_refs (
                    bit_ref UUID NOT NULL,
                    part_ref UUID NOT NULL,
                    PRIMARY KEY(bit_ref, part_ref)
                );
                ALTER TABLE bit_part_refs ADD CONSTRAINT bit_part_bit_refs FOREIGN KEY (bit_ref) REFERENCES bits (bit_arbitrary_ident) ON DELETE NO ACTION;
                ALTER TABLE bit_part_refs ADD CONSTRAINT bit_part_part_refs FOREIGN KEY (part_ref) REFERENCES parts (part_arbitrary_ident) ON DELETE NO ACTION;
            |]
            let
                isNamedTable :: Text -> Statement -> Bool
                isNamedTable targetName (StatementCreateTable CreateTable { name }) = name == targetName
                isNamedTable _ _ = False
            let (Just statement) = find (isNamedTable "bit_part_refs") statements
            let compileOutput = compileStatementPreview statements statement |> Text.strip
        
            it "should compile CanCreate instance with sqlQuery" $ \statement -> do
                getInstanceDecl "CanCreate" compileOutput `shouldBe` [trimming|
                    instance CanCreate Generated.ActualTypes.BitPartRef where
                        create :: (?modelContext :: ModelContext) => Generated.ActualTypes.BitPartRef -> IO Generated.ActualTypes.BitPartRef
                        create model = do
                            sqlQuerySingleRow "INSERT INTO bit_part_refs (bit_ref, part_ref) VALUES (?, ?) RETURNING bit_ref, part_ref" ((model.bitRef, model.partRef))
                        createMany [] = pure []
                        createMany models = do
                            sqlQuery (Query $ "INSERT INTO bit_part_refs (bit_ref, part_ref) VALUES " <> (ByteString.intercalate ", " (List.map (\_ -> "(?, ?)") models)) <> " RETURNING bit_ref, part_ref") (List.concat $ List.map (\model -> [toField (model.bitRef), toField (model.partRef)]) models)
                        createRecordDiscardResult :: (?modelContext :: ModelContext) => Generated.ActualTypes.BitPartRef -> IO ()
                        createRecordDiscardResult model = do
                            sqlExecDiscardResult "INSERT INTO bit_part_refs (bit_ref, part_ref) VALUES (?, ?)" ((model.bitRef, model.partRef))
                    |]
            it "should compile CanUpdate instance with sqlQuery" $ \statement -> do
                getInstanceDecl "CanUpdate" compileOutput `shouldBe` [trimming|
                    instance CanUpdate Generated.ActualTypes.BitPartRef where
                        updateRecord model = do
                            sqlQuerySingleRow "UPDATE bit_part_refs SET bit_ref = ?, part_ref = ? WHERE (bit_ref, part_ref) = (?, ?) RETURNING bit_ref, part_ref" ((fieldWithUpdate #bitRef model, fieldWithUpdate #partRef model, model.bitRef, model.partRef))
                        updateRecordDiscardResult model = do
                            sqlExecDiscardResult "UPDATE bit_part_refs SET bit_ref = ?, part_ref = ? WHERE (bit_ref, part_ref) = (?, ?)" ((fieldWithUpdate #bitRef model, fieldWithUpdate #partRef model, model.bitRef, model.partRef))
                    |]
            it "should compile FromRow instance" $ \statement -> do
                getInstanceDecl "FromRow" compileOutput `shouldBe` [trimming|
                    instance FromRow Generated.ActualTypes.BitPartRef where
                        fromRow = do
                            bitRef <- field
                            partRef <- field
                            let theRecord = Generated.ActualTypes.BitPartRef bitRef partRef def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }
                            pure theRecord
                    |]
            it "should compile Table instance" $ \statement -> do
                getInstanceDecl "(ToField bitRef, ToField partRef) => IHP.ModelSupport.Table" compileOutput `shouldBe` [trimming|
                    instance (ToField bitRef, ToField partRef) => IHP.ModelSupport.Table (BitPartRef' bitRef partRef) where
                        tableName = "bit_part_refs"
                        tableNameByteString = Data.Text.Encoding.encodeUtf8 "bit_part_refs"
                        columnNames = ["bit_ref","part_ref"]
                        primaryKeyColumnNames = ["bit_ref","part_ref"]
                        primaryKeyConditionForId (Id (bitRef, partRef)) = Many [Plain "(", toField bitRef, Plain ",", toField partRef, Plain ")"]
                        {-# INLINABLE primaryKeyConditionForId #-}
                    |]
            it "should compile FromRow instance of table that references part of a composite key" $ \statement -> do
                let (Just statement) = find (isNamedTable "parts") statements
                let compileOutput = compileStatementPreview statements statement |> Text.strip
                getInstanceDecl "FromRow" compileOutput `shouldBe` [trimming|
                    instance FromRow Generated.ActualTypes.Part where
                        fromRow = do
                            partArbitraryIdent <- field
                            let theRecord = Generated.ActualTypes.Part partArbitraryIdent (QueryBuilder.filterWhere (#partRef, partArbitraryIdent) (QueryBuilder.query @BitPartRef)) def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }
                            pure theRecord
                    |]
            it "should compile QueryBuilder.FilterPrimaryKey instance" $ \statement -> do
                getInstanceDecl "QueryBuilder.FilterPrimaryKey" compileOutput `shouldBe` [trimming|
                    instance QueryBuilder.FilterPrimaryKey "bit_part_refs" where
                        filterWhereId (Id (bitRef, partRef)) builder =
                            builder |> QueryBuilder.filterWhere (#bitRef, bitRef) |> QueryBuilder.filterWhere (#partRef, partRef)
                        {-# INLINE filterWhereId #-}
                    |]
        describe "compileFilterPrimaryKeyInstance" do
            it "should compile FilterPrimaryKey instance when primary key is called id" do
                let statement = StatementCreateTable $ CreateTable {
                        name = "things",
                        columns = [ Column "id" PUUID Nothing True True Nothing ],
                        primaryKeyConstraint = PrimaryKeyConstraint ["id"],
                        constraints = [],
                        unlogged = False
                    }
                let compileOutput = compileStatementPreview [statement] statement |> Text.strip
                
                getInstanceDecl "QueryBuilder.FilterPrimaryKey" compileOutput `shouldBe` [trimming|
                    instance QueryBuilder.FilterPrimaryKey "things" where
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
        takeInstanceDecl [] = [] -- EOF reached
