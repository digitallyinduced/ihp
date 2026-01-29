{-|
Module: Test.SchemaCompilerSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.SchemaCompilerSpec where

import Test.Hspec
import IHP.Prelude
import IHP.SchemaCompiler
import IHP.Postgres.Types
import qualified Data.Text as Text
import Test.IDE.SchemaDesigner.ParserSpec (parseSqlStatements, col, table)

tests = do
    describe "SchemaCompiler" do
        describe "compileEnumDataDefinitions" do
            it "should deal with enum values that have spaces" do
                let statement = CreateEnumType { name = "mood", values = ["happy", "very happy", "sad", "very sad"] }
                let output = compileStatementPreview [statement] statement |> Text.strip

                output `shouldBe` [trimming|
                    data Mood = Happy | VeryHappy | Sad | VerySad deriving (Eq, Show, Read, Enum, Bounded, Ord)
                    instance FromField Mood where
                        fromField = Decoders.enum \case
                            "happy" -> Just Happy
                            "very happy" -> Just VeryHappy
                            "sad" -> Just Sad
                            "very sad" -> Just VerySad
                            _ -> Nothing
                    instance Default Mood where def = Happy
                    instance DefaultParamEncoder Mood where
                        defaultParam = Encoders.nonNullable (Encoders.enum \case
                                Happy -> "happy"
                                VeryHappy -> "very happy"
                                Sad -> "sad"
                                VerySad -> "very sad"
                        )
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
                        fromField = Decoders.enum \case
                            "Alberta" -> Just Alberta
                            "BritishColumbia" -> Just Britishcolumbia
                            "Saskatchewan" -> Just Saskatchewan
                            "Manitoba" -> Just Manitoba
                            "Ontario" -> Just Ontario
                            "Quebec" -> Just Quebec
                            "NovaScotia" -> Just Novascotia
                            "NewBrunswick" -> Just Newbrunswick
                            "PrinceEdwardIsland" -> Just Princeedwardisland
                            "NewfoundlandAndLabrador" -> Just Newfoundlandandlabrador
                            _ -> Nothing
                    instance Default Province where def = Alberta
                    instance DefaultParamEncoder Province where
                        defaultParam = Encoders.nonNullable (Encoders.enum \case
                                Alberta -> "Alberta"
                                Britishcolumbia -> "BritishColumbia"
                                Saskatchewan -> "Saskatchewan"
                                Manitoba -> "Manitoba"
                                Ontario -> "Ontario"
                                Quebec -> "Quebec"
                                Novascotia -> "NovaScotia"
                                Newbrunswick -> "NewBrunswick"
                                Princeedwardisland -> "PrinceEdwardIsland"
                                Newfoundlandandlabrador -> "NewfoundlandAndLabrador"
                        )
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
                        fromField = Decoders.enum \case
                            "APARTMENT" -> Just PropertyTypeApartment
                            "HOUSE" -> Just House
                            _ -> Nothing
                    instance Default PropertyType where def = PropertyTypeApartment
                    instance DefaultParamEncoder PropertyType where
                        defaultParam = Encoders.nonNullable (Encoders.enum \case
                                PropertyTypeApartment -> "APARTMENT"
                                House -> "HOUSE"
                        )
                    instance InputValue PropertyType where
                        inputValue PropertyTypeApartment = "APARTMENT" :: Text
                        inputValue House = "HOUSE" :: Text
                    instance DeepSeq.NFData PropertyType where rnf a = seq a ()
                    instance IHP.Controller.Param.ParamReader PropertyType where readParameter = IHP.Controller.Param.enumParamReader; readParameterJSON = IHP.Controller.Param.enumParamReaderJSON
                |]
        describe "compileCreate" do
            let statement = StatementCreateTable $ (table "users") {
                    columns = [ col "id" PUUID ],
                    primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                }
            let compileOutput = compileStatementPreview [statement] statement |> Text.strip

            it "should compile CanCreate instance with sqlQuery" $ \statement -> do
                getInstanceDecl "CanCreate" compileOutput `shouldBe` [trimming|
                    instance CanCreate Generated.ActualTypes.User where
                        create :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO Generated.ActualTypes.User
                        create model = do
                            let theSnippet = sql "INSERT INTO users (id) VALUES (" <> param model.id <> sql ") RETURNING id"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.User)
                            sqlQuerySingleRow theSnippet decoder
                        createMany [] = pure []
                        createMany models = do
                            let theSnippet = sql "INSERT INTO users (id) VALUES " <> mconcat $ List.intersperse (sql ", ") $ List.map (\model -> sql "(" <> param model.id <> sql ")") models <> sql " RETURNING id"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.User)
                            sqlQuery theSnippet decoder
                        createRecordDiscardResult :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO ()
                        createRecordDiscardResult model = do
                            let theSnippet = sql "INSERT INTO users (id) VALUES (" <> param model.id <> sql ")"
                            sqlExecDiscardResult theSnippet
                    |]
            it "should compile CanUpdate instance with sqlQuery" $ \statement -> do
                getInstanceDecl "CanUpdate" compileOutput `shouldBe` [trimming|
                    instance CanUpdate Generated.ActualTypes.User where
                        updateRecord model = do
                            let theSnippet = sql "UPDATE users SET " <> sql "id = " <> case fieldWithUpdate #id model of { NoUpdate _ -> sql "id"; Update v -> param v } <> sql " WHERE id = " <> param model.id <> sql " RETURNING id"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.User)
                            sqlQuerySingleRow theSnippet decoder
                        updateRecordDiscardResult model = do
                            let theSnippet = sql "UPDATE users SET " <> sql "id = " <> case fieldWithUpdate #id model of { NoUpdate _ -> sql "id"; Update v -> param v } <> sql " WHERE id = " <> param model.id
                            sqlExecDiscardResult theSnippet
                    |]

            it "should compile CanUpdate instance with an array type with an explicit cast" do
                let statement = StatementCreateTable $ (table "users") {
                    columns = [ (col "id" PUUID) { notNull = True, isUnique = True }, col "ids" (PArray PUUID)],
                    primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                }
                let compileOutput = compileStatementPreview [statement] statement |> Text.strip

                getInstanceDecl "CanUpdate" compileOutput `shouldBe` [trimming|
                    instance CanUpdate Generated.ActualTypes.User where
                        updateRecord model = do
                            let theSnippet = sql "UPDATE users SET " <> sql "id = " <> case fieldWithUpdate #id model of { NoUpdate _ -> sql "id"; Update v -> param v } <> sql ", " <> sql "ids = " <> case fieldWithUpdate #ids model of { NoUpdate _ -> sql "ids"; Update v -> param v <> sql " :: UUID[]" } <> sql " WHERE id = " <> param model.id <> sql " RETURNING id, ids"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.User)
                            sqlQuerySingleRow theSnippet decoder
                        updateRecordDiscardResult model = do
                            let theSnippet = sql "UPDATE users SET " <> sql "id = " <> case fieldWithUpdate #id model of { NoUpdate _ -> sql "id"; Update v -> param v } <> sql ", " <> sql "ids = " <> case fieldWithUpdate #ids model of { NoUpdate _ -> sql "ids"; Update v -> param v <> sql " :: UUID[]" } <> sql " WHERE id = " <> param model.id
                            sqlExecDiscardResult theSnippet
                    |]
            it "should deal with double default values" do
                let statement = StatementCreateTable (table "users")
                        { columns =
                            [ (col "id" PUUID) { notNull = True, isUnique = True }
                            , col "ids" (PArray PUUID)
                            , (col "electricity_unit_price" PDouble) { defaultValue = Just (TypeCastExpression (DoubleExpression 0.17) PDouble), notNull = True }
                            ]
                        , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        }
                let compileOutput = compileStatementPreview [statement] statement |> Text.strip

                compileOutput `shouldBe` [trimming|
                    data User'  = User {id :: (Id' "users"), ids :: (Maybe [UUID]), electricityUnitPrice :: Double, meta :: MetaBag} deriving (Eq, Show)

                    type instance PrimaryKey "users" = UUID

                    type User = User' 

                    type instance GetTableName (User' ) = "users"
                    type instance GetModelByTableName "users" = User

                    instance Default (Id' "users") where def = Id def

                    instance () => IHP.ModelSupport.Table (User' ) where
                        tableName = "users"
                        tableNameByteString = Data.Text.Encoding.encodeUtf8 "users"
                        columnNames = ["id","ids","electricity_unit_price"]
                        primaryKeyColumnNames = ["id"]
                        primaryKeyConditionForId (Id (id)) = param id
                        {-# INLINABLE primaryKeyConditionForId #-}
                        primaryKeyEncoder = contramap (\(Id pk) -> pk) (Encoders.param defaultParam)
                        {-# INLINE primaryKeyEncoder #-}


                    instance InputValue Generated.ActualTypes.User where inputValue = IHP.ModelSupport.recordToInputValue


                    instance FromRow Generated.ActualTypes.User where
                        fromRow = do
                            id <- fmap Id (Decoders.column (Decoders.nonNullable Decoders.uuid))
                            ids <- Decoders.column (Decoders.nullable (Decoders.listArray (Decoders.nonNullable Decoders.uuid)))
                            electricityUnitPrice <- Decoders.column (Decoders.nonNullable Decoders.float8)
                            let theRecord = Generated.ActualTypes.User id ids electricityUnitPrice def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }
                            pure theRecord


                    type instance GetModelName (User' ) = "User"

                    instance CanCreate Generated.ActualTypes.User where
                        create :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO Generated.ActualTypes.User
                        create model = do
                            let theSnippet = sql "INSERT INTO users (id, ids, electricity_unit_price) VALUES (" <> param model.id <> sql ", " <> param model.ids <> sql " :: UUID[]" <> sql ", " <> case fieldWithDefault #electricityUnitPrice model of { Default -> sql "DEFAULT"; NonDefault v -> param v } <> sql ") RETURNING id, ids, electricity_unit_price"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.User)
                            sqlQuerySingleRow theSnippet decoder
                        createMany [] = pure []
                        createMany models = do
                            let theSnippet = sql "INSERT INTO users (id, ids, electricity_unit_price) VALUES " <> mconcat $ List.intersperse (sql ", ") $ List.map (\model -> sql "(" <> param model.id <> sql ", " <> param model.ids <> sql " :: UUID[]" <> sql ", " <> case fieldWithDefault #electricityUnitPrice model of { Default -> sql "DEFAULT"; NonDefault v -> param v } <> sql ")") models <> sql " RETURNING id, ids, electricity_unit_price"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.User)
                            sqlQuery theSnippet decoder
                        createRecordDiscardResult :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO ()
                        createRecordDiscardResult model = do
                            let theSnippet = sql "INSERT INTO users (id, ids, electricity_unit_price) VALUES (" <> param model.id <> sql ", " <> param model.ids <> sql " :: UUID[]" <> sql ", " <> case fieldWithDefault #electricityUnitPrice model of { Default -> sql "DEFAULT"; NonDefault v -> param v } <> sql ")"
                            sqlExecDiscardResult theSnippet

                    instance CanUpdate Generated.ActualTypes.User where
                        updateRecord model = do
                            let theSnippet = sql "UPDATE users SET " <> sql "id = " <> case fieldWithUpdate #id model of { NoUpdate _ -> sql "id"; Update v -> param v } <> sql ", " <> sql "ids = " <> case fieldWithUpdate #ids model of { NoUpdate _ -> sql "ids"; Update v -> param v <> sql " :: UUID[]" } <> sql ", " <> sql "electricity_unit_price = " <> case fieldWithUpdate #electricityUnitPrice model of { NoUpdate _ -> sql "electricity_unit_price"; Update v -> param v } <> sql " WHERE id = " <> param model.id <> sql " RETURNING id, ids, electricity_unit_price"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.User)
                            sqlQuerySingleRow theSnippet decoder
                        updateRecordDiscardResult model = do
                            let theSnippet = sql "UPDATE users SET " <> sql "id = " <> case fieldWithUpdate #id model of { NoUpdate _ -> sql "id"; Update v -> param v } <> sql ", " <> sql "ids = " <> case fieldWithUpdate #ids model of { NoUpdate _ -> sql "ids"; Update v -> param v <> sql " :: UUID[]" } <> sql ", " <> sql "electricity_unit_price = " <> case fieldWithUpdate #electricityUnitPrice model of { NoUpdate _ -> sql "electricity_unit_price"; Update v -> param v } <> sql " WHERE id = " <> param model.id
                            sqlExecDiscardResult theSnippet

                    instance Record Generated.ActualTypes.User where
                        {-# INLINE newRecord #-}
                        newRecord = Generated.ActualTypes.User def def 0.17  def


                    instance QueryBuilder.FilterPrimaryKey "users" where
                        filterWhereId id builder =
                            builder |> QueryBuilder.filterWhere (#id, id)
                        {-# INLINE filterWhereId #-}
                |]
            it "should deal with integer default values for double columns" do
                let statement = StatementCreateTable (table "users")
                        { columns =
                            [ (col "id" PUUID) { notNull = True, isUnique = True }
                            , col "ids" (PArray PUUID)
                            , (col "electricity_unit_price" PDouble) { defaultValue = Just (IntExpression 0), notNull = True }
                            ]
                        , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        }
                let compileOutput = compileStatementPreview [statement] statement |> Text.strip

                compileOutput `shouldBe` [trimming|
                    data User'  = User {id :: (Id' "users"), ids :: (Maybe [UUID]), electricityUnitPrice :: Double, meta :: MetaBag} deriving (Eq, Show)

                    type instance PrimaryKey "users" = UUID

                    type User = User' 

                    type instance GetTableName (User' ) = "users"
                    type instance GetModelByTableName "users" = User

                    instance Default (Id' "users") where def = Id def

                    instance () => IHP.ModelSupport.Table (User' ) where
                        tableName = "users"
                        tableNameByteString = Data.Text.Encoding.encodeUtf8 "users"
                        columnNames = ["id","ids","electricity_unit_price"]
                        primaryKeyColumnNames = ["id"]
                        primaryKeyConditionForId (Id (id)) = param id
                        {-# INLINABLE primaryKeyConditionForId #-}
                        primaryKeyEncoder = contramap (\(Id pk) -> pk) (Encoders.param defaultParam)
                        {-# INLINE primaryKeyEncoder #-}


                    instance InputValue Generated.ActualTypes.User where inputValue = IHP.ModelSupport.recordToInputValue


                    instance FromRow Generated.ActualTypes.User where
                        fromRow = do
                            id <- fmap Id (Decoders.column (Decoders.nonNullable Decoders.uuid))
                            ids <- Decoders.column (Decoders.nullable (Decoders.listArray (Decoders.nonNullable Decoders.uuid)))
                            electricityUnitPrice <- Decoders.column (Decoders.nonNullable Decoders.float8)
                            let theRecord = Generated.ActualTypes.User id ids electricityUnitPrice def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }
                            pure theRecord


                    type instance GetModelName (User' ) = "User"

                    instance CanCreate Generated.ActualTypes.User where
                        create :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO Generated.ActualTypes.User
                        create model = do
                            let theSnippet = sql "INSERT INTO users (id, ids, electricity_unit_price) VALUES (" <> param model.id <> sql ", " <> param model.ids <> sql " :: UUID[]" <> sql ", " <> case fieldWithDefault #electricityUnitPrice model of { Default -> sql "DEFAULT"; NonDefault v -> param v } <> sql ") RETURNING id, ids, electricity_unit_price"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.User)
                            sqlQuerySingleRow theSnippet decoder
                        createMany [] = pure []
                        createMany models = do
                            let theSnippet = sql "INSERT INTO users (id, ids, electricity_unit_price) VALUES " <> mconcat $ List.intersperse (sql ", ") $ List.map (\model -> sql "(" <> param model.id <> sql ", " <> param model.ids <> sql " :: UUID[]" <> sql ", " <> case fieldWithDefault #electricityUnitPrice model of { Default -> sql "DEFAULT"; NonDefault v -> param v } <> sql ")") models <> sql " RETURNING id, ids, electricity_unit_price"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.User)
                            sqlQuery theSnippet decoder
                        createRecordDiscardResult :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO ()
                        createRecordDiscardResult model = do
                            let theSnippet = sql "INSERT INTO users (id, ids, electricity_unit_price) VALUES (" <> param model.id <> sql ", " <> param model.ids <> sql " :: UUID[]" <> sql ", " <> case fieldWithDefault #electricityUnitPrice model of { Default -> sql "DEFAULT"; NonDefault v -> param v } <> sql ")"
                            sqlExecDiscardResult theSnippet

                    instance CanUpdate Generated.ActualTypes.User where
                        updateRecord model = do
                            let theSnippet = sql "UPDATE users SET " <> sql "id = " <> case fieldWithUpdate #id model of { NoUpdate _ -> sql "id"; Update v -> param v } <> sql ", " <> sql "ids = " <> case fieldWithUpdate #ids model of { NoUpdate _ -> sql "ids"; Update v -> param v <> sql " :: UUID[]" } <> sql ", " <> sql "electricity_unit_price = " <> case fieldWithUpdate #electricityUnitPrice model of { NoUpdate _ -> sql "electricity_unit_price"; Update v -> param v } <> sql " WHERE id = " <> param model.id <> sql " RETURNING id, ids, electricity_unit_price"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.User)
                            sqlQuerySingleRow theSnippet decoder
                        updateRecordDiscardResult model = do
                            let theSnippet = sql "UPDATE users SET " <> sql "id = " <> case fieldWithUpdate #id model of { NoUpdate _ -> sql "id"; Update v -> param v } <> sql ", " <> sql "ids = " <> case fieldWithUpdate #ids model of { NoUpdate _ -> sql "ids"; Update v -> param v <> sql " :: UUID[]" } <> sql ", " <> sql "electricity_unit_price = " <> case fieldWithUpdate #electricityUnitPrice model of { NoUpdate _ -> sql "electricity_unit_price"; Update v -> param v } <> sql " WHERE id = " <> param model.id
                            sqlExecDiscardResult theSnippet

                    instance Record Generated.ActualTypes.User where
                        {-# INLINE newRecord #-}
                        newRecord = Generated.ActualTypes.User def def 0  def


                    instance QueryBuilder.FilterPrimaryKey "users" where
                        filterWhereId id builder =
                            builder |> QueryBuilder.filterWhere (#id, id)
                        {-# INLINE filterWhereId #-}
                |]
            it "should not touch GENERATED columns" do
                let statement = StatementCreateTable (table "users")
                        { columns =
                            [ (col "id" PUUID) { notNull = True, isUnique = True }
                            , (col "ts" PTSVector) { notNull = True, generator = Just (ColumnGenerator { generate = VarExpression "someResult", stored = False }) }
                            ]
                        , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        }
                let compileOutput = compileStatementPreview [statement] statement |> Text.strip

                compileOutput `shouldBe` [trimming|
                    data User'  = User {id :: (Id' "users"), ts :: (Maybe TSVector), meta :: MetaBag} deriving (Eq, Show)

                    type instance PrimaryKey "users" = UUID

                    type User = User' 

                    type instance GetTableName (User' ) = "users"
                    type instance GetModelByTableName "users" = User

                    instance Default (Id' "users") where def = Id def

                    instance () => IHP.ModelSupport.Table (User' ) where
                        tableName = "users"
                        tableNameByteString = Data.Text.Encoding.encodeUtf8 "users"
                        columnNames = ["id","ts"]
                        primaryKeyColumnNames = ["id"]
                        primaryKeyConditionForId (Id (id)) = param id
                        {-# INLINABLE primaryKeyConditionForId #-}
                        primaryKeyEncoder = contramap (\(Id pk) -> pk) (Encoders.param defaultParam)
                        {-# INLINE primaryKeyEncoder #-}


                    instance InputValue Generated.ActualTypes.User where inputValue = IHP.ModelSupport.recordToInputValue


                    instance FromRow Generated.ActualTypes.User where
                        fromRow = do
                            id <- fmap Id (Decoders.column (Decoders.nonNullable Decoders.uuid))
                            ts <- Decoders.column (Decoders.nullable fromField)
                            let theRecord = Generated.ActualTypes.User id ts def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }
                            pure theRecord


                    type instance GetModelName (User' ) = "User"

                    instance CanCreate Generated.ActualTypes.User where
                        create :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO Generated.ActualTypes.User
                        create model = do
                            let theSnippet = sql "INSERT INTO users (id) VALUES (" <> param model.id <> sql ") RETURNING id, ts"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.User)
                            sqlQuerySingleRow theSnippet decoder
                        createMany [] = pure []
                        createMany models = do
                            let theSnippet = sql "INSERT INTO users (id) VALUES " <> mconcat $ List.intersperse (sql ", ") $ List.map (\model -> sql "(" <> param model.id <> sql ")") models <> sql " RETURNING id, ts"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.User)
                            sqlQuery theSnippet decoder
                        createRecordDiscardResult :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO ()
                        createRecordDiscardResult model = do
                            let theSnippet = sql "INSERT INTO users (id) VALUES (" <> param model.id <> sql ")"
                            sqlExecDiscardResult theSnippet

                    instance CanUpdate Generated.ActualTypes.User where
                        updateRecord model = do
                            let theSnippet = sql "UPDATE users SET " <> sql "id = " <> case fieldWithUpdate #id model of { NoUpdate _ -> sql "id"; Update v -> param v } <> sql " WHERE id = " <> param model.id <> sql " RETURNING id, ts"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.User)
                            sqlQuerySingleRow theSnippet decoder
                        updateRecordDiscardResult model = do
                            let theSnippet = sql "UPDATE users SET " <> sql "id = " <> case fieldWithUpdate #id model of { NoUpdate _ -> sql "id"; Update v -> param v } <> sql " WHERE id = " <> param model.id
                            sqlExecDiscardResult theSnippet

                    instance Record Generated.ActualTypes.User where
                        {-# INLINE newRecord #-}
                        newRecord = Generated.ActualTypes.User def def  def


                    instance QueryBuilder.FilterPrimaryKey "users" where
                        filterWhereId id builder =
                            builder |> QueryBuilder.filterWhere (#id, id)
                        {-# INLINE filterWhereId #-}
                |]
            it "should handle tablets with generated columns" do
                let statement = StatementCreateTable CreateTable
                        { name = "posts"
                        , columns =
                            [ (col "id" PUUID) { notNull = True, isUnique = True }
                            , (col "title" PText) { notNull = True }
                            , (col "body" PText) { notNull = True }
                            , (col "body_index_col" PTSVector) { generator = Just (ColumnGenerator { generate = CallExpression "to_tsvector" [TextExpression "english", CallExpression "coalesce" [VarExpression "body", TextExpression ""]], stored = True }) }
                            ]
                        , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        , constraints = []
                        , unlogged = False
                        }
                let compileOutput = compileStatementPreview [statement] statement |> Text.strip

                -- The key point: RETURNING clause should include body_index_col even though it's generated
                getInstanceDecl "CanCreate" compileOutput `shouldBe` [trimming|
                    instance CanCreate Generated.ActualTypes.Post where
                        create :: (?modelContext :: ModelContext) => Generated.ActualTypes.Post -> IO Generated.ActualTypes.Post
                        create model = do
                            let theSnippet = sql "INSERT INTO posts (id, title, body) VALUES (" <> param model.id <> sql ", " <> param model.title <> sql ", " <> param model.body <> sql ") RETURNING id, title, body, body_index_col"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.Post)
                            sqlQuerySingleRow theSnippet decoder
                        createMany [] = pure []
                        createMany models = do
                            let theSnippet = sql "INSERT INTO posts (id, title, body) VALUES " <> mconcat $ List.intersperse (sql ", ") $ List.map (\model -> sql "(" <> param model.id <> sql ", " <> param model.title <> sql ", " <> param model.body <> sql ")") models <> sql " RETURNING id, title, body, body_index_col"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.Post)
                            sqlQuery theSnippet decoder
                        createRecordDiscardResult :: (?modelContext :: ModelContext) => Generated.ActualTypes.Post -> IO ()
                        createRecordDiscardResult model = do
                            let theSnippet = sql "INSERT INTO posts (id, title, body) VALUES (" <> param model.id <> sql ", " <> param model.title <> sql ", " <> param model.body <> sql ")"
                            sqlExecDiscardResult theSnippet
                    |]

                getInstanceDecl "CanUpdate" compileOutput `shouldBe` [trimming|
                    instance CanUpdate Generated.ActualTypes.Post where
                        updateRecord model = do
                            let theSnippet = sql "UPDATE posts SET " <> sql "id = " <> case fieldWithUpdate #id model of { NoUpdate _ -> sql "id"; Update v -> param v } <> sql ", " <> sql "title = " <> case fieldWithUpdate #title model of { NoUpdate _ -> sql "title"; Update v -> param v } <> sql ", " <> sql "body = " <> case fieldWithUpdate #body model of { NoUpdate _ -> sql "body"; Update v -> param v } <> sql " WHERE id = " <> param model.id <> sql " RETURNING id, title, body, body_index_col"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.Post)
                            sqlQuerySingleRow theSnippet decoder
                        updateRecordDiscardResult model = do
                            let theSnippet = sql "UPDATE posts SET " <> sql "id = " <> case fieldWithUpdate #id model of { NoUpdate _ -> sql "id"; Update v -> param v } <> sql ", " <> sql "title = " <> case fieldWithUpdate #title model of { NoUpdate _ -> sql "title"; Update v -> param v } <> sql ", " <> sql "body = " <> case fieldWithUpdate #body model of { NoUpdate _ -> sql "body"; Update v -> param v } <> sql " WHERE id = " <> param model.id
                            sqlExecDiscardResult theSnippet
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
                    type instance GetModelByTableName "landing_pages" = LandingPage

                    instance Default (Id' "landing_pages") where def = Id def

                    instance () => IHP.ModelSupport.Table (LandingPage' paragraphCtasLandingPages paragraphCtasToLandingPages) where
                        tableName = "landing_pages"
                        tableNameByteString = Data.Text.Encoding.encodeUtf8 "landing_pages"
                        columnNames = ["id"]
                        primaryKeyColumnNames = ["id"]
                        primaryKeyConditionForId (Id (id)) = param id
                        {-# INLINABLE primaryKeyConditionForId #-}
                        primaryKeyEncoder = contramap (\(Id pk) -> pk) (Encoders.param defaultParam)
                        {-# INLINE primaryKeyEncoder #-}


                    instance InputValue Generated.ActualTypes.LandingPage where inputValue = IHP.ModelSupport.recordToInputValue


                    instance FromRow Generated.ActualTypes.LandingPage where
                        fromRow = do
                            id <- fmap Id (Decoders.column (Decoders.nonNullable Decoders.uuid))
                            let theRecord = Generated.ActualTypes.LandingPage id def def def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }
                            pure theRecord


                    type instance GetModelName (LandingPage' _ _) = "LandingPage"

                    instance CanCreate Generated.ActualTypes.LandingPage where
                        create :: (?modelContext :: ModelContext) => Generated.ActualTypes.LandingPage -> IO Generated.ActualTypes.LandingPage
                        create model = do
                            let theSnippet = sql "INSERT INTO landing_pages (id) VALUES (" <> case fieldWithDefault #id model of { Default -> sql "DEFAULT"; NonDefault v -> param v } <> sql ") RETURNING id"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.LandingPage)
                            sqlQuerySingleRow theSnippet decoder
                        createMany [] = pure []
                        createMany models = do
                            let theSnippet = sql "INSERT INTO landing_pages (id) VALUES " <> mconcat $ List.intersperse (sql ", ") $ List.map (\model -> sql "(" <> case fieldWithDefault #id model of { Default -> sql "DEFAULT"; NonDefault v -> param v } <> sql ")") models <> sql " RETURNING id"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.LandingPage)
                            sqlQuery theSnippet decoder
                        createRecordDiscardResult :: (?modelContext :: ModelContext) => Generated.ActualTypes.LandingPage -> IO ()
                        createRecordDiscardResult model = do
                            let theSnippet = sql "INSERT INTO landing_pages (id) VALUES (" <> case fieldWithDefault #id model of { Default -> sql "DEFAULT"; NonDefault v -> param v } <> sql ")"
                            sqlExecDiscardResult theSnippet

                    instance CanUpdate Generated.ActualTypes.LandingPage where
                        updateRecord model = do
                            let theSnippet = sql "UPDATE landing_pages SET " <> sql "id = " <> case fieldWithUpdate #id model of { NoUpdate _ -> sql "id"; Update v -> param v } <> sql " WHERE id = " <> param model.id <> sql " RETURNING id"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.LandingPage)
                            sqlQuerySingleRow theSnippet decoder
                        updateRecordDiscardResult model = do
                            let theSnippet = sql "UPDATE landing_pages SET " <> sql "id = " <> case fieldWithUpdate #id model of { NoUpdate _ -> sql "id"; Update v -> param v } <> sql " WHERE id = " <> param model.id
                            sqlExecDiscardResult theSnippet

                    instance Record Generated.ActualTypes.LandingPage where
                        {-# INLINE newRecord #-}
                        newRecord = Generated.ActualTypes.LandingPage def def def def


                    instance QueryBuilder.FilterPrimaryKey "landing_pages" where
                        filterWhereId id builder =
                            builder |> QueryBuilder.filterWhere (#id, id)
                        {-# INLINE filterWhereId #-}
                |]
            it "should not use DEFAULT for array columns" do
                let statement = StatementCreateTable (table "users")
                        { columns =
                            [ (col "id" PUUID) { notNull = True, isUnique = True }
                            , (col "keywords" (PArray PText)) { defaultValue = Just (VarExpression "NULL") }
                            ]
                        , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        }
                let compileOutput = compileStatementPreview [statement] statement |> Text.strip

                getInstanceDecl "CanCreate" compileOutput `shouldBe` [trimming|
                    instance CanCreate Generated.ActualTypes.User where
                        create :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO Generated.ActualTypes.User
                        create model = do
                            let theSnippet = sql "INSERT INTO users (id, keywords) VALUES (" <> param model.id <> sql ", " <> param model.keywords <> sql " :: TEXT[]" <> sql ") RETURNING id, keywords"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.User)
                            sqlQuerySingleRow theSnippet decoder
                        createMany [] = pure []
                        createMany models = do
                            let theSnippet = sql "INSERT INTO users (id, keywords) VALUES " <> mconcat $ List.intersperse (sql ", ") $ List.map (\model -> sql "(" <> param model.id <> sql ", " <> param model.keywords <> sql " :: TEXT[]" <> sql ")") models <> sql " RETURNING id, keywords"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.User)
                            sqlQuery theSnippet decoder
                        createRecordDiscardResult :: (?modelContext :: ModelContext) => Generated.ActualTypes.User -> IO ()
                        createRecordDiscardResult model = do
                            let theSnippet = sql "INSERT INTO users (id, keywords) VALUES (" <> param model.id <> sql ", " <> param model.keywords <> sql " :: TEXT[]" <> sql ")"
                            sqlExecDiscardResult theSnippet
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
                            let theSnippet = sql "INSERT INTO things (thing_arbitrary_ident) VALUES (" <> case fieldWithDefault #thingArbitraryIdent model of { Default -> sql "DEFAULT"; NonDefault v -> param v } <> sql ") RETURNING thing_arbitrary_ident"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.Thing)
                            sqlQuerySingleRow theSnippet decoder
                        createMany [] = pure []
                        createMany models = do
                            let theSnippet = sql "INSERT INTO things (thing_arbitrary_ident) VALUES " <> mconcat $ List.intersperse (sql ", ") $ List.map (\model -> sql "(" <> case fieldWithDefault #thingArbitraryIdent model of { Default -> sql "DEFAULT"; NonDefault v -> param v } <> sql ")") models <> sql " RETURNING thing_arbitrary_ident"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.Thing)
                            sqlQuery theSnippet decoder
                        createRecordDiscardResult :: (?modelContext :: ModelContext) => Generated.ActualTypes.Thing -> IO ()
                        createRecordDiscardResult model = do
                            let theSnippet = sql "INSERT INTO things (thing_arbitrary_ident) VALUES (" <> case fieldWithDefault #thingArbitraryIdent model of { Default -> sql "DEFAULT"; NonDefault v -> param v } <> sql ")"
                            sqlExecDiscardResult theSnippet
                    |]
            it "should compile CanUpdate instance with sqlQuery" $ \statement -> do
                getInstanceDecl "CanUpdate" compileOutput `shouldBe` [trimming|
                    instance CanUpdate Generated.ActualTypes.Thing where
                        updateRecord model = do
                            let theSnippet = sql "UPDATE things SET " <> sql "thing_arbitrary_ident = " <> case fieldWithUpdate #thingArbitraryIdent model of { NoUpdate _ -> sql "thing_arbitrary_ident"; Update v -> param v } <> sql " WHERE thing_arbitrary_ident = " <> param model.thingArbitraryIdent <> sql " RETURNING thing_arbitrary_ident"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.Thing)
                            sqlQuerySingleRow theSnippet decoder
                        updateRecordDiscardResult model = do
                            let theSnippet = sql "UPDATE things SET " <> sql "thing_arbitrary_ident = " <> case fieldWithUpdate #thingArbitraryIdent model of { NoUpdate _ -> sql "thing_arbitrary_ident"; Update v -> param v } <> sql " WHERE thing_arbitrary_ident = " <> param model.thingArbitraryIdent
                            sqlExecDiscardResult theSnippet
                    |]
            it "should compile FromRow instance" $ \statement -> do
                getInstanceDecl "FromRow" compileOutput `shouldBe` [trimming|
                    instance FromRow Generated.ActualTypes.Thing where
                        fromRow = do
                            thingArbitraryIdent <- fmap Id (Decoders.column (Decoders.nonNullable Decoders.uuid))
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
                        primaryKeyConditionForId (Id (thingArbitraryIdent)) = param thingArbitraryIdent
                        {-# INLINABLE primaryKeyConditionForId #-}
                        primaryKeyEncoder = contramap (\(Id pk) -> pk) (Encoders.param defaultParam)
                        {-# INLINE primaryKeyEncoder #-}
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
                            let theSnippet = sql "INSERT INTO bit_part_refs (bit_ref, part_ref) VALUES (" <> param model.bitRef <> sql ", " <> param model.partRef <> sql ") RETURNING bit_ref, part_ref"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.BitPartRef)
                            sqlQuerySingleRow theSnippet decoder
                        createMany [] = pure []
                        createMany models = do
                            let theSnippet = sql "INSERT INTO bit_part_refs (bit_ref, part_ref) VALUES " <> mconcat $ List.intersperse (sql ", ") $ List.map (\model -> sql "(" <> param model.bitRef <> sql ", " <> param model.partRef <> sql ")") models <> sql " RETURNING bit_ref, part_ref"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.BitPartRef)
                            sqlQuery theSnippet decoder
                        createRecordDiscardResult :: (?modelContext :: ModelContext) => Generated.ActualTypes.BitPartRef -> IO ()
                        createRecordDiscardResult model = do
                            let theSnippet = sql "INSERT INTO bit_part_refs (bit_ref, part_ref) VALUES (" <> param model.bitRef <> sql ", " <> param model.partRef <> sql ")"
                            sqlExecDiscardResult theSnippet
                    |]
            it "should compile CanUpdate instance with sqlQuery" $ \statement -> do
                getInstanceDecl "CanUpdate" compileOutput `shouldBe` [trimming|
                    instance CanUpdate Generated.ActualTypes.BitPartRef where
                        updateRecord model = do
                            let theSnippet = sql "UPDATE bit_part_refs SET " <> sql "bit_ref = " <> case fieldWithUpdate #bitRef model of { NoUpdate _ -> sql "bit_ref"; Update v -> param v } <> sql ", " <> sql "part_ref = " <> case fieldWithUpdate #partRef model of { NoUpdate _ -> sql "part_ref"; Update v -> param v } <> sql " WHERE (bit_ref, part_ref) = " <> param model.bitRef <> sql ", " <> param model.partRef <> sql " RETURNING bit_ref, part_ref"
                            let decoder = Decoders.rowList (fromRow @Generated.ActualTypes.BitPartRef)
                            sqlQuerySingleRow theSnippet decoder
                        updateRecordDiscardResult model = do
                            let theSnippet = sql "UPDATE bit_part_refs SET " <> sql "bit_ref = " <> case fieldWithUpdate #bitRef model of { NoUpdate _ -> sql "bit_ref"; Update v -> param v } <> sql ", " <> sql "part_ref = " <> case fieldWithUpdate #partRef model of { NoUpdate _ -> sql "part_ref"; Update v -> param v } <> sql " WHERE (bit_ref, part_ref) = " <> param model.bitRef <> sql ", " <> param model.partRef
                            sqlExecDiscardResult theSnippet
                    |]
            it "should compile FromRow instance" $ \statement -> do
                getInstanceDecl "FromRow" compileOutput `shouldBe` [trimming|
                    instance FromRow Generated.ActualTypes.BitPartRef where
                        fromRow = do
                            bitRef <- fmap Id (Decoders.column (Decoders.nonNullable Decoders.uuid))
                            partRef <- fmap Id (Decoders.column (Decoders.nonNullable Decoders.uuid))
                            let theRecord = Generated.ActualTypes.BitPartRef bitRef partRef def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }
                            pure theRecord
                    |]
            it "should compile Table instance" $ \statement -> do
                getInstanceDecl "(DefaultParamEncoder bitRef, DefaultParamEncoder partRef) => IHP.ModelSupport.Table" compileOutput `shouldBe` [trimming|
                    instance (DefaultParamEncoder bitRef, DefaultParamEncoder partRef) => IHP.ModelSupport.Table (BitPartRef' bitRef partRef) where
                        tableName = "bit_part_refs"
                        tableNameByteString = Data.Text.Encoding.encodeUtf8 "bit_part_refs"
                        columnNames = ["bit_ref","part_ref"]
                        primaryKeyColumnNames = ["bit_ref","part_ref"]
                        primaryKeyConditionForId (Id (bitRef, partRef)) = sql "(" <> param bitRef <> sql "," <> param partRef <> sql ")"
                        {-# INLINABLE primaryKeyConditionForId #-}
                        primaryKeyEncoder = divide (\(Id (a, b)) -> (a, b)) (Encoders.param defaultParam) (Encoders.param defaultParam)
                        {-# INLINE primaryKeyEncoder #-}
                    |]
            it "should compile FromRow instance of table that references part of a composite key" $ \statement -> do
                let (Just statement) = find (isNamedTable "parts") statements
                let compileOutput = compileStatementPreview statements statement |> Text.strip
                getInstanceDecl "FromRow" compileOutput `shouldBe` [trimming|
                    instance FromRow Generated.ActualTypes.Part where
                        fromRow = do
                            partArbitraryIdent <- fmap Id (Decoders.column (Decoders.nonNullable Decoders.uuid))
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
                let statement = StatementCreateTable $ (table "things") {
                        columns = [ (col "id" PUUID) { notNull = True, isUnique = True } ],
                        primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                    }
                let compileOutput = compileStatementPreview [statement] statement |> Text.strip
                
                getInstanceDecl "QueryBuilder.FilterPrimaryKey" compileOutput `shouldBe` [trimming|
                    instance QueryBuilder.FilterPrimaryKey "things" where
                        filterWhereId id builder =
                            builder |> QueryBuilder.filterWhere (#id, id)
                        {-# INLINE filterWhereId #-}
                    |]

        describe "simple mode (compileRelationSupport = False)" do
            let simpleOptions = previewCompilerOptions { compileRelationSupport = False }
            it "should produce no type parameters and no QueryBuilder fields for a table with FK and has-many relations" do
                let statements = parseSqlStatements [trimming|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL
                    );
                    CREATE TABLE posts (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        title TEXT NOT NULL,
                        user_id UUID NOT NULL
                    );
                    CREATE TABLE comments (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        post_id UUID NOT NULL,
                        body TEXT NOT NULL
                    );
                    ALTER TABLE posts ADD CONSTRAINT posts_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
                    ALTER TABLE comments ADD CONSTRAINT comments_ref_post_id FOREIGN KEY (post_id) REFERENCES posts (id) ON DELETE NO ACTION;
                |]
                let
                    isTargetTable :: Text -> Statement -> Bool
                    isTargetTable targetName (StatementCreateTable CreateTable { name }) = name == targetName
                    isTargetTable _ _ = False
                let (Just postStatement) = find (isTargetTable "posts") statements
                let compileOutput = compileStatementPreviewWith simpleOptions statements postStatement |> Text.strip

                -- data Post' has no type parameters, no QueryBuilder field, and userId has concrete type
                compileOutput `shouldBe` [trimming|
                    data Post'  = Post {id :: (Id' "posts"), title :: Text, userId :: (Id' "users"), meta :: MetaBag} deriving (Eq, Show)

                    type instance PrimaryKey "posts" = UUID

                    type Post = Post' 

                    type instance GetTableName (Post' ) = "posts"
                    type instance GetModelByTableName "posts" = Post

                    instance Default (Id' "posts") where def = Id def

                    instance () => IHP.ModelSupport.Table (Post' ) where
                        tableName = "posts"
                        tableNameByteString = Data.Text.Encoding.encodeUtf8 "posts"
                        columnNames = ["id","title","user_id"]
                        primaryKeyColumnNames = ["id"]
                        primaryKeyConditionForId (Id (id)) = toField id
                        {-# INLINABLE primaryKeyConditionForId #-}


                    instance InputValue Generated.ActualTypes.Post where inputValue = IHP.ModelSupport.recordToInputValue


                    instance FromRow Generated.ActualTypes.Post where
                        fromRow = do
                            id <- field
                            title <- field
                            userId <- field
                            let theRecord = Generated.ActualTypes.Post id title userId def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }
                            pure theRecord


                    type instance GetModelName (Post' ) = "Post"

                    instance CanCreate Generated.ActualTypes.Post where
                        create :: (?modelContext :: ModelContext) => Generated.ActualTypes.Post -> IO Generated.ActualTypes.Post
                        create model = do
                            sqlQuerySingleRow "INSERT INTO posts (id, title, user_id) VALUES (?, ?, ?) RETURNING id, title, user_id" ((fieldWithDefault #id model, model.title, model.userId))
                        createMany [] = pure []
                        createMany models = do
                            sqlQuery (Query $ "INSERT INTO posts (id, title, user_id) VALUES " <> (ByteString.intercalate ", " (List.map (\_ -> "(?, ?, ?)") models)) <> " RETURNING id, title, user_id") (List.concat $ List.map (\model -> [toField (fieldWithDefault #id model), toField (model.title), toField (model.userId)]) models)
                        createRecordDiscardResult :: (?modelContext :: ModelContext) => Generated.ActualTypes.Post -> IO ()
                        createRecordDiscardResult model = do
                            sqlExecDiscardResult "INSERT INTO posts (id, title, user_id) VALUES (?, ?, ?)" ((fieldWithDefault #id model, model.title, model.userId))

                    instance CanUpdate Generated.ActualTypes.Post where
                        updateRecord model = do
                            sqlQuerySingleRow "UPDATE posts SET id = ?, title = ?, user_id = ? WHERE id = ? RETURNING id, title, user_id" ((fieldWithUpdate #id model, fieldWithUpdate #title model, fieldWithUpdate #userId model, model.id))
                        updateRecordDiscardResult model = do
                            sqlExecDiscardResult "UPDATE posts SET id = ?, title = ?, user_id = ? WHERE id = ?" ((fieldWithUpdate #id model, fieldWithUpdate #title model, fieldWithUpdate #userId model, model.id))

                    instance Record Generated.ActualTypes.Post where
                        {-# INLINE newRecord #-}
                        newRecord = Generated.ActualTypes.Post def def def  def


                    instance QueryBuilder.FilterPrimaryKey "posts" where
                        filterWhereId id builder =
                            builder |> QueryBuilder.filterWhere (#id, id)
                        {-# INLINE filterWhereId #-}
                |]
            it "should produce no type parameters for a table that is referenced by other tables" do
                let statements = parseSqlStatements [trimming|
                    CREATE TABLE users (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL
                    );
                    CREATE TABLE posts (
                        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                        user_id UUID NOT NULL
                    );
                    ALTER TABLE posts ADD CONSTRAINT posts_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
                |]
                let
                    isTargetTable :: Text -> Statement -> Bool
                    isTargetTable targetName (StatementCreateTable CreateTable { name }) = name == targetName
                    isTargetTable _ _ = False
                let (Just userStatement) = find (isTargetTable "users") statements
                let compileOutput = compileStatementPreviewWith simpleOptions statements userStatement |> Text.strip

                -- User has no has-many posts field, no type parameters
                getInstanceDecl "Record" compileOutput `shouldBe` [trimming|
                    instance Record Generated.ActualTypes.User where
                        {-# INLINE newRecord #-}
                        newRecord = Generated.ActualTypes.User def  def
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
