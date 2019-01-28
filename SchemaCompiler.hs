module Foundation.SchemaCompiler where
import ClassyPrelude
import ClassyPrelude
import Data.String.Conversions (cs)
import Model.Schema (database)
import Foundation.SchemaSupport
import Foundation.NameSupport (tableNameToModelName, columnNameToFieldName, pluralToSingular)
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified System.Directory as Directory
import qualified Data.Set
import Data.List ((!!), (\\))
import Data.List.Split
import qualified Foundation.SqlCompiler


-- USE LINE PRAGMA IN OUTPUT
--{-# LINE 42 "Foo.vhs" #-}

c = compile
main = compile
compile :: IO ()
compile = do
    let validationErrors = validate database
    if validationErrors /= [] then
            error $ "Schema.hs contains errors: " <> cs (unsafeHead validationErrors)
        else
            return ()
    let compiled = map (\table -> (getFilePath table, compileTable table)) database
    let compiledStubs = map (\table -> (getStubFilePath table, compileStub table)) database
    mapM_ writeTable compiled
    mapM_ writeStub compiledStubs
    writeTable (getTypesFilePath, compileTypes database)
    Foundation.SqlCompiler.main


writeTable :: (FilePath, Text) -> IO ()
writeTable (path, content) = do
    alreadyExists <- Directory.doesFileExist path
    existingContent <- if alreadyExists then readFile path else return ""
    when (existingContent /= cs content) $ do
        putStrLn $ "Updating " <> cs path
        writeFile (cs path) (cs content)

writeStub :: (FilePath, Text) -> IO ()
writeStub (path, content) = do
    stubExists <- Directory.doesFileExist (cs path)
    if not stubExists then writeFile (cs path) (cs content) else return ()

section = "\n"
compileTable table@(Table name attributes) =
    "-- This file is auto generated and will be overriden regulary. Please edit `src/Model/" <> tableNameToModelName name <> ".hs` to customize the Model"
    <> section
    <> "{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, TypeFamilies, DataKinds, TypeOperators, UndecidableInstances, TypeApplications  #-}"
    <> section
    <> "module Model.Generated." <> tableNameToModelName name <> " where\n\n"
    <> "import Foundation.HaskellSupport\n"
    <> "import Foundation.ModelSupport\n"
    <> "import ClassyPrelude hiding (id, const) \n"
    <> "import qualified Data.Function\n"
    <> "import Database.PostgreSQL.Simple\n"
    <> "import Database.PostgreSQL.Simple.FromRow\n"
    <> "import Database.PostgreSQL.Simple.FromField hiding (Field, name)\n"
    <> "import Database.PostgreSQL.Simple.ToField hiding (Field)\n"
    <> "import Foundation.Controller.Param (ParamName (..))\n"
    <> "import qualified Data.Function\n"
    <> "import Model.Generated.Types\n"
    <> "import Database.PostgreSQL.Simple.Types (Query (Query))\n"
    <> "import GHC.TypeLits\n"
    <> "import Data.Default (def)\n"
    <> "import Foundation.ValidationSupport\n"
    <> "import Data.UUID (UUID)\n"
    <> "import qualified Foundation.GeneratedModelSupport\n"
    <> "import GHC.OverloadedLabels\n"
    <> "import Data.Default\n"
    <> "import GHC.Records (getField)\n"
    <> "import qualified Data.Proxy\n"
    <> "import Foundation.QueryBuilder\n"
    <> section
    <> compileCreate table
    <> section
    <> compileUpdate table
    <> section
    <> compileUnit table
    <> section
    <> compileBuild table
    <> section
    <> section
    <> section
    <> compileIdentity table
    <> section
    <> compileCombine table
    <> section
    <> section
    <> compileErrorHints table
    <> section
    <> section
    <> compileConst table
    <> section
    <> compileReadParams table
    <> section

compileTypes :: [Table] -> Text
compileTypes database =
        prelude
        <> "\n\n"
        <> intercalate "\n\n" (map compileGenericDataDefinition database)
        <> intercalate "\n\n" (map compileTypes' database)
        <> section
    where
        prelude = "-- This file is auto generated and will be overriden regulary. Please edit `src/Model/Schema.hs` to customize the Types"
                  <> section
                  <> "{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, TypeFamilies, DataKinds, TypeOperators, UndecidableInstances, ConstraintKinds, ImpredicativeTypes  #-}"
                  <> section
                  <> "module Model.Generated.Types where\n\n"
                  <> "import Foundation.HaskellSupport\n"
                  <> "import Foundation.ModelSupport\n"
                  <> "import Foundation.ModelSupport\n"
                  <> "import ClassyPrelude hiding (id) \n"
                  <> "import Database.PostgreSQL.Simple\n"
                  <> "import Database.PostgreSQL.Simple.FromRow\n"
                  <> "import Database.PostgreSQL.Simple.FromField hiding (Field, name)\n"
                  <> "import Database.PostgreSQL.Simple.ToField hiding (Field)\n"
                  <> "import Foundation.Controller.Param (ParamName (..))\n"
                  <> "import qualified Data.Function\n"
                  <> "import GHC.TypeLits\n"
                  <> "import Data.UUID (UUID)\n"
                  <> "import Data.Default\n"
                  <> "import qualified Foundation.QueryBuilder as QueryBuilder\n"
                  <> "import Foundation.UrlGeneratorSupport (UrlArgument (..))\n"
                  <> "import qualified Data.Proxy\n"
                  <> "import GHC.Records\n"
                  <> "import qualified Foundation.ValidationSupport"

compileTypes' table@(Table name attributes) =
    "-- Types for " <> cs name <> "\n\n"
    <> compileTypeAlias table
    <> compileNewTypeAlias table
    <> compileNewOrSavedTypeAlias table
    <> compileEnumDataDefinitions table
    <> section
    <> compileFromRowInstance table
    <> section
    <> compileIsNewInstance table
    <> section
    <> compileHasModelNameInstance table
    <> compileHasTableNameInstance table
    <> section
    <> compileIdNewType table
    <> section
    <> compileModelFieldValueTypeInstances table
    <> section
    <> compileInclude table
    <> compileCanValidate2 table
    <> section
    <> compileColumnNames table


compileStub table@(Table name attributes) =
    "module Model." <> tableNameToModelName name <> " (module Model.Generated." <> tableNameToModelName name <> ") where\n\n"
    <> "import Foundation.ModelPrelude\n"
    <> "import Model.Generated." <> tableNameToModelName name <> "\n"
    <> section
    <> "instance ValidateRecord New" <> tableNameToModelName name <> " ControllerContext where\n"
    <> "    validateRecord2 = validateRecord $ do\n"
    <> "        validateNothing\n"


getFilePath :: Table -> FilePath
getFilePath (Table name attributes) = "src/Model/Generated/" <> (cs $ tableNameToModelName name) <> ".hs"

getStubFilePath :: Table -> FilePath
getStubFilePath (Table name attributes) = "src/Model/" <> (cs $ tableNameToModelName name) <> ".hs"

getTypesFilePath :: FilePath
getTypesFilePath = "src/Model/Generated/Types.hs"

getValidatorsFilePath :: FilePath
getValidatorsFilePath = "src/Model/Generated/Validators.hs"

compileDataDefinition :: Table -> Text
compileDataDefinition table@(Table name attributes) =
    "data " <> tableNameToModelName name <> " = " <> tableNameToModelName name <> " { " <> compileFields attributes <> " }\n"
        where
            compileFields :: [Attribute] -> Text
            compileFields attributes = intercalate ", " $ map compileField attributes
            compileField :: Attribute -> Text
            compileField (Field fieldName fieldType) = columnNameToFieldName fieldName <> " :: " <> haskellType table (columnNameToFieldName fieldName) fieldType

haskellType :: Table -> Text -> FieldType -> Text
haskellType table fieldName field =
    let
        atomicType = 
            case field of
                SerialField {} -> "Int"
                TextField {}   -> "Text"
                IntField {}    -> "Int"
                EnumField {}   -> tableNameToModelName fieldName
                BoolField {}   -> "Bool"
                Timestamp {}   -> "UTCTime"
                UUIDField {}   -> "UUID"
        actualType =
            if isPrimaryKey field
                then primaryKeyTypeName table
                else
                    if isJust (references field)
                        then primaryKeyTypeName' (fromJust (references field))
                        else atomicType
    in if allowNull field then "(Maybe " <> actualType <> ")" else actualType


compileTypeAlias :: Table -> Text
compileTypeAlias table@(Table name attributes) =
		"type " <> tableNameToModelName name <> " = " <> tableNameToModelName name <> "' " <> compileFields attributes <> "\n"
    where
        compileFields :: [Attribute] -> Text
        compileFields attributes = intercalate " " $ map compileField attributes
        compileField :: Attribute -> Text
        compileField (Field fieldName fieldType) = haskellType table fieldName fieldType
        compileField (HasMany name inverseOf) = "(QueryBuilder.QueryBuilder " <> compileInverseOf inverseOf <> ")"
            where
                compileInverseOf Nothing = tableNameToModelName name
                compileInverseOf (Just name) = tableNameToModelName name


compileNewTypeAlias :: Table -> Text
compileNewTypeAlias table@(Table name attributes) =
		"type New" <> tableNameToModelName name <> " = " <> tableNameToModelName name <> "' " <> compileFields attributes <> "\n"
		<> "type instance New " <> tableNameToModelName name <> " = " <> tableNameToModelName name <> "' " <> compileFields attributes <> "\n"
        <> "type instance GetModelById " <> tableNameToModelName name <> "Id = " <> tableNameToModelName name <> "\n"
    where
        compileFields :: [Attribute] -> Text
        compileFields attributes = intercalate " " $ map compileField attributes
        compileField :: Attribute -> Text
        compileField (Field fieldName fieldType) = haskellType' fieldName fieldType
        compileField (HasMany {}) = "()"
        haskellType' fieldName fieldType | isJust (defaultValue fieldType) = "()"
        haskellType' fieldName fieldType = haskellType table fieldName fieldType

compileModelFieldValueTypeInstances :: Table -> Text
compileModelFieldValueTypeInstances table@(Table name attributes) =
        intercalate "\n" $ map compileModelFieldValueTypeInstance $ fieldsOnly attributes
    where
        compileModelFieldValueTypeInstance :: Attribute -> Text
        compileModelFieldValueTypeInstance (Field fieldName fieldType) =
            "type instance ModelFieldValue " <> modelType <> " " <> tshow (columnNameToFieldName fieldName) <> " = " <> haskellType table fieldName fieldType
        modelType = "(" <> tableNameToModelName name <> "' " <> intercalate " " (map (const "_") attributes) <> ")"

compileNewOrSavedTypeAlias :: Table -> Text
compileNewOrSavedTypeAlias table@(Table name attributes) =
		"type NewOrSaved" <> tableNameToModelName name <> " = forall " <> (intercalate " " getAttributesWithDefaultValue) <> ". " <> compileNewOrSavedType table <> "\n"
	    where
            getAttributesWithDefaultValue = map getName $ filter hasDefaultValue attributes
            getName (Field fieldName _) = fieldName
            getName (HasMany {name}) = name
            hasDefaultValue (Field fieldName fieldType) | isJust (defaultValue fieldType) = True
            hasDefaultValue (Field _ (UUIDField { references = Just _ })) = True
            hasDefaultValue (HasMany {}) = True
            hasDefaultValue _ = False

compileNewOrSavedType :: Table -> Text
compileNewOrSavedType table@(Table name attributes) =
		"" <> tableNameToModelName name <> "' " <> compileFields attributes
    where
        compileFields :: [Attribute] -> Text
        compileFields attributes = intercalate " " $ map compileField attributes
        compileField :: Attribute -> Text
        compileField (Field fieldName fieldType) = haskellType' fieldName fieldType
        compileField (HasMany {name}) = name
        haskellType' fieldName fieldType | (isJust (defaultValue fieldType)) = fieldName
        haskellType' fieldName (UUIDField { references = Just _ }) = fieldName
        haskellType' fieldName fieldType = haskellType table fieldName fieldType

compileAnyType :: Table -> Text
compileAnyType table@(Table name attributes) =
		tableNameToModelName name <> "' " <> compileFields attributes
    where
        compileFields :: [Attribute] -> Text
        compileFields attributes = intercalate " " $ map compileField attributes
        compileField :: Attribute -> Text
        compileField (Field fieldName fieldType) = haskellType' fieldName fieldType
        compileField (HasMany {name}) = name
        haskellType' fieldName fieldType = fieldName


compileIdNewType :: Table -> Text
compileIdNewType table@(Table name attributes) =
	"newtype " <> typeName <> " = " <> typeName <> " UUID deriving (Eq)\n"
	<> "instance NewTypeWrappedUUID " <> typeName <> " where unwrap (" <> typeName <> " value) = value; wrap = "<> typeName <> "\n"
	-- <> "instance HasId " <> typeName <> " where type IdType " <> typeName <> " = UUID; get #id (" <> typeName <> " value) = value\n"
	<> "instance Show " <> typeName <> " where show id = show (unwrap id)\n"
    <> "instance UrlArgument " <> typeName <> " where toText = toText . unwrap\n"
    <> "instance Default " <> typeName <> " where def = wrap def\n"
    <> "instance ToField " <> typeName <> " where toField = toField . unwrap\n"
    <> "instance IsNewId " <> typeName <> " where isNewId _ = False\n"
    <> "instance FromField " <> typeName <> " where fromField value metaData = do fieldValue <- fromField value metaData; return $ wrap fieldValue\n"
    <> "instance QueryBuilder.Fetchable " <> typeName <> " " <> tableNameToModelName name <> " where type FetchResult " <> typeName <> " " <> tableNameToModelName name <> " = " <> tableNameToModelName name <> "; fetch = QueryBuilder.genericFetchIdOne; fetchOneOrNothing = QueryBuilder.genericfetchIdOneOrNothing; fetchOne = QueryBuilder.genericFetchIdOne\n"
    <> "instance QueryBuilder.Fetchable (Maybe " <> typeName <> ") " <> tableNameToModelName name <> " where type FetchResult (Maybe " <> typeName <> ") " <> tableNameToModelName name <> " = [" <> tableNameToModelName name <> "]; fetch (Just a) = QueryBuilder.genericFetchId a; fetchOneOrNothing Nothing = return Nothing; fetchOneOrNothing (Just a) = QueryBuilder.genericfetchIdOneOrNothing a; fetchOne (Just a) = QueryBuilder.genericFetchIdOne a\n"
    <> "instance QueryBuilder.Fetchable [" <> typeName <> "] " <> tableNameToModelName name <> " where type FetchResult [" <> typeName <> "] " <> tableNameToModelName name <> " = [" <> tableNameToModelName name <> "]; fetch = QueryBuilder.genericFetchIds; fetchOneOrNothing = QueryBuilder.genericfetchIdsOneOrNothing; fetchOne = QueryBuilder.genericFetchIdsOne\n"
    where typeName = primaryKeyTypeName table

primaryKeyTypeName :: Table -> Text
primaryKeyTypeName (Table name _) = primaryKeyTypeName' name

primaryKeyTypeName' :: Text -> Text
primaryKeyTypeName' name = tableNameToModelName name <> "Id"

defaultDerivingClause :: Text
defaultDerivingClause = "deriving (Eq, Show, Generic)"

compileGenericDataDefinition :: Table -> Text
compileGenericDataDefinition table@(Table name attributes) =
		"data " <> tableNameToModelName name <> "' " <> params <> " = " <> tableNameToModelName name <> " { " <> compileFields attributes <> " } " <> defaultDerivingClause <> " \n"
    where
        params :: Text
        params = intercalate " " allParameters
        allParameters :: [Text]
        allParameters = take (ClassyPrelude.length attributes) (map (\n -> ("p" <> (cs $ show n))) [0..])
        compileFields :: [Attribute] -> Text
        compileFields attributes = intercalate ", " $ map compileField (zip attributes [0..])
        compileField :: (Attribute, Int) -> Text
        compileField (attribute, n) = columnNameToFieldName fieldName <> " :: " <> (allParameters !! n)
            where
                fieldName =
                    case attribute of
                        Field fieldName _ -> fieldName
                        HasMany {name} -> name

compileEnumDataDefinitions :: Table -> Text
compileEnumDataDefinitions table@(Table name attributes) =
        (intercalate "\n" (map compileEnumField enumFields))
        <> section
        <> section
        <> (intercalate "\n" (map compileFromFieldInstance enumFields))
        <> section
        <> section
        <> (intercalate "\n" (map compileToFieldInstance enumFields))
        <> section
        <> section
        <> (intercalate "\n" (map compileInputValueInstance enumFields))
        <> section
        <> (intercalate "\n" (map compileDefaultInstance enumFields))
        <> section
    where
        isEnumField (Field _ (EnumField {})) = True
        isEnumField _ = False
        enumFields = filter isEnumField attributes
        compileEnumField (Field fieldName (EnumField {values})) = "data " <> tableNameToModelName fieldName <> " = " <> (intercalate " | " (map tableNameToModelName values)) <> " deriving (Eq, Show)"
        compileFromFieldInstance (Field fieldName (EnumField {values})) = "instance FromField " <> tableNameToModelName fieldName <> " where\n" <> indent (intercalate "\n" ((map compileFromFieldInstanceForValue values) <> [compileFromFieldInstanceForError, compileFromFieldInstanceForNull]))
        compileFromFieldInstanceForValue value = "fromField field (Just " <> tshow value <> ") = return " <> tableNameToModelName value
        compileFromFieldInstanceForError = "fromField field (Just value) = returnError ConversionFailed field \"Unexpected value for enum value\""
        compileFromFieldInstanceForNull = "fromField field Nothing = returnError UnexpectedNull field \"Unexpected null for enum value\""
        compileDefaultInstance (Field fieldName (EnumField {values})) = "instance Default " <> tableNameToModelName fieldName <> " where def = " <> tableNameToModelName (unsafeHead values) <> "\n"

        compileToFieldInstance (Field fieldName (EnumField { values })) = "instance ToField " <> tableNameToModelName fieldName <> " where\n" <> indent (intercalate "\n" (map compileToFieldInstanceForValue values))
        compileToFieldInstanceForValue value = "toField " <> tableNameToModelName value <> " = toField (" <> tshow value <> " :: Text)"

        compileInputValueInstance (Field fieldName (EnumField { values })) = "instance InputValue " <> tableNameToModelName fieldName <> " where\n" <> indent (intercalate "\n" (map compileInputValue values))
        compileInputValue value = "inputValue " <> tableNameToModelName value <> " = " <> tshow value <> " :: Text"

compileToRowValues bindingValues = if (ClassyPrelude.length bindingValues == 1) then "Only (" <> (unsafeHead bindingValues) <> ")" else "(" <> intercalate ") :. (" (map (\list -> if ClassyPrelude.length list == 1 then "Only (" <> (unsafeHead list) <> ")" else intercalate ", " list) (chunksOf 8 bindingValues)) <> ")"


fieldNames :: [Attribute] -> [Text]
fieldNames = map (\(Field fieldName _) -> fieldName) . fieldsOnly

compileCreate table@(Table name attributes) =
    let
        modelName = tableNameToModelName name
        columns = intercalate ", " $ fieldNames attributes
        values = intercalate ", " $ map toValue $ fieldsOnly attributes
        toValue (Field fieldName fieldType) =
            case defaultValue fieldType of
                Just (SqlDefaultValue sql) -> sql
                otherwise   -> "?"

        toBinding modelName (Field fieldName fieldType) =
            case defaultValue fieldType of
                Just (SqlDefaultValue _) -> Nothing
                otherwise   -> Just ("let " <> modelName <> "{" <> columnNameToFieldName fieldName <> "} = model in " <> columnNameToFieldName fieldName)
        bindings :: [Text]
        bindings = let bindingValues = map fromJust $ filter isJust (map (toBinding modelName) $ fieldsOnly attributes) in bindingValues
    in
        "instance CanCreate New" <> modelName <> " where\n"
        <> indent (
            "create :: (?modelContext :: ModelContext) => New" <> modelName <> " -> IO " <> modelName <> "\n"
                <> "type Created New" <> modelName <> " = " <> modelName <> "\n"
                <> "create model = do\n"
                <> indent ("let (ModelContext conn) = ?modelContext\n"
                    <> "result <- Database.PostgreSQL.Simple.query conn \"INSERT INTO " <> name <> " (" <> columns <> ") VALUES (" <> values <> ") RETURNING *\" (" <> compileToRowValues bindings <> ")\n"
                    <> "return (unsafeHead result)\n"
                    )
                <> "createMany models = do\n"
                <> indent ("let (ModelContext conn) = ?modelContext\n"
                    <> "Database.PostgreSQL.Simple.query conn (Query $ \"INSERT INTO " <> name <> " (" <> columns <> ") VALUES \" <> (intercalate \", \" (map (\\_ -> \"(" <> values <> ")\") models)) <> \" RETURNING *\") (concat $ map (\\model -> [" <> (intercalate ", " (map (\b -> "toField (" <> b <> ")") bindings)) <> "]) models)\n"
                    )
            )

toBinding modelName attribute = Just ("let " <> modelName <> "{" <> columnNameToFieldName fieldName <> "} = model in " <> columnNameToFieldName fieldName)
    where
        fieldName =
            case attribute of
                Field fieldName _ -> fieldName
                HasMany {name} -> name

compileUpdate table@(Table name attributes) =
    let
        modelName = tableNameToModelName name
        columns = intercalate ", " $ map toColumn attributes
        toColumn (Field fieldName fieldType) = fieldName
        values = intercalate ", " $ map toValue attributes
        toValue (Field fieldName fieldType) =
            case defaultValue fieldType of
                Just (SqlDefaultValue sql) -> sql
                Nothing -> "?"
        bindings :: Text
        bindings =
            let
                bindingValues = (map fromJust $ filter isJust (map (toBinding modelName) $ fieldsOnly attributes)) <> (["getField @\"id\" model"])
            in
                compileToRowValues bindingValues

        updates = intercalate ", " (map fromJust $ filter isJust $ map update $ fieldsOnly attributes)
        update (Field fieldName fieldType) =
            case fieldType of
                SerialField {} -> Nothing
                otherwise -> Just $ fieldName <> " = ?"
    in
        "instance CanUpdate " <> modelName <> " where\n"
        <> indent ("updateRecord model = do\n"
                <> indent ("let (ModelContext conn) = ?modelContext\n"
                    <> "result <- Database.PostgreSQL.Simple.query conn \"UPDATE " <> name <> " SET " <> updates <> " WHERE id = ? RETURNING *\" (" <> bindings <> ")\n"
                    <> "return (unsafeHead result)\n"
                )
            )

compileFromRowInstance table@(Table name attributes) =
    defaultFromRow
    -- <> "instance FromRow " <> tableNameToModelName name <> " where "<> (indent "fromRow = " <> tableNameToModelName name <> " <$> " <>  (intercalate " <*> " $ map (const "field") $ fieldsOnly attributes))
    where
        defaultFromRow =
            "instance FromRow " <> tableNameToModelName name <> " where "
            <> ("fromRow = do model <- " <> modelConstructor <> " <$> " <>  (intercalate " <*> " $ map (const "field") $ fieldsOnly attributes)) <> "; return " <> tableNameToModelName name <> " { " <> intercalate ", " (map compileQuery attributes) <> " }\n"
        modelConstructor = "(\\" <> intercalate " " (map compileParam $ fieldsOnly attributes) <> " -> " <> tableNameToModelName name <> " " <> intercalate " " (map compileValue attributes) <> ")"
        compileParam (Field fieldName _) = fieldName
        compileParam (HasMany {name}) = name
        compileValue field@(Field _ _) = compileParam field
        compileValue (HasMany {}) = "()"

        compileQuery field@(Field fieldName _) = columnNameToFieldName fieldName <> " = (" <> (fromJust $ toBinding (tableNameToModelName name) field) <> ")"
        compileQuery (HasMany hasManyName inverseOf) = columnNameToFieldName hasManyName <> " = (QueryBuilder.filterWhere (Data.Proxy.Proxy @" <> tshow (compileInverseOf inverseOf <> "Id") <> ", " <> (fromJust $ toBinding (tableNameToModelName name) (Field "id" (UUIDField {})) ) <> ") (QueryBuilder.query @" <> tableNameToModelName hasManyName <>"))"
            where
                compileInverseOf Nothing = columnNameToFieldName (pluralToSingular name)
                compileInverseOf (Just name) = columnNameToFieldName (pluralToSingular name)

compileUnit :: Table -> Text
compileUnit table@(Table name attributes) =
        "unit :: value -> " <> tableNameToModelName name <> "' " <> compileFields attributes <>  "\n"
		<> "unit value = " <> tableNameToModelName name <> " " <> compileFields attributes <> "\n"
    where
		compileFields :: [Attribute] -> Text
		compileFields attributes = intercalate " " $ map compileField attributes
		compileField :: Attribute -> Text
		compileField _ = "value"

compileBuild :: Table -> Text
compileBuild table@(Table name attributes) =
        "build :: " <> tableNameToModelName name <> "' " <> compileTypes attributes <> "\n"
		<> "build = " <> tableNameToModelName name <> " " <> compileFields attributes <> "\n"
    where
        compileFields :: [Attribute] -> Text
        compileFields attributes = intercalate " " $ map compileField attributes
        compileField :: Attribute -> Text
        compileField (HasMany {}) = "()"
        compileField (Field {}) = "()"

        compileTypes :: [Attribute] -> Text
        compileTypes attributes = intercalate " " $ map compileType attributes
        compileType :: Attribute -> Text
        compileType (HasMany {name}) = "()"
        compileType (Field {}) = "()"

compileIdentity :: Table -> Text
compileIdentity table@(Table name attributes) =
		"identity = " <> tableNameToModelName name <> " " <> compileFields attributes <> "\n"
    where
		compileFields :: [Attribute] -> Text
		compileFields attributes = intercalate " " $ map compileField attributes
		compileField :: Attribute -> Text
		compileField _ = "Data.Function.id"

compileCombine table@(Table tableName attributes) =
        "combine (" <> tableNameToModelName tableName <> " " <> (intercalate " " (attributesToArgs "arg" attributes)) <> ") (" <> tableNameToModelName tableName <> " " <> (intercalate " " (attributesToArgs "f" attributes)) <> ") = " <> tableNameToModelName tableName <> " " <> (intercalate " " (attributesToApplications attributes))
    where
        attributesToArgs :: Text -> [Attribute] -> [Text]
        attributesToArgs prefix attributes = map (\n -> prefix <> tshow n) $ (map snd (zip attributes [0..]))
        attributesToApplications attributes = map (\n -> "(f" <> tshow n <> " arg" <> tshow n <> ") ") $ (map snd (zip attributes [0..]))

compileConst table@(Table tableName attributes) =
        "const :: " <> typeSignature <> "\n"
        <> "const model = model { " <> intercalate ", " (map compileField attributes) <> " }\n"
        <> "buildConst = const build\n"
    where
        compileField field = columnNameToFieldName fieldName <> " = (Data.Function.const (" <> fromJust (toBinding (tableNameToModelName tableName) field) <> "))"
            where
                fieldName =
                    case field of
                        Field fieldName _ -> fieldName
                        HasMany {name} -> name
        typeSignature = tableNameToModelName tableName <> "' " <> (intercalate " " $ map (\i -> "p" <> tshow i) typeArgumentNumbers) <> " -> " <> tableNameToModelName tableName <> "' " <> (intercalate " " $ map (\i -> "(p" <> tshow i <> "' -> " <> "p" <> tshow i <> ")") typeArgumentNumbers)
        typeArgumentNumbers :: [Int]
        typeArgumentNumbers = (map snd (zip attributes [1..]))

compileErrorHints table@(Table tableName attributes) =
        intercalate "\n" (mkUniq $ map compileErrorHintForAttribute $ fieldsOnly attributesWithoutDefaultValues)
    where
        attributesWithoutDefaultValues = filter (not . hasDefaultValue) attributes
        hasDefaultValue (Field _ fieldValue) | isJust (defaultValue fieldValue) = True
        hasDefaultValue (Field _ _) = False
        hasDefaultValue (HasMany {}) = False
        compileArgument currentAttribute attribute =
            if currentAttribute == attribute
                then "()"
                else case attribute of
                    Field name _ -> name
                    HasMany {name} -> name
        compileArguments attributes currentAttribute = map (compileArgument currentAttribute) attributes

        compileErrorHintForAttribute :: Attribute -> Text
        compileErrorHintForAttribute attribute =
            let
                arguments :: Text
                arguments = intercalate " " (compileArguments attributes attribute)
                name = case attribute of Field name _ -> name; HasMany {name} -> name
            in
                "instance TypeError (GHC.TypeLits.Text \"Parameter `" <> name <> "` is missing\" ':$$: 'GHC.TypeLits.Text \"Add something like `" <> name <> " = ...`\") => (Foundation.ModelSupport.CanCreate (" <> ((tableNameToModelName tableName) :: Text) <> "' " <> arguments <> ")) where type Created (" <> ((tableNameToModelName tableName) :: Text) <> "' " <> arguments <> ") = (); create = error \"Unreachable\";"



compileCanValidate2 :: Table -> Text
compileCanValidate2 table@(Table name attributes) =
        ""
        <> "type instance Foundation.ValidationSupport.ValidatorResultFor (" <> compileNewOrSavedType table <> ") = " <> compileValidatorResultType <> "\n"
        <> "instance Default (" <> compileValidatorResultType <> ") where def = " <> compileValidatorResultConstructor <> "\n"
    where
        compileValidatorResultType = tableNameToModelName name <> "' " <> intercalate " " (map (const "Foundation.ValidationSupport.ValidatorResult") attributes)
        compileValidatorResultConstructor = tableNameToModelName name <> " " <> intercalate " " (map (const "Foundation.ValidationSupport.Success") attributes)

compileIsNewInstance table@(Table name attributes) =
    "instance IsNewId id => IsNew (" <> compileAnyType table <> ") where isNew (" <> tableNameToModelName name <> " { id }) = isNewId id\n"

compileHasModelNameInstance table@(Table name attributes) = "instance HasModelName (" <> compileAnyType table <> ") where getModelName _ = " <> tshow (tableNameToModelName name) <> "\n"
compileHasTableNameInstance table@(Table name attributes) = "instance HasTableName (" <> compileNewOrSavedType table <> ") where getTableName _ = " <> tshow name <> "\n"
        <> "\ntype instance GetTableName (" <> tableNameToModelName name <> "' " <> getTableNameTypeArgs <> " ) = " <> tshow name <> "\n"
    where
        getTableNameTypeArgs :: Text
        getTableNameTypeArgs = intercalate " " $ map toArg attributes
        toArg (Field fieldName _) = fieldName
        toArg (HasMany {name}) =  name

compileReadParams :: Table -> Text
compileReadParams (Table tableName _) = "readParams = combine (Foundation.ModelSupport.columnNames (Data.Proxy.Proxy @" <> tableNameToModelName tableName <> "))\n"

compileColumnNames table@(Table tableName attributes) = "instance ColumnNames " <> instanceHead <> " where " <> typeDef <> "; columnNames _ = " <> tableNameToModelName tableName <> " " <> compiledFields
    where
        instanceHead = "(" <> tableNameToModelName tableName <> "' " <> intercalate " " (map toName attributes) <> ")"
            where
                toName (Field fieldName _) = fieldName
                toName (HasMany {name}) = name
        typeDef = "type ColumnNamesRecord " <> instanceHead <> " = " <> tableNameToModelName tableName <> "' " <> intercalate " " (map (const "ByteString") attributes)
        compiledFields = intercalate " " (map compileField attributes)
        compileField (Field fieldName _) = "(" <>tshow fieldName <> " :: ByteString)"
        compileField (HasMany {name}) = "(" <>tshow name <> " :: ByteString)"

compileInclude table@(Table tableName attributes) = intercalate "\n" $ map compileInclude' attributes
    where
        compileInclude' :: Attribute -> Text
        compileInclude' attribute = "type instance Include " <> tshow (columnNameToFieldName fieldName) <> " (" <> leftModelType <> ") = " <> rightModelType <> "\n"
            where
                leftModelType :: Text
                leftModelType = intercalate " " $ (tableNameToModelName tableName <> "'"):(map compileTypeVariable attributes)
                rightModelType :: Text
                rightModelType = intercalate " " $ (tableNameToModelName tableName <> "'"):(map compileTypeVariable' attributes)
                compileTypeVariable :: Attribute -> Text
                compileTypeVariable (Field fieldName _) = fieldName
                compileTypeVariable (HasMany {name}) = name
                compileTypeVariable' :: Attribute -> Text
                compileTypeVariable' (Field fieldName' _) | fieldName' == fieldName = "(GetModelById (ModelFieldValue (" <> leftModelType <> ") " <> tshow (columnNameToFieldName fieldName) <> "))"
                compileTypeVariable' (HasMany {name}) | name == fieldName = "[" <> tableNameToModelName (pluralToSingular name) <> "]"
                compileTypeVariable' otherwise = compileTypeVariable otherwise
                fieldName =
                    case attribute of
                        (Field fieldName _) -> fieldName
                        (HasMany {name}) -> name 


--compileAttributeBag :: Table -> Text
--compileAttributeBag table@(Table name attributes) = "class To" <> tableNameToModelName name <> "Attributes where\n    to"
indent :: Text -> Text
indent code =
        intercalate "\n" $ map indentLine $ Text.splitOn "\n" code
    where
        indentLine ""   = ""
        indentLine line = "    " <> line


mkUniq :: Ord a => [a] -> [a]
mkUniq = Data.Set.toList . Data.Set.fromList
