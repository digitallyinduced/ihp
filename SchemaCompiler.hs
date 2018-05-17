module Foundation.SchemaCompiler where
import ClassyPrelude
import ClassyPrelude
import Data.String.Conversions (cs)
import Model.Schema (database)
import Foundation.SchemaSupport
import Foundation.NameSupport (tableNameToModelName, columnNameToFieldName)
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
    writeTable (getValidatorsFilePath, compileValidators database)
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
    <> section
    <> compileCreate table
    <> section
    <> compileUpdate table
    <> section
    <> compileFindAll table
    <> section
    <> compileUnit table
    <> section
    <> compileBuild table
    <> section
    <> compileAttributeNames table
    <> section
    -- <> compileAssign table
    <> section
    <> compileIdentity table
    <> section
    <> compileCombine table
    <> section
    <> section
    <> compileErrorHints table
    <> section
    <> compileCanFilterInstance table
    <> section
    <> compileFieldModel table
    <> section
    <> compileBuildValidator table
    <> section
    <> compileColumnNames table
    <> section
    <> compileConst table
    <> section
    <> compileReadParams table
    <> section

compileTypes :: [Table] -> Text
compileTypes database = prelude <> "\n\n" <> intercalate "\n\n" (map compileTypes' database) <> section <> compileHasInstances database
    where
        prelude = "-- This file is auto generated and will be overriden regulary. Please edit `src/Model/Schema.hs` to customize the Types"
                  <> section
                  <> "{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, TypeFamilies, DataKinds, TypeOperators, UndecidableInstances, ConstraintKinds  #-}"
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

compileTypes' table@(Table name attributes) =
    "-- Types for " <> cs name <> "\n\n"
    <> compileGenericDataDefinition table
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


compileValidators :: [Table] -> Text
compileValidators database = prelude <> "\n\n" <> intercalate "\n\n" (map compileCanValidate database)
    where
        prelude = "-- This file is auto generated and will be overriden regulary."
                  <> section
                  <> "{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, TypeFamilies, DataKinds, TypeOperators, UndecidableInstances, ConstraintKinds, ScopedTypeVariables, TypeFamilies  #-}"
                  <> section
                  <> "module Model.Generated.Validators where\n\n"
                  <> "import Foundation.ValidationSupport\n\n"
                  <> "import Model.Generated.Types\n\n"
                  <> "import ClassyPrelude\n\n"
                  <> "import Data.UUID (UUID)\n"
                  <> "import GHC.Records\n"
                  <> "import Data.Proxy\n"
                  <> "import GHC.TypeLits (symbolVal, KnownSymbol)\n"
                  <> "import Foundation.ModelSupport (ModelFieldType)\n"
                  <> intercalate "\n" (map (\(Table name attributes) -> "import qualified Model." <> tableNameToModelName name <> " (validator, combine, fields, Field(..))\n") database)



compileStub table@(Table name attributes) =
    "module Model." <> tableNameToModelName name <> " (module Model.Generated." <> tableNameToModelName name <> ", validator) where\n\n"
    <> "import Foundation.ModelPrelude\n"
    <> "import Model.Generated." <> tableNameToModelName name <> "\n"
    <> section
    <> "validator = buildValidator"

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
        actualType =
            case field of
                SerialField {} -> "Int"
                TextField {}   -> "Text"
                IntField {}    -> "Int"
                EnumField {}   -> tableNameToModelName fieldName
                BoolField {}   -> "Bool"
                Timestamp {}   -> "UTCTime"
                UUIDField {isPrimaryKey, references}   ->
                    if isPrimaryKey
                        then primaryKeyTypeName table
                        else
                            if isJust references
                                then primaryKeyTypeName' (fromJust references)
                                else "UUID"
    in if allowNull field then "(Maybe " <> actualType <> ")" else actualType


compileTypeAlias :: Table -> Text
compileTypeAlias table@(Table name attributes) =
		"type " <> tableNameToModelName name <> " = " <> tableNameToModelName name <> "' " <> compileFields attributes <> "\n"
    where
		compileFields :: [Attribute] -> Text
		compileFields attributes = intercalate " " $ map compileField attributes
		compileField :: Attribute -> Text
		compileField (Field fieldName fieldType) = haskellType table fieldName fieldType


compileNewTypeAlias :: Table -> Text
compileNewTypeAlias table@(Table name attributes) =
		"type New" <> tableNameToModelName name <> " = " <> tableNameToModelName name <> "' " <> compileFields attributes <> "\n"
        <> "type instance GetModelById " <> tableNameToModelName name <> "Id = " <> tableNameToModelName name <> "\n"
    where
		compileFields :: [Attribute] -> Text
		compileFields attributes = intercalate " " $ map compileField attributes
		compileField :: Attribute -> Text
		compileField (Field fieldName fieldType) = haskellType' fieldName fieldType
		haskellType' fieldName fieldType | isJust (defaultValue fieldType) = "()"
		haskellType' fieldName fieldType = haskellType table fieldName fieldType

compileModelFieldValueTypeInstances :: Table -> Text
compileModelFieldValueTypeInstances table@(Table name attributes) =
        intercalate "\n" $ map compileModelFieldValueTypeInstance attributes
    where
        compileModelFieldValueTypeInstance :: Attribute -> Text
        compileModelFieldValueTypeInstance (Field fieldName fieldType) =
            "type instance ModelFieldValue " <> modelType <> " " <> tshow (columnNameToFieldName fieldName) <> " = " <> haskellType table fieldName fieldType
        modelType = tableNameToModelName name

compileNewOrSavedTypeAlias :: Table -> Text
compileNewOrSavedTypeAlias table@(Table name attributes) =
		"type NewOrSaved" <> tableNameToModelName name <> " = forall " <> (intercalate " " getAttributesWithDefaultValue) <> ". " <> compileNewOrSavedType table <> "\n"
	    where
	        getAttributesWithDefaultValue = map (\(Field name _) -> name) $ filter hasDefaultValue attributes
	        hasDefaultValue (Field fieldName fieldType) | isJust (defaultValue fieldType) = True
	        hasDefaultValue _ = False

compileNewOrSavedType :: Table -> Text
compileNewOrSavedType table@(Table name attributes) =
		"" <> tableNameToModelName name <> "' " <> compileFields attributes
    where
		compileFields :: [Attribute] -> Text
		compileFields attributes = intercalate " " $ map compileField attributes
		compileField :: Attribute -> Text
		compileField (Field fieldName fieldType) = haskellType' fieldName fieldType
		haskellType' fieldName fieldType | isJust (defaultValue fieldType) = fieldName
		haskellType' fieldName fieldType = haskellType table fieldName fieldType

compileIdNewType :: Table -> Text
compileIdNewType table@(Table name attributes) =
	"newtype " <> typeName <> " = " <> typeName <> " UUID deriving (Eq)\n"
	<> "instance NewTypeWrappedUUID " <> typeName <> " where unwrap (" <> typeName <> " value) = value; wrap = "<> typeName <> "\n"
	<> "instance HasId " <> typeName <> " where type IdType " <> typeName <> " = UUID; getId (" <> typeName <> " value) = value\n"
	<> "instance Show " <> typeName <> " where show id = show (unwrap id)\n"
    <> "instance Default " <> typeName <> " where def = wrap def\n"
    <> "instance ToField " <> typeName <> " where toField = toField . unwrap\n"
    <> "instance FromField " <> typeName <> " where fromField value metaData = do fieldValue <- fromField value metaData; return $ wrap fieldValue\n"
    where typeName = primaryKeyTypeName table

primaryKeyTypeName :: Table -> Text
primaryKeyTypeName (Table name _) = primaryKeyTypeName' name

primaryKeyTypeName' :: Text -> Text
primaryKeyTypeName' name = tableNameToModelName name <> "Id"

defaultDerivingClause :: Text
defaultDerivingClause = "deriving (Eq, Show)"

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
        compileField (Field fieldName fieldType, n) = columnNameToFieldName fieldName <> " :: " <> (allParameters !! n)

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
    where
        isEnumField (Field _ (EnumField {})) = True
        isEnumField _ = False
        enumFields = filter isEnumField attributes
        compileEnumField (Field fieldName (EnumField {values})) = "data " <> tableNameToModelName fieldName <> " = " <> (intercalate " | " (map tableNameToModelName values)) <> " deriving (Eq, Show)"
        compileFromFieldInstance (Field fieldName (EnumField {values})) = "instance FromField " <> tableNameToModelName fieldName <> " where\n" <> indent (intercalate "\n" ((map compileFromFieldInstanceForValue values) <> [compileFromFieldInstanceForError, compileFromFieldInstanceForNull]))
        compileFromFieldInstanceForValue value = "fromField field (Just " <> tshow value <> ") = return " <> tableNameToModelName value
        compileFromFieldInstanceForError = "fromField field (Just value) = returnError ConversionFailed field \"Unexpected value for enum value\""
        compileFromFieldInstanceForNull = "fromField field Nothing = returnError UnexpectedNull field \"Unexpected null for enum value\""

        compileToFieldInstance (Field fieldName (EnumField { values })) = "instance ToField " <> tableNameToModelName fieldName <> " where\n" <> indent (intercalate "\n" (map compileToFieldInstanceForValue values))
        compileToFieldInstanceForValue value = "toField " <> tableNameToModelName value <> " = toField (" <> tshow value <> " :: Text)"

        compileInputValueInstance (Field fieldName (EnumField { values })) = "instance InputValue " <> tableNameToModelName fieldName <> " where\n" <> indent (intercalate "\n" (map compileInputValue values))
        compileInputValue value = "inputValue " <> tableNameToModelName value <> " = " <> tshow value <> " :: Text"

compileToRowValues bindingValues = if (ClassyPrelude.length bindingValues == 1) then "Only (" <> (unsafeHead bindingValues) <> ")" else "(" <> intercalate ") :. (" (map (\list -> if ClassyPrelude.length list == 1 then "Only (" <> (unsafeHead list) <> ")" else intercalate ", " list) (chunksOf 8 bindingValues)) <> ")"

compileCreate table@(Table name attributes) =
    let
        modelName = tableNameToModelName name
        columns = intercalate ", " $ map toColumn attributes
        toColumn (Field fieldName fieldType) = fieldName
        values = intercalate ", " $ map toValue attributes
        toValue (Field fieldName fieldType) =
            case defaultValue fieldType of
                Just (SqlDefaultValue sql) -> sql
                otherwise   -> "?"

        toBinding modelName (Field fieldName fieldType) =
            case defaultValue fieldType of
                Just (SqlDefaultValue _) -> Nothing
                otherwise   -> Just ("let " <> modelName <> "{" <> columnNameToFieldName fieldName <> "} = model in " <> columnNameToFieldName fieldName)
        bindings :: Text
        bindings = let bindingValues = map fromJust $ filter isJust (map (toBinding modelName) attributes) in compileToRowValues bindingValues
    in
        "instance CanCreate New" <> modelName <> " where\n"
        <> indent (
            "create :: (?modelContext :: ModelContext) => New" <> modelName <> " -> IO " <> modelName <> "\n"
                <> "type Created New" <> modelName <> " = " <> modelName <> "\n"
                <> "create model = do\n"
                <> indent ("let (ModelContext conn) = ?modelContext\n"
                    <> "result <- Database.PostgreSQL.Simple.query conn \"INSERT INTO " <> name <> " (" <> columns <> ") VALUES (" <> values <> ") RETURNING *\" (" <> bindings <> ")\n"
                    <> "return (unsafeHead result)\n"
                    )
            )

toBinding modelName (Field fieldName fieldType) =
    case fieldType of
        SerialField {} -> Nothing
        otherwise   -> Just ("let " <> modelName <> "{" <> columnNameToFieldName fieldName <> "} = model in " <> columnNameToFieldName fieldName)

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
                bindingValues = (map fromJust $ filter isJust (map (toBinding modelName) attributes)) <> (["getId model"])
            in
                compileToRowValues bindingValues

        updates = intercalate ", " (map fromJust $ filter isJust $ map update attributes)
        update (Field fieldName fieldType) =
            case fieldType of
                SerialField {} -> Nothing
                otherwise -> Just $ fieldName <> " = ?"
    in
        "update :: (?modelContext :: ModelContext) => " <> modelName <> " -> IO " <> modelName <> " \n"
        <> "update model = do\n"
        <> indent ("let (ModelContext conn) = ?modelContext\n"
            <> "result <- Database.PostgreSQL.Simple.query conn \"UPDATE " <> name <> " SET " <> updates <> " WHERE id = ? RETURNING *\" (" <> bindings <> ")\n"
            <> "return (unsafeHead result)\n"
        )

compileFindAll table@(Table name attributes) =
    let
        modelName = tableNameToModelName name
    in
        "findAll :: (?modelContext :: ModelContext) => IO [" <> modelName <> "]\n"
        <> "findAll = do\n"
        <> indent (
            "let (ModelContext conn) = ?modelContext\n"
            <> "projects <- Database.PostgreSQL.Simple.query_ conn \"SELECT * FROM " <> name <> "\"\n"
            <> "return projects\n"
        )


compileFind table@(Table name attributes) =
    let
        modelName = tableNameToModelName name
    in
        "find :: (?modelContext :: ModelContext) => " <> primaryKeyTypeName table <> " -> IO " <> modelName <> "\n"
        <> "find id = do\n"
        <> indent (
            "result <- findOrNothing id\n"
            <> "return (fromMaybe (error \"Model cannot be found \") result)"
        )

compileFromRowInstance table@(Table name attributes) =
    "instance FromRow " <> tableNameToModelName name <> " where \n"
    <> (indent "fromRow = " <> tableNameToModelName name <> " <$> " <>  (intercalate " <*> " $ map (const "field") attributes))

compileUnit :: Table -> Text
compileUnit table@(Table name attributes) =
        "unit :: value -> " <> tableNameToModelName name <> "' " <> compileFields attributes <>  "\n"
		<> "unit value = " <> tableNameToModelName name <> " " <> compileFields attributes <> "\n"
    where
		compileFields :: [Attribute] -> Text
		compileFields attributes = intercalate " " $ map compileField attributes
		compileField :: Attribute -> Text
		compileField _ = "value"

compileBuild _ = "build = unit ()\n"

compileIdentity :: Table -> Text
compileIdentity table@(Table name attributes) =
		"identity = " <> tableNameToModelName name <> " " <> compileFields attributes <> "\n"
    where
		compileFields :: [Attribute] -> Text
		compileFields attributes = intercalate " " $ map compileField attributes
		compileField :: Attribute -> Text
		compileField _ = "Data.Function.id"

compileHasId :: Table -> Text
compileHasId table@(Table name attributes) = "instance HasId " <> tableNameToModelName name <> " where getId (" <> tableNameToModelName name <> "{id}) = id\n"


compileAttributeNames table@(Table tableName attributes) =
        "data Field = " <> (intercalate " | " (map compileAttributeName attributes)) <> " deriving (Show)"
        <> section
        <> section
        <> "instance ParamName Field where \n" <> (intercalate "\n" (map compileParamName attributes)) <> "\n"
        <> "instance FormField Field where \n" <> (intercalate "\n" (map compileFormFieldName attributes))
        <> section
        <> "instance FormFieldValue Field " <> tableNameToModelName tableName <> " where \n" <> (intercalate "\n" (map compileFormFieldValue attributes))
        <> section
        <> "instance FormFieldValue Field New" <> tableNameToModelName tableName <> " where \n" <> (intercalate "\n" (map compileFormFieldValue attributes))
        <> section
        <> intercalate "\n" (map compileIsLabel attributes)
    where
        moduleNamePrefix = "Model.Generated." <> tableNameToModelName tableName <> "."
        compileAttributeName (Field name _) = tableNameToModelName name
        compileParamName (Field name _) = indent $ "paramName " <> moduleNamePrefix <> (tableNameToModelName name) <> " = \"" <> name <> "\""
        compileFormFieldName (Field name _) = indent $ "formFieldName " <> moduleNamePrefix <> (tableNameToModelName name) <> " = \"" <> name <> "\""
        compileFormFieldValue (Field name _) = indent $ "formFieldValue " <> moduleNamePrefix <> (tableNameToModelName name) <> " (" <> tableNameToModelName tableName <> " { " <> columnNameToFieldName name <> " }) = inputValue " <> columnNameToFieldName name
        compileIsLabel (Field fieldName fieldType) = "instance IsLabel " <> tshow (columnNameToFieldName fieldName) <> " Field where fromLabel = " <> moduleNamePrefix <> tableNameToModelName fieldName

compileFieldModel table@(Table tableName attributes) =
        "fields = " <> tableNameToModelName tableName <> " " <> (intercalate " " (map compileAttributeName attributes))
    where
        compileAttributeName (Field name _) = "Model.Generated." <> tableNameToModelName tableName <> "." <> tableNameToModelName name

compileAssign table@(Table tableName attributes) =
        ""
        --"assignField :: Field -> Text -> " <> tableNameToModelName tableName <> " -> " <> tableNameToModelName tableName <> "\n"
        <> "assignField field value model = case field of \n" <> intercalate "\n" (map compileAssignField attributes) <> " \n"
        <> "assign :: [(Field, Text)] -> " <> tableNameToModelName tableName <> " -> " <> tableNameToModelName tableName <> "\n"
        <> "assign = error \"unreachable\""
    where
        compileAssignField (Field name _) = "   " <>  tableNameToModelName name <> " -> (model :: " <> tableNameToModelName tableName <> ") { " <> name <> " = value }"

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
        compileField field@(Field fieldName fieldType) = columnNameToFieldName fieldName <> " = (Data.Function.const (" <> fromJust (toBinding (tableNameToModelName tableName) field) <> "))"
        typeSignature = tableNameToModelName tableName <> "' " <> (intercalate " " $ map (\i -> "p" <> tshow i) typeArgumentNumbers) <> " -> " <> tableNameToModelName tableName <> "' " <> (intercalate " " $ map (\i -> "(p" <> tshow i <> "' -> " <> "p" <> tshow i <> ")") typeArgumentNumbers)
        typeArgumentNumbers :: [Int]
        typeArgumentNumbers = (map snd (zip attributes [1..]))

compileErrorHints table@(Table tableName attributes) =
        intercalate "\n" (mkUniq $ map compileErrorHintForAttribute attributesWithoutDefaultValues)
    where
        attributesWithoutDefaultValues = filter (not . hasDefaultValue) attributes
        hasDefaultValue (Field _ fieldValue) | isJust (defaultValue fieldValue) = True
        hasDefaultValue (Field _ _) = False
        compileArgument currentAttribute attribute@(Field name _) = if currentAttribute == attribute then "()" else name
        compileArguments attributes currentAttribute = map (compileArgument currentAttribute) attributes

        compileErrorHintForAttribute :: Attribute -> Text
        compileErrorHintForAttribute attribute@(Field name _) =
            let
                arguments :: Text
                arguments = intercalate " " (compileArguments attributes attribute)
            in
                "instance TypeError (GHC.TypeLits.Text \"Parameter `" <> name <> "` is missing\" ':$$: 'GHC.TypeLits.Text \"Add something like `" <> name <> " = ...`\") => (Foundation.ModelSupport.CanCreate (" <> ((tableNameToModelName tableName) :: Text) <> "' " <> arguments <> ")) where type Created (" <> ((tableNameToModelName tableName) :: Text) <> "' " <> arguments <> ") = (); create = error \"Unreachable\";"

compileHasInstances :: [Table] -> Text
compileHasInstances tables = intercalate "\n" $ mkUniq $  concat [ (mkUniq $ concat $ map compileHasClass tables), hasIdInt:(concat $ map compileHasInstance tables) ]
    where
        allFields :: [Attribute]
        allFields = concat $ map (\(Table _ attributes) -> attributes) tables
        hasIdInt = "instance HasId UUID where type IdType UUID = UUID; getId a = a"
        compileHasClass (Table tableName tableAttributes) = map (\field -> compileHasClass' field) tableAttributes
        compileHasClass' (Field fieldName fieldType) = "class (Show (" <> tableNameToModelName fieldName <> "Type a)) => Has" <> tableNameToModelName fieldName <> " a where type " <> tableNameToModelName fieldName <> "Type a; get" <> tableNameToModelName fieldName <> " :: a -> " <> tableNameToModelName fieldName <> "Type a"
        compileHasInstance table@(Table tableName tableAttributes) = concat [ map compileHasInstance' tableAttributes, map compileHasInstanceError fieldsNotInTable ]
            where
                compileHasInstance' (Field fieldName fieldType) = "instance Has" <> tableNameToModelName fieldName <> " " <> tableNameToModelName tableName <> " where type " <> tableNameToModelName fieldName <> "Type " <> tableNameToModelName tableName <> " = " <> haskellType table fieldName fieldType <> "; get" <> tableNameToModelName fieldName <> " " <> tableNameToModelName tableName <> "{" <> columnNameToFieldName fieldName <> "} = " <> columnNameToFieldName fieldName
                compileHasInstanceError (Field fieldName fieldType) = "instance TypeError (GHC.TypeLits.Text \"" <> tableNameToModelName tableName <> " has no field `" <> columnNameToFieldName fieldName <> "`\") => Has" <> tableNameToModelName fieldName <> " " <> tableNameToModelName tableName <> " where type " <> tableNameToModelName fieldName <> "Type " <> tableNameToModelName tableName <> " = (); get" <> tableNameToModelName fieldName <> " _ = error \"unreachable\""
                fieldsNotInTable = filter (\(Field fieldName _) -> not $ hasFieldByName fieldName tableAttributes) allFields

                hasFieldByName name fields = length (filter (\(Field fieldName _) -> fieldName == name) fields) > 0

compileCanFilterInstance table@(Table tableName attributes) =
        compileCriteriaTypeAlias
        <> "instance FindWhere (" <> criteria <> ") where\n"
        <> indent (
                compileFindWhereResult table
                <> compileFindWhere table
                <> compileBuildCriteria table
            )
    where
        compileCriteriaTypeAlias = "type " <> tableNameToModelName tableName <> "Criteria = " <> criteria <> "\n"
        criteria =
                tableNameToModelName tableName <> "' " <> compileFields attributes
            where
                compileFields :: [Attribute] -> Text
                compileFields attributes = intercalate " " $ map compileField attributes
                compileField :: Attribute -> Text
                compileField (Field fieldName fieldType) = "(QueryCondition " <> haskellType table fieldName fieldType <> ")"

        compileFindWhereResult table@(Table tableName attributes) = "type FindWhereResult (" <> criteria <> ") = " <> tableNameToModelName tableName <> "\n"

        compileFindWhere :: Table -> Text
        compileFindWhere table@(Table tableName attributes) =
                "findWhere :: (?modelContext :: ModelContext) => " <> criteria <> " -> IO [FindWhereResult (" <> criteria <> ")]\n"
                <> "findWhere criteria = do\n"
                <> indent (
                        "let (ModelContext conn) = ?modelContext\n"
                        <> intercalate "\n" (map compileLetBinding (zip [1..] attributes)) <> "\n"
                        <> "Database.PostgreSQL.Simple.query conn (Query ((\"SELECT * FROM " <> tableName <> " WHERE \" <> " <> queryConditions <> ") :: ByteString)) (" <> compileToRowValues (map (\(n, _) -> "snd val" <> tshow n) (zip [1..] attributes)) <> ")\n"
                    )
            where
                modelName = tableNameToModelName tableName
                queryConditions = intercalate " <> \" AND \" <> " (map (\arg -> "fst " <> letBindingName arg) (zip [1..] attributes))
                compileLetBinding arg@(n, Field fieldName fieldType) = "let " <> letBindingName arg <> " = " <> " (let " <> modelName <> " {" <> columnNameToFieldName fieldName <> "} = criteria in toSQLCondition \"" <> fieldName <> "\" " <> columnNameToFieldName fieldName <> ") "
                letBindingName (n, Field fieldName fieldType) = "val" <> tshow  n
        compileBuildCriteria :: Table -> Text
        compileBuildCriteria table@(Table name attributes) =
                "buildCriteria = " <> tableNameToModelName name <> " " <> compileFields attributes <> "\n"
            where
                compileFields :: [Attribute] -> Text
                compileFields attributes = intercalate " " $ map compileField attributes
                compileField :: Attribute -> Text
                compileField (Field fieldName fieldType) = "(NoCondition :: QueryCondition " <> haskellType table fieldName fieldType <> ")"

compileBuildValidator :: Table -> Text
compileBuildValidator table@(Table name attributes) =
        compileTypeAlias
        <> section
        <> "buildValidator :: " <> tableNameToModelName name <> "' " <> compileTypes attributes <> "\n"
        <> "buildValidator = " <> tableNameToModelName name <> " " <> compileFields attributes <> "\n"
    where
		compileFields :: [Attribute] -> Text
		compileFields attributes = intercalate " " $ map compileField attributes
		compileField :: Attribute -> Text
		compileField _ = "ValidatorIdentity"
		compileTypeAlias = "type " <> tableNameToModelName name <> "ValidatorIdentity  = forall " <> intercalate " " (map (\(_, i) -> "a" <> tshow i) (zip attributes [0..])) <> ". " <> tableNameToModelName name <> "' " <> compileTypes attributes
		compileTypes :: [Attribute] -> Text
		compileTypes attributes = intercalate " " $ map compileType (zip attributes [0..])
		compileType (_, i) = "(ValidatorIdentity a" <> tshow i <> ")"


compileCanValidate :: Table -> Text
compileCanValidate table@(Table name attributes) =
        compileCanValidateInstance (tableNameToModelName name) False
        <> compileCanValidateInstance (tableNameToModelName name) True
        <> section
        <> section
        <> ("type instance ModelFieldType (" <> compileNewOrSavedType table <> ") = Model." <> tableNameToModelName name <> ".Field\n")
        <> "instance CanValidateField (" <> compileNewOrSavedType table <> ") where\n"
        <> indent compileValidateModelField
    where
        compileCanValidateInstance :: Text -> Bool -> Text
        compileCanValidateInstance modelName isNew =
            "instance CanValidate " <> (if isNew then "New" else "") <> modelName <> " where\n"
                <> indent (
                    compileValidateModelResult modelName isNew
                    <> compileValidate modelName
                    <> section
                    <> compileIsValid modelName
                    <> section
                )
            where
        compileValidate :: Text -> Text
        compileValidate modelName = "validate model = let combine = Model." <> modelName <> ".combine in combine model (combine Model." <> modelName <> ".fields (combine (Model." <> modelName <> ".validator) (" <> modelName <> " " <> intercalate " " (map compileAttribute attributes) <> ")))"
            where
                compileAttribute _ = "Foundation.ValidationSupport.validateField"

        compileValidateModelResult modelName isNew = "type ValidateModelResult " <> (if isNew then "New" else "") <>  modelName <> " = " <> modelName <> "' " <> intercalate " " (map compileAttribute attributes) <> "\n"
            where
                compileAttribute _ = "ValidatorResult"
        compileCanValidateModelField = "type Model Model." <> tableNameToModelName name <> ".Field = Model." <> tableNameToModelName name <> ".Field\n"
        compileIsValid :: Text -> Text
        compileIsValid modelName = "isValid model = validate model == (" <> modelName <> " " <> intercalate " " (map (\_ -> "Success") attributes) <> ")"
        compileValidateModelField = intercalate "\n" (map compileValidateModelField' attributes)
        compileValidateModelField' (Field fieldName fieldType) = "validateModelField model Model." <> tableNameToModelName name <> "." <> tableNameToModelName fieldName <> " = let " <> tableNameToModelName name <> "{" <> columnNameToFieldName fieldName <> "} = " <> ("let combine = Model." <> tableNameToModelName name <> ".combine in combine model (combine Model." <> tableNameToModelName name <> ".fields (combine (Model." <> tableNameToModelName name <> ".validator) (" <> tableNameToModelName name <> " " <> intercalate " " (map compileAttribute attributes) <> ")))") <> " in " <> columnNameToFieldName fieldName
            where
                compileAttribute _ = "Foundation.ValidationSupport.validateField"

compileIsNewInstance table@(Table name attributes) =
    "instance IsNew New" <> tableNameToModelName name <> " where isNew _ = True\n"
    <> "instance IsNew " <> tableNameToModelName name <> " where isNew _ = False\n"

compileHasModelNameInstance table@(Table name attributes) = "instance HasModelName (" <> compileNewOrSavedType table <> ") where getModelName _ = " <> tshow (tableNameToModelName name) <> "\n"
compileHasTableNameInstance table@(Table name attributes) = "instance HasTableName (" <> compileNewOrSavedType table <> ") where getTableName _ = " <> tshow name <> "\ntype instance GetTableName (New" <> tableNameToModelName name <> ") = " <> tshow name <> "\n" <> "\ntype instance GetTableName (" <> tableNameToModelName name <> ") = " <> tshow name <> "\n"

compileReadParams :: Table -> Text
compileReadParams _ = "readParams = combine columnNames\n"

compileColumnNames table@(Table tableName attributes) = "columnNames = " <> tableNameToModelName tableName <> " " <> compiledFields
    where
        compiledFields = intercalate " " (map compileField attributes)
        compileField (Field fieldName _) = "(" <>tshow fieldName <> " :: ByteString)"

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
