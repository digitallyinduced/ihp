module Foundation.SchemaCompiler where
import ClassyPrelude
import Data.String.Conversions (cs)
import Model.Schema (database)
import Foundation.SchemaSupport
import Foundation.NameSupport (tableNameToModelName)
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified System.Directory as Directory
import qualified Data.Set
import Data.List ((!!), (\\))
import Data.List.Split


-- USE LINE PRAGMA IN OUTPUT
--{-# LINE 42 "Foo.vhs" #-}

c = compile
main = compile
compile :: IO ()
compile = do
    let compiled = map (\table -> (getFilePath table, compileTable table)) database
    let compiledStubs = map (\table -> (getStubFilePath table, compileStub table)) database
    mapM_ writeTable compiled
    mapM_ writeStub compiledStubs
    writeTable (getTypesFilePath, compileTypes database)
    writeTable (getValidatorsFilePath, compileValidators database)


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
    <> "import ClassyPrelude hiding (id) \n"
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
    <> section
    <> compileCreate table
    <> section
    <> compileUpdate table
    <> section
    <> compileDelete table
    <> section
    <> compileFindAll table
    <> section
    <> compileFindOrNothing table
    <> section
    <> compileFind table
    <> section
    <> compileUnit table
    <> section
    <> compileBuild table
    <> section
    <> compileFindByAttributes table
    <> section
    <> compileFindOneByAttributes table
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
                  <> "import ClassyPrelude hiding (id) \n"
                  <> "import Database.PostgreSQL.Simple\n"
                  <> "import Database.PostgreSQL.Simple.FromRow\n"
                  <> "import Database.PostgreSQL.Simple.FromField hiding (Field, name)\n"
                  <> "import Database.PostgreSQL.Simple.ToField hiding (Field)\n"
                  <> "import Foundation.Controller.Param (ParamName (..))\n"
                  <> "import qualified Data.Function\n"
                  <> "import GHC.TypeLits\n"

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
    <> section


compileValidators :: [Table] -> Text
compileValidators database = prelude <> "\n\n" <> intercalate "\n\n" (map compileCanValidate database)
    where
        prelude = "-- This file is auto generated and will be overriden regulary."
                  <> section
                  <> "{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, TypeFamilies, DataKinds, TypeOperators, UndecidableInstances, ConstraintKinds  #-}"
                  <> section
                  <> "module Model.Generated.Validators where\n\n"
                  <> "import Foundation.ValidationSupport\n\n"
                  <> "import Model.Generated.Types\n\n"
                  <> "import ClassyPrelude\n\n"
                  <> intercalate "\n" (map (\(Table name attributes) -> "import qualified Model." <> tableNameToModelName name <> " (validator, combine, fields, Field(..))\n") database)



compileStub table@(Table name attributes) =
    "module Model." <> tableNameToModelName name <> " (module Model.Generated." <> tableNameToModelName name <> ") where\n\n"
    <> "import Model.Generated." <> tableNameToModelName name <> "\n"
    <> "import Model.Generated.Types\n"
    <> "import Foundation.HaskellSupport\n"
    <> "import Foundation.ModelSupport\n"
    <> "import ClassyPrelude hiding (id) \n"
    <> "import Database.PostgreSQL.Simple\n"
    <> "import Database.PostgreSQL.Simple.FromRow\n"
    <> section
    <> "-- Here you can customize the model"
    <> section

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
            compileField (Field fieldName fieldType) = fieldName <> " :: " <> haskellType fieldName fieldType

haskellType :: Text -> FieldType -> Text
haskellType fieldName (SerialField) = "Int"
haskellType fieldName (TextField _) = "Text"
haskellType fieldName (IntField) = "Int"
haskellType fieldName (EnumField {}) = tableNameToModelName fieldName
haskellType fieldName (BoolField{}) = "Bool"

compileTypeAlias :: Table -> Text
compileTypeAlias table@(Table name attributes) =
		"type " <> tableNameToModelName name <> " = " <> tableNameToModelName name <> "' " <> compileFields attributes <> "\n"
    where
		compileFields :: [Attribute] -> Text
		compileFields attributes = intercalate " " $ map compileField attributes
		compileField :: Attribute -> Text
		compileField (Field fieldName fieldType) = haskellType fieldName fieldType


compileNewTypeAlias :: Table -> Text
compileNewTypeAlias table@(Table name attributes) =
		"type New" <> tableNameToModelName name <> " = " <> tableNameToModelName name <> "' " <> compileFields attributes <> "\n"
    where
		compileFields :: [Attribute] -> Text
		compileFields attributes = intercalate " " $ map compileField attributes
		compileField :: Attribute -> Text
		compileField (Field fieldName fieldType) = haskellType' fieldName fieldType
		haskellType' fieldName (SerialField) = "()"
		haskellType' fieldName fieldType = haskellType fieldName fieldType

compileNewOrSavedTypeAlias :: Table -> Text
compileNewOrSavedTypeAlias table@(Table name attributes) =
		"type NewOrSaved" <> tableNameToModelName name <> " = forall id. " <> compileNewOrSavedType table <> "\n"

compileNewOrSavedType :: Table -> Text
compileNewOrSavedType table@(Table name attributes) =
		"" <> tableNameToModelName name <> "' " <> compileFields attributes
    where
		compileFields :: [Attribute] -> Text
		compileFields attributes = intercalate " " $ map compileField attributes
		compileField :: Attribute -> Text
		compileField (Field fieldName fieldType) = haskellType' fieldName fieldType
		haskellType' fieldName (SerialField) = "id"
		haskellType' fieldName fieldType = haskellType fieldName fieldType

compileGenericDataDefinition :: Table -> Text
compileGenericDataDefinition table@(Table name attributes) =
		"data " <> tableNameToModelName name <> "' " <> params <> " = " <> tableNameToModelName name <> " { " <> compileFields attributes <> " } deriving (Eq, Show)\n"
    where
        params :: Text
        params = intercalate " " allParameters
        allParameters :: [Text]
        allParameters = take (ClassyPrelude.length attributes) (map (\n -> ("p" <> (cs $ show n))) [0..])
        compileFields :: [Attribute] -> Text
        compileFields attributes = intercalate ", " $ map compileField (zip attributes [0..])
        compileField :: (Attribute, Int) -> Text
        compileField (Field fieldName fieldType, n) = fieldName <> " :: " <> (allParameters !! n)

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
        compileEnumField (Field fieldName (EnumField values)) = "data " <> tableNameToModelName fieldName <> " = " <> (intercalate " | " (map tableNameToModelName values)) <> " deriving (Eq, Show)"
        compileFromFieldInstance (Field fieldName (EnumField values)) = "instance FromField " <> tableNameToModelName fieldName <> " where\n" <> indent (intercalate "\n" ((map compileFromFieldInstanceForValue values) <> [compileFromFieldInstanceForError, compileFromFieldInstanceForNull]))
        compileFromFieldInstanceForValue value = "fromField field (Just " <> tshow value <> ") = return " <> tableNameToModelName value
        compileFromFieldInstanceForError = "fromField field (Just value) = returnError ConversionFailed field \"Unexpected value for enum value\""
        compileFromFieldInstanceForNull = "fromField field Nothing = returnError UnexpectedNull field \"Unexpected null for enum value\""

        compileToFieldInstance (Field fieldName (EnumField values)) = "instance ToField " <> tableNameToModelName fieldName <> " where\n" <> indent (intercalate "\n" (map compileToFieldInstanceForValue values))
        compileToFieldInstanceForValue value = "toField " <> tableNameToModelName value <> " = toField (" <> tshow value <> " :: Text)"

        compileInputValueInstance (Field fieldName (EnumField values)) = "instance InputValue " <> tableNameToModelName fieldName <> " where\n" <> indent (intercalate "\n" (map compileInputValue values))
        compileInputValue value = "inputValue " <> tableNameToModelName value <> " = " <> tshow value <> " :: Text"

compileToRowValues bindingValues = if (ClassyPrelude.length bindingValues == 1) then "Only (" <> (unsafeHead bindingValues) <> ")" else "(" <> intercalate ") :. (" (map (intercalate ", ") (chunksOf 8 bindingValues)) <> ")"

compileCreate table@(Table name attributes) =
    let
        modelName = tableNameToModelName name
        columns = intercalate ", " $ map toColumn attributes
        toColumn (Field fieldName fieldType) = fieldName
        values = intercalate ", " $ map toValue attributes
        toValue (Field fieldName fieldType) =
            case fieldType of
                SerialField -> "DEFAULT"
                otherwise   -> "?"
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
        SerialField -> Nothing
        otherwise   -> Just ("let " <> modelName <> "{" <> fieldName <> "} = model in " <> fieldName)

compileUpdate table@(Table name attributes) =
    let
        modelName = tableNameToModelName name
        columns = intercalate ", " $ map toColumn attributes
        toColumn (Field fieldName fieldType) = fieldName
        values = intercalate ", " $ map toValue attributes
        toValue (Field fieldName fieldType) =
            case fieldType of
                SerialField -> "DEFAULT"
                otherwise   -> "?"
        bindings :: Text
        bindings =
            let
                bindingValues = (map fromJust $ filter isJust (map (toBinding modelName) attributes)) <> (["getId model"])
            in
                compileToRowValues bindingValues

        updates = intercalate ", " (map fromJust $ filter isJust $ map update attributes)
        update (Field fieldName fieldType) =
            case fieldType of
                SerialField -> Nothing
                otherwise -> Just $ fieldName <> " = ?"
    in
        "update :: (?modelContext :: ModelContext) => " <> modelName <> " -> IO " <> modelName <> " \n"
        <> "update model = do\n"
        <> indent ("let (ModelContext conn) = ?modelContext\n"
            <> "result <- Database.PostgreSQL.Simple.query conn \"UPDATE " <> name <> " SET " <> updates <> " WHERE id = ? RETURNING *\" (" <> bindings <> ")\n"
            <> "return (unsafeHead result)\n"
        )

compileDelete table@(Table name attributes) =
    let
        modelName = tableNameToModelName name
        bindings :: Text
        bindings = "Only (getId model)"
    in
        "delete :: (?modelContext :: ModelContext) => " <> modelName <> " -> IO () \n"
        <> "delete model = do\n"
        <> indent ("let (ModelContext conn) = ?modelContext\n"
            <> "Database.PostgreSQL.Simple.execute conn \"DELETE FROM " <> name <> " WHERE id = ?\" (" <> bindings <> ")\n"
            <> "return ()\n"
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


compileFindOrNothing table@(Table name attributes) =
    let
        modelName = tableNameToModelName name
    in
        "findOrNothing :: (?modelContext :: ModelContext) => Int -> IO (Maybe " <> modelName <> ")\n"
        <> "findOrNothing id = do\n"
        <> indent (
            "let (ModelContext conn) = ?modelContext\n"
            <> "results <- Database.PostgreSQL.Simple.query conn \"SELECT * FROM " <> name <> " WHERE id = ?\" [id]\n"
            <> "return $ headMay results\n"
        )

compileFind table@(Table name attributes) =
    let
        modelName = tableNameToModelName name
    in
        "find :: (?modelContext :: ModelContext) => Int -> IO " <> modelName <> "\n"
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

compileFindByAttributes table@(Table tableName attributes) =
        intercalate "\n\n" $ map compileFindByAttribute attributes
    where
        compileFindByAttribute (Field name fieldType) =
            let
                modelName = tableNameToModelName tableName
                fieldName = tableNameToModelName name
            in
                "findBy" <> fieldName <> " :: (?modelContext :: ModelContext) => " <> haskellType name fieldType <> " -> IO [" <> modelName <> "]\n"
                <> "findBy" <> fieldName <> " value = do\n"
                <> indent (
                    "let (ModelContext conn) = ?modelContext\n"
                    <> "results <- Database.PostgreSQL.Simple.query conn \"SELECT * FROM " <> tableName <> " WHERE " <> name <> " = ?\" [value]\n"
                    <> "return results\n"
                )

        compileFindByAttribute _ = ""

compileFindOneByAttributes table@(Table tableName attributes) =
        intercalate "\n\n" $ map compileFindByAttribute attributes
    where
        compileFindByAttribute (Field name fieldType) =
            let
                modelName = tableNameToModelName tableName
                fieldName = tableNameToModelName name
            in
                "findOneBy" <> fieldName <> " :: (?modelContext :: ModelContext) => " <> haskellType name fieldType <> " -> IO " <> modelName <> "\n"
                <> "findOneBy" <> fieldName <> " value = do\n"
                <> indent (
                    "let (ModelContext conn) = ?modelContext\n"
                    <> "results <- Database.PostgreSQL.Simple.query conn \"SELECT * FROM " <> tableName <> " WHERE " <> name <> " = ? LIMIT 1\" [value]\n"
                    <> "return (fromMaybe (error \"Not found\") (headMay results))\n"
                )

        compileFindByAttribute _ = ""

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
    where
        compileAttributeName (Field name _) = tableNameToModelName name
        compileParamName (Field name _) = indent $ "paramName " <> (tableNameToModelName name) <> " = \"" <> name <> "\""
        compileFormFieldName (Field name _) = indent $ "formFieldName " <> (tableNameToModelName name) <> " = \"" <> name <> "\""
        compileFormFieldValue (Field name _) = indent $ "formFieldValue " <> (tableNameToModelName name) <> " (" <> tableNameToModelName tableName <> " { " <> name <> " }) = inputValue " <> name

compileFieldModel table@(Table tableName attributes) =
        "fields = " <> tableNameToModelName tableName <> " " <> (intercalate " " (map compileAttributeName attributes))
    where
        compileAttributeName (Field name _) = tableNameToModelName name

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

compileErrorHints table@(Table tableName attributes) =
        intercalate "\n" (map compileErrorHintForAttribute attributesExceptSerials)
    where
        attributesExceptSerials = filter (not . isSerial) attributes
        isSerial (Field _ SerialField) = True
        isSerial (Field _ _) = False
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
compileHasInstances tables = intercalate "\n" $ concat [ (mkUniq $ concat $ map compileHasClass tables), hasIdInt:(concat $ map compileHasInstance tables) ]
    where
        allFields :: [Attribute]
        allFields = concat $ map (\(Table _ attributes) -> attributes) tables
        hasIdInt = "instance HasId Int where type IdType Int = Int; getId a = a"
        compileHasClass (Table tableName tableAttributes) = map (\field -> compileHasClass' field) tableAttributes
        compileHasClass' (Field fieldName fieldType) = "class (Show (" <> tableNameToModelName fieldName <> "Type a)) => Has" <> tableNameToModelName fieldName <> " a where type " <> tableNameToModelName fieldName <> "Type a; get" <> tableNameToModelName fieldName <> " :: a -> " <> tableNameToModelName fieldName <> "Type a"
        compileHasInstance (Table tableName tableAttributes) = concat [ map compileHasInstance' tableAttributes, map compileHasInstanceError fieldsNotInTable ]
            where
                compileHasInstance' (Field fieldName fieldType) = "instance Has" <> tableNameToModelName fieldName <> " " <> tableNameToModelName tableName <> " where type " <> tableNameToModelName fieldName <> "Type " <> tableNameToModelName tableName <> " = " <> haskellType fieldName fieldType <> "; get" <> tableNameToModelName fieldName <> " " <> tableNameToModelName tableName <> "{" <> fieldName <> "} = " <> fieldName
                compileHasInstanceError (Field fieldName fieldType) = "instance TypeError (GHC.TypeLits.Text \"" <> tableNameToModelName tableName <> " has no field `" <> fieldName <> "`\") => Has" <> tableNameToModelName fieldName <> " " <> tableNameToModelName tableName <> " where type " <> tableNameToModelName fieldName <> "Type " <> tableNameToModelName tableName <> " = (); get" <> tableNameToModelName fieldName <> " _ = error \"unreachable\""
                fieldsNotInTable = (mkUniq allFields) Data.List.\\ tableAttributes

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
                compileField (Field fieldName fieldType) = "(QueryCondition " <> haskellType fieldName fieldType <> ")"

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
                compileLetBinding arg@(n, Field fieldName fieldType) = "let " <> letBindingName arg <> " = " <> " (let " <> modelName <> " {" <> fieldName <> "} = criteria in toSQLCondition \"" <> fieldName <> "\" " <> fieldName <> ") "
                letBindingName (n, Field fieldName fieldType) = "val" <> tshow  n
        compileBuildCriteria :: Table -> Text
        compileBuildCriteria table@(Table name attributes) =
                "buildCriteria = " <> tableNameToModelName name <> " " <> compileFields attributes <> "\n"
            where
                compileFields :: [Attribute] -> Text
                compileFields attributes = intercalate " " $ map compileField attributes
                compileField :: Attribute -> Text
                compileField (Field fieldName fieldType) = "(NoCondition :: QueryCondition " <> haskellType fieldName fieldType <> ")"

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
        <> "instance CanValidateField (" <> compileNewOrSavedType table <> ") Model." <> tableNameToModelName name <> ".Field where\n"
        <> indent (
                compileValidateModelField
            )
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
        compileValidateModelResult modelName isNew = "type ValidateModelResult " <> (if isNew then "New" else "") <>  modelName <> " = " <> modelName <> "' " <> intercalate " " (map compileAttribute attributes) <> "\n"
            where
                compileAttribute _ = "ValidatorResult"
        compileCanValidateModelField = "type Model Model." <> tableNameToModelName name <> ".Field = Model." <> tableNameToModelName name <> ".Field\n"
        compileValidate :: Text -> Text
        compileValidate modelName = "validate model = let combine = Model." <> modelName <> ".combine in combine model (combine Model." <> modelName <> ".fields (combine (Model." <> modelName <> ".validator) (" <> modelName <> " " <> intercalate " " (map compileAttribute attributes) <> ")))"
            where
                compileAttribute _ = "Foundation.ValidationSupport.validateField"
        compileIsValid :: Text -> Text
        compileIsValid modelName = "isValid model = validate model == (" <> modelName <> " " <> intercalate " " (map (\_ -> "Success") attributes) <> ")"
        compileValidateModelField = intercalate "\n" (map compileValidateModelField' attributes)
        compileValidateModelField' (Field fieldName fieldType) = "validateModelField model Model." <> tableNameToModelName name <> "." <> tableNameToModelName fieldName <> " = let " <> tableNameToModelName name <> "{" <> fieldName <> "} = " <> ("let combine = Model." <> tableNameToModelName name <> ".combine in combine model (combine Model." <> tableNameToModelName name <> ".fields (combine (Model." <> tableNameToModelName name <> ".validator) (" <> tableNameToModelName name <> " " <> intercalate " " (map compileAttribute attributes) <> ")))") <> " in " <> fieldName
            where
                compileAttribute _ = "Foundation.ValidationSupport.validateField"

compileIsNewInstance table@(Table name attributes) =
    "instance IsNew New" <> tableNameToModelName name <> " where isNew _ = True\n"
    <> "instance IsNew " <> tableNameToModelName name <> " where isNew _ = False\n"

compileHasModelNameInstance table@(Table name attributes) = "instance HasModelName (" <> compileNewOrSavedType table <> ") where getModelName _ = " <> tshow (tableNameToModelName name) <> "\n"

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