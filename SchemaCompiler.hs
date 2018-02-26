module Foundation.SchemaCompiler where
import ClassyPrelude
import Data.String.Conversions (cs)
import Model.Schema (database)
import Foundation.SchemaSupport
import Foundation.NameSupport (tableNameToModelName)
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified System.Directory as Directory
import Data.List ((!!))


-- USE LINE PRAGMA IN OUTPUT
--{-# LINE 42 "Foo.vhs" #-}

c = compile
compile :: IO ()
compile = do
    let compiled = map (\table -> (getFilePath table, compileTable table)) database
    let compiledStubs = map (\table -> (getStubFilePath table, compileStub table)) database
    mapM_ writeTable compiled
    mapM_ writeStub compiledStubs

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
    <> "{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, TypeFamilies #-}"
    <> section
    <> "module Model.Generated." <> tableNameToModelName name <> " where\n\n"
    <> "import Foundation.HaskellSupport\n"
    <> "import Foundation.ModelSupport\n"
    <> "import ClassyPrelude hiding (id) \n"
    <> "import Database.PostgreSQL.Simple\n"
    <> "import Database.PostgreSQL.Simple.FromRow\n"
    <> "import Foundation.ControllerSupport (ParamName (..))\n"
    <> "import qualified Data.Function\n"
    <> section
    <> compileGenericDataDefinition table
    <> compileTypeAlias table
    <> compileNewTypeAlias table
    <> section
    <> compileFromRowInstance table
    <> section
    <> section
    <> compileCreate table
    <> section
    <> compileUpdate table
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
    <> compileAttributeNames table
    <> section
    <> compileAssign table
    <> section
    <> compileIdentity table
    <> section
    <> compileCombine table
    <> section
    <> compileHasId table


compileStub table@(Table name attributes) =
    "module Model." <> tableNameToModelName name <> " (module Model.Generated." <> tableNameToModelName name <> ") where\n\n"
    <> "import Model.Generated." <> tableNameToModelName name <> "\n"
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

compileDataDefinition :: Table -> Text
compileDataDefinition table@(Table name attributes) = "data " <> tableNameToModelName name <> " = " <> tableNameToModelName name <> " { " <> compileFields attributes <> " }\n"
    where
		compileFields :: [Attribute] -> Text
		compileFields attributes = intercalate ", " $ map compileField attributes
		compileField :: Attribute -> Text
		compileField (Field fieldName fieldType) = fieldName <> " :: " <> haskellType fieldType
		haskellType :: FieldType -> Text
		haskellType (SerialField) = "Int"
		haskellType (TextField _) = "Text"
		haskellType (IntField) = "Int"

compileTypeAlias :: Table -> Text
compileTypeAlias table@(Table name attributes) =
		"type " <> tableNameToModelName name <> " = " <> tableNameToModelName name <> "' " <> compileFields attributes <> "\n"
    where
		compileFields :: [Attribute] -> Text
		compileFields attributes = intercalate " " $ map compileField attributes
		compileField :: Attribute -> Text
		compileField (Field fieldName fieldType) = haskellType fieldType
		haskellType :: FieldType -> Text
		haskellType (SerialField) = "Int"
		haskellType (TextField _) = "Text"
		haskellType (IntField) = "Int"


compileNewTypeAlias :: Table -> Text
compileNewTypeAlias table@(Table name attributes) =
		"type New" <> tableNameToModelName name <> " = " <> tableNameToModelName name <> "' " <> compileFields attributes <> "\n"
    where
		compileFields :: [Attribute] -> Text
		compileFields attributes = intercalate " " $ map compileField attributes
		compileField :: Attribute -> Text
		compileField (Field fieldName fieldType) = haskellType fieldType
		haskellType :: FieldType -> Text
		haskellType (SerialField) = "()"
		haskellType (TextField _) = "Text"
		haskellType (IntField) = "Int"

compileGenericDataDefinition :: Table -> Text
compileGenericDataDefinition table@(Table name attributes) =
		"data " <> tableNameToModelName name <> "' " <> params <> " = " <> tableNameToModelName name <> " { " <> compileFields attributes <> " }\n"
    where
        params :: Text
        params = intercalate " " allParameters
        allParameters :: [Text]
        allParameters = take (ClassyPrelude.length attributes) (map (\n -> ("p" <> (cs $ show n))) [0..])
        compileFields :: [Attribute] -> Text
        compileFields attributes = intercalate ", " $ map compileField (zip attributes [0..])
        compileField :: (Attribute, Int) -> Text
        compileField (Field fieldName fieldType, n) = fieldName <> " :: " <> (allParameters !! n)

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
        bindings = let bindingValues = map fromJust $ filter isJust (map toBinding attributes) in if (ClassyPrelude.length bindingValues == 1) then "Only (" <> (unsafeHead bindingValues) <> ")" else intercalate ", " bindingValues
        toBinding (Field fieldName fieldType) =
            case fieldType of
                SerialField -> Nothing
                otherwise   -> Just $ fieldName <> " model"
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
                bindingValues = (map fromJust $ filter isJust (map toBinding attributes)) <> (["id model"])
            in
                if (ClassyPrelude.length bindingValues == 1)
                    then "Only (" <> (unsafeHead bindingValues) <> ")"
                    else intercalate ", " bindingValues
        toBinding (Field fieldName fieldType) =
            case fieldType of
                SerialField -> Nothing
                otherwise   -> Just $ fieldName <> " model"
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
        "unit :: " <> tableNameToModelName name <> "' " <> compileFields attributes <>  "\n"
		<> "unit = " <> tableNameToModelName name <> " " <> compileFields attributes <> "\n"
    where
		compileFields :: [Attribute] -> Text
		compileFields attributes = intercalate " " $ map compileField attributes
		compileField :: Attribute -> Text
		compileField _ = "()"

compileBuild _ = "build = unit\n"

compileIdentity :: Table -> Text
compileIdentity table@(Table name attributes) =
		"identity = " <> tableNameToModelName name <> " " <> compileFields attributes <> "\n"
    where
		compileFields :: [Attribute] -> Text
		compileFields attributes = intercalate " " $ map compileField attributes
		compileField :: Attribute -> Text
		compileField _ = "Data.Function.id"

compileHasId :: Table -> Text
compileHasId table@(Table name attributes) = "instance HasId " <> tableNameToModelName name <> " where getId model = id model\n"

compileFindByAttributes table@(Table tableName attributes) =
        intercalate "\n\n" $ map compileFindByAttribute attributes
    where
        compileFindByAttribute (Field name _) =
            let
                modelName = tableNameToModelName tableName
                fieldName = tableNameToModelName name
            in
                "findBy" <> fieldName <> " :: (?modelContext :: ModelContext) => Int -> IO [" <> modelName <> "]\n"
                <> "findBy" <> fieldName <> " value = do\n"
                <> indent (
                    "let (ModelContext conn) = ?modelContext\n"
                    <> "results <- Database.PostgreSQL.Simple.query conn \"SELECT * FROM " <> tableName <> " WHERE " <> name <> " = ?\" [value]\n"
                    <> "return results\n"
                )

        compileFindByAttribute _ = ""

compileAttributeNames table@(Table tableName attributes) =
        "data Field = " <> (intercalate " | " (map compileAttributeName attributes)) <> " deriving (Show)"
        <> section
        <> section
        <> "instance ParamName Field where \n" <> (intercalate "\n" (map compileParamName attributes)) <> "\n"
        <> "instance FormField Field where \n"
        <> "    type Model Field = " <> tableNameToModelName tableName <> "\n"
        <> (intercalate "\n" (map compileFormFieldName attributes))
        <> section
        <> (intercalate "\n" (map compileFormFieldValue attributes))
    where
        compileAttributeName (Field name _) = tableNameToModelName name
        compileParamName (Field name _) = indent $ "paramName " <> (tableNameToModelName name) <> " = \"" <> name <> "\""
        compileFormFieldName (Field name _) = indent $ "formFieldName " <> (tableNameToModelName name) <> " = \"" <> name <> "\""
        compileFormFieldValue (Field name _) = indent $ "formFieldValue " <> (tableNameToModelName name) <> " (" <> tableNameToModelName tableName <> " { " <> name <> " }) = inputValue " <> name

compileAssign table@(Table tableName attributes) =
        ""
        --"assignField :: Field -> Text -> " <> tableNameToModelName tableName <> " -> " <> tableNameToModelName tableName <> "\n"
        <> "assignField field value model = case field of \n" <> intercalate "\n" (map compileAssignField attributes) <> " \n"
        <> "assign :: [(Field, Text)] -> " <> tableNameToModelName tableName <> " -> " <> tableNameToModelName tableName <> "\n"
        <> "assign = undefined"
    where
        compileAssignField (Field name _) = "   " <>  tableNameToModelName name <> " -> model { " <> name <> " = value }"

compileCombine table@(Table tableName attributes) =
        "combine (" <> tableNameToModelName tableName <> " " <> (intercalate " " (attributesToArgs "arg" attributes)) <> ") (" <> tableNameToModelName tableName <> " " <> (intercalate " " (attributesToArgs "f" attributes)) <> ") = " <> tableNameToModelName tableName <> " " <> (intercalate " " (attributesToApplications attributes))
    where
        attributesToArgs :: Text -> [Attribute] -> [Text]
        attributesToArgs prefix attributes = map (\n -> prefix <> tshow n) $ (map snd (zip attributes [0..]))
        attributesToApplications attributes = map (\n -> "(f" <> tshow n <> " arg" <> tshow n <> ") ") $ (map snd (zip attributes [0..]))

--compileAttributeBag :: Table -> Text
--compileAttributeBag table@(Table name attributes) = "class To" <> tableNameToModelName name <> "Attributes where\n    to"
indent :: Text -> Text
indent code =
        intercalate "\n" $ map indentLine $ Text.splitOn "\n" code
    where
        indentLine ""   = ""
        indentLine line = "    " <> line