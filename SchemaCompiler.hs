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
    writeFile (cs path) (cs content)

writeStub :: (FilePath, Text) -> IO ()
writeStub (path, content) = do
    stubExists <- Directory.doesFileExist (cs path)
    if not stubExists then writeFile (cs path) (cs content) else return ()


section = "\n"
compileTable table@(Table name attributes) =
    "-- This file is auto generated and will be overriden regulary. Please edit `src/Model/" <> tableNameToModelName name <> ".hs` to customize the Model"
    <> section
    <> "{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}"
    <> section
    <> "module Model.Generated." <> tableNameToModelName name <> " where\n\n"
    <> "import Foundation.HaskellSupport\n"
    <> "import Foundation.ModelSupport\n"
    <> "import ClassyPrelude hiding (id) \n"
    <> "import Database.PostgreSQL.Simple\n"
    <> "import Database.PostgreSQL.Simple.FromRow\n"
    <> section
    <> compileTypeAlias table
    <> section
    <> compileGenericDataDefinition table
    <> section
    <> compileFromRowInstance table
    <> section
    <> section
    <> compileCreate table
    <> section
    <> compileFindAll table
    <> section
    <> compileFind table
    <> section
    <> compileUnit table
    <> section
    <> compileFindByAttributes table
    <> section


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
compileDataDefinition table@(Table name attributes) =
		"data " <> tableNameToModelName name <> " = " <> tableNameToModelName name <> " { " <> compileFields attributes <> " }\n"
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
        "create :: (?modelContext :: ModelContext) => " <> modelName <> " -> IO " <> modelName <> " \n"
        <> "create model = do\n"
        <> indent ("let (ModelContext conn) = ?modelContext\n"
            <> "result <- Database.PostgreSQL.Simple.query conn \"INSERT INTO " <> name <> " (" <> columns <> ") VALUES (" <> values <> ") RETURNING *\" (" <> bindings <> ")\n"
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
        "find :: (?modelContext :: ModelContext) => Int -> IO (Maybe " <> modelName <> ")\n"
        <> "find id = do\n"
        <> indent (
            "let (ModelContext conn) = ?modelContext\n"
            <> "results <- Database.PostgreSQL.Simple.query conn \"SELECT * FROM " <> name <> " WHERE id = ?\" [id]\n"
            <> "return $ headMay results\n"
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




--compileAttributeBag :: Table -> Text
--compileAttributeBag table@(Table name attributes) = "class To" <> tableNameToModelName name <> "Attributes where\n    to"
indent :: Text -> Text
indent code =
        intercalate "\n" $ map indentLine $ Text.splitOn "\n" code
    where
        indentLine ""   = ""
        indentLine line = "    " <> line