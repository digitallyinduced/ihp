module Foundation.SchemaCompiler where
import ClassyPrelude
import Data.String.Conversions (cs)
import Model.Schema (database)
import Foundation.SchemaSupport
import Foundation.NameSupport (tableNameToModelName)
import qualified Data.Text as Text


-- USE LINE PRAGMA IN OUTPUT
{-# LINE 42 "Foo.vhs" #-}

c = compile
compile :: IO ()
compile = do
    let compiled = map (\table -> (getFilePath table, compileTable table)) database
    mapM_ writeTable compiled

writeTable :: (FilePath, Text) -> IO ()
writeTable (path, content) = do
    writeFile (cs path) (cs content)

section = "\n"
compileTable table@(Table name attributes) =
    "module Model.Generated." <> tableNameToModelName name <> " where\n\n"
    <> "import Foundation.HaskellSupport\n"
    <> "import Foundation.ModelSupport\n"
    <> "import ClassyPrelude\n"
    <> "import Database.PostgreSQL.Simple\n"
    <> "import Database.PostgreSQL.Simple.FromRow\n"
    <> section
    <> compileDataDefinition table
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

getFilePath :: Table -> FilePath
getFilePath (Table name attributes) = "src/Model/Generated/" <> (cs $ tableNameToModelName name) <> ".hs"

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
        bindings = intercalate ", " $ map toBinding attributes
        toBinding (Field fieldName fieldType) = fieldName <> " model"
    in
        "create :: (?modelContext :: ModelContext) => " <> modelName <> " -> IO " <> modelName <> " \n"
        <> "create model = do\n"
        <> indent ("let (ModelContext conn) = ?modelContext\n"
            <> "PG.execute conn \"INSERT INTO " <> name <> " (" <> columns <> ") VALUES (" <> values <> ")\" [" <> bindings <> "]\n"
            <> "return model\n"
        )

compileFindAll table@(Table name attributes) =
    let
        modelName = tableNameToModelName name
    in
        "findAll :: (?modelContext :: ModelContext) => IO [" <> modelName <> "]\n"
        <> "findAll = do\n"
        <> indent (
            "let (ModelContext conn) = ?modelContext\n"
            <> "projects <- PG.query_ conn \"SELECT * FROM projects\"\n"
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
            <> "results <- PG.query conn \"SELECT * FROM " <> name <> " WHERE id = ?\" [id]\n"
            <> "return $ headMay results\n"
        )


compileFromRowInstance table@(Table name attributes) =
    "instance FromRow " <> tableNameToModelName name <> " where \n"
    <> (indent "fromRow = " <> tableNameToModelName name <> " <$> " <>  (intercalate " <*> " $ map (const "field") attributes))

--compileAttributeBag :: Table -> Text
--compileAttributeBag table@(Table name attributes) = "class To" <> tableNameToModelName name <> "Attributes where\n    to"
indent :: Text -> Text
indent code =
        intercalate "\n" $ map indentLine $ Text.splitOn "\n" code
    where
        indentLine ""   = ""
        indentLine line = "    " <> line