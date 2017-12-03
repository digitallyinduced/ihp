module Foundation.SchemaCompiler where
import ClassyPrelude
import Data.String.Conversions (cs)
import Model.Schema (database)
import Foundation.SchemaSupport
import Foundation.NameSupport (tableNameToModelName)
import qualified Data.Text as Text


-- USE LINE PRAGMA IN OUTPUT
{-# LINE 42 "Foo.vhs" #-}

compile :: IO ()
compile = putStrLn $ intercalate "\n\n" $ map compileTable database

section = "\n"
compileTable table@(Table name attributes) =
    "module Model." <> tableNameToModelName name <> " where \n"
    <> "import Database.PostgreSQL.Simple\n"
    <> "import Database.PostgreSQL.Simple.FromRow\n"
    <> section
    <> compileDataDefinition table
    <> section
    <> compileFromRowInstance table

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
    "create = "

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