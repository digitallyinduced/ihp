module Foundation.SqlCompiler where
import           ClassyPrelude
import           Data.String.Conversions  (cs)
import qualified Data.Text                as Text
import qualified Foundation.NameSupport
import           Foundation.SchemaSupport
import           Model.Schema
import qualified System.Directory         as Directory

main :: IO ()
main = writeFileIfNecessary "src/Model/Schema.sql" compileDatabase

writeFileIfNecessary :: FilePath -> Text -> IO ()
writeFileIfNecessary path content = do
    alreadyExists <- Directory.doesFileExist path
    existingContent <- if alreadyExists then readFile path else return ""
    when (existingContent /= cs content) $ do
        putStrLn $ "Updating " <> cs path
        writeFile (cs path) (cs content)

compileDatabase = "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";\n\n" <>(lineSep $ map compileTable database) <> "\n" <> (lineSep $ map compileConstraints database)

compileTable table@(Table name attributes) =
    "-- Please don't make any modifications to this file as it's auto generated. Use src/Model/Schema.hs to change the schema\n"
    <> (lineSep (map (compileCreateEnum table) $ fieldsOnly attributes))
    <> "CREATE TABLE " <> name <> " (\n" <> indent (intercalate ",\n" $ map (compileAttribute table) $ fieldsOnly attributes) <> "\n);"

compileAttribute :: Table -> Attribute -> Text
compileAttribute table field@(Field name fieldType) = name <> " " <> compileType fieldType
    where
        compileType SerialField {}                        = "SERIAL PRIMARY KEY"
        compileType TextField { defaultValue, allowNull, isPrimaryKey }            = compileTokens ["TEXT", compileDefaultValue defaultValue, compilePrimaryKeyConstraint isPrimaryKey, compileNullConstraint allowNull]
        compileType IntField { defaultValue, references, allowNull, isPrimaryKey } = compileTokens ["INT", compileDefaultValue defaultValue, compilePrimaryKeyConstraint isPrimaryKey, compileNullConstraint allowNull]
        compileType BoolField { defaultValue, allowNull, isPrimaryKey }            = compileTokens ["BOOLEAN", compileDefaultValue defaultValue, compilePrimaryKeyConstraint isPrimaryKey, compileNullConstraint allowNull]
        compileType EnumField { defaultValue, allowNull, isPrimaryKey }            = compileTokens [enumTypeName table field, compileDefaultValue defaultValue, compilePrimaryKeyConstraint isPrimaryKey, compileNullConstraint allowNull]
        compileType Timestamp { defaultValue, allowNull, isPrimaryKey }            = compileTokens ["TIMESTAMP WITH TIME ZONE", compileDefaultValue defaultValue, compilePrimaryKeyConstraint isPrimaryKey, compileNullConstraint allowNull]
        compileType UUIDField { defaultValue, allowNull, isPrimaryKey }            = compileTokens ["UUID", compileDefaultValue defaultValue, compilePrimaryKeyConstraint isPrimaryKey, compileNullConstraint allowNull]

        compileDefaultValue (Just (SqlDefaultValue value)) = "DEFAULT " <> value
        compileDefaultValue _                              = ""

        compileNullConstraint :: Bool -> Text
        compileNullConstraint True  = ""
        compileNullConstraint False = "NOT NULL"

        compilePrimaryKeyConstraint :: Bool -> Text
        compilePrimaryKeyConstraint True = "PRIMARY KEY"
        compilePrimaryKeyConstraint False = ""

compileCreateEnum :: Table -> Attribute -> Text
compileCreateEnum table field@(Field fieldName (EnumField { defaultValue, values })) = "CREATE TYPE " <> enumTypeName table field <> " AS ENUM (" <> commaSep (map valueToSql values) <> ");\n"
    where
        valueToSql value = "'" <> value <> "'"
        commaSep = intercalate ", "
compileCreateEnum _ _ = ""

compileConstraints table@(Table _ attributes) =
        lineSep $ map (compileConstraints' table) attributes
    where
        compileConstraints' (Table tableName _) (Field name (UUIDField { references })) =
            case references of
                Just refName -> "ALTER TABLE " <> tableName <> " ADD CONSTRAINT " <> tableName <> "_ref_" <> name <> " FOREIGN KEY (" <> name <> ") REFERENCES " <> refName <> " (id);"
                Nothing -> ""
        compileConstraints' _ _ = ""


enumTypeName :: Table -> Attribute -> Text
enumTypeName (Table tableName _) (Field fieldName _) = toUpper (Foundation.NameSupport.pluralToSingular tableName) <> "_" <> toUpper fieldName

compileTokens :: [Text] -> Text
compileTokens tokens = intercalate " " $ filter (\token -> token /= "") tokens

lineSep values = intercalate "\n" $ filter (\line -> line /= "") values

indent :: Text -> Text
indent code =
        intercalate "\n" $ map indentLine $ Text.splitOn "\n" code
    where
        indentLine ""   = ""
        indentLine line = "    " <> line
