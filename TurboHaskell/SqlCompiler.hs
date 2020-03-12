module TurboHaskell.SqlCompiler where
import           ClassyPrelude
import           Data.String.Conversions  (cs)
import qualified Data.Text                as Text
import qualified TurboHaskell.NameSupport
import           TurboHaskell.SchemaSupport
import qualified System.Directory         as Directory
import TurboHaskell.SchemaTypes
import qualified Text.Countable as Countable

main :: [Table] -> IO ()
main database = writeFileIfNecessary "Application/Schema.sql" (compileDatabase database)

writeFileIfNecessary :: FilePath -> Text -> IO ()
writeFileIfNecessary path content = do
    alreadyExists <- Directory.doesFileExist path
    existingContent <- if alreadyExists then readFile path else return ""
    when (existingContent /= cs content) $ do
        putStrLn $ "Updating " <> cs path
        writeFile (cs path) (cs content)

compileDatabase database = "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";\n\n" <>(lineSep $ map compileTable database) <> "\n" <> (lineSep $ map compileConstraints database)

compileTable table@(Table name attributes) =
    "-- Please don't make any modifications to this file as it's auto generated. Use Application/Schema.hs to change the schema\n"
    <> (lineSep (map (compileCreateEnum table) $ fieldsOnly attributes))
    <> "CREATE TABLE " <> name <> " (\n" <> indent (intercalate ",\n" $ map (compileAttribute table) $ fieldsOnly attributes) <> "\n);"

compileAttribute :: Table -> Attribute -> Text
compileAttribute table field@(Field name fieldType) = name <> " " <> compileType fieldType
    where
        compileType SerialField {}                        = "SERIAL PRIMARY KEY"
        compileType TextField { defaultValue, allowNull, isPrimaryKey, unique }            = compileTokens ["TEXT", compileDefaultValue defaultValue, compilePrimaryKeyConstraint isPrimaryKey, compileNullConstraint allowNull, compileUnique unique]
        compileType IntField { defaultValue, references, allowNull, isPrimaryKey, unique } = compileTokens ["INT", compileDefaultValue defaultValue, compilePrimaryKeyConstraint isPrimaryKey, compileNullConstraint allowNull, compileUnique unique]
        compileType BoolField { defaultValue, allowNull, isPrimaryKey, unique }            = compileTokens ["BOOLEAN", compileDefaultValue defaultValue, compilePrimaryKeyConstraint isPrimaryKey, compileNullConstraint allowNull, compileUnique unique]
        compileType EnumField { defaultValue, allowNull, isPrimaryKey, unique }            = compileTokens [enumTypeName table field, compileDefaultValue defaultValue, compilePrimaryKeyConstraint isPrimaryKey, compileNullConstraint allowNull, compileUnique unique]
        compileType Timestamp { defaultValue, allowNull, isPrimaryKey, unique }            = compileTokens ["TIMESTAMP WITH TIME ZONE", compileDefaultValue defaultValue, compilePrimaryKeyConstraint isPrimaryKey, compileNullConstraint allowNull, compileUnique unique]
        compileType UUIDField { defaultValue, allowNull, isPrimaryKey, unique }            = compileTokens ["UUID", compileDefaultValue defaultValue, compilePrimaryKeyConstraint isPrimaryKey, compileNullConstraint allowNull, compileUnique unique]
        compileType PointField { defaultValue, allowNull, isPrimaryKey, unique }            = compileTokens ["POINT", compileDefaultValue defaultValue, compilePrimaryKeyConstraint isPrimaryKey, compileNullConstraint allowNull, compileUnique unique]
        compileType FloatField { defaultValue, references, allowNull, isPrimaryKey, unique } = compileTokens ["REAL", compileDefaultValue defaultValue, compilePrimaryKeyConstraint isPrimaryKey, compileNullConstraint allowNull, compileUnique unique]
        compileType DoubleField { defaultValue, references, allowNull, isPrimaryKey, unique } = compileTokens ["DOUBLE PRECISION", compileDefaultValue defaultValue, compilePrimaryKeyConstraint isPrimaryKey, compileNullConstraint allowNull, compileUnique unique]

        compileDefaultValue (Just (SqlDefaultValue value)) = "DEFAULT " <> value
        compileDefaultValue _                              = ""

        compileNullConstraint :: Bool -> Text
        compileNullConstraint True  = ""
        compileNullConstraint False = "NOT NULL"

        compilePrimaryKeyConstraint :: Bool -> Text
        compilePrimaryKeyConstraint True = "PRIMARY KEY"
        compilePrimaryKeyConstraint False = ""

        compileUnique :: Bool -> Text
        compileUnique True = "UNIQUE"
        compileUnique False = ""

compileCreateEnum :: Table -> Attribute -> Text
compileCreateEnum table field@(Field fieldName (EnumField { defaultValue, values })) = "CREATE TYPE " <> enumTypeName table field <> " AS ENUM (" <> commaSep (map valueToSql values) <> ");\n"
    where
        valueToSql value = "'" <> value <> "'"
        commaSep = intercalate ", "
compileCreateEnum _ _ = ""

compileConstraints table@(Table _ attributes) =
        lineSep $ map (compileConstraints' table) attributes
    where
        compileOnDelete NoAction = ""
        compileOnDelete Restrict = " ON DELETE RESTRICT"
        compileOnDelete SetNull = " ON DELETE SET NULL"
        compileOnDelete Cascade = " ON DELETE CASCADE"
        compileConstraints' (Table tableName _) (Field name (UUIDField { references, onDelete })) =
            case references of
                Just refName -> "ALTER TABLE " <> tableName <> " ADD CONSTRAINT " <> tableName <> "_ref_" <> name <> " FOREIGN KEY (" <> name <> ") REFERENCES " <> refName <> " (id)" <> compileOnDelete onDelete <> ";"
                Nothing -> ""
        compileConstraints' _ _ = ""


enumTypeName :: Table -> Attribute -> Text
enumTypeName (Table tableName _) (Field fieldName _) = toUpper (Countable.singularize tableName) <> "_" <> toUpper fieldName

compileTokens :: [Text] -> Text
compileTokens tokens = unwords $ filter (\token -> token /= "") tokens

lineSep values = intercalate "\n" $ filter (\line -> line /= "") values

indent :: Text -> Text
indent code =
        intercalate "\n" $ map indentLine $ Text.splitOn "\n" code
    where
        indentLine ""   = ""
        indentLine line = "    " <> line
