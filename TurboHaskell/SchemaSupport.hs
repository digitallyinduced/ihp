module TurboHaskell.SchemaSupport where
import ClassyPrelude hiding (length, bool)
import Data.Maybe (fromJust)
import qualified Data.List as List
import TurboHaskell.SchemaTypes
import TurboHaskell.HaskellSupport
import Data.Char (isLower, isUpper)
import qualified Data.Text as Text
import Data.String.Conversions (cs)

table :: Text -> Table
table name = Table name []

field :: Text -> FieldType -> Attribute
field = Field

(Table name fields) + field = Table name (fields <> [field])

serial :: FieldType
serial = SerialField { defaultValue = Just (SqlDefaultValue "DEFAULT"), references = Nothing, allowNull = False, isPrimaryKey = True, unique = False, onDelete = NoAction }

uuid :: FieldType
uuid = UUIDField { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False, onDelete = NoAction, unique = False }

primaryKey :: FieldType
primaryKey = uuid { defaultValue = Just (SqlDefaultValue "uuid_generate_v4()"), references = Nothing, allowNull = False, isPrimaryKey = True, unique = False }

text :: FieldType
text = TextField { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False, unique = False }

int :: FieldType
int = IntField { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False, unique = False }

integer :: FieldType
integer = int

enum :: [Text] -> FieldType
enum values = EnumField { defaultValue = Nothing, references = Nothing, values, allowNull = False, isPrimaryKey = False, unique = False }

bool :: FieldType
bool = BoolField { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False, unique = False }

boolean :: FieldType
boolean = bool

timestamp :: FieldType
timestamp = Timestamp { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False, unique = False }

point :: FieldType
point = PointField { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False, unique = False }

float :: FieldType
float = FloatField { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False, unique = False }

double :: FieldType
double = DoubleField { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False, unique = False }

belongsTo = BelongsTo
hasMany name = HasMany { name = name, inverseOf = Nothing }

createdAt = field "created_at" timestamp { defaultValue = Just (SqlDefaultValue "NOW()") }
updatedAt = field "updated_at" timestamp { defaultValue = Just (SqlDefaultValue "NOW()") }

validate :: [Table] -> [Text]
validate database = concat $ map (validateTable database) database

validateTable :: [Table] -> Table -> [Text]
validateTable _ (Table "" _) = pure "Table name cannot be empty"
validateTable _ (Table name []) = pure $ "Table " <> name <> " needs to have atleast one field"
validateTable database table@(Table name attributes) = catMaybes $ map (validateAttribute database table) attributes

validateAttribute :: [Table] -> Table -> Attribute -> Maybe Text
validateAttribute database table field = 
    case validateReferences database table field of
        Nothing -> case all (not . isUpper) (get #name field) of
            True -> validateOnDelete database table field
            False -> error $ (cs $ get #name field) <> "You need to use underscores and all lowerCase instead of CamelCase in Schema.hs."
        error -> error

validateReferences :: [Table] -> Table -> Attribute -> Maybe Text
validateReferences database table (Field fieldName (UUIDField { references })) =
    case references of
        Just tableName ->
            let
                referencedTable = List.find (\(Table tableName' _) -> tableName' == tableName) database
                (Table refTableName _) = table
            in
                case referencedTable of
                    Just _ -> Nothing
                    Nothing -> Just ("In the definition `+ field \"" <> fieldName <> "\" uuid { references = Just \"" <> tableName <> "\" }` the table named `" <> tableName <> "` could not be found in the Schema.hs")
        Nothing -> Nothing
validateReferences _ _ _ = Nothing

validateOnDelete :: [Table] -> Table -> Attribute -> Maybe Text
validateOnDelete database (Table tableName _) (Field fieldName (UUIDField { references = Just _, onDelete = NoAction })) = Just ("In the definition `+ field \"" <> fieldName <> "\" uuid { ... }` of table `" <> tableName <> "` the `onDelete = Cascade` or `onDelete = SetNull` was not specified in the Schema.hs")
validateOnDelete _ _ _ = Nothing 


fieldsOnly :: [Attribute] -> [Attribute]
fieldsOnly = filter isField
    where
        isField (Field _ _ ) = True
        isField otherwise = False

fieldsWithoutDefaultValue :: [Attribute] -> [Attribute]
fieldsWithoutDefaultValue = filter hasNoDefaultValue
    where
        hasNoDefaultValue (Field _ fieldType) = isNothing (defaultValue fieldType)

fieldsWithDefaultValue :: [Attribute] -> [Attribute]
fieldsWithDefaultValue = filter (\(Field _ fieldType) -> isJust (defaultValue fieldType))

setHasManyRelations :: [Table] -> [Table]
setHasManyRelations tables = map setHasManyRelations' tables
    where
        setHasManyRelations' :: Table -> Table
        setHasManyRelations' table@(Table tableName _) = foldl' (\b a -> (TurboHaskell.SchemaSupport.+) b a) table hasManyStatements
            where
                hasManyStatements :: [Attribute]
                hasManyStatements = foreignKeyColumns
                    |> map (\(tableName, field) -> HasMany { name = tableName, inverseOf = Just (fieldName field) })
                    |> reverse . List.nubBy (\a b -> a { inverseOf = Nothing } == b { inverseOf = Nothing} ) . reverse

                fieldName :: Attribute -> Text
                fieldName (Field fieldName _) = fieldName

                foreignKeyColumns :: [(Text, Attribute)]
                foreignKeyColumns = join (map tableToForeignColumns tables)

                tableToForeignColumns :: Table -> [(Text, Attribute)]
                tableToForeignColumns (Table tableName' fields) =
                    fields
                    |> filter (\f -> getReferenceField f == Just tableName)
                    |> map (\f -> (tableName', f))

                getReferenceField :: Attribute -> Maybe Text
                getReferenceField (Field _ fieldType) = references fieldType
                getReferenceField _ = Nothing

schema :: [Table] -> [Table]
schema database = setHasManyRelations database

getFieldName :: Attribute -> Text
getFieldName (Field fieldName _) = fieldName