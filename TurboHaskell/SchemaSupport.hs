module TurboHaskell.SchemaSupport where
import ClassyPrelude hiding (length)
import Data.Maybe (fromJust)
import qualified Data.List as List
import TurboHaskell.SchemaTypes

table :: Text -> Table
table name = Table name []

field :: Text -> FieldType Text -> Attribute' Text
field = Field

(Table name fields) + field = Table name (fields <> [field])

serial :: FieldType Text
serial = SerialField { defaultValue = Just (SqlDefaultValue "DEFAULT"), references = Nothing, allowNull = False, isPrimaryKey = True, unique = False }

uuid :: FieldType Text
uuid = UUIDField { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False, onDelete = NoAction, unique = False }

primaryKey :: FieldType Text
primaryKey = uuid { defaultValue = Just (SqlDefaultValue "uuid_generate_v4()"), references = Nothing, allowNull = False, isPrimaryKey = True, unique = False }

text :: FieldType Text
text = TextField { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False, unique = False }

int :: FieldType Text
int = IntField { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False, unique = False }

enum :: [Text] -> FieldType Text
enum values = EnumField { defaultValue = Nothing, references = Nothing, values, allowNull = False, isPrimaryKey = False, unique = False }

bool :: FieldType Text
bool = BoolField { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False, unique = False }

timestamp :: FieldType Text
timestamp = Timestamp { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False, unique = False }

point :: FieldType Text
point = PointField { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False, unique = False }

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
        Nothing -> validateOnDelete database table field
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
