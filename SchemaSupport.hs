module Foundation.SchemaSupport where
import ClassyPrelude hiding (length)
import Data.Maybe (fromJust)
import qualified Data.List as List

data Table = Table Text [Attribute]
           deriving (Show, Eq, Ord)

data Attribute = Field Name FieldType
               | BelongsTo Name
               | HasMany { name :: Name, inverseOf :: Maybe Name }
               deriving (Show, Eq, Ord)

newtype SqlType = SqlType Text
type Name = Text

data DefaultValue = SqlDefaultValue Text deriving (Show, Eq, Ord)

data OnDelete = NoAction | Restrict | SetNull | Cascade deriving (Show, Eq, Ord)

data FieldType =
             SerialField { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | TextField { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | IntField { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | BoolField { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | EnumField { defaultValue :: Maybe DefaultValue,  references :: Maybe Text, values :: [Text], allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | UUIDField { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, onDelete :: OnDelete, unique :: Bool }
           | Timestamp { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | PointField { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           deriving (Show, Eq, Ord)

table :: Text -> Table
table name = Table name []

field = Field

(Table name fields) + field = Table name (fields <> [field])

serial = SerialField { defaultValue = Just (SqlDefaultValue "DEFAULT"), references = Nothing, allowNull = False, isPrimaryKey = True, unique = False }
uuid = UUIDField { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False, onDelete = NoAction, unique = False }
primaryKey = uuid { defaultValue = Just (SqlDefaultValue "uuid_generate_v4()"), references = Nothing, allowNull = False, isPrimaryKey = True, unique = False }
text = TextField { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False, unique = False }
int = IntField { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False, unique = False }
enum values = EnumField { defaultValue = Nothing, references = Nothing, values, allowNull = False, isPrimaryKey = False, unique = False }
bool = BoolField { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False, unique = False }
timestamp = Timestamp { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False, unique = False }
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