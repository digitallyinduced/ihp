module Foundation.SchemaSupport where
    import ClassyPrelude hiding (length)
    import Data.Maybe (fromJust)
    import qualified Data.List as List

    data Table = Table Text [Attribute]
               deriving (Show, Eq, Ord)

    data Attribute = Field Name FieldType
                   | BelongsTo Name
                   | HasMany Table
                   deriving (Show, Eq, Ord)

    newtype SqlType = SqlType Text
    type Name = Text

    data DefaultValue = SqlDefaultValue Text deriving (Show, Eq, Ord)

    data FieldType =
                 SerialField { defaultValue :: Maybe DefaultValue, allowNull :: Bool, isPrimaryKey :: Bool }
               | TextField { defaultValue :: Maybe DefaultValue, allowNull :: Bool, isPrimaryKey :: Bool }
               | IntField { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool }
               | BoolField { defaultValue :: Maybe DefaultValue, allowNull :: Bool, isPrimaryKey :: Bool }
               | EnumField { defaultValue :: Maybe DefaultValue,  values :: [Text], allowNull :: Bool, isPrimaryKey :: Bool }
               | UUIDField { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool  }
               | Timestamp { defaultValue :: Maybe DefaultValue, allowNull :: Bool, isPrimaryKey :: Bool }
               | ArrayField {innerType :: FieldType, defaultValue :: Maybe DefaultValue, allowNull :: Bool, isPrimaryKey :: Bool }
               deriving (Show, Eq, Ord)

    table :: Text -> Table
    table name = Table name []

    field = Field

    (Table name fields) + field = Table name (fields <> [field])

    serial = SerialField { defaultValue = Just (SqlDefaultValue "DEFAULT"), allowNull = False, isPrimaryKey = True }
    uuid = UUIDField { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False }
    primaryKey = uuid { defaultValue = Just (SqlDefaultValue "uuid_generate_v4()"), allowNull = False, isPrimaryKey = True }
    text = TextField { defaultValue = Nothing, allowNull = False, isPrimaryKey = False }
    int = IntField { defaultValue = Nothing, references = Nothing, allowNull = False, isPrimaryKey = False }
    enum values = EnumField { defaultValue = Nothing, values, allowNull = False, isPrimaryKey = False }
    bool = BoolField { defaultValue = Nothing, allowNull = False, isPrimaryKey = False }
    timestamp = Timestamp { defaultValue = Nothing, allowNull = False, isPrimaryKey = False }
    array innerType = ArrayField {innerType, defaultValue = Nothing, allowNull = False, isPrimaryKey = False }

    belongsTo = BelongsTo
    hasMany = HasMany

    createdAt = field "created_at" int
    updatedAt = field "updated_at" int

    validate :: [Table] -> [Text]
    validate database = concat $ map (validateTable database) database

    validateTable :: [Table] -> Table -> [Text]
    validateTable _ (Table "" _) = pure "Table name cannot be empty"
    validateTable _ (Table name []) = pure $ "Table " <> name <> " needs to have atleast one field"
    validateTable database table@(Table name attributes) = catMaybes $ map (validateAttribute database table) attributes

    validateAttribute :: [Table] -> Table -> Attribute -> Maybe Text
    validateAttribute database table (Field fieldName (UUIDField { references })) =
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
    validateAttribute _ _ _ = Nothing
