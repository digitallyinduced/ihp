module Foundation.SchemaSupport where
    import ClassyPrelude hiding (length)
    import Data.Maybe (fromJust)

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

    belongsTo = BelongsTo
    hasMany = HasMany

    createdAt = field "created_at" int
    updatedAt = field "updated_at" int

    validate :: [Table] -> [Text]
    validate = map fromJust . filter isJust . map validateTable

    validateTable :: Table -> Maybe Text
    validateTable (Table "" _) = Just "Table name cannot be empty"
    validateTable (Table name []) = Just $ "Table " <> name <> " needs to have atleast one field"
    validateTable _ = Nothing