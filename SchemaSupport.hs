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

    data FieldType = SerialField { defaultValue :: Maybe DefaultValue }
               | TextField { defaultValue :: Maybe DefaultValue }
               | IntField { defaultValue :: Maybe DefaultValue, references :: Maybe Text }
               | BoolField { defaultValue :: Maybe DefaultValue }
               | EnumField { defaultValue :: Maybe DefaultValue,  values :: [Text] }
               | UUIDField { defaultValue :: Maybe DefaultValue, references :: Maybe Text  }
               | Timestamp { defaultValue :: Maybe DefaultValue }
               deriving (Show, Eq, Ord)

    table :: Text -> Table
    table name = Table name []

    field = Field

    (Table name fields) + field = Table name (fields <> [field])

    serial = SerialField { defaultValue = Just (SqlDefaultValue "DEFAULT") }
    uuid = UUIDField { defaultValue = Nothing, references = Nothing }
    primaryKey = uuid { defaultValue = Just (SqlDefaultValue "uuid_generate_v4()") }
    text = TextField { defaultValue = Nothing }
    int = IntField { defaultValue = Nothing, references = Nothing }
    enum values = EnumField { defaultValue = Nothing, values }
    bool = BoolField { defaultValue = Nothing }
    timestamp = Timestamp { defaultValue = Nothing }

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