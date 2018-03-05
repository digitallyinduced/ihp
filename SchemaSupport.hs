module Foundation.SchemaSupport where
    import ClassyPrelude hiding (length)

    data Table = Table Text [Attribute]
               deriving (Show, Eq, Ord)

    data Attribute = Field Name FieldType
                   | BelongsTo Name
                   | HasMany Table
                   deriving (Show, Eq, Ord)

    newtype SqlType = SqlType Text
    type Name = Text

    data FieldType = SerialField
               | TextField { length ::  Int }
               | IntField
               | BoolField
               | EnumField { values :: [Text] }
               deriving (Show, Eq, Ord)

    table :: Text -> Table
    table name = Table name []

    field = Field

    (Table name fields) + field = Table name (fields <> [field])

    serial = SerialField
    text = TextField { length = 64 }
    int = IntField
    enum = EnumField
    bool = BoolField

    belongsTo = BelongsTo
    hasMany = HasMany

    createdAt = field "created_at" int
    updatedAt = field "updated_at" int
