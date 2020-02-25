module TurboHaskell.SchemaCompiler where
import ClassyPrelude
import Data.String.Conversions (cs)
import TurboHaskell.SchemaSupport
import TurboHaskell.NameSupport (tableNameToModelName, columnNameToFieldName)
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified System.Directory as Directory
import qualified Data.Set
import Data.List ((!!), (\\))
import Data.List.Split
import qualified TurboHaskell.SqlCompiler
import TurboHaskell.SchemaTypes
import TurboHaskell.HaskellSupport
import qualified Text.Countable as Countable


-- USE LINE PRAGMA IN OUTPUT
--{-# LINE 42 "Foo.vhs" #-}

haskellType :: Table -> Attribute -> Text
haskellType table (Field fieldName field) =
    let
        atomicType = 
            case field of
                SerialField {} -> "Int"
                TextField {}   -> "Text"
                IntField {}    -> "Int"
                EnumField {}   -> tableNameToModelName fieldName
                BoolField {}   -> "Bool"
                Timestamp {}   -> "UTCTime"
                UUIDField {}   -> "UUID"
                PointField {}  -> "Point"
        actualType =
            if isPrimaryKey field
                then "(" <> primaryKeyTypeName table <> ")"
                else
                    if isJust (references field)
                        then "(" <> primaryKeyTypeName' (fromJust (references field)) <> ")"
                        else atomicType
    in if allowNull field then "(Maybe " <> actualType <> ")" else actualType
haskellType table (HasMany {name}) = "(QueryBuilder.QueryBuilder " <> tableNameToModelName name <> ")"

compile :: [Table] -> IO ()
compile database = do
    let validationErrors = validate database
    when (not (null validationErrors)) (error $ "Schema.hs contains errors: " <> cs (unsafeHead validationErrors))
    writeTable (getTypesFilePath, compileTypes database)
    TurboHaskell.SqlCompiler.main database


writeTable :: (FilePath, Text) -> IO ()
writeTable (path, content) = do
    alreadyExists <- Directory.doesFileExist path
    existingContent <- if alreadyExists then readFile path else return ""
    when (existingContent /= cs content) $ do
        putStrLn $ "Updating " <> cs path
        writeFile (cs path) (cs content)

writeStub :: (FilePath, Text) -> IO ()
writeStub (path, content) = do
    stubExists <- Directory.doesFileExist (cs path)
    if not stubExists then writeFile (cs path) (cs content) else return ()

section = "\n"

compileTypes :: [Table] -> Text
compileTypes database =
        prelude
        <> "\n\n"
        <> intercalate "\n\n" (map compileGeneric2DataDefinition database)
        <> intercalate "\n\n" (map (compileTypes' database) database)
        <> section
    where
        prelude = "-- This file is auto generated and will be overriden regulary. Please edit `Application/Schema.hs` to customize the Types"
                  <> section
                  <> "{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, TypeFamilies, DataKinds, TypeOperators, UndecidableInstances, ConstraintKinds, ImpredicativeTypes, StandaloneDeriving  #-}"
                  <> section
                  <> "module Generated.Types where\n\n"
                  <> "import TurboHaskell.HaskellSupport\n"
                  <> "import TurboHaskell.ModelSupport\n"
                  <> "import TurboHaskell.SchemaTypes\n"
                  <> "import ClassyPrelude hiding (id) \n"
                  <> "import Database.PostgreSQL.Simple\n"
                  <> "import Database.PostgreSQL.Simple.FromRow\n"
                  <> "import Database.PostgreSQL.Simple.FromField hiding (Field, name)\n"
                  <> "import Database.PostgreSQL.Simple.ToField hiding (Field)\n"
                  <> "import TurboHaskell.Controller.Param (ParamName (..))\n"
                  <> "import qualified Data.Function\n"
                  <> "import GHC.TypeLits\n"
                  <> "import Data.UUID (UUID)\n"
                  <> "import Data.Default\n"
                  <> "import qualified TurboHaskell.QueryBuilder as QueryBuilder\n"
                  <> "import qualified Data.Proxy\n"
                  <> "import GHC.Records\n"
                  <> "import qualified TurboHaskell.ValidationSupport\n"
                  <> "import TurboHaskell.DatabaseSupport.Point\n"
                  <> "import Data.Data\n"
                  <> "import qualified Control.Applicative\n"
                  <> "import Database.PostgreSQL.Simple.Types (Query (Query))\n"

compileTypes' database table@(Table name attributes) =
    "-- Types for " <> cs name <> "\n\n"
    <> compileTypeAlias table
    <> compileNewTypeAlias table
    <> compileEnumDataDefinitions table
    <> section
    <> compileFromRowInstance table database
    <> section

    <> section
    <> compileHasTableNameInstance table
    <> section
    <> compileGetModelName table
    <> section
    <> section
    <> compileInclude table
    <> section
    <> section
    <> compileCreate table
    <> section
    <> compileUpdate table
    <> section
    <> compileBuild table
    <> section
    <> compileSetFieldInstances table
    <> section
    <> compileUpdateFieldInstances table
    <> section


getTypesFilePath :: FilePath
getTypesFilePath = "build/Generated/Types.hs"

compileTypeAlias :: Table -> Text
compileTypeAlias table@(Table name attributes) =
        "type " <> tableNameToModelName name <> " = " <> tableNameToModelName name <> "' " <> intercalate " " (map (haskellType table) attributes) <> "\n"



compileNewTypeAlias :: Table -> Text
compileNewTypeAlias table@(Table name attributes) =
        "type New" <> tableNameToModelName name <> " = " <> tableNameToModelName name <> "' " <> intercalate " " (map compileAttribute attributes) <> "\n"
        <> "type NewOrSaved" <> tableNameToModelName name <> " " <> newOrSavedArgs <> " = " <> tableNameToModelName name <> "' " <> intercalate " " (map compileNewOrSavedAttribute attributes) <> "\n"
        <> "type instance New (" <> compileTypePattern table <> ") = New" <> tableNameToModelName name <> "\n"
        <> "type instance GetModelByTableName " <> tshow name <> " = " <> tableNameToModelName name <> "\n"
    where
        compileAttribute field@(Field fieldName fieldType) | isJust (defaultValue fieldType) = "(FieldWithDefault " <> haskellType table field <> ")"
        compileAttribute field = haskellType table field

        compileNewOrSavedAttribute field@(Field fieldName fieldType) | isJust (defaultValue fieldType) = fieldName
        compileNewOrSavedAttribute field = haskellType table field

        newOrSavedArgs :: Text
        newOrSavedArgs =
            attributes
            |> filter hasDefaultValue
            |> map (\(Field fieldName _) -> fieldName)
            |> unwords 

        hasDefaultValue (Field _ fieldType) | isJust (defaultValue fieldType) = True
        hasDefaultValue _ = False


compileNewOrSavedType :: Table -> Text
compileNewOrSavedType table@(Table name attributes) =
        "" <> tableNameToModelName name <> "' " <> intercalate " " (map compileAttribute attributes)
    where
        compileAttribute :: Attribute -> Text
        compileAttribute (Field fieldName _) = fieldName
        compileAttribute (HasMany {name}) = name

primaryKeyTypeName :: Table -> Text
primaryKeyTypeName (Table name _) = primaryKeyTypeName' name

primaryKeyTypeName' :: Text -> Text
primaryKeyTypeName' name = "Id' " <> tshow name <> ""

compileGeneric2DataDefinition :: Table -> Text
compileGeneric2DataDefinition table@(Table name attributes) =
        "data " <> tableNameToModelName name <> "' " <> typeArguments <> " = " <> tableNameToModelName name <> " {" <> compileFields attributes <> ", meta :: MetaBag } deriving (Eq, Show)\n"
    where
        typeArguments :: Text
        typeArguments = intercalate " " (map compileTypeArgument attributes)
        compileTypeArgument :: Attribute -> Text
        compileTypeArgument (Field fieldName _) = fieldName
        compileTypeArgument (HasMany { name }) = name
        compileFields :: [Attribute] -> Text
        compileFields attributes = intercalate ", " $ map compileField (zip attributes [0..])
        compileField :: (Attribute, Int) -> Text
        
        compileField (attribute, n) = columnNameToFieldName (compileTypeArgument attribute) <> " :: " <> compileTypeArgument attribute


compileEnumDataDefinitions :: Table -> Text
compileEnumDataDefinitions table@(Table name attributes) =
        (intercalate "\n" (map compileEnumField enumFields))
        <> section
        <> section
        <> (intercalate "\n" (map compileFromFieldInstance enumFields))
        <> section
        <> section
        <> (intercalate "\n" (map compileToFieldInstance enumFields))
        <> section
        <> section
        <> (intercalate "\n" (map compileInputValueInstance enumFields))
        <> section
        <> (intercalate "\n" (map compileDefaultInstance enumFields))
        <> section
    where
        isEnumField (Field _ (EnumField {})) = True
        isEnumField _ = False
        enumFields = filter isEnumField attributes
        compileEnumField (Field fieldName (EnumField {values})) = "data " <> tableNameToModelName fieldName <> " = " <> (intercalate " | " (map tableNameToModelName values)) <> " deriving (Eq, Show, Read, Enum)"
        compileFromFieldInstance (Field fieldName (EnumField {values})) = "instance FromField " <> tableNameToModelName fieldName <> " where\n" <> indent (intercalate "\n" ((map compileFromFieldInstanceForValue values) <> [compileFromFieldInstanceForError, compileFromFieldInstanceForNull]))
        compileFromFieldInstanceForValue value = "fromField field (Just " <> tshow value <> ") = return " <> tableNameToModelName value
        compileFromFieldInstanceForError = "fromField field (Just value) = returnError ConversionFailed field \"Unexpected value for enum value\""
        compileFromFieldInstanceForNull = "fromField field Nothing = returnError UnexpectedNull field \"Unexpected null for enum value\""
        compileDefaultInstance (Field fieldName (EnumField {values})) = "instance Default " <> tableNameToModelName fieldName <> " where def = " <> tableNameToModelName (unsafeHead values) <> "\n"

        compileToFieldInstance (Field fieldName (EnumField { values })) = "instance ToField " <> tableNameToModelName fieldName <> " where\n" <> indent (intercalate "\n" (map compileToFieldInstanceForValue values))
        compileToFieldInstanceForValue value = "toField " <> tableNameToModelName value <> " = toField (" <> tshow value <> " :: Text)"

        compileInputValueInstance (Field fieldName (EnumField { values })) = "instance InputValue " <> tableNameToModelName fieldName <> " where\n" <> indent (intercalate "\n" (map compileInputValue values))
        compileInputValue value = "inputValue " <> tableNameToModelName value <> " = " <> tshow value <> " :: Text"

compileToRowValues bindingValues = if (ClassyPrelude.length bindingValues == 1) then "Only (" <> (unsafeHead bindingValues) <> ")" else "(" <> intercalate ") :. (" (map (\list -> if ClassyPrelude.length list == 1 then "Only (" <> (unsafeHead list) <> ")" else intercalate ", " list) (chunksOf 8 bindingValues)) <> ")"


fieldNames :: [Attribute] -> [Text]
fieldNames = map (\(Field fieldName _) -> fieldName) . fieldsOnly

compileCreate table@(Table name attributes) =
    let
        modelName = tableNameToModelName name
        columns = intercalate ", " $ fieldNames attributes
        values = intercalate ", " $ map toValue $ fieldsOnly attributes
        toValue (Field fieldName fieldType) =
            case defaultValue fieldType of
                Just (SqlDefaultValue sql) -> sql
                otherwise   -> "?"

        toBinding modelName (Field fieldName fieldType) =
            case defaultValue fieldType of
                Just (SqlDefaultValue _) -> Nothing
                otherwise   -> Just ("let " <> modelName <> "{" <> columnNameToFieldName fieldName <> "} = model in " <> columnNameToFieldName fieldName)
        bindings :: [Text]
        bindings = let bindingValues = map fromJust $ filter isJust (map (toBinding modelName) $ fieldsOnly attributes) in bindingValues
    in
        "instance CanCreate New" <> modelName <> " where\n"
        <> indent (
            "create :: (?modelContext :: ModelContext) => New" <> modelName <> " -> IO " <> modelName <> "\n"
                <> "type Created New" <> modelName <> " = " <> modelName <> "\n"
                <> "create model = do\n"
                <> indent ("let (ModelContext conn) = ?modelContext\n"
                    <> "result <- Database.PostgreSQL.Simple.query conn \"INSERT INTO " <> name <> " (" <> columns <> ") VALUES (" <> values <> ") RETURNING *\" (" <> compileToRowValues bindings <> ")\n"
                    <> "return (unsafeHead result)\n"
                    )
                <> "createMany models = do\n"
                <> indent ("let (ModelContext conn) = ?modelContext\n"
                    <> "Database.PostgreSQL.Simple.query conn (Query $ \"INSERT INTO " <> name <> " (" <> columns <> ") VALUES \" <> (intercalate \", \" (map (\\_ -> \"(" <> values <> ")\") models)) <> \" RETURNING *\") (concat $ map (\\model -> [" <> (intercalate ", " (map (\b -> "toField (" <> b <> ")") bindings)) <> "]) models)\n"
                    )
            )

toBinding modelName attribute = Just ("let " <> modelName <> "{" <> columnNameToFieldName fieldName <> "} = model in " <> columnNameToFieldName fieldName)
    where
        fieldName =
            case attribute of
                Field fieldName _ -> fieldName
                HasMany {name} -> name

compileUpdate table@(Table name attributes) =
    let
        modelName = tableNameToModelName name
        columns = intercalate ", " $ map toColumn attributes
        toColumn (Field fieldName fieldType) = fieldName
        values = intercalate ", " $ map toValue attributes
        toValue (Field fieldName fieldType) =
            case defaultValue fieldType of
                Just (SqlDefaultValue sql) -> sql
                Nothing -> "?"
        bindings :: Text
        bindings =
            let
                bindingValues = (map fromJust $ filter isJust (map (toBinding modelName) $ fieldsOnly attributes)) <> (["getField @\"id\" model"])
            in
                compileToRowValues bindingValues

        updates = intercalate ", " (map fromJust $ filter isJust $ map update $ fieldsOnly attributes)
        update (Field fieldName fieldType) =
            case fieldType of
                SerialField {} -> Nothing
                otherwise -> Just $ fieldName <> " = ?"
    in
        "instance CanUpdate " <> modelName <> " where\n"
        <> indent ("updateRecord model = do\n"
                <> indent ("let (ModelContext conn) = ?modelContext\n"
                    <> "result <- Database.PostgreSQL.Simple.query conn \"UPDATE " <> name <> " SET " <> updates <> " WHERE id = ? RETURNING *\" (" <> bindings <> ")\n"
                    <> "return (unsafeHead result)\n"
                )
            )


compileFromRowInstance table@(Table name attributes) database =
    defaultFromRow
    -- <> "instance FromRow " <> tableNameToModelName name <> " where "<> (indent "fromRow = " <> tableNameToModelName name <> " <$> " <>  (intercalate " <*> " $ map (const "field") $ fieldsOnly attributes))
    where
        defaultFromRow =
            "instance FromRow " <> tableNameToModelName name <> " where "
            <> ("fromRow = do model <- " <> modelConstructor <> " <$> " <>  (intercalate " <*> " $ map (const "field") $ fieldsOnly attributes)) <> "; return " <> tableNameToModelName name <> " { " <> intercalate ", " (map compileQuery attributes) <> ", meta = def }\n"
        modelConstructor = "(\\" <> intercalate " " (map compileParam $ fieldsOnly attributes) <> " -> " <> tableNameToModelName name <> " " <> intercalate " " (map compileValue attributes) <> " def)"
        compileParam (Field fieldName _) = fieldName
        compileParam (HasMany {name}) = name
        compileValue field@(Field _ _) = compileParam field
        compileValue (HasMany {}) = "()"

        compileQuery field@(Field fieldName _) = columnNameToFieldName fieldName <> " = (" <> (fromJust $ toBinding (tableNameToModelName name) field) <> ")"
        compileQuery (HasMany hasManyName inverseOf) = columnNameToFieldName hasManyName <> " = (QueryBuilder.filterWhere (Data.Proxy.Proxy @" <> tshow relatedFieldName <> ", " <> (fromJust $ toBinding' (tableNameToModelName name) relatedIdField)  <> ") (QueryBuilder.query @" <> tableNameToModelName hasManyName <>"))"
            where
                compileInverseOf Nothing = (columnNameToFieldName (singularize name)) <> "Id"
                compileInverseOf (Just name) = columnNameToFieldName (singularize name)
                relatedFieldName = compileInverseOf inverseOf
                relatedIdField = relatedField "id"
                relatedForeignKeyField = relatedField relatedFieldName

                relatedField :: Text -> Attribute
                relatedField relatedFieldName =
                    let
                        isFieldName name (Field fieldName _) = (columnNameToFieldName fieldName) == name
                        (Table _ attributes) = relatedTable
                    in case find (isFieldName relatedFieldName) (fieldsOnly attributes) of
                        Just a -> a
                        Nothing ->
                            let (Table tableName _) = relatedTable
                            in error (
                                    "Could not find field "
                                    <> show relatedFieldName
                                    <> " in table"
                                    <> cs tableName
                                    <> " "
                                    <> (show $ fieldsOnly attributes)
                                    <> ".\n\nThis is caused by `+ hasMany " <> show hasManyName <> "`"
                                )
                relatedTable = case find (\(Table tableName _) -> tableName == hasManyName) database of
                    Just t -> t
                    Nothing -> error ("Could not find table " <> show hasManyName)

                toBinding' modelName attributes =
                    case relatedForeignKeyField of
                        Field _ fieldType | allowNull fieldType -> Just $ "Just (" <> fromJust (toBinding modelName attributes) <> ")"
                        otherwise -> toBinding modelName attributes

compileBuild :: Table -> Text
compileBuild table@(Table name attributes) =
        "instance Record New" <> tableNameToModelName name <> " where\n"
        <> "    {-# INLINE newRecord #-}\n"
        <> "    newRecord = " <> tableNameToModelName name <> " " <> intercalate " " (map (const "def") attributes) <> " def\n"



compileHasTableNameInstance table@(Table name attributes) = "\ntype instance GetTableName (" <> tableNameToModelName name <> "' " <> intercalate " " (map (const "_") attributes) <>  ") = " <> tshow name <> "\n"

compileGetModelName table@(Table name attributes) = "\ntype instance GetModelName (" <> tableNameToModelName name <> "' " <> intercalate " " (map (const "_") attributes) <>  ") = " <> tshow (tableNameToModelName name) <> "\n"

compileDataTypePattern :: Table -> Text
compileDataTypePattern table@(Table name attributes) = tableNameToModelName name <> " " <> intercalate " " (map compileAttribute attributes)
    where
        compileAttribute :: Attribute -> Text
        compileAttribute (Field name _) = name
        compileAttribute (HasMany {name}) = name


compileTypePattern :: Table -> Text
compileTypePattern table@(Table name attributes) = tableNameToModelName name <> "' " <> intercalate " " (map compileAttribute attributes)
    where
        compileAttribute :: Attribute -> Text
        compileAttribute (Field name _) = name
        compileAttribute (HasMany {name}) = name

compileInclude table@(Table tableName attributes) = intercalate "\n" $ map compileInclude' (filter isRef attributes)
    where
        isRef :: Attribute -> Bool
        isRef (Field _ fieldType) = isJust (references fieldType)
        isRef (HasMany {}) = True

        compileInclude' :: Attribute -> Text
        compileInclude' attribute = "type instance Include " <> tshow (columnNameToFieldName fieldName) <> " (" <> leftModelType <> ") = " <> rightModelType <> "\n"
            where
                leftModelType :: Text
                leftModelType = intercalate " " $ (tableNameToModelName tableName <> "'"):(map compileTypeVariable attributes)
                rightModelType :: Text
                rightModelType = intercalate " " $ (tableNameToModelName tableName <> "'"):(map compileTypeVariable' attributes)
                compileTypeVariable :: Attribute -> Text
                compileTypeVariable (Field fieldName _) = fieldName
                compileTypeVariable (HasMany {name}) = name
                compileTypeVariable' :: Attribute -> Text
                compileTypeVariable' (Field fieldName' _) | fieldName' == fieldName = "(GetModelById " <> fieldName' <> ")"
                compileTypeVariable' (HasMany {name}) | name == fieldName = "[" <> tableNameToModelName (singularize name) <> "]"
                compileTypeVariable' otherwise = compileTypeVariable otherwise
                fieldName =
                    case attribute of
                        (Field fieldName _) -> fieldName
                        (HasMany {name}) -> name 


compileSetFieldInstances :: Table -> Text
compileSetFieldInstances table@(Table tableName attributes) = intercalate "\n" (map compileSetField (map getFieldName attributes <> ["meta"]))
    where
        getFieldName (Field fieldName _) = fieldName
        getFieldName (HasMany fieldName _) = fieldName

        compileSetField fieldName = "instance SetField " <> tshow (columnNameToFieldName fieldName) <> " (" <> compileTypePattern table <>  ") " <> setFieldType <> " where\n    {-# INLINE setField #-}\n    setField newValue (" <> compileDataTypePattern table <> " meta) = " <> tableNameToModelName tableName <> " " <> (intercalate " " (map compileAttribute attributes)) <> " " <> (if fieldName == "meta" then "newValue" else "meta")
            where
                setFieldType = if fieldName == "meta" then "MetaBag" else fieldName
                compileAttribute :: Attribute -> Text
                compileAttribute field = compileAttribute' (getFieldName field)
                compileAttribute' name = if fieldName == name then "newValue" else name

compileUpdateFieldInstances :: Table -> Text
compileUpdateFieldInstances table@(Table tableName attributes) = intercalate "\n" (map compileSetField' attributes)
    where
        compileSetField' (Field fieldName _) = compileSetField fieldName
        compileSetField' (HasMany fieldName _) = compileSetField fieldName
        compileSetField fieldName = "instance UpdateField " <> tshow (columnNameToFieldName fieldName) <> " (" <> compileTypePattern table <>  ") (" <> compileTypePattern' table fieldName  <> ") " <> fieldName <> " " <> fieldName <> "' where\n    {-# INLINE updateField #-}\n    updateField newValue (" <> compileDataTypePattern table <> " meta) = " <> tableNameToModelName tableName <> " " <> (intercalate " " (map compileAttribute attributes)) <> " meta"
            where
                compileAttribute :: Attribute -> Text
                compileAttribute (Field name _) = if fieldName == name then "newValue" else name
                compileAttribute (HasMany {name}) = if fieldName == name then "newValue" else name

                compileTypePattern' :: Table -> Text -> Text
                compileTypePattern' table@(Table name attributes) fieldName = tableNameToModelName name <> "' " <> intercalate " " (map (\f -> if f == fieldName then fieldName <> "'" else f) (map compileAttribute' attributes))
                    where
                        compileAttribute' :: Attribute -> Text
                        compileAttribute' (Field name _) = name
                        compileAttribute' (HasMany {name}) = name

--compileAttributeBag :: Table -> Text
--compileAttributeBag table@(Table name attributes) = "class To" <> tableNameToModelName name <> "Attributes where\n    to"
indent :: Text -> Text
indent code =
        intercalate "\n" $ map indentLine $ Text.splitOn "\n" code
    where
        indentLine ""   = ""
        indentLine line = "    " <> line

