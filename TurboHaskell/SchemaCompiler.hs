module TurboHaskell.SchemaCompiler where
import ClassyPrelude
import Data.String.Conversions (cs)
import TurboHaskell.NameSupport (tableNameToModelName, columnNameToFieldName)
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified System.Directory as Directory
import qualified Data.Set
import Data.List ((!!), (\\))
import Data.List.Split
import qualified TurboHaskell.SqlCompiler
import TurboHaskell.HaskellSupport
import qualified Text.Countable as Countable


import qualified TurboHaskell.IDE.SchemaDesigner.Parser as SchemaDesigner
import TurboHaskell.IDE.SchemaDesigner.Types

compile :: IO ()
compile = do
    let options = fullCompileOptions
    SchemaDesigner.parseSchemaSql >>= \case
        Left parserError -> fail (cs parserError)
        Right statements -> do
            -- let validationErrors = validate database
            -- unless (null validationErrors) (error $ "Schema.hs contains errors: " <> cs (unsafeHead validationErrors))
            writeIfDifferent typesFilePath (compileTypes options (Schema statements))
            -- TurboHaskell.SqlCompiler.main database

typesFilePath :: FilePath
typesFilePath = "build/Generated/Types.hs"

singularize word = Countable.singularize word

newtype Schema = Schema [Statement]

data CompilerOptions = CompilerOptions {
        -- | We can toggle the generation of @SetField@ and @GetField@ instances.
        -- This is e.g. disabled when showing the code preview in the schema designer
        -- as it's very noisy and does not add any values. But of course it's needed
        -- when do a compilation for the Types.hs
        compileGetAndSetFieldInstances :: Bool
    }

fullCompileOptions :: CompilerOptions
fullCompileOptions = CompilerOptions { compileGetAndSetFieldInstances = True }

previewCompilerOptions :: CompilerOptions
previewCompilerOptions = CompilerOptions { compileGetAndSetFieldInstances = False }

haskellType :: (?schema :: Schema) => Statement -> Column -> Text
haskellType table Column { name, primaryKey } | primaryKey = "(" <> primaryKeyTypeName table <> ")"
haskellType table column@(Column { columnType, notNull }) =
    let
        atomicType = 
            case columnType of
                "INT" -> "Int"
                "TEXT" -> "Text"
                "BOOL"   -> "Bool"
                "BOOLEAN"   -> "Bool"
                "TIMESTAMP WITH TIME ZONE" -> "UTCTime"
                "UUID" -> "UUID"
                "POINT" -> "Point"
                "FLOAT" -> "Float"
                "DOUBLE PRECISION" -> "Double"
                customType -> tableNameToModelName customType
        actualType =
            case findForeignKeyConstraint table column of
                Just (ForeignKeyConstraint { referenceTable }) -> "(" <> primaryKeyTypeName' referenceTable <> ")"
                _ -> atomicType
    in
        if not notNull
            then "(Maybe " <> actualType <> ")"
            else actualType
-- haskellType table (HasMany {name}) = "(QueryBuilder.QueryBuilder " <> tableNameToModelName name <> ")"


writeIfDifferent :: FilePath -> Text -> IO ()
writeIfDifferent path content = do
    alreadyExists <- Directory.doesFileExist path
    existingContent <- if alreadyExists then readFile path else pure ""
    when (existingContent /= cs content) $ do
        putStrLn $ "Updating " <> cs path
        writeFile (cs path) (cs content)



section = "\n"

compileTypes :: CompilerOptions -> Schema -> Text
compileTypes options schema@(Schema statements) =
        prelude
        <> "\n\n"
        <> let ?schema = schema in intercalate "\n\n" (map (compileStatement options) statements)
        <> section
    where
        prelude = "-- This file is auto generated and will be overriden regulary. Please edit `Application/Schema.hs` to customize the Types"
                  <> section
                  <> "{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, TypeFamilies, DataKinds, TypeOperators, UndecidableInstances, ConstraintKinds, ImpredicativeTypes, StandaloneDeriving  #-}"
                  <> section
                  <> "module Generated.Types where\n\n"
                  <> "import TurboHaskell.HaskellSupport\n"
                  <> "import TurboHaskell.ModelSupport\n"
                  <> "import CorePrelude hiding (id) \n"
                  <> "import Data.Time.Clock \n"
                  <> "import qualified Data.List as List \n"
                  <> "import qualified Data.ByteString as ByteString \n"
                  <> "import Database.PostgreSQL.Simple\n"
                  <> "import Database.PostgreSQL.Simple.FromRow\n"
                  <> "import Database.PostgreSQL.Simple.FromField hiding (Field, name)\n"
                  <> "import Database.PostgreSQL.Simple.ToField hiding (Field)\n"
                  <> "import TurboHaskell.Controller.Param ()\n"
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

compileStatementPreview :: [Statement] -> Statement -> Text
compileStatementPreview statements statement =  let ?schema = Schema statements in compileStatement previewCompilerOptions statement

compileStatement :: (?schema :: Schema) => CompilerOptions -> Statement -> Text
compileStatement CompilerOptions { compileGetAndSetFieldInstances } table@(CreateTable {}) =
    compileGeneric2DataDefinition table
    <> compileTypeAlias table
    <> compileNewTypeAlias table
    <> compileFromRowInstance table
    <> compileHasTableNameInstance table
    <> compileGetModelName table
    <> compileInclude table
    <> compileCreate table
    <> section
    <> compileUpdate table
    <> section
    <> compileBuild table
    <> section
    <> if compileGetAndSetFieldInstances
            then compileSetFieldInstances table <> compileUpdateFieldInstances table
            else ""
    <> section

compileStatement _ enum@(CreateEnumType {}) = compileEnumDataDefinitions enum
compileStatement _ _ = ""

compileTypeAlias :: (?schema :: Schema) => Statement -> Text
compileTypeAlias table@(CreateTable { name, columns }) =
        "type " <> tableNameToModelName name <> " = " <> tableNameToModelName name <> "' " <> unwords (map (haskellType table) (variableAttributes table)) <> "\n"

compileNewTypeAlias :: Statement -> Text
compileNewTypeAlias table@(CreateTable { name, columns }) =
        "type instance GetModelByTableName " <> tshow name <> " = " <> tableNameToModelName name <> "\n"
        <> "type New" <> tableNameToModelName name <> " = " <> tableNameToModelName name <> "\n"

primaryKeyTypeName :: Statement -> Text
primaryKeyTypeName CreateTable { name } = primaryKeyTypeName' name

primaryKeyTypeName' :: Text -> Text
primaryKeyTypeName' name = "Id' " <> tshow name <> ""

compileGeneric2DataDefinition :: (?schema :: Schema) => Statement -> Text
compileGeneric2DataDefinition table@(CreateTable { name, columns }) =
        "data " <> tableNameToModelName name <> "' " <> typeArguments <> " = " <> tableNameToModelName name <> " {" <> compileFields <> ", meta :: MetaBag } deriving (Eq, Show)\n"
    where
        vars = variableAttributes table

        typeArguments :: Text
        typeArguments = unwords (map (get #name) vars)
        -- compileTypeArgument (HasMany { name }) = name
        compileFields :: Text
        compileFields = intercalate ", " (map compileField columns)
        compileField :: Column -> Text
        compileField column = columnNameToFieldName (get #name column) <> " :: " <> (if isVariableAttribute table column then get #name column else haskellType table column)

variableAttributes :: (?schema :: Schema) => Statement -> [Column]
variableAttributes table@(CreateTable { columns }) = filter (isVariableAttribute table) columns

isVariableAttribute :: (?schema :: Schema) => Statement -> Column -> Bool
isVariableAttribute = isRefCol


-- | Returns @True@ when the coluns is referencing another column via foreign key constraint
isRefCol :: (?schema :: Schema) => Statement -> Column -> Bool
isRefCol table column = isJust (findForeignKeyConstraint table column)

-- | Returns the foreign key constraint bound on the given column
findForeignKeyConstraint :: (?schema :: Schema) => Statement -> Column -> Maybe Constraint
findForeignKeyConstraint CreateTable { name } column =
        case find isFkConstraint statements of
            Just (AddConstraint { constraint }) -> Just constraint
            Nothing -> Nothing
    where
        isFkConstraint (AddConstraint { tableName, constraint = ForeignKeyConstraint { columnName }}) = tableName == name && columnName == get #name column
        isFkConstraint _ = False

        (Schema statements) = ?schema

compileEnumDataDefinitions :: Statement -> Text
compileEnumDataDefinitions enum@(CreateEnumType { name, values }) =
        "data " <> modelName <> " = " <> (intercalate " | " valueConstructors) <> " deriving (Eq, Show, Read, Enum)\n"
        <> "instance FromField " <> modelName <> " where\n"
        <> indent (unlines (map compileFromFieldInstanceForValue values))
        <> "    fromField field (Just value) = returnError ConversionFailed field \"Unexpected value for enum value\""
        <> "    fromField field Nothing = returnError UnexpectedNull field \"Unexpected null for enum value\"\n"
        <> "instance Default " <> modelName <> " where def = " <> tableNameToModelName (unsafeHead values) <> "\n"
        <> "instance ToField " <> modelName <> " where\n" <> indent (unlines (map compileToFieldInstanceForValue values))
        <> "instance InputValue " <> modelName <> " where\n" <> indent (unlines (map compileInputValue values)) <> "\n"
    where
        modelName = tableNameToModelName name
        valueConstructors = map tableNameToModelName values
        compileFromFieldInstanceForValue value = "fromField field (Just " <> tshow value <> ") = pure " <> tableNameToModelName value
        compileToFieldInstanceForValue value = "toField " <> tableNameToModelName value <> " = toField (" <> tshow value <> " :: Text)"
        compileInputValue value = "inputValue " <> tableNameToModelName value <> " = " <> tshow value <> " :: Text"

compileToRowValues :: [Text] -> Text
compileToRowValues bindingValues | length bindingValues == 1 = "Only (" <> (unsafeHead bindingValues) <> ")"
compileToRowValues bindingValues = "(" <> intercalate ") :. (" (map (\list -> if length list == 1 then "Only (" <> (unsafeHead list) <> ")" else intercalate ", " list) (chunksOf 8 bindingValues)) <> ")"


compileCreate :: Statement -> Text
compileCreate table@(CreateTable { name, columns }) =
    let
        modelName = tableNameToModelName name
        columnNames = commaSep (map (get #name) columns)
        values = commaSep (map toValue columns)

        toValue Column { defaultValue = Just theDefaultValue } = theDefaultValue
        toValue _ = "?"

        toBinding Column { defaultValue = Just theDefaultValue } = Nothing
        toBinding Column { name } = Just $ "let " <> modelName <> "{" <> columnNameToFieldName name <> "} = model in " <> columnNameToFieldName name

        bindings :: [Text]
        bindings = mapMaybe toBinding columns
    in
        "instance CanCreate " <> modelName <> " where\n"
        <> indent (
            "create :: (?modelContext :: ModelContext) => " <> modelName <> " -> IO " <> modelName <> "\n"
                <> "create model = do\n"
                <> indent ("let (ModelContext conn) = ?modelContext\n"
                    <> "result <- Database.PostgreSQL.Simple.query conn \"INSERT INTO " <> name <> " (" <> columnNames <> ") VALUES (" <> values <> ") RETURNING *\" (" <> compileToRowValues bindings <> ")\n"
                    <> "pure (List.head result)\n"
                    )
                <> "createMany models = do\n"
                <> indent ("let (ModelContext conn) = ?modelContext\n"
                    <> "Database.PostgreSQL.Simple.query conn (Query $ \"INSERT INTO " <> name <> " (" <> columnNames <> ") VALUES \" <> (ByteString.intercalate \", \" (List.map (\\_ -> \"(" <> values <> ")\") models)) <> \" RETURNING *\") (List.concat $ List.map (\\model -> [" <> (intercalate ", " (map (\b -> "toField (" <> b <> ")") bindings)) <> "]) models)\n"
                    )
            )

commaSep = intercalate ", "

toBinding :: Text -> Column -> Text
toBinding modelName Column { name } = "let " <> modelName <> "{" <> columnNameToFieldName name <> "} = model in " <> columnNameToFieldName name

compileUpdate :: Statement -> Text
compileUpdate table@(CreateTable { name, columns }) =
    let
        modelName = tableNameToModelName name
        values = commaSep (map toValue columns)

        toValue Column { defaultValue = Just theDefaultValue } = theDefaultValue
        toValue _ = "?"

        bindings :: Text
        bindings =
            let
                bindingValues = (map (toBinding modelName) columns) <> (["getField @\"id\" model"])
            in
                compileToRowValues bindingValues

        updates = commaSep (map (\column -> get #name column <> " = ?") columns)
    in
        "instance CanUpdate " <> modelName <> " where\n"
        <> indent ("updateRecord model = do\n"
                <> indent ("let (ModelContext conn) = ?modelContext\n"
                    <> "result <- Database.PostgreSQL.Simple.query conn \"UPDATE " <> name <> " SET " <> updates <> " WHERE id = ? RETURNING *\" (" <> bindings <> ")\n"
                    <> "pure (List.head result)\n"
                )
            )


compileFromRowInstance :: (?schema :: Schema) => Statement -> Text
compileFromRowInstance table@(CreateTable { name, columns }) =
            "instance FromRow " <> modelName <> " where "
            <> ("fromRow = do model <- " <> modelConstructor <> " <$> " <>  (intercalate " <*> " $ map (const "field") columns)) <> "; pure " <> modelName <> " { " <> intercalate ", " (map compileQuery columns) <> ", meta = def }\n"
    where
        modelName = tableNameToModelName name
        modelConstructor = "(\\" <> unwords (map (get #name) columns) <> " -> " <> modelName <> " " <> unwords (map (get #name) columns) <> " def)"


        compileQuery column@(Column { name }) = columnNameToFieldName name <> " = (" <> toBinding modelName column <> ")"
        --compileQuery (HasMany hasManyName inverseOf) = columnNameToFieldName hasManyName <> " = (QueryBuilder.filterWhere (Data.Proxy.Proxy @" <> tshow relatedFieldName <> ", " <> (fromJust $ toBinding' (tableNameToModelName name) relatedIdField)  <> ") (QueryBuilder.query @" <> tableNameToModelName hasManyName <>"))"
        --    where
        --        compileInverseOf Nothing = (columnNameToFieldName (singularize name)) <> "Id"
        --        compileInverseOf (Just name) = columnNameToFieldName (singularize name)
        --        relatedFieldName = compileInverseOf inverseOf
        --        relatedIdField = relatedField "id"
        --        relatedForeignKeyField = relatedField relatedFieldName
        --        relatedField :: Text -> Attribute
        --        relatedField relatedFieldName =
        --            let
        --                isFieldName name (Field fieldName _) = (columnNameToFieldName fieldName) == name
        --                (Table _ attributes) = relatedTable
        --            in case find (isFieldName relatedFieldName) (fieldsOnly attributes) of
        --                Just a -> a
        --                Nothing ->
        --                    let (Table tableName _) = relatedTable
        --                    in error (
        --                            "Could not find field "
        --                            <> show relatedFieldName
        --                            <> " in table"
        --                            <> cs tableName
        --                            <> " "
        --                            <> (show $ fieldsOnly attributes)
        --                            <> ".\n\nThis is caused by `+ hasMany " <> show hasManyName <> "`"
        --                        )
        --        relatedTable = case find (\(Table tableName _) -> tableName == hasManyName) database of
        --            Just t -> t
        --            Nothing -> error ("Could not find table " <> show hasManyName)
        --        toBinding' modelName attributes =
        --            case relatedForeignKeyField of
        --                Field _ fieldType | allowNull fieldType -> Just $ "Just (" <> fromJust (toBinding modelName attributes) <> ")"
        --                otherwise -> toBinding modelName attributes

compileBuild :: Statement -> Text
compileBuild table@(CreateTable { name, columns }) =
        "instance Record " <> tableNameToModelName name <> " where\n"
        <> "    {-# INLINE newRecord #-}\n"
        <> "    newRecord = " <> tableNameToModelName name <> " " <> unwords (map toDefaultValueExpr columns) <> " def\n"


toDefaultValueExpr :: Column -> Text
toDefaultValueExpr Column { columnType, notNull, defaultValue = Just theDefaultValue } =
            let
                wrapNull False value = "(Just " <> value <> ")"
                wrapNull True value = value
            in
                if theDefaultValue == "null"
                    then "Nothing"
                    else
                        case columnType of
                            "TEXT" -> wrapNull notNull (tshow theDefaultValue)
                            "BOOl" -> wrapNull notNull (tshow (toLower theDefaultValue == "true"))
                            _ -> "def"
toDefaultValueExpr _ = "def"

compileHasTableNameInstance :: (?schema :: Schema) => Statement -> Text
compileHasTableNameInstance table@(CreateTable { name }) = "type instance GetTableName (" <> tableNameToModelName name <> "' " <> unwords (map (const "_") (variableAttributes table)) <>  ") = " <> tshow name <> "\n"

compileGetModelName :: (?schema :: Schema) => Statement -> Text
compileGetModelName table@(CreateTable { name }) = "type instance GetModelName (" <> tableNameToModelName name <> "' " <> unwords (map (const "_") (variableAttributes table)) <>  ") = " <> tshow (tableNameToModelName name) <> "\n"

compileDataTypePattern :: Statement -> Text
compileDataTypePattern CreateTable { name, columns} = tableNameToModelName name <> " " <> unwords (map (get #name) columns)


compileTypePattern :: (?schema :: Schema) => Statement -> Text
compileTypePattern table@(CreateTable { name }) = tableNameToModelName name <> "' " <> unwords (map (get #name) (variableAttributes table))

compileInclude :: (?schema :: Schema) => Statement -> Text
compileInclude table@(CreateTable { name, columns }) = unlines (map compileInclude' (filter (isRefCol table) columns))
    where
        varAttributes = variableAttributes table
        modelName = tableNameToModelName name
        modelConstructor = modelName <> "'"

        compileInclude' :: Column -> Text
        compileInclude' column = "type instance Include " <> tshow (columnNameToFieldName (get #name column)) <> " (" <> leftModelType <> ") = " <> rightModelType <> "\n"
            where
                leftModelType :: Text
                leftModelType = unwords (modelConstructor:(map (get #name) varAttributes))
                rightModelType :: Text
                rightModelType = unwords (modelConstructor:(map compileTypeVariable' varAttributes))
                compileTypeVariable' :: Column -> Text
                compileTypeVariable' Column { name} | name == get #name column = "(GetModelById " <> name <> ")"
                -- compileTypeVariable' (HasMany {name}) | name == fieldName = "[" <> tableNameToModelName (singularize name) <> "]"
                compileTypeVariable' otherwise = get #name otherwise


compileSetFieldInstances :: (?schema :: Schema) => Statement -> Text
compileSetFieldInstances table@(CreateTable { name, columns }) = unlines (map compileSetField columns <> [setMetaField])
    where
        setMetaField = "instance SetField \"meta\" (" <> compileTypePattern table <>  ") MetaBag where\n    {-# INLINE setField #-}\n    setField newValue (" <> compileDataTypePattern table <> " meta) = " <> tableNameToModelName name <> " " <> (unwords (map (get #name) columns)) <> " newValue"

        compileSetField column@(Column { name }) = "instance SetField " <> tshow (columnNameToFieldName name) <> " (" <> compileTypePattern table <>  ") " <> setFieldType <> " where\n    {-# INLINE setField #-}\n    setField newValue (" <> compileDataTypePattern table <> " meta) = " <> tableNameToModelName (get #name table) <> " " <> (unwords (map compileAttribute columns)) <> " meta"
            where
                setFieldType = if isVariableAttribute table column
                    then name
                    else haskellType table column

                compileAttribute Column { name = name' } | name' == name = "newValue"
                compileAttribute Column { name } = name

compileUpdateFieldInstances :: (?schema :: Schema) => Statement -> Text
compileUpdateFieldInstances table@(CreateTable { name, columns }) = unlines (map compileSetField columns)
    where
        compileSetField column@(Column { name }) = "instance UpdateField " <> tshow (columnNameToFieldName name) <> " (" <> compileTypePattern table <>  ") (" <> compileTypePattern' name  <> ") " <> valueTypeA <> " " <> valueTypeB <> " where\n    {-# INLINE updateField #-}\n    updateField newValue (" <> compileDataTypePattern table <> " meta) = " <> tableNameToModelName (get #name table) <> " " <> (unwords (map compileAttribute columns)) <> " meta"
            where
                (valueTypeA, valueTypeB) =
                    if isVariableAttribute table column
                        then (name, name <> "'")
                        else
                            let hsType = haskellType table column
                            in (hsType, hsType)

                compileAttribute Column { name = name' } | name' == name = "newValue"
                compileAttribute Column { name } = name

                compileTypePattern' ::  Text -> Text
                compileTypePattern' name = tableNameToModelName (get #name table) <> "' " <> unwords (map (\f -> if f == name then name <> "'" else f) (map (get #name) (variableAttributes table)))

indent :: Text -> Text
indent code = code
        |> Text.lines
        |> map indentLine
        |> Text.unlines
    where
        indentLine ""   = ""
        indentLine line = "    " <> line

