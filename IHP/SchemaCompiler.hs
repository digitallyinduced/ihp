module IHP.SchemaCompiler
( compile
, compileStatementPreview
) where

import ClassyPrelude
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import IHP.NameSupport (tableNameToModelName, columnNameToFieldName, enumValueToControllerName, pluralize)
import qualified Data.Text as Text
import qualified System.Directory as Directory
import Data.List.Split
import IHP.HaskellSupport
import qualified IHP.IDE.SchemaDesigner.Parser as SchemaDesigner
import IHP.IDE.SchemaDesigner.Types
import Control.Monad.Fail
import qualified IHP.IDE.SchemaDesigner.Compiler as SqlCompiler

compile :: IO ()
compile = do
    let options = fullCompileOptions
    SchemaDesigner.parseSchemaSql >>= \case
        Left parserError -> fail (cs parserError)
        Right statements -> do
            -- let validationErrors = validate database
            -- unless (null validationErrors) (error $ "Schema.hs contains errors: " <> cs (unsafeHead validationErrors))
            Directory.createDirectoryIfMissing True "build/Generated"
            writeIfDifferent typesFilePath (compileTypes options (Schema statements))

typesFilePath :: FilePath
typesFilePath = "build/Generated/Types.hs"

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

atomicType :: PostgresType -> Text
atomicType = \case
    PSmallInt -> "Int"
    PInt -> "Int"
    PBigInt -> "Integer"
    PJSONB -> "Data.Aeson.Value"
    PText -> "Text"
    PBoolean   -> "Bool"
    PTimestampWithTimezone -> "UTCTime"
    PUUID -> "UUID"
    PSerial -> "Int"
    PBigserial -> "Integer"
    PReal -> "Float"
    PDouble -> "Double"
    PDate -> "Data.Time.Calendar.Day"
    PBinary -> "(Binary ByteString)"
    PTime -> "TimeOfDay"
    PCustomType theType -> tableNameToModelName theType
    PTimestamp -> "LocalTime"
    (PNumeric _ _) -> "Scientific"
    (PVaryingN _) -> "Text"
    (PCharacterN _) -> "Text"
    PArray type_ -> "[" <> atomicType type_ <> "]"
    PPoint -> "Point"
    PPolygon -> "Polygon"
    PInet -> "Net.IP.IP"
    PTSVector -> "TSVector"

haskellType :: (?schema :: Schema) => CreateTable -> Column -> Text
haskellType table@CreateTable { name = tableName, primaryKeyConstraint } column@Column { name, columnType, notNull }
    | [name] == primaryKeyConstraint.primaryKeyColumnNames  = "(" <> primaryKeyTypeName tableName <> ")"
    | otherwise =
        let
            actualType =
                case findForeignKeyConstraint table column of
                    Just (ForeignKeyConstraint { referenceTable }) -> "(" <> primaryKeyTypeName referenceTable <> ")"
                    _ -> atomicType columnType
        in
            if not notNull
                then "(Maybe " <> actualType <> ")"
                else actualType
-- haskellType table (HasMany {name}) = "(QueryBuilder.QueryBuilder " <> tableNameToModelName name <> ")"


writeIfDifferent :: FilePath -> Text -> IO ()
writeIfDifferent path content = do
    alreadyExists <- Directory.doesFileExist path
    existingContent <- if alreadyExists then readFile path else pure ""
    when (existingContent /= cs content) do
        putStrLn $ "Updating " <> cs path
        writeFile (cs path) (cs content)



section = "\n"

compileTypes :: CompilerOptions -> Schema -> Text
compileTypes options schema@(Schema statements) =
        prelude
        <> "\n\n"
        <> let ?schema = schema
            in intercalate "\n\n" (map (compileStatement options) statements)
        <> section
    where
        prelude = "-- This file is auto generated and will be overriden regulary. Please edit `Application/Schema.sql` to change the Types\n"
                  <> "{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, TypeFamilies, DataKinds, TypeOperators, UndecidableInstances, ConstraintKinds, StandaloneDeriving  #-}\n"
                  <> "{-# OPTIONS_GHC -Wno-unused-imports -Wno-dodgy-imports -Wno-unused-matches #-}\n"
                  <> "module Generated.Types where\n\n"
                  <> "import IHP.HaskellSupport\n"
                  <> "import IHP.ModelSupport\n"
                  <> "import CorePrelude hiding (id)\n"
                  <> "import Data.Time.Clock\n"
                  <> "import Data.Time.LocalTime\n"
                  <> "import qualified Data.Time.Calendar\n"
                  <> "import qualified Data.List as List\n"
                  <> "import qualified Data.ByteString as ByteString \n"
                  <> "import qualified Net.IP \n"
                  <> "import Database.PostgreSQL.Simple\n"
                  <> "import Database.PostgreSQL.Simple.FromRow\n"
                  <> "import Database.PostgreSQL.Simple.FromField hiding (Field, name)\n"
                  <> "import Database.PostgreSQL.Simple.ToField hiding (Field)\n"
                  <> "import qualified IHP.Controller.Param\n"
                  <> "import GHC.TypeLits\n"
                  <> "import Data.UUID (UUID)\n"
                  <> "import Data.Default\n"
                  <> "import qualified IHP.QueryBuilder as QueryBuilder\n"
                  <> "import qualified Data.Proxy\n"
                  <> "import GHC.Records\n"
                  <> "import Data.Data\n"
                  <> "import qualified Data.String.Conversions\n"
                  <> "import qualified Data.Text.Encoding\n"
                  <> "import qualified Data.Aeson\n"
                  <> "import Database.PostgreSQL.Simple.Types (Query (Query), Binary ( .. ))\n"
                  <> "import qualified Database.PostgreSQL.Simple.Types\n"
                  <> "import IHP.Job.Types\n"
                  <> "import IHP.Job.Queue ()\n"
                  <> "import qualified Data.Dynamic\n"
                  <> "import Data.Scientific\n"

compileStatementPreview :: [Statement] -> Statement -> Text
compileStatementPreview statements statement = let ?schema = Schema statements in compileStatement previewCompilerOptions statement

compileStatement :: (?schema :: Schema) => CompilerOptions -> Statement -> Text
compileStatement CompilerOptions { compileGetAndSetFieldInstances } (StatementCreateTable table) =
    case get #primaryKeyConstraint table of
        -- Skip generation of tables with no primary keys
        PrimaryKeyConstraint [] -> ""
        _ -> compileData table
            <> compileTypeAlias table
            <> compileFromRowInstance table
            <> compileHasTableNameInstance table
            <> compileGetModelName table
            <> compilePrimaryKeyInstance table
            <> section
            <> compileInclude table
            <> compileCreate table
            <> section
            <> compileUpdate table
            <> section
            <> compileBuild table
            <> compileTableInstance table
            <> (if needsHasFieldId table
                    then compileHasFieldId table
                    else "")
            <> section
            <> (if compileGetAndSetFieldInstances
                    then compileSetFieldInstances table <> compileUpdateFieldInstances table
                    else "")
            <> section

compileStatement _ enum@(CreateEnumType {}) = compileEnumDataDefinitions enum
compileStatement _ _ = ""

compileTypeAlias :: (?schema :: Schema) => CreateTable -> Text
compileTypeAlias table@(CreateTable { name, columns }) =
        "type "
        <> modelName
        <> " = "
        <> modelName
        <> "' "
        <> unwords (map (haskellType table) (variableAttributes table))
        <> hasManyDefaults
        <> "\n"
    where
        modelName = tableNameToModelName name
        hasManyDefaults = columnsReferencingTable name
                |> map (\(tableName, columnName) -> "(QueryBuilder.QueryBuilder \"" <> tableName <> "\")")
                |> unwords

primaryKeyTypeName :: Text -> Text
primaryKeyTypeName name = "Id' " <> tshow name <> ""

compileData :: (?schema :: Schema) => CreateTable -> Text
compileData table@(CreateTable { name, columns }) =
        "data " <> modelName <> "' " <> typeArguments
        <> " = " <> modelName <> " {"
        <>
            table
            |> dataFields
            |> map (\(fieldName, fieldType) -> fieldName <> " :: " <> fieldType)
            |> commaSep
        <> "} deriving (Eq, Show)\n"
        <> "instance InputValue " <> modelName <> " where inputValue = IHP.ModelSupport.recordToInputValue\n"
    where
        modelName = tableNameToModelName name
        typeArguments :: Text
        typeArguments = dataTypeArguments table |> unwords

-- | Returns all the type arguments of the data structure for an entity
dataTypeArguments :: (?schema :: Schema) => CreateTable -> [Text]
dataTypeArguments table = (map columnNameToFieldName belongsToVariables) <> hasManyVariables
    where
        belongsToVariables = variableAttributes table |> map (get #name)
        hasManyVariables =
            columnsReferencingTable (get #name table)
            |> compileQueryBuilderFields
            |> map snd

-- | Returns the field names and types for the @data MyRecord = MyRecord { .. }@ for a given table
dataFields :: (?schema :: Schema) => CreateTable -> [(Text, Text)]
dataFields table@(CreateTable { name, columns }) = columnFields <> queryBuilderFields <> [("meta", "MetaBag")]
    where
        columnFields = columns |> map columnField

        columnField column =
            let fieldName = columnNameToFieldName (get #name column)
            in
                ( fieldName
                , if isVariableAttribute table column
                        then fieldName
                        else haskellType table column
                )

        queryBuilderFields = columnsReferencingTable name |> compileQueryBuilderFields

compileQueryBuilderFields :: [(Text, Text)] -> [(Text, Text)]
compileQueryBuilderFields columns = map compileQueryBuilderField columns
    where
        compileQueryBuilderField (refTableName, refColumnName) =
            let
                -- Given a relationship like the following:
                --
                -- CREATE TABLE referrals (
                --     user_id UUID NOT NULL,
                --     referred_user_id UUID DEFAULT uuid_generate_v4() NOT NULL
                -- );
                --
                -- We would have two fields on the @User@ record called @referrals@ which are
                -- going to be used with fetchRelated (user >>= fetchRelated #referrals).
                --
                -- Of course having two fields in the same record does not work, so we have to
                -- detect these duplicate query builder fields and use a more qualified name.
                --
                -- In the example this will lead to two fileds called @referralsUsers@ and @referralsReferredUsers@
                -- being added to the data structure.
                hasDuplicateQueryBuilder =
                    columns
                    |> map fst
                    |> map columnNameToFieldName
                    |> filter (columnNameToFieldName refTableName ==)
                    |> length
                    |> (\count -> count > 1)

                stripIdSuffix :: Text -> Text
                stripIdSuffix name = fromMaybe name (Text.stripSuffix "_id" name)

                fieldName = if hasDuplicateQueryBuilder
                    then
                        (refTableName <> "_" <> (refColumnName |> stripIdSuffix))
                        |> columnNameToFieldName
                        |> pluralize
                    else columnNameToFieldName refTableName
            in
                (fieldName, fieldName)


-- | Finds all the columns referencing a specific table via a foreign key constraint
--
-- __Example:__
--
-- Given the schema:
--
-- > CREATE TABLE users (id SERIAL, company_id INT);
-- > CREATE TABLE companies (id SERIAL);
--
-- you can do the following:
--
-- >>> columnsReferencingTable "companies"
-- [ ("users", "company_id") ]
columnsReferencingTable :: (?schema :: Schema) => Text -> [(Text, Text)]
columnsReferencingTable theTableName =
    let
        (Schema statements) = ?schema
    in
        statements
        |> mapMaybe \case
            AddConstraint { tableName, constraint = ForeignKeyConstraint { columnName, referenceTable, referenceColumn } } | referenceTable == theTableName -> Just (tableName, columnName)
            _ -> Nothing

variableAttributes :: (?schema :: Schema) => CreateTable -> [Column]
variableAttributes table@(CreateTable { columns }) = filter (isVariableAttribute table) columns

isVariableAttribute :: (?schema :: Schema) => CreateTable -> Column -> Bool
isVariableAttribute = isRefCol


-- | Returns @True@ when the coluns is referencing another column via foreign key constraint
isRefCol :: (?schema :: Schema) => CreateTable -> Column -> Bool
isRefCol table column = isJust (findForeignKeyConstraint table column)

-- | Returns the foreign key constraint bound on the given column
findForeignKeyConstraint :: (?schema :: Schema) => CreateTable -> Column -> Maybe Constraint
findForeignKeyConstraint CreateTable { name } column =
        case find isFkConstraint statements of
            Just (AddConstraint { constraint }) -> Just constraint
            Nothing -> Nothing
    where
        isFkConstraint (AddConstraint { tableName, constraint = ForeignKeyConstraint { columnName }}) = tableName == name && columnName == get #name column
        isFkConstraint _ = False

        (Schema statements) = ?schema

compileEnumDataDefinitions :: (?schema :: Schema) => Statement -> Text
compileEnumDataDefinitions CreateEnumType { values = [] } = "" -- Ignore enums without any values
compileEnumDataDefinitions enum@(CreateEnumType { name, values }) =
        "data " <> modelName <> " = " <> (intercalate " | " valueConstructors) <> " deriving (Eq, Show, Read, Enum)\n"
        <> "instance FromField " <> modelName <> " where\n"
        <> indent (unlines (map compileFromFieldInstanceForValue values))
        <> "    fromField field (Just value) = returnError ConversionFailed field (\"Unexpected value for enum value. Got: \" <> Data.String.Conversions.cs value)\n"
        <> "    fromField field Nothing = returnError UnexpectedNull field \"Unexpected null for enum value\"\n"
        <> "instance Default " <> modelName <> " where def = " <> enumValueToConstructorName (unsafeHead values) <> "\n"
        <> "instance ToField " <> modelName <> " where\n" <> indent (unlines (map compileToFieldInstanceForValue values))
        <> "instance InputValue " <> modelName <> " where\n" <> indent (unlines (map compileInputValue values)) <> "\n"
        <> "instance IHP.Controller.Param.ParamReader " <> modelName <> " where readParameter = IHP.Controller.Param.enumParamReader; readParameterJSON = IHP.Controller.Param.enumParamReaderJSON\n"
    where
        modelName = tableNameToModelName name
        valueConstructors = map enumValueToConstructorName values

        enumValueToConstructorName :: Text -> Text
        enumValueToConstructorName enumValue = if isEnumValueUniqueInSchema enumValue
                then enumValueToControllerName enumValue
                else modelName <> (enumValueToControllerName enumValue)

        compileFromFieldInstanceForValue value = "fromField field (Just value) | value == (Data.Text.Encoding.encodeUtf8 " <> tshow value <> ") = pure " <> enumValueToConstructorName value
        compileToFieldInstanceForValue value = "toField " <> enumValueToConstructorName value <> " = toField (" <> tshow value <> " :: Text)"
        compileInputValue value = "inputValue " <> enumValueToConstructorName value <> " = " <> tshow value <> " :: Text"

        -- Let's say we have a schema like this:
        --
        -- > CREATE TYPE property_type AS ENUM ('APARTMENT', 'HOUSE');
        -- > CREATE TYPE apartment_type AS ENUM ('LOFT', 'APARTMENT');
        --
        -- A naive enum implementation will generate these data constructors:
        --
        -- > data PropertyType = Apartment | House
        -- > data ApartmentType = Loft | Apartment
        --
        -- Now we have two data constructors with the name 'Apartment'. This fails to compile.
        --
        -- To avoid this we detect if a name is unique across the schema. When it's not unique
        -- we use the following naming schema:
        --
        -- > data PropertyType = PropertyTypeApartment | House
        -- > data ApartmentType = Loft | ApartmentTypeApartment
        --
        -- This function returns True if the given enumValue (like 'APARTMENT') is unique across the schema.
        isEnumValueUniqueInSchema :: Text -> Bool
        isEnumValueUniqueInSchema enumValue =
                ?schema
                |> \case Schema statements -> statements
                |> filter (\case
                        CreateEnumType { name, values } | enumValue `elem` values -> True
                        _                                                         -> False
                    )
                |> length
                |> \count -> count == 1

compileToRowValues :: [Text] -> Text
compileToRowValues bindingValues | length bindingValues == 1 = "Only (" <> (unsafeHead bindingValues) <> ")"
compileToRowValues bindingValues = "(" <> intercalate ") :. (" (map (\list -> if length list == 1 then "Only (" <> (unsafeHead list) <> ")" else intercalate ", " list) (chunksOf 8 bindingValues)) <> ")"

-- When we do an INSERT or UPDATE query like @INSERT INTO values (uuids) VALUES (?)@ where the type of @uuids@ is @UUID[]@
-- we need to add a typecast to the placeholder @?@, otherwise this will throw an sql error
-- See https://github.com/digitallyinduced/ihp/issues/593
-- See https://github.com/digitallyinduced/ihp/issues/913
columnPlaceholder :: Column -> Text
columnPlaceholder column@Column { columnType } = if columnPlaceholderNeedsTypecast column
        then "? :: " <> SqlCompiler.compilePostgresType columnType
        else "?"
    where
        columnPlaceholderNeedsTypecast Column { columnType = PArray {} } = True
        columnPlaceholderNeedsTypecast _ = False

compileCreate :: CreateTable -> Text
compileCreate table@(CreateTable { name, columns }) =
    let
        modelName = tableNameToModelName name
        columnNames = commaSep (map (get #name) columns)
        values = commaSep (map columnPlaceholder columns)

        toBinding column@(Column { name }) =
            if hasExplicitOrImplicitDefault column
                then "fieldWithDefault #" <> columnNameToFieldName name <> " model"
                else "get #" <> columnNameToFieldName name <> " model"


        bindings :: [Text]
        bindings = map toBinding columns

        createManyFieldValues :: Text
        createManyFieldValues = if null bindings
                then "()"
                else "(List.concat $ List.map (\\model -> [" <> (intercalate ", " (map (\b -> "toField (" <> b <> ")") bindings)) <> "]) models)"
    in
        "instance CanCreate " <> modelName <> " where\n"
        <> indent (
            "create :: (?modelContext :: ModelContext) => " <> modelName <> " -> IO " <> modelName <> "\n"
                <> "create model = do\n"
                <> indent ("List.head <$> sqlQuery \"INSERT INTO " <> name <> " (" <> columnNames <> ") VALUES (" <> values <> ") RETURNING " <> columnNames <> "\" (" <> compileToRowValues bindings <> ")\n")
                <> "createMany [] = pure []\n"
                <> "createMany models = do\n"
                <> indent ("sqlQuery (Query $ \"INSERT INTO " <> name <> " (" <> columnNames <> ") VALUES \" <> (ByteString.intercalate \", \" (List.map (\\_ -> \"(" <> values <> ")\") models)) <> \" RETURNING " <> columnNames <> "\") " <> createManyFieldValues <> "\n"
                    )
            )

commaSep :: [Text] -> Text
commaSep = intercalate ", "

toBinding :: Text -> Column -> Text
toBinding modelName Column { name } = "let " <> modelName <> "{" <> columnNameToFieldName name <> "} = model in " <> columnNameToFieldName name

compileUpdate :: CreateTable -> Text
compileUpdate table@(CreateTable { name, columns }) =
    let
        modelName = tableNameToModelName name

        toUpdateBinding Column { name } = "fieldWithUpdate #" <> columnNameToFieldName name <> " model"
        toPrimaryKeyBinding Column { name } = "get #" <> columnNameToFieldName name <> " model"

        bindings :: Text
        bindings =
            let
                bindingValues = map toUpdateBinding columns <> map toPrimaryKeyBinding (primaryKeyColumns table)
            in
                compileToRowValues bindingValues

        updates = commaSep (map (\column -> get #name column <> " = " <> columnPlaceholder column ) columns)

        columnNames = columns
                |> map (get #name)
                |> intercalate ", "
    in
        "instance CanUpdate " <> modelName <> " where\n"
        <> indent ("updateRecord model = do\n"
                <> indent (
                    "List.head <$> sqlQuery \"UPDATE " <> name <> " SET " <> updates <> " WHERE id = ? RETURNING " <> columnNames <> "\" (" <> bindings <> ")\n"
                )
            )

compileFromRowInstance :: (?schema :: Schema) => CreateTable -> Text
compileFromRowInstance table@(CreateTable { name, columns }) = cs [i|
instance FromRow #{modelName} where
    fromRow = do
#{unsafeInit . indent . indent . unlines $ map columnBinding columnNames}
        let theRecord = #{modelName} #{intercalate " " (map compileField (dataFields table))}
        pure theRecord

|]
    where
        modelName = tableNameToModelName name
        columnNames = map (columnNameToFieldName . get #name) columns
        columnBinding columnName = columnName <> " <- field"

        referencing = columnsReferencingTable (get #name table)

        compileField (fieldName, _)
            | isColumn fieldName = fieldName
            | isManyToManyField fieldName = let (Just ref) = find (\(n, _) -> columnNameToFieldName n == fieldName) referencing in compileSetQueryBuilder ref
            | fieldName == "meta" = "def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }"
            | otherwise = "def"

        isPrimaryKey name = name `elem` table.primaryKeyConstraint.primaryKeyColumnNames
        isColumn name = name `elem` columnNames
        isManyToManyField fieldName = fieldName `elem` (referencing |> map (columnNameToFieldName . fst))

        compileSetQueryBuilder (refTableName, refFieldName) = "(QueryBuilder.filterWhere (#" <> columnNameToFieldName refFieldName <> ", " <> primaryKeyField <> ") (QueryBuilder.query @" <> tableNameToModelName refTableName <> "))"
            where
                -- | When the referenced column is nullable, we have to wrap the @Id@ in @Just@
                primaryKeyField :: Text
                primaryKeyField = if get #notNull refColumn then "id" else "Just id"

                (Just refTable) = let (Schema statements) = ?schema in
                        statements
                        |> find \case
                                StatementCreateTable CreateTable { name } -> name == refTableName
                                otherwise -> False

                refColumn :: Column
                refColumn = refTable
                        |> \case StatementCreateTable CreateTable { columns } -> columns
                        |> find (\col -> get #name col == refFieldName)
                        |> \case
                            Just refColumn -> refColumn
                            Nothing -> error (cs $ "Could not find " <> get #name refTable <> "." <> refFieldName <> " referenced by a foreign key constraint. Make sure that there is no typo in the foreign key constraint")

        compileQuery column@(Column { name }) = columnNameToFieldName name <> " = (" <> toBinding modelName column <> ")"
        -- compileQuery column@(Column { name }) | isReferenceColum column = columnNameToFieldName name <> " = (" <> toBinding modelName column <> ")"
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

compileBuild :: (?schema :: Schema) => CreateTable -> Text
compileBuild table@(CreateTable { name, columns }) =
        "instance Record " <> tableNameToModelName name <> " where\n"
        <> "    {-# INLINE newRecord #-}\n"
        <> "    newRecord = " <> tableNameToModelName name <> " " <> unwords (map toDefaultValueExpr columns) <> " " <> (columnsReferencingTable name |> map (const "def") |> unwords) <> " def\n"
        <> "instance Default (Id' \"" <> name <> "\") where def = Id def"


toDefaultValueExpr :: Column -> Text
toDefaultValueExpr Column { columnType, notNull, defaultValue = Just theDefaultValue } =
            let
                wrapNull False value = "(Just " <> value <> ")"
                wrapNull True value = value

                isNullExpr (VarExpression varName) = toUpper varName == "NULL"
                isNullExpr _ = False

                -- We remove type casts here, as we need the actual value literal for setting our default value
                theNormalizedDefaultValue = theDefaultValue |> SchemaDesigner.removeTypeCasts
            in
                if isNullExpr theDefaultValue
                    then "Nothing"
                    else
                        case columnType of
                            PText -> case theNormalizedDefaultValue of
                                TextExpression value -> wrapNull notNull (tshow value)
                                otherwise            -> error ("toDefaultValueExpr: TEXT column needs to have a TextExpression as default value. Got: " <> show otherwise)
                            PBoolean -> case theNormalizedDefaultValue of
                                VarExpression value -> wrapNull notNull (tshow (toLower value == "true"))
                                otherwise           -> error ("toDefaultValueExpr: BOOL column needs to have a VarExpression as default value. Got: " <> show otherwise)
                            PDouble -> case theNormalizedDefaultValue of
                                DoubleExpression value -> wrapNull notNull (tshow value)
                                IntExpression value -> wrapNull notNull (tshow value)
                                otherwise           -> error ("toDefaultValueExpr: DOUBLE column needs to have a DoubleExpression as default value. Got: " <> show otherwise)
                            _ -> "def"
toDefaultValueExpr _ = "def"

compileHasTableNameInstance :: (?schema :: Schema) => CreateTable -> Text
compileHasTableNameInstance table@(CreateTable { name }) =
    "type instance GetTableName (" <> tableNameToModelName name <> "' " <> unwords (map (const "_") (dataTypeArguments table)) <>  ") = " <> tshow name <> "\n"
    <> "type instance GetModelByTableName " <> tshow name <> " = " <> tableNameToModelName name <> "\n"

compilePrimaryKeyInstance :: (?schema :: Schema) => CreateTable -> Text
compilePrimaryKeyInstance table@(CreateTable { name, columns, constraints }) = cs [i|
type instance PrimaryKey #{tshow name} = #{idType}

instance QueryBuilder.FilterPrimaryKey "#{name}" where
    filterWhereId #{primaryKeyPattern} builder =
        builder |> #{intercalate " |> " primaryKeyFilters}
    {-# INLINE filterWhereId #-}
|]
    where
        idType :: Text
        idType = case primaryKeyColumns table of
                [] -> error $ "Impossible happened in compilePrimaryKeyInstance. No primary keys found for table " <> cs name <> ". At least one primary key is required."
                [column] -> atomicType (get #columnType column) -- PrimaryKey User = UUID
                cs -> "(" <> intercalate ", " (map colType cs) <> ")" -- PrimaryKey PostsTag = (Id' "posts", Id' "tags")
            where
                colType column = haskellType table column

        primaryKeyPattern = case primaryKeyColumns table of
            [] -> error $ "Impossible happened in compilePrimaryKeyInstance. No primary keys found for table " <> cs name <> ". At least one primary key is required."
            [c] -> get #name c
            cs -> "(Id (" <> intercalate ", " (map (columnNameToFieldName . get #name) cs) <> "))"

        primaryKeyFilters :: [Text]
        primaryKeyFilters = map primaryKeyFilter $ primaryKeyColumns table

        primaryKeyFilter :: Column -> Text
        primaryKeyFilter Column {name} = "QueryBuilder.filterWhere (#" <> columnNameToFieldName name <> ", " <> columnNameToFieldName name <> ")"

compileTableInstance :: (?schema :: Schema) => CreateTable -> Text
compileTableInstance table@(CreateTable { name, columns, constraints }) = cs [i|
instance #{instanceHead} where
    tableName = \"#{name}\"
    tableNameByteString = \"#{name}\"
    columnNames = #{columnNames}
    primaryKeyCondition #{pattern} = #{condition}
    {-# INLINABLE primaryKeyCondition #-}
|]
    where
        instanceHead :: Text
        instanceHead = instanceConstraints <> " => Table (" <> compileTypePattern table <> ")"
            where
                instanceConstraints =
                    table
                    |> primaryKeyColumns
                    |> map (get #name)
                    |> map columnNameToFieldName
                    |> filter (\field -> field `elem` (dataTypeArguments table))
                    |> map (\field -> "ToField " <> field)
                    |> intercalate ", "
                    |> \inner -> "(" <> inner <> ")"

        primaryKeyColumnNames :: [Text]
        primaryKeyColumnNames = (primaryKeyColumns table) |> map (get #name)

        primaryKeyFieldNames :: [Text]
        primaryKeyFieldNames = primaryKeyColumnNames |> map columnNameToFieldName

        pattern :: Text
        pattern = tableNameToModelName name <> " { " <> intercalate ", " primaryKeyFieldNames <> " }"

        condition :: Text
        condition = primaryKeyColumns table
                |> map primaryKeyToCondition
                |> intercalate ", "
                |> \listInner -> "[" <> listInner <> "]"

        primaryKeyToCondition :: Column -> Text
        primaryKeyToCondition column = "(\"" <> get #name column <> "\", toField " <> columnNameToFieldName (get #name column) <> ")"

        columnNames = columns
                |> map (get #name)
                |> tshow

compileGetModelName :: (?schema :: Schema) => CreateTable -> Text
compileGetModelName table@(CreateTable { name }) = "type instance GetModelName (" <> tableNameToModelName name <> "' " <> unwords (map (const "_") (dataTypeArguments table)) <>  ") = " <> tshow (tableNameToModelName name) <> "\n"

compileDataTypePattern :: (?schema :: Schema) => CreateTable -> Text
compileDataTypePattern table@(CreateTable { name }) = tableNameToModelName name <> " " <> unwords (table |> dataFields |> map fst)

compileTypePattern :: (?schema :: Schema) => CreateTable -> Text
compileTypePattern table@(CreateTable { name }) = tableNameToModelName name <> "' " <> unwords (dataTypeArguments table)

compileInclude :: (?schema :: Schema) => CreateTable -> Text
compileInclude table@(CreateTable { name, columns }) = (belongsToIncludes <> hasManyIncludes) |> unlines
    where
        belongsToIncludes = map compileBelongsTo (filter (isRefCol table) columns)
        hasManyIncludes = columnsReferencingTable name |> map compileHasMany
        typeArgs = dataTypeArguments table
        modelName = tableNameToModelName name
        modelConstructor = modelName <> "'"

        includeType :: Text -> Text -> Text
        includeType fieldName includedType = "type instance Include " <> tshow fieldName <> " (" <> leftModelType <> ") = " <> rightModelType <> "\n"
            where
                leftModelType = unwords (modelConstructor:typeArgs)
                rightModelType = unwords (modelConstructor:(map compileTypeVariable' typeArgs))
                compileTypeVariable' name | name == fieldName = includedType
                compileTypeVariable' name = name

        compileBelongsTo :: Column -> Text
        compileBelongsTo column = includeType (columnNameToFieldName (get #name column)) ("(GetModelById " <> columnNameToFieldName (get #name column) <> ")")

        compileHasMany :: (Text, Text) -> Text
        compileHasMany (refTableName, refColumnName) = includeType (columnNameToFieldName refTableName) ("[" <> tableNameToModelName refTableName <> "]")


compileSetFieldInstances :: (?schema :: Schema) => CreateTable -> Text
compileSetFieldInstances table@(CreateTable { name, columns }) = unlines (map compileSetField (dataFields table))
    where
        setMetaField = "instance SetField \"meta\" (" <> compileTypePattern table <>  ") MetaBag where\n    {-# INLINE setField #-}\n    setField newValue (" <> compileDataTypePattern table <> ") = " <> tableNameToModelName name <> " " <> (unwords (map (get #name) columns)) <> " newValue"
        modelName = tableNameToModelName name
        typeArgs = dataTypeArguments table
        compileSetField (name, fieldType) =
            "instance SetField " <> tshow name <> " (" <> compileTypePattern table <>  ") " <> fieldType <> " where\n" <>
            "    {-# INLINE setField #-}\n" <>
            "    setField newValue (" <> compileDataTypePattern table <> ") =\n" <>
            "        " <> modelName <> " " <> (unwords (map compileAttribute (table |> dataFields |> map fst)))
            where
                compileAttribute name'
                    | name' == name = "newValue"
                    | name' == "meta" = "(meta { touchedFields = \"" <> name <> "\" : touchedFields meta })"
                    | otherwise = name'

compileUpdateFieldInstances :: (?schema :: Schema) => CreateTable -> Text
compileUpdateFieldInstances table@(CreateTable { name, columns }) = unlines (map compileSetField (dataFields table))
    where
        modelName = tableNameToModelName name
        typeArgs = dataTypeArguments table
        compileSetField (name, fieldType) = "instance UpdateField " <> tshow name <> " (" <> compileTypePattern table <>  ") (" <> compileTypePattern' name  <> ") " <> valueTypeA <> " " <> valueTypeB <> " where\n    {-# INLINE updateField #-}\n    updateField newValue (" <> compileDataTypePattern table <> ") = " <> modelName <> " " <> (unwords (map compileAttribute (table |> dataFields |> map fst)))
            where
                (valueTypeA, valueTypeB) =
                    if name `elem` typeArgs
                        then (name, name <> "'")
                        else (fieldType, fieldType)

                compileAttribute name'
                    | name' == name = "newValue"
                    | name' == "meta" = "(meta { touchedFields = \"" <> name <> "\" : touchedFields meta })"
                    | otherwise = name'

                compileTypePattern' ::  Text -> Text
                compileTypePattern' name = tableNameToModelName (get #name table) <> "' " <> unwords (map (\f -> if f == name then name <> "'" else f) (dataTypeArguments table))

compileHasFieldId :: (?schema :: Schema) => CreateTable -> Text
compileHasFieldId table@CreateTable { name, primaryKeyConstraint } = cs [i|
instance HasField "id" #{tableNameToModelName name} (Id' "#{name}") where
    getField (#{compileDataTypePattern table}) = #{compilePrimaryKeyValue}
    {-# INLINE getField #-}
|]
    where
        compilePrimaryKeyValue = case primaryKeyConstraint.primaryKeyColumnNames of
            [id] -> columnNameToFieldName id
            ids -> "Id (" <> commaSep (map columnNameToFieldName ids) <> ")"

needsHasFieldId :: CreateTable -> Bool
needsHasFieldId CreateTable { primaryKeyConstraint } =
  case primaryKeyConstraint.primaryKeyColumnNames of
    [] -> False
    ["id"] -> False
    _ -> True

primaryKeyColumns :: CreateTable -> [Column]
primaryKeyColumns CreateTable { name, columns, primaryKeyConstraint } =
    map getColumn primaryKeyConstraint.primaryKeyColumnNames
  where
    getColumn columnName = case find ((==) columnName . get #name) columns of
      Just c -> c
      Nothing -> error ("Missing column " <> cs columnName <> " used in primary key for " <> cs name)

-- | Indents a block of code with 4 spaces.
--
-- Empty lines are not indented.
indent :: Text -> Text
indent code = code
        |> Text.lines
        |> map indentLine
        |> Text.unlines
    where
        indentLine ""   = ""
        indentLine line = "    " <> line

-- | Returns 'True' when the column has an explicit default value or when it's a SERIAL or BIGSERIAL
hasExplicitOrImplicitDefault :: Column -> Bool
hasExplicitOrImplicitDefault column = case column of
        Column { defaultValue = Just _ } -> True
        Column { columnType = PSerial } -> True
        Column { columnType = PBigserial } -> True
        _ -> False
