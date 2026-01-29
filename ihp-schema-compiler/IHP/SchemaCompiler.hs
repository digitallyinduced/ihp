module IHP.SchemaCompiler
( compile
, compileStatementPreview
) where

import ClassyPrelude
import Data.String.Conversions (cs)
import "interpolate" Data.String.Interpolate (i)
import IHP.NameSupport (tableNameToModelName, columnNameToFieldName, enumValueToControllerName)
import qualified Data.Text as Text
import qualified System.Directory.OsPath as Directory
import Data.List.Split
import IHP.HaskellSupport
import qualified IHP.SchemaCompiler.Parser as SchemaDesigner
import qualified IHP.Postgres.Parser as PostgresParser
import IHP.Postgres.Types
import qualified IHP.Postgres.Compiler as SqlCompiler
import qualified Control.Exception as Exception
import NeatInterpolation
import Text.Countable (pluralize)
import System.OsPath (OsPath, encodeUtf, decodeUtf)
import qualified System.OsPath as OsPath


data CompileException = CompileException ByteString deriving (Show)
instance Exception CompileException where
    displayException (CompileException message) = cs message

compile :: IO ()
compile = do
    let options = fullCompileOptions
    SchemaDesigner.parseSchemaSql >>= \case
        Left parserError -> Exception.throwIO (CompileException parserError)
        Right statements -> do
            -- let validationErrors = validate database
            -- unless (null validationErrors) (error $ "Schema.hs contains errors: " <> cs (unsafeHead validationErrors))
            Directory.createDirectoryIfMissing True "build/Generated"

            forEach (compileModules options (Schema statements)) \(path, body) -> do
                    writeIfDifferent path body

compileModules :: CompilerOptions -> Schema -> [(OsPath, Text)]
compileModules options schema =
    [ ("build/Generated/Enums.hs", compileEnums options schema)
    , ("build/Generated/ActualTypes.hs", compileTypes options schema)
    ] <> tableModules options schema <>
    [ ("build/Generated/Types.hs", compileIndex schema)
    ]

applyTables :: (CreateTable -> (OsPath, Text)) -> Schema -> [(OsPath, Text)]
applyTables applyFunction schema =
    let ?schema = schema
    in
        schema.statements
        |> mapMaybe (\case
                StatementCreateTable table | tableHasPrimaryKey table -> Just (applyFunction table)
                otherwise -> Nothing
            )

tableModules :: CompilerOptions -> Schema -> [(OsPath, Text)]
tableModules options schema =
    let ?schema = schema
    in
        applyTables (tableModule options) schema
        <> applyTables tableIncludeModule schema

tableModule :: (?schema :: Schema) => CompilerOptions -> CreateTable -> (OsPath, Text)
tableModule options table =
        ((OsPath.</>) "build/Generated" (either (error . show) id (encodeUtf (cs (tableNameToModelName table.name) <> ".hs"))), body)
    where
        body = Text.unlines
            [ prelude
            , tableModuleBody options table
            ]
        moduleName = "Generated." <> tableNameToModelName table.name
        prelude = [trimming|
            -- This file is auto generated and will be overriden regulary. Please edit `Application/Schema.sql` to change the Types\n"
            {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, TypeFamilies, DataKinds, TypeOperators, UndecidableInstances, ConstraintKinds, StandaloneDeriving  #-}
            {-# OPTIONS_GHC -Wno-unused-imports -Wno-dodgy-imports -Wno-unused-matches #-}
            module $moduleName where
            $defaultImports
            import Generated.ActualTypes
        |]

tableIncludeModule :: (?schema :: Schema) => CreateTable -> (OsPath, Text)
tableIncludeModule table =
        ((OsPath.</>) "build/Generated" (either (error . show) id (encodeUtf (cs (tableNameToModelName table.name) <> "Include.hs"))), prelude <> compileInclude table)
    where
        moduleName = "Generated." <> tableNameToModelName table.name <> "Include"
        prelude = [trimming|
            -- This file is auto generated and will be overriden regulary. Please edit `Application/Schema.sql` to change the Types\n"
            module $moduleName where
            import Generated.ActualTypes
            import IHP.ModelSupport (Include, GetModelById)
        |] <> "\n\n"


tableModuleBody :: (?schema :: Schema) => CompilerOptions -> CreateTable -> Text
tableModuleBody options table = Text.unlines
    [ compileInputValueInstance table
    , compileFromRowInstance table
    , compileGetModelName table
    , compileCreate table
    , compileUpdate table
    , compileBuild table
    , compileFilterPrimaryKeyInstance table
    , if needsHasFieldId table
            then compileHasFieldId table
            else ""
    , if options.compileGetAndSetFieldInstances
            then compileSetFieldInstances table <> compileUpdateFieldInstances table
            else ""
    ]

newtype Schema = Schema { statements :: [Statement] }

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
    (PInterval _) -> "PGInterval"
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
haskellType table@CreateTable { name = tableName, primaryKeyConstraint } column@Column { name, columnType, notNull, generator }
    | [name] == primaryKeyColumnNames primaryKeyConstraint = "(" <> primaryKeyTypeName tableName <> ")"
    | otherwise =
        let
            actualType =
                case findForeignKeyConstraint table column of
                    Just (ForeignKeyConstraint { referenceTable }) -> "(" <> primaryKeyTypeName referenceTable <> ")"
                    _ -> atomicType columnType
        in
            if not notNull || isJust generator
                then "(Maybe " <> actualType <> ")"
                else actualType
-- haskellType table (HasMany {name}) = "(QueryBuilder.QueryBuilder " <> tableNameToModelName name <> ")"


writeIfDifferent :: OsPath -> Text -> IO ()
writeIfDifferent path content = do
    fp <- decodeUtf path
    alreadyExists <- Directory.doesFileExist path
    existingContent <- if alreadyExists then readFile fp else pure ""
    when (existingContent /= cs content) do
        putStrLn $ "Updating " <> cs fp
        writeFile fp (cs content)

compileTypes :: CompilerOptions -> Schema -> Text
compileTypes options schema@(Schema statements) = Text.unlines
        [ prelude
        , let ?schema = schema in body
        ]
    where
        body :: (?schema :: Schema) => Text
        body =
            statements
                |> mapMaybe (\case
                    StatementCreateTable table | tableHasPrimaryKey table -> Just (compileActualTypesForTable table)
                    otherwise -> Nothing
                )
                |> Text.intercalate "\n\n"
        prelude = [trimming|
            -- This file is auto generated and will be overriden regulary. Please edit `Application/Schema.sql` to change the Types\n"
            {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, TypeFamilies, DataKinds, TypeOperators, UndecidableInstances, ConstraintKinds, StandaloneDeriving  #-}
            {-# OPTIONS_GHC -Wno-unused-imports -Wno-dodgy-imports -Wno-unused-matches #-}
            module Generated.ActualTypes (module Generated.ActualTypes, module Generated.Enums) where
            $defaultImports
            import Generated.Enums
        |]

compileActualTypesForTable :: (?schema :: Schema) => CreateTable -> Text
compileActualTypesForTable table = Text.unlines
    [ compileData table
    , compilePrimaryKeyInstance table
    , compileTypeAlias table
    , compileHasTableNameInstance table
    , compileDefaultIdInstance table
    , compileTableInstance table
    ]

compileIndex :: Schema -> Text
compileIndex schema = [trimming|
        -- This file is auto generated and will be overriden regulary. Please edit `Application/Schema.sql` to change the Types\n"
        module Generated.Types ($rexports) where
        import Generated.ActualTypes
        $tableModuleImports
    |]
        where
            tableModuleNames =
                schema.statements
                |> map (\case
                        StatementCreateTable table ->
                            let modelName = tableNameToModelName table.name
                            in
                                [ "Generated." <> modelName
                                , "Generated." <> modelName <> "Include"
                                ]
                        otherwise -> []
                    )
                |> concat
            tableModuleImports = tableModuleNames
                    |> map (\name -> "import " <> name)
                    |> Text.unlines

            rexportedModules = ["Generated.ActualTypes"] <> tableModuleNames

            rexports = rexportedModules
                    |> map (\moduleName -> "module " <> moduleName)
                    |> Text.intercalate ", "


defaultImports = [trimming|
    import IHP.HaskellSupport
    import IHP.ModelSupport
    import CorePrelude hiding (id)
    import Data.Time.Clock
    import Data.Time.LocalTime
    import qualified Data.Time.Calendar
    import qualified Data.List as List
    import qualified Data.ByteString as ByteString
    import qualified Net.IP
    import qualified IHP.Controller.Param
    import GHC.TypeLits
    import Data.UUID (UUID)
    import Data.Default
    import qualified IHP.QueryBuilder as QueryBuilder
    import qualified Data.Proxy
    import GHC.Records
    import Data.Data
    import qualified Data.String.Conversions
    import qualified Data.Text.Encoding
    import qualified Data.Aeson
    import qualified Hasql.DynamicStatements.Snippet as Snippet
    import Hasql.DynamicStatements.Snippet (Snippet, sql, param, DefaultParamEncoder)
    import qualified Hasql.Decoders as Decoders
    import IHP.Job.Types
    import IHP.Job.Queue ()
    import qualified Control.DeepSeq as DeepSeq
    import qualified Data.Dynamic
    import Data.Scientific
|]



compileEnums :: CompilerOptions -> Schema -> Text
compileEnums options schema@(Schema statements) = Text.unlines
        [ prelude
        , let ?schema = schema
          in intercalate "\n\n" (mapMaybe compileStatement statements)
        ]
    where
        compileStatement enum@(CreateEnumType {}) = Just (compileEnumDataDefinitions enum)
        compileStatement _ = Nothing
        prelude = [trimming|
            -- This file is auto generated and will be overriden regulary. Please edit `Application/Schema.sql` to change the Types\n"
            module Generated.Enums where
            import CorePrelude
            import IHP.ModelSupport
            import qualified IHP.Controller.Param
            import Data.Default
            import qualified IHP.QueryBuilder as QueryBuilder
            import qualified Data.String.Conversions
            import qualified Data.Text.Encoding
            import qualified Control.DeepSeq as DeepSeq
            import qualified Hasql.Decoders as Decoders
            import qualified Hasql.DynamicStatements.Snippet as Snippet
            import Hasql.DynamicStatements.Snippet (Snippet, sql, param, DefaultParamEncoder)
        |]

compileStatementPreview :: [Statement] -> Statement -> Text
compileStatementPreview statements statement =
    let ?schema = Schema statements
    in
        case statement of
            CreateEnumType {} -> compileEnumDataDefinitions statement
            StatementCreateTable table -> Text.unlines
                [ compileActualTypesForTable table
                , tableModuleBody previewCompilerOptions table
                ]

-- | Skip generation of tables with no primary keys
tableHasPrimaryKey :: CreateTable -> Bool
tableHasPrimaryKey table = table.primaryKeyConstraint /= (PrimaryKeyConstraint [])

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
    where
        modelName = tableNameToModelName name
        typeArguments :: Text
        typeArguments = dataTypeArguments table |> unwords

compileInputValueInstance :: CreateTable -> Text
compileInputValueInstance table =
        "instance InputValue " <> modelName <> " where inputValue = IHP.ModelSupport.recordToInputValue\n"
    where
        modelName = qualifiedConstructorNameFromTableName table.name

-- | Returns all the type arguments of the data structure for an entity
dataTypeArguments :: (?schema :: Schema) => CreateTable -> [Text]
dataTypeArguments table = (map columnNameToFieldName belongsToVariables) <> hasManyVariables
    where
        belongsToVariables = variableAttributes table |> map (.name)
        hasManyVariables =
            columnsReferencingTable table.name
            |> compileQueryBuilderFields
            |> map snd

-- | Returns the field names and types for the @data MyRecord = MyRecord { .. }@ for a given table
dataFields :: (?schema :: Schema) => CreateTable -> [(Text, Text)]
dataFields table@(CreateTable { name, columns }) = columnFields <> queryBuilderFields <> [("meta", "MetaBag")]
    where
        columnFields = columns |> map columnField

        columnField column =
            let fieldName = columnNameToFieldName column.name
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
        isFkConstraint (AddConstraint { tableName, constraint = ForeignKeyConstraint { columnName }}) = tableName == name && columnName == column.name
        isFkConstraint _ = False

        (Schema statements) = ?schema

compileEnumDataDefinitions :: (?schema :: Schema) => Statement -> Text
compileEnumDataDefinitions CreateEnumType { values = [] } = "" -- Ignore enums without any values
compileEnumDataDefinitions enum@(CreateEnumType { name, values }) =
        "data " <> modelName <> " = " <> (intercalate " | " valueConstructors) <> " deriving (Eq, Show, Read, Enum, Bounded, Ord)\n"
        <> "instance FromField " <> modelName <> " where\n"
        <> "    fromField = Decoders.enum \\case\n"
        <> indent (indent (unlines (map compileFromFieldCase values)))
        <> "        _ -> Nothing\n"
        <> "instance Default " <> modelName <> " where def = " <> enumValueToConstructorName (unsafeHead values) <> "\n"
        <> "instance DefaultParamEncoder " <> modelName <> " where\n"
        <> indent (unlines (map compileDefaultParamEncoderCase values))
        <> "instance InputValue " <> modelName <> " where\n" <> indent (unlines (map compileInputValue values))
        <> "instance DeepSeq.NFData " <> modelName <> " where" <> " rnf a = seq a ()" <> "\n"
        <> "instance IHP.Controller.Param.ParamReader " <> modelName <> " where readParameter = IHP.Controller.Param.enumParamReader; readParameterJSON = IHP.Controller.Param.enumParamReaderJSON\n"
    where
        modelName = tableNameToModelName name
        valueConstructors = map enumValueToConstructorName values

        enumValueToConstructorName :: Text -> Text
        enumValueToConstructorName enumValue = if isEnumValueUniqueInSchema enumValue
                then enumValueToControllerName enumValue
                else modelName <> (enumValueToControllerName enumValue)

        compileFromFieldCase value = tshow value <> " -> Just " <> enumValueToConstructorName value
        compileDefaultParamEncoderCase value = "-- TODO: DefaultParamEncoder for " <> enumValueToConstructorName value
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

qualifiedConstructorNameFromTableName :: Text -> Text
qualifiedConstructorNameFromTableName unqualifiedName = "Generated.ActualTypes." <> (tableNameToModelName unqualifiedName)

compileCreate :: CreateTable -> Text
compileCreate table@(CreateTable { name, columns }) =
    let
        writableColumns = onlyWritableColumns columns
        modelName = qualifiedConstructorNameFromTableName name
        columnNames = commaSep (map (.name) writableColumns)
        allColumnNames = commaSep (map (.name) columns)

        toSnippetBinding column@(Column { name }) =
                if hasExplicitOrImplicitDefault column && not isArrayColumn
                    then "case fieldWithDefault #" <> columnNameToFieldName name <> " model of { Default -> sql \"DEFAULT\"; NonDefault v -> param v" <> typeCast <> " }"
                    else "param model." <> columnNameToFieldName name <> typeCast
            where
                isArrayColumn = case column.columnType of
                    PArray _ -> True
                    _        -> False
                typeCast = case column.columnType of
                    PArray _ -> " <> sql \" :: " <> SqlCompiler.compilePostgresType column.columnType <> "\""
                    _ -> ""

        snippetBindings :: [Text]
        snippetBindings = map toSnippetBinding writableColumns

        valuesSnippet = intercalate " <> sql \", \" <> " snippetBindings

        createManySnippetBindings :: Text
        createManySnippetBindings = if null snippetBindings
            then "sql \"()\""
            else "mconcat $ List.intersperse (sql \", \") $ List.map (\\model -> sql \"(\" <> " <> valuesSnippet <> " <> sql \")\") models"
    in
        "instance CanCreate " <> modelName <> " where\n"
        <> indent (
            "create :: (?modelContext :: ModelContext) => " <> modelName <> " -> IO " <> modelName <> "\n"
                <> "create model = do\n"
                <> indent ("let theSnippet = sql \"INSERT INTO " <> name <> " (" <> columnNames <> ") VALUES (\" <> " <> valuesSnippet <> " <> sql \") RETURNING " <> allColumnNames <> "\"\n"
                    <> "let decoder = Decoders.rowList (fromRow @" <> modelName <> ")\n"
                    <> "sqlQuerySingleRow theSnippet decoder\n")
                <> "createMany [] = pure []\n"
                <> "createMany models = do\n"
                <> indent ("let theSnippet = sql \"INSERT INTO " <> name <> " (" <> columnNames <> ") VALUES \" <> " <> createManySnippetBindings <> " <> sql \" RETURNING " <> allColumnNames <> "\"\n"
                    <> "let decoder = Decoders.rowList (fromRow @" <> modelName <> ")\n"
                    <> "sqlQuery theSnippet decoder\n"
                    )
            )
        <> indent (
            "createRecordDiscardResult :: (?modelContext :: ModelContext) => " <> modelName <> " -> IO ()\n"
                <> "createRecordDiscardResult model = do\n"
                <> indent ("let theSnippet = sql \"INSERT INTO " <> name <> " (" <> columnNames <> ") VALUES (\" <> " <> valuesSnippet <> " <> sql \")\"\n"
                    <> "sqlExecDiscardResult theSnippet\n")
            )

commaSep :: [Text] -> Text
commaSep = intercalate ", "

onlyWritableColumns columns = columns |> filter (\Column { generator } -> isNothing generator)

compileUpdate :: CreateTable -> Text
compileUpdate table@(CreateTable { name, columns }) =
    let
        modelName = qualifiedConstructorNameFromTableName name
        writableColumns = onlyWritableColumns columns

        toUpdateSnippet :: Column -> Text
        toUpdateSnippet column@Column { name = colName } =
            "sql \"" <> colName <> " = \" <> case fieldWithUpdate #" <> columnNameToFieldName colName <> " model of { NoUpdate _ -> sql \"" <> colName <> "\"; Update v -> param v" <> typeCast column <> " }"
            where
                typeCast col = if columnPlaceholderNeedsTypecast col
                    then " <> sql \" :: " <> SqlCompiler.compilePostgresType col.columnType <> "\""
                    else ""
                columnPlaceholderNeedsTypecast Column { columnType = PArray {} } = True
                columnPlaceholderNeedsTypecast _ = False

        updateSnippets = intercalate " <> sql \", \" <> " (map toUpdateSnippet writableColumns)

        toPrimaryKeySnippet :: Column -> Text
        toPrimaryKeySnippet Column { name = colName } = "param model." <> columnNameToFieldName colName

        primaryKeySnippets = intercalate " <> sql \", \" <> " (map toPrimaryKeySnippet (primaryKeyColumns table))

        allColumnNames = columns
                |> map (.name)
                |> intercalate ", "

        primaryKeyPattern = case primaryKeyColumns table of
                                [] -> error $ "Impossible happened in compileUpdate. No primary keys found for table " <> cs name <> ". At least one primary key is required."
                                [col] -> col.name
                                cols -> "(" <> commaSep (map (\col -> col.name) cols) <> ")"
    in
        "instance CanUpdate " <> modelName <> " where\n"
        <> indent ("updateRecord model = do\n"
                <> indent (
                    "let theSnippet = sql \"UPDATE " <> name <> " SET \" <> " <> updateSnippets <> " <> sql \" WHERE " <> primaryKeyPattern <> " = \" <> " <> primaryKeySnippets <> " <> sql \" RETURNING " <> allColumnNames <> "\"\n"
                    <> "let decoder = Decoders.rowList (fromRow @" <> modelName <> ")\n"
                    <> "sqlQuerySingleRow theSnippet decoder\n"
                )
            )
        <> indent ("updateRecordDiscardResult model = do\n"
                <> indent (
                    "let theSnippet = sql \"UPDATE " <> name <> " SET \" <> " <> updateSnippets <> " <> sql \" WHERE " <> primaryKeyPattern <> " = \" <> " <> primaryKeySnippets <> "\n"
                    <> "sqlExecDiscardResult theSnippet\n"
                )
            )

compileFromRowInstance :: (?schema :: Schema) => CreateTable -> Text
compileFromRowInstance table@(CreateTable { name, columns }) = cs [i|
instance FromRow #{modelName} where
    fromRow = do
#{unsafeInit . indent . indent . unlines $ map columnBinding columns}
        let theRecord = #{modelName} #{intercalate " " (map compileField (dataFields table))}
        pure theRecord

|]
    where
        modelName = qualifiedConstructorNameFromTableName name
        columnNames = map (columnNameToFieldName . (.name)) columns
        columnBinding column = let fieldName = columnNameToFieldName column.name
                                   decoder = columnDecoder table column
                               in fieldName <> " <- " <> decoder

        referencing = columnsReferencingTable table.name

        compileField (fieldName, _)
            | isColumn fieldName = fieldName
            | isOneToManyField fieldName = let (Just ref) = find (\(n, _) -> columnNameToFieldName n == fieldName) referencing in compileSetQueryBuilder ref
            | fieldName == "meta" = "def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }"
            | otherwise = "def"

        isPrimaryKey name = name `elem` primaryKeyColumnNames table.primaryKeyConstraint
        isColumn name = name `elem` columnNames
        isOneToManyField fieldName = fieldName `elem` (referencing |> map (columnNameToFieldName . fst))

        compileSetQueryBuilder (refTableName, refFieldName) = "(QueryBuilder.filterWhere (#" <> columnNameToFieldName refFieldName <> ", " <> primaryKeyField <> ") (QueryBuilder.query @" <> tableNameToModelName refTableName <> "))"
            where
                -- | When the referenced column is nullable, we have to wrap the @Id@ in @Just@
                primaryKeyField :: Text
                primaryKeyField = if refColumn.notNull then actualPrimaryKeyField else "Just " <> actualPrimaryKeyField
                actualPrimaryKeyField :: Text
                actualPrimaryKeyField = case primaryKeyColumns table of
                        [] -> error $ "Impossible happened in compilePrimaryKeyInstance. No primary keys found for table " <> cs name <> ". At least one primary key is required."
                        [pk] -> columnNameToFieldName pk.name
                        pks -> error $ "No support yet for composite foreign keys. Tables cannot have foreign keys to table '" <> cs name <> "' which has more than one column as its primary key."


                (Just refTable) = let (Schema statements) = ?schema in
                        statements
                        |> find \case
                                StatementCreateTable CreateTable { name } -> name == refTableName
                                otherwise -> False

                refColumn :: Column
                refColumn = refTable
                        |> \case StatementCreateTable CreateTable { columns } -> columns
                        |> find (\col -> col.name == refFieldName)
                        |> \case
                            Just refColumn -> refColumn
                            Nothing -> error (cs $ "Could not find " <> refTable.name <> "." <> refFieldName <> " referenced by a foreign key constraint. Make sure that there is no typo in the foreign key constraint")

compileBuild :: (?schema :: Schema) => CreateTable -> Text
compileBuild table@(CreateTable { name, columns }) =
    let
        constructor = qualifiedConstructorNameFromTableName name
    in
        "instance Record " <> constructor <> " where\n"
        <> "    {-# INLINE newRecord #-}\n"
        <> "    newRecord = " <> constructor <> " " <> unwords (map toDefaultValueExpr columns) <> " " <> (columnsReferencingTable name |> map (const "def") |> unwords) <> " def\n"

-- | Generates the hasql decoder expression for a column.
-- This produces an expression of type @Decoders.Row a@ to be used inside a @FromRow@ instance.
-- For nullable columns, it wraps the decoder with 'Decoders.nullable'.
-- For non-nullable columns, it uses 'Decoders.nonNullable'.
columnDecoder :: (?schema :: Schema) => CreateTable -> Column -> Text
columnDecoder table column@Column { columnType, notNull, generator }
    | [column.name] == primaryKeyColumnNames table.primaryKeyConstraint =
        -- Primary key columns use the Id constructor
        let innerDecoder = atomicDecoder columnType
        in if not notNull || isJust generator
            then "Decoders.column (Decoders.nullable (fmap Id " <> innerDecoder <> "))"
            else "fmap Id (Decoders.column (Decoders.nonNullable " <> innerDecoder <> "))"
    | otherwise =
        case findForeignKeyConstraint table column of
            Just (ForeignKeyConstraint { referenceTable }) ->
                -- Foreign key columns use the Id constructor wrapping the referenced table's primary key type
                let innerDecoder = atomicDecoder (foreignKeyColumnType referenceTable)
                in if not notNull || isJust generator
                    then "Decoders.column (Decoders.nullable (fmap Id " <> innerDecoder <> "))"
                    else "fmap Id (Decoders.column (Decoders.nonNullable " <> innerDecoder <> "))"
            _ ->
                let innerDecoder = atomicDecoder columnType
                in if not notNull || isJust generator
                    then "Decoders.column (Decoders.nullable " <> innerDecoder <> ")"
                    else "Decoders.column (Decoders.nonNullable " <> innerDecoder <> ")"
    where
        -- Look up the primary key type of a referenced table to determine the correct decoder
        foreignKeyColumnType :: Text -> PostgresType
        foreignKeyColumnType refTableName =
            let (Schema statements) = ?schema
            in case find (\case StatementCreateTable CreateTable { name } -> name == refTableName; _ -> False) statements of
                Just (StatementCreateTable refTable) ->
                    case primaryKeyColumns refTable of
                        [pkCol] -> pkCol.columnType
                        _ -> columnType -- fallback to the FK column's own type
                _ -> columnType -- fallback

-- | Maps a PostgreSQL type to the corresponding hasql decoder value expression.
-- Returns a @Decoders.Value a@ expression string.
atomicDecoder :: PostgresType -> Text
atomicDecoder = \case
    PSmallInt -> "(fromIntegral <$> Decoders.int2)"
    PInt -> "(fromIntegral <$> Decoders.int4)"
    PBigInt -> "(fromIntegral <$> Decoders.int8)"
    PJSONB -> "Decoders.jsonb (\\v -> case Data.Aeson.fromJSON v of { Data.Aeson.Success a -> Right a; Data.Aeson.Error e -> Left (fromString e) })"
    PText -> "Decoders.text"
    PBoolean -> "Decoders.bool"
    PTimestampWithTimezone -> "Decoders.timestamptz"
    PUUID -> "Decoders.uuid"
    PSerial -> "(fromIntegral <$> Decoders.int4)"
    PBigserial -> "(fromIntegral <$> Decoders.int8)"
    PReal -> "Decoders.float4"
    PDouble -> "Decoders.float8"
    PDate -> "Decoders.date"
    PBinary -> "(Decoders.bytea)"
    PTime -> "Decoders.time"
    (PInterval _) -> "(Decoders.custom (\\_ bytes -> Right (PGInterval bytes)))"
    PCustomType theType -> "fromField"
    PTimestamp -> "Decoders.timestamp"
    (PNumeric _ _) -> "(Decoders.numeric)"
    (PVaryingN _) -> "Decoders.text"
    (PCharacterN _) -> "Decoders.text"
    PArray type_ -> "(Decoders.listArray (Decoders.nonNullable " <> atomicDecoder type_ <> "))"
    PPoint -> "fromField"
    PPolygon -> "fromField"
    PInet -> "fromField"
    PTSVector -> "fromField"

compileDefaultIdInstance :: CreateTable -> Text
compileDefaultIdInstance table = "instance Default (Id' \"" <> table.name <> "\") where def = Id def"


toDefaultValueExpr :: Column -> Text
toDefaultValueExpr Column { columnType, notNull, defaultValue = Just theDefaultValue } =
            let
                wrapNull False value = "(Just " <> value <> ")"
                wrapNull True value = value

                isNullExpr (VarExpression varName) = toUpper varName == "NULL"
                isNullExpr _ = False

                -- We remove type casts here, as we need the actual value literal for setting our default value
                theNormalizedDefaultValue = theDefaultValue |> PostgresParser.removeTypeCasts
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
    <> "type instance GetModelByTableName " <> tshow name <> " = Generated.ActualTypes." <> tableNameToModelName name <> "\n"

compilePrimaryKeyInstance :: (?schema :: Schema) => CreateTable -> Text
compilePrimaryKeyInstance table@(CreateTable { name, columns, constraints }) = [trimming|type instance PrimaryKey $symbol = $idType|] <> "\n"
    where
        symbol = tshow name
        idType :: Text
        idType = case primaryKeyColumns table of
                [] -> error $ "Impossible happened in compilePrimaryKeyInstance. No primary keys found for table " <> cs name <> ". At least one primary key is required."
                [column] -> atomicType column.columnType -- PrimaryKey User = UUID
                cs -> "(" <> intercalate ", " (map colType cs) <> ")" -- PrimaryKey PostsTag = (Id' "posts", Id' "tags")
            where
                colType column = haskellType table column

compileFilterPrimaryKeyInstance :: (?schema :: Schema) => CreateTable -> Text
compileFilterPrimaryKeyInstance table@(CreateTable { name, columns, constraints }) = cs [i|
instance QueryBuilder.FilterPrimaryKey "#{name}" where
    filterWhereId #{primaryKeyPattern} builder =
        builder |> #{intercalate " |> " primaryKeyFilters}
    {-# INLINE filterWhereId #-}
|]
    where
        primaryKeyPattern = case primaryKeyColumns table of
            [] -> error $ "Impossible happened in compilePrimaryKeyInstance. No primary keys found for table " <> cs name <> ". At least one primary key is required."
            [c] -> columnNameToFieldName c.name
            cs -> "(Id (" <> intercalate ", " (map (columnNameToFieldName . (.name)) cs) <> "))"

        primaryKeyFilters :: [Text]
        primaryKeyFilters = map primaryKeyFilter $ primaryKeyColumns table

        primaryKeyFilter :: Column -> Text
        primaryKeyFilter Column {name} = "QueryBuilder.filterWhere (#" <> columnNameToFieldName name <> ", " <> columnNameToFieldName name <> ")"

compileTableInstance :: (?schema :: Schema) => CreateTable -> Text
compileTableInstance table@(CreateTable { name, columns, constraints }) = cs [i|
instance #{instanceHead} where
    tableName = \"#{name}\"
    tableNameByteString = Data.Text.Encoding.encodeUtf8 \"#{name}\"
    columnNames = #{columnNames}
    primaryKeyColumnNames = #{primaryKeyColumnNames}
    primaryKeyConditionForId (#{pattern}) = #{condition}
    {-# INLINABLE primaryKeyConditionForId #-}
|]
    where
        instanceHead :: Text
        instanceHead = instanceConstraints <> " => IHP.ModelSupport.Table (" <> compileTypePattern table <> ")"
            where
                instanceConstraints =
                    table
                    |> primaryKeyColumns
                    |> map (.name)
                    |> map columnNameToFieldName
                    |> filter (\field -> field `elem` (dataTypeArguments table))
                    |> map (\field -> "DefaultParamEncoder " <> field)
                    |> intercalate ", "
                    |> \inner -> "(" <> inner <> ")"

        primaryKeyColumnNames :: [Text]
        primaryKeyColumnNames = primaryKeyColumns table |> map (.name)

        primaryKeyFieldNames :: [Text]
        primaryKeyFieldNames = primaryKeyColumnNames |> map columnNameToFieldName

        pattern :: Text
        pattern = "Id (" <> intercalate ", " primaryKeyFieldNames <> ")"

        condition :: Text
        condition = case primaryKeyColumns table of
                            [] -> error $ "Impossible happened in compileUpdate. No primary keys found for table " <> cs name <> ". At least one primary key is required."
                            [column] -> primaryKeyToCondition column
                            cols -> "sql \"(\" <> " <> intercalate " <> sql \",\" <> " (map primaryKeyToCondition cols) <> " <> sql \")\""

        primaryKeyToCondition :: Column -> Text
        primaryKeyToCondition column = "param " <> columnNameToFieldName column.name

        columnNames = columns
                |> map (.name)
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
        hasManyIncludes = columnsReferencingTable name
                |> (\refs -> zip (map fst refs) (map fst (compileQueryBuilderFields refs)))
                |> map compileHasMany
        typeArgs = dataTypeArguments table
        modelName = tableNameToModelName name
        modelConstructor = modelName <> "'"

        includeType :: Text -> Text -> Text
        includeType fieldName includedType = "type instance Include " <> tshow fieldName <> " (" <> leftModelType <> ") = " <> rightModelType
            where
                leftModelType = unwords (modelConstructor:typeArgs)
                rightModelType = unwords (modelConstructor:(map compileTypeVariable' typeArgs))
                compileTypeVariable' name | name == fieldName = includedType
                compileTypeVariable' name = name

        compileBelongsTo :: Column -> Text
        compileBelongsTo column = includeType (columnNameToFieldName column.name) ("(GetModelById " <> columnNameToFieldName column.name <> ")")

        compileHasMany :: (Text, Text) -> Text
        compileHasMany (refTableName, refColumnName) = includeType refColumnName ("[" <> tableNameToModelName refTableName <> "]")


compileSetFieldInstances :: (?schema :: Schema) => CreateTable -> Text
compileSetFieldInstances table@(CreateTable { name, columns }) = unlines (map compileSetField (dataFields table))
    where
        setMetaField = "instance SetField \"meta\" (" <> compileTypePattern table <>  ") MetaBag where\n    {-# INLINE setField #-}\n    setField newValue (" <> compileDataTypePattern table <> ") = " <> tableNameToModelName name <> " " <> (unwords (map (.name) columns)) <> " newValue"
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
                compileTypePattern' name = tableNameToModelName table.name <> "' " <> unwords (map (\f -> if f == name then name <> "'" else f) (dataTypeArguments table))

compileHasFieldId :: (?schema :: Schema) => CreateTable -> Text
compileHasFieldId table@CreateTable { name, primaryKeyConstraint } = cs [i|
instance HasField "id" #{tableNameToModelName name} (Id' "#{name}") where
    getField (#{compileDataTypePattern table}) = #{compilePrimaryKeyValue}
    {-# INLINE getField #-}
|]
    where
        compilePrimaryKeyValue = case primaryKeyColumnNames primaryKeyConstraint of
            [id] -> columnNameToFieldName id
            ids -> "Id (" <> commaSep (map columnNameToFieldName ids) <> ")"

needsHasFieldId :: CreateTable -> Bool
needsHasFieldId CreateTable { primaryKeyConstraint } =
  case primaryKeyColumnNames primaryKeyConstraint of
    [] -> False
    ["id"] -> False
    _ -> True

primaryKeyColumns :: CreateTable -> [Column]
primaryKeyColumns CreateTable { name, columns, primaryKeyConstraint } =
    map getColumn (primaryKeyColumnNames primaryKeyConstraint)
  where
    getColumn columnName = case find ((==) columnName . (.name)) columns of
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
