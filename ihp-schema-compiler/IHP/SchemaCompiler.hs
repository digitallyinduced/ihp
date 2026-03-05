module IHP.SchemaCompiler
( compile
, compileStatementPreview
, compileStatementPreviewWith
, CompilerOptions(..)
, fullCompileOptions
, previewCompilerOptions
, compileCreateStatement
, compileUpdateStatement
, compileFetchByIdStatement
, Schema(..)
) where

import ClassyPrelude
import Data.Maybe (fromJust)
import Data.String.Conversions (cs)
import "interpolate" Data.String.Interpolate (i)
import IHP.NameSupport (tableNameToModelName, columnNameToFieldName, enumValueToControllerName)
import qualified Data.Text as Text
import qualified System.Directory.OsPath as Directory
import IHP.HaskellSupport
import qualified IHP.SchemaCompiler.Parser as SchemaDesigner
import qualified IHP.Postgres.Parser as PostgresParser
import IHP.Postgres.Types
import qualified Control.Exception as Exception
import qualified System.Environment
import NeatInterpolation
import Text.Countable (pluralize)
import System.OsPath (OsPath, encodeUtf, decodeUtf)
import qualified System.OsPath as OsPath


data CompileException = CompileException ByteString deriving (Show)
instance Exception CompileException where
    displayException (CompileException message) = cs message

compile :: IO ()
compile = do
    relationSupport <- System.Environment.lookupEnv "IHP_RELATION_SUPPORT"
    let options = fullCompileOptions { compileRelationSupport = relationSupport /= Just "0" }
    SchemaDesigner.parseSchemaSql >>= \case
        Left parserError -> Exception.throwIO (CompileException parserError)
        Right statements -> do
            -- let validationErrors = validate database
            -- unless (null validationErrors) (error $ "Schema.hs contains errors: " <> cs (unsafeHead validationErrors))
            Directory.createDirectoryIfMissing True "build/Generated"
            Directory.createDirectoryIfMissing True "build/Generated/ActualTypes"
            Directory.createDirectoryIfMissing True "build/Generated/Statements"

            forEach (compileModules options (Schema statements)) \(path, body) -> do
                    writeIfDifferent path body

compileModules :: CompilerOptions -> Schema -> [(OsPath, Text)]
compileModules options schema =
    let ?compilerOptions = options
    in [ ("build/Generated/Enums.hs", compileEnums options schema)
       , ("build/Generated/ActualTypes/PrimaryKeys.hs", compilePrimaryKeysModule schema)
       ]
       <> actualTypesTableModules schema
       <> [ ("build/Generated/ActualTypes.hs", compileTypes options schema) ]
       <> tableModules options schema
       <> statementModules schema
       <> [ ("build/Generated/Statements.hs", compileStatementsIndex schema)
          , ("build/Generated/Types.hs", compileIndex schema)
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

actualTypesTableModules :: (?compilerOptions :: CompilerOptions) => Schema -> [(OsPath, Text)]
actualTypesTableModules schema =
    let ?schema = schema
    in applyTables actualTypesTableModule schema

actualTypesTableModule :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> (OsPath, Text)
actualTypesTableModule table =
        ((OsPath.</>) "build/Generated/ActualTypes" (either (error . show) id (encodeUtf (cs (tableNameToModelName table.name) <> ".hs"))), body)
    where
        body = Text.unlines
            [ prelude
            , compileActualTypesForTable table
            ]
        moduleName = "Generated.ActualTypes." <> tableNameToModelName table.name
        prelude = [trimming|
            -- This file is auto generated and will be overriden regulary. Please edit `Application/Schema.sql` to change the Types\n"
            {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, TypeFamilies, DataKinds, TypeOperators, UndecidableInstances, ConstraintKinds, StandaloneDeriving  #-}
            {-# OPTIONS_GHC -Wno-unused-imports -Wno-dodgy-imports -Wno-unused-matches #-}
            module $moduleName where
            $defaultImports
            import Generated.Enums
            import Generated.ActualTypes.PrimaryKeys
        |]

tableModules :: (?compilerOptions :: CompilerOptions) => CompilerOptions -> Schema -> [(OsPath, Text)]
tableModules options schema =
    let ?schema = schema
    in
        applyTables (tableModule options) schema
        <> if options.compileRelationSupport
            then applyTables tableIncludeModule schema
            else []

tableModule :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CompilerOptions -> CreateTable -> (OsPath, Text)
tableModule options table =
        ((OsPath.</>) "build/Generated" (either (error . show) id (encodeUtf (cs (tableNameToModelName table.name) <> ".hs"))), body)
    where
        body = Text.unlines
            [ prelude
            , tableModuleBody options table
            ]
        moduleName = "Generated." <> tableNameToModelName table.name
        modelName = tableNameToModelName table.name
        statementImports = Text.unlines
            [ "import qualified Generated.Statements.Create" <> modelName
            , "import qualified Generated.Statements.Update" <> modelName
            ]
        prelude = [trimming|
            -- This file is auto generated and will be overriden regulary. Please edit `Application/Schema.sql` to change the Types\n"
            {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, TypeFamilies, DataKinds, TypeOperators, UndecidableInstances, ConstraintKinds, StandaloneDeriving  #-}
            {-# OPTIONS_GHC -Wno-unused-imports -Wno-dodgy-imports -Wno-unused-matches #-}
            module $moduleName where
            $defaultImports
            import Generated.ActualTypes
            $statementImports
        |]

tableIncludeModule :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> (OsPath, Text)
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


tableModuleBody :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CompilerOptions -> CreateTable -> Text
tableModuleBody options table = Text.unlines
    [ compileInputValueInstance table
    , compileFromRowInstance table
    , compileFromRowHasqlInstance table
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
        compileGetAndSetFieldInstances :: Bool,
        -- | When disabled, generates simpler types without Include\/fetchRelated type machinery.
        -- This removes type parameters from record types, removes has-many QueryBuilder fields,
        -- and skips generating Include modules. Set @IHP_RELATION_SUPPORT=0@ to disable.
        compileRelationSupport :: Bool
    }

fullCompileOptions :: CompilerOptions
fullCompileOptions = CompilerOptions { compileGetAndSetFieldInstances = True, compileRelationSupport = True }

previewCompilerOptions :: CompilerOptions
previewCompilerOptions = CompilerOptions { compileGetAndSetFieldInstances = False, compileRelationSupport = True }

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
    (PInterval _) -> "Interval"
    PCustomType theType -> tableNameToModelName theType
    PTimestamp -> "LocalTime"
    (PNumeric _ _) -> "Scientific"
    (PVaryingN _) -> "Text"
    (PCharacterN _) -> "Text"
    PArray type_ -> "[" <> atomicType type_ <> "]"
    PPoint -> "Point"
    PPolygon -> "Polygon"
    PInet -> "Inet"
    PTSVector -> "Tsvector"
    PSingleChar -> "Text"
    PTrigger -> error "atomicType: PTrigger not supported"
    PEventTrigger -> error "atomicType: PEventTrigger not supported"

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

compileTypes :: (?compilerOptions :: CompilerOptions) => CompilerOptions -> Schema -> Text
compileTypes options schema@(Schema statements) = [trimming|
        -- This file is auto generated and will be overriden regulary. Please edit `Application/Schema.sql` to change the Types\n"
        {-# OPTIONS_GHC -Wno-unused-imports -Wno-dodgy-imports -Wno-unused-matches #-}
        module Generated.ActualTypes ($rexports) where
        import Generated.Enums
        import Generated.ActualTypes.PrimaryKeys
        $perTableImports
    |]
    where
        tableModelNames =
            statements
            |> mapMaybe (\case
                StatementCreateTable table | tableHasPrimaryKey table -> Just (tableNameToModelName table.name)
                otherwise -> Nothing
            )

        perTableModuleNames = map (\n -> "Generated.ActualTypes." <> n) tableModelNames

        perTableImports = perTableModuleNames
            |> map (\n -> "import " <> n)
            |> Text.unlines

        rexports = ("module Generated.Enums" : "module Generated.ActualTypes.PrimaryKeys" : map (\n -> "module " <> n) perTableModuleNames)
            |> Text.intercalate ", "

compileActualTypesForTable :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileActualTypesForTable table = Text.unlines
    [ compileData table
    , compileTypeAlias table
    , compileHasTableNameInstance table
    , compileTableInstance table
    ]

-- | Like 'compileActualTypesForTable' but includes PrimaryKey and Default Id' instances inline.
-- Used by 'compileStatementPreviewWith' so the IDE preview and tests see a self-contained output.
compileActualTypesForTablePreview :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileActualTypesForTablePreview table = Text.unlines
    [ compileData table
    , compilePrimaryKeyInstance table
    , compileTypeAlias table
    , compileHasTableNameInstance table
    , compileDefaultIdInstance table
    , compileTableInstance table
    ]

compileIndex :: (?compilerOptions :: CompilerOptions) => Schema -> Text
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
                                [ "Generated." <> modelName ]
                                <> if ?compilerOptions.compileRelationSupport
                                    then [ "Generated." <> modelName <> "Include" ]
                                    else []
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
    import Database.PostgreSQL.Simple
    import Database.PostgreSQL.Simple.FromRow
    import Database.PostgreSQL.Simple.FromField hiding (Field, name)
    import Database.PostgreSQL.Simple.ToField hiding (Field)
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
    import Database.PostgreSQL.Simple.Types (Query (Query), Binary ( .. ))
    import qualified Database.PostgreSQL.Simple.Types
    import IHP.Job.Types
    import IHP.Job.Queue (textToEnumJobStatus)
    import qualified Control.DeepSeq as DeepSeq
    import qualified Data.Dynamic
    import Data.Scientific
    import IHP.Hasql.FromRow (FromRowHasql(..))
    import qualified Hasql.Decoders as Decoders
    import qualified Hasql.Encoders
    import qualified Hasql.Implicits.Encoders
    import qualified Hasql.DynamicStatements.Snippet as Snippet
    import IHP.Hasql.Encoders ()
    import qualified Hasql.Mapping.IsScalar as Mapping
    import Hasql.PostgresqlTypes ()
    import qualified Data.Set
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
            import Database.PostgreSQL.Simple
            import Database.PostgreSQL.Simple.FromField hiding (Field, name)
            import Database.PostgreSQL.Simple.ToField hiding (Field)
            import qualified IHP.Controller.Param
            import Data.Default
            import qualified IHP.QueryBuilder as QueryBuilder
            import qualified Data.String.Conversions
            import qualified Data.Text.Encoding
            import qualified Control.DeepSeq as DeepSeq
            import qualified Hasql.Encoders
            import qualified Hasql.Decoders
            import qualified Hasql.Implicits.Encoders
            import qualified Hasql.Mapping.IsScalar as Mapping
            import qualified Data.HashMap.Strict as HashMap
        |]

compilePrimaryKeysModule :: (?compilerOptions :: CompilerOptions) => Schema -> Text
compilePrimaryKeysModule schema@(Schema statements) =
    let ?schema = schema
    in Text.unlines
        [ prelude
        , statements
            |> mapMaybe (\case
                StatementCreateTable table | tableHasPrimaryKey table ->
                    Just (compilePrimaryKeyInstance table <> compileDefaultIdInstance table)
                _ -> Nothing)
            |> Text.intercalate "\n"
        ]
    where
        prelude = [trimming|
            -- This file is auto generated and will be overriden regulary. Please edit `Application/Schema.sql` to change the Types\n"
            {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, TypeFamilies, DataKinds, TypeOperators, UndecidableInstances, ConstraintKinds, StandaloneDeriving  #-}
            {-# OPTIONS_GHC -Wno-unused-imports -Wno-dodgy-imports -Wno-unused-matches #-}
            module Generated.ActualTypes.PrimaryKeys where
            $defaultImports
            import Generated.Enums
        |]

compileStatementPreview :: [Statement] -> Statement -> Text
compileStatementPreview = compileStatementPreviewWith previewCompilerOptions

compileStatementPreviewWith :: CompilerOptions -> [Statement] -> Statement -> Text
compileStatementPreviewWith options statements statement =
    let ?schema = Schema statements
        ?compilerOptions = options
    in
        case statement of
            CreateEnumType {} -> compileEnumDataDefinitions statement
            StatementCreateTable table -> Text.unlines
                [ compileActualTypesForTablePreview table
                , tableModuleBody options table
                ]
            _ -> ""

-- | Skip generation of tables with no primary keys
tableHasPrimaryKey :: CreateTable -> Bool
tableHasPrimaryKey table = table.primaryKeyConstraint /= (PrimaryKeyConstraint [])

compileTypeAlias :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileTypeAlias table@(CreateTable { name, columns }) =
        "type "
        <> modelName
        <> " = "
        <> modelName
        <> "'"
        <> spacePrefix (unwords (map (haskellType table) (variableAttributes table)) <> hasManyDefaults)
        <> "\n"
    where
        modelName = tableNameToModelName name
        hasManyDefaults
            | ?compilerOptions.compileRelationSupport =
                columnsReferencingTable name
                |> map (\(tableName, columnName) -> "(QueryBuilder.QueryBuilder \"" <> tableName <> "\")")
                |> unwords
            | otherwise = ""

primaryKeyTypeName :: Text -> Text
primaryKeyTypeName name = "Id' " <> tshow name <> ""

compileData :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileData table@(CreateTable { name, columns }) =
        "data " <> modelName <> "'" <> spacePrefix typeArguments
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
dataTypeArguments :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> [Text]
dataTypeArguments table
    | not ?compilerOptions.compileRelationSupport = []
    | otherwise = (map columnNameToFieldName belongsToVariables) <> hasManyVariables
    where
        belongsToVariables = variableAttributes table |> map (.name)
        hasManyVariables =
            columnsReferencingTable table.name
            |> compileQueryBuilderFields
            |> map snd

-- | Returns the field names and types for the @data MyRecord = MyRecord { .. }@ for a given table
dataFields :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> [(Text, Text)]
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

        queryBuilderFields
            | ?compilerOptions.compileRelationSupport = columnsReferencingTable name |> compileQueryBuilderFields
            | otherwise = []

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

variableAttributes :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> [Column]
variableAttributes table@(CreateTable { columns }) = filter (isVariableAttribute table) columns

isVariableAttribute :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Column -> Bool
isVariableAttribute table column
    | not ?compilerOptions.compileRelationSupport = False
    | otherwise = isRefCol table column


-- | Returns @True@ when the coluns is referencing another column via foreign key constraint
isRefCol :: (?schema :: Schema) => CreateTable -> Column -> Bool
isRefCol table column = isJust (findForeignKeyConstraint table column)

-- | Returns the foreign key constraint bound on the given column
findForeignKeyConstraint :: (?schema :: Schema) => CreateTable -> Column -> Maybe Constraint
findForeignKeyConstraint CreateTable { name } column =
        case find isFkConstraint statements of
            Just (AddConstraint { constraint }) -> Just constraint
            _ -> Nothing
    where
        isFkConstraint (AddConstraint { tableName, constraint = ForeignKeyConstraint { columnName }}) = tableName == name && columnName == column.name
        isFkConstraint _ = False

        (Schema statements) = ?schema

compileEnumDataDefinitions :: (?schema :: Schema) => Statement -> Text
compileEnumDataDefinitions CreateEnumType { values = [] } = "" -- Ignore enums without any values
compileEnumDataDefinitions enum@(CreateEnumType { name, values }) =
        "data " <> modelName <> " = " <> (intercalate " | " valueConstructors) <> " deriving (Eq, Show, Read, Enum, Bounded, Ord)\n"
        <> "instance FromField " <> modelName <> " where\n"
        <> indent (unlines (map compileFromFieldInstanceForValue values))
        <> "    fromField field (Just value) = returnError ConversionFailed field (\"Unexpected value for enum value. Got: \" <> Data.String.Conversions.cs value)\n"
        <> "    fromField field Nothing = returnError UnexpectedNull field \"Unexpected null for enum value\"\n"
        <> "instance Default " <> modelName <> " where def = " <> enumValueToConstructorName (unsafeHead values) <> "\n"
        <> "instance ToField " <> modelName <> " where\n" <> indent (unlines (map compileToFieldInstanceForValue values))
        <> "instance InputValue " <> modelName <> " where\n" <> indent (unlines (map compileInputValue values))
        <> "instance DeepSeq.NFData " <> modelName <> " where" <> " rnf a = seq a ()" <> "\n"
        <> "instance IHP.Controller.Param.ParamReader " <> modelName <> " where readParameter = IHP.Controller.Param.enumParamReader; readParameterJSON = IHP.Controller.Param.enumParamReaderJSON\n"
        -- textToEnum function for hasql decoder using HashMap for O(1) lookup
        <> "textToEnum" <> modelName <> "Map :: HashMap.HashMap Text " <> modelName <> "\n"
        <> "textToEnum" <> modelName <> "Map = HashMap.fromList [" <> intercalate ", " (map compileTextToEnumMapEntry values) <> "]\n"
        <> "textToEnum" <> modelName <> " :: Text -> Maybe " <> modelName <> "\n"
        <> "textToEnum" <> modelName <> " t = HashMap.lookup t textToEnum" <> modelName <> "Map\n"
        -- DefaultParamEncoder for hasql queries
        <> "instance Hasql.Implicits.Encoders.DefaultParamEncoder " <> modelName <> " where\n"
        <> "    defaultParam = Hasql.Encoders.nonNullable (Hasql.Encoders.enum (Just \"public\") " <> tshow (Text.toLower name) <> " inputValue)\n"
        <> "instance Hasql.Implicits.Encoders.DefaultParamEncoder (Maybe " <> modelName <> ") where\n"
        <> "    defaultParam = Hasql.Encoders.nullable (Hasql.Encoders.enum (Just \"public\") " <> tshow (Text.toLower name) <> " inputValue)\n"
        <> "instance Hasql.Implicits.Encoders.DefaultParamEncoder [" <> modelName <> "] where\n"
        <> "    defaultParam = Hasql.Encoders.nonNullable $ Hasql.Encoders.foldableArray $ Hasql.Encoders.nonNullable (Hasql.Encoders.enum (Just \"public\") " <> tshow (Text.toLower name) <> " inputValue)\n"
        -- IsScalar instance for hasql-mapping (used by generated statement modules)
        <> "instance Mapping.IsScalar " <> modelName <> " where\n"
        <> "    encoder = Hasql.Encoders.enum (Just \"public\") " <> tshow (Text.toLower name) <> " inputValue\n"
        <> "    decoder = Hasql.Decoders.enum (Just \"public\") " <> tshow (Text.toLower name) <> " textToEnum" <> modelName <> "\n"
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
        compileTextToEnumMapEntry value = "(" <> tshow value <> ", " <> enumValueToConstructorName value <> ")"

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
compileEnumDataDefinitions _ = ""

qualifiedConstructorNameFromTableName :: Text -> Text
qualifiedConstructorNameFromTableName unqualifiedName = "Generated.ActualTypes." <> (tableNameToModelName unqualifiedName)

-- | Check if a column type has a native hasql encoder via DefaultParamEncoder
--
-- Trigger types don't have encoders as they're not real data columns.
hasqlSupportsColumnType :: PostgresType -> Bool
hasqlSupportsColumnType = \case
    PTrigger -> False
    PEventTrigger -> False
    (PArray inner) -> hasqlSupportsColumnType inner
    _ -> True

compileCreate :: CreateTable -> Text
compileCreate table@(CreateTable { name, columns }) =
    let
        writableColumns = onlyWritableColumns columns
        modelName = qualifiedConstructorNameFromTableName name
        funcName = tableNameToModelName name
        columnNames = commaSep (map (.name) writableColumns)
        allColumnNames = commaSep (map (.name) columns)

        -- Hasql snippet bindings for INSERT VALUES
        toSnippetBinding column@(Column { name }) =
                if hasExplicitOrImplicitDefault column && not isArrayColumn
                    then "fieldWithDefaultSnippet #" <> columnNameToFieldName name <> " model"
                    else "Snippet.param model." <> columnNameToFieldName name
            where
                isArrayColumn = case column.columnType of
                    PArray _ -> True
                    _        -> False

        snippetBindings :: [Text]
        snippetBindings = map toSnippetBinding writableColumns

        -- Build the snippet expression for a single INSERT VALUES clause
        snippetValueExpr = intercalate " <> Snippet.sql \", \" <> " snippetBindings

        -- For createMany, build snippet for each model
        snippetCreateManyValueExpr = "mconcat $ List.intersperse (Snippet.sql \", \") $ List.map (\\model -> Snippet.sql \"(\" <> " <> snippetValueExpr <> " <> Snippet.sql \")\") models"

        -- Hasql bodies
        isDynamic = hasAnyDefaults writableColumns
        hasqlCreateBody = if isDynamic
            then "let pool = ?modelContext.hasqlPool\n"
                <> "let touched = Data.Set.fromList model.meta.touchedFields\n"
                <> "sqlStatementHasql pool model (Generated.Statements.Create" <> funcName <> ".statement touched)"
            else "let pool = ?modelContext.hasqlPool\n"
                <> "sqlStatementHasql pool model Generated.Statements.Create" <> funcName <> ".statement"
        hasqlCreateManyBody = "let pool = ?modelContext.hasqlPool\n"
                <> "let snippet = Snippet.sql \"INSERT INTO " <> name <> " (" <> columnNames <> ") VALUES \" <> (" <> snippetCreateManyValueExpr <> ") <> Snippet.sql \" RETURNING " <> allColumnNames <> "\"\n"
                <> "sqlQueryHasql pool snippet (Decoders.rowList (hasqlRowDecoder @" <> modelName <> "))"
        hasqlCreateDiscardBody = "let pool = ?modelContext.hasqlPool\n"
                <> "let snippet = Snippet.sql \"INSERT INTO " <> name <> " (" <> columnNames <> ") VALUES (\" <> " <> snippetValueExpr <> " <> Snippet.sql \")\"\n"
                <> "sqlExecHasql pool snippet"
    in
        -- Instance block: delegate to top-level functions
        "instance CanCreate " <> modelName <> " where\n"
        <> indent ("create = create" <> funcName <> "\n")
        <> indent ("createMany = createMany" <> funcName <> "\n")
        <> indent ("createRecordDiscardResult = createRecordDiscardResult" <> funcName <> "\n")
        -- create<Model>
        <> "\n"
        <> "create" <> funcName <> " :: (?modelContext :: ModelContext) => " <> modelName <> " -> IO " <> modelName <> "\n"
        <> "create" <> funcName <> " model = do\n"
        <> indent (hasqlCreateBody <> "\n")
        -- createMany<Model>
        <> "\n"
        <> "createMany" <> funcName <> " :: (?modelContext :: ModelContext) => [" <> modelName <> "] -> IO [" <> modelName <> "]\n"
        <> "createMany" <> funcName <> " [] = pure []\n"
        <> "createMany" <> funcName <> " models = do\n"
        <> indent (hasqlCreateManyBody <> "\n")
        -- createRecordDiscardResult<Model>
        <> "\n"
        <> "createRecordDiscardResult" <> funcName <> " :: (?modelContext :: ModelContext) => " <> modelName <> " -> IO ()\n"
        <> "createRecordDiscardResult" <> funcName <> " model = do\n"
        <> indent (hasqlCreateDiscardBody <> "\n")

commaSep :: [Text] -> Text
commaSep = intercalate ", "

-- | Prefixes text with a space if non-empty, returns empty text otherwise.
-- Avoids trailing spaces when type arguments are empty.
spacePrefix :: Text -> Text
spacePrefix "" = ""
spacePrefix t = " " <> t

toBinding :: Text -> Column -> Text
toBinding modelName Column { name } = "let " <> modelName <> "{" <> columnNameToFieldName name <> "} = model in " <> columnNameToFieldName name

onlyWritableColumns columns = columns |> filter (\Column { generator } -> isNothing generator)

compileUpdate :: CreateTable -> Text
compileUpdate table@(CreateTable { name, columns }) =
    let
        modelName = qualifiedConstructorNameFromTableName name
        funcName = tableNameToModelName name
        writableColumns = onlyWritableColumns columns

        allColumnNames = columns
                |> map (.name)
                |> intercalate ", "

        -- Hasql snippet for SET clause: "col1 = " <> snippetBinding <> ", col2 = " <> ...
        snippetSetClause = intercalate " <> Snippet.sql \", \" <> " $
            map (\column -> "Snippet.sql \"" <> column.name <> " = \" <> fieldWithUpdateSnippet #" <> columnNameToFieldName column.name <> " model") writableColumns

        -- Hasql snippet for WHERE clause
        snippetWhereClause = case primaryKeyColumns table of
            [] -> error $ "Impossible happened in compileUpdate. No primary keys found for table " <> cs name
            [col] -> "Snippet.sql \"" <> col.name <> " = \" <> Snippet.param model." <> columnNameToFieldName col.name
            cols -> "Snippet.sql \"(" <> commaSep (map (.name) cols) <> ") = (\" <> "
                    <> intercalate " <> Snippet.sql \", \" <> " (map (\col -> "Snippet.param model." <> columnNameToFieldName col.name) cols)
                    <> " <> Snippet.sql \")\""

        -- Hasql bodies: skip DB round-trip when nothing is touched
        hasqlUpdateBody = "let touched = Data.Set.fromList model.meta.touchedFields\n"
                <> "if Data.Set.null touched then pure model else do\n"
                <> "let pool = ?modelContext.hasqlPool\n"
                <> "sqlStatementHasql pool model (Generated.Statements.Update" <> funcName <> ".statement touched)"
        hasqlUpdateDiscardBody = "let pool = ?modelContext.hasqlPool\n"
                <> "let snippet = Snippet.sql \"UPDATE " <> name <> " SET \" <> " <> snippetSetClause <> " <> Snippet.sql \" WHERE \" <> " <> snippetWhereClause <> "\n"
                <> "sqlExecHasql pool snippet"
    in
        -- Instance block: delegate to top-level functions
        "instance CanUpdate " <> modelName <> " where\n"
        <> indent ("updateRecord = updateRecord" <> funcName <> "\n")
        <> indent ("updateRecordDiscardResult = updateRecordDiscardResult" <> funcName <> "\n")
        -- updateRecord<Model>
        <> "\n"
        <> "updateRecord" <> funcName <> " :: (?modelContext :: ModelContext) => " <> modelName <> " -> IO " <> modelName <> "\n"
        <> "updateRecord" <> funcName <> " model = do\n"
        <> indent (hasqlUpdateBody <> "\n")
        -- updateRecordDiscardResult<Model>
        <> "\n"
        <> "updateRecordDiscardResult" <> funcName <> " :: (?modelContext :: ModelContext) => " <> modelName <> " -> IO ()\n"
        <> "updateRecordDiscardResult" <> funcName <> " model = do\n"
        <> indent (hasqlUpdateDiscardBody <> "\n")

compileFromRowInstance :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileFromRowInstance table@(CreateTable { name, columns }) = cs [i|instance FromRow #{modelName} where
    fromRow = do
#{unsafeInit . indent . indent . unlines $ map columnBinding columnNames}
        let theRecord = #{modelName} #{intercalate " " (map compileField (dataFields table))}
        pure theRecord
|]
    where
        modelName = qualifiedConstructorNameFromTableName name
        columnNames = map (columnNameToFieldName . (.name)) columns
        columnBinding columnName = columnName <> " <- field"

        referencing = columnsReferencingTable table.name
        referencingWithFieldNames = zip (map fst (compileQueryBuilderFields referencing)) referencing

        compileField (fieldName, _)
            | isColumn fieldName = fieldName
            | isOneToManyField fieldName = let (Just (_, ref)) = find (\(n, _) -> n == fieldName) referencingWithFieldNames in compileFromRowQueryBuilder table ref
            | fieldName == "meta" = "def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }"
            | otherwise = "def"

        isColumn colName = colName `elem` columnNames
        isOneToManyField fieldName = fieldName `elem` (map fst referencingWithFieldNames)

-- | Generates a FromRowHasql instance for hasql-based queries
--
-- This is parallel to 'compileFromRowInstance' but generates code for the
-- hasql decoder instead of postgresql-simple's FromRow.
-- Uses applicative style (<$>/<*>) since hasql 1.10's Decoders.Row is
-- Applicative but not Monad. Column values are bound via a lambda so that
-- one-to-many QueryBuilders can reference the decoded primary key.
compileFromRowHasqlInstance :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileFromRowHasqlInstance table@(CreateTable { name, columns }) =
    let modelName = qualifiedConstructorNameFromTableName name
        decoderBody = compileRowDecoderBody table columns (compileFromRowQueryBuilder table) "        "
    in cs [i|instance FromRowHasql #{modelName} where
    hasqlRowDecoder = #{decoderBody}
|]

-- | Compile a QueryBuilder expression for a one-to-many field in FromRow/FromRowHasql instances
compileFromRowQueryBuilder :: (?schema :: Schema) => CreateTable -> (Text, Text) -> Text
compileFromRowQueryBuilder table (refTableName, refFieldName) = "(QueryBuilder.filterWhere (#" <> columnNameToFieldName refFieldName <> ", " <> primaryKeyField <> ") (QueryBuilder.query @" <> tableNameToModelName refTableName <> "))"
    where
        primaryKeyField :: Text
        primaryKeyField = if refColumn.notNull then actualPrimaryKeyField else "Just " <> actualPrimaryKeyField
        actualPrimaryKeyField :: Text
        actualPrimaryKeyField = case primaryKeyColumns table of
                [] -> error $ "Impossible happened in compileFromRowHasqlInstance. No primary keys found for table " <> cs table.name <> ". At least one primary key is required."
                [pk] -> columnNameToFieldName pk.name
                pks -> error $ "No support yet for composite foreign keys. Tables cannot have foreign keys to table '" <> cs table.name <> "' which has more than one column as its primary key."

        (Just refTable) = let (Schema statements) = ?schema in
                statements
                |> find \case
                        StatementCreateTable CreateTable { name } -> name == refTableName
                        otherwise -> False

        refColumn :: Column
        refColumn = refTable
                |> \case StatementCreateTable CreateTable { columns } -> columns
                         _ -> error "refColumn: expected StatementCreateTable"
                |> find (\col -> col.name == refFieldName)
                |> \case
                    Just refColumn -> refColumn
                    Nothing -> error (cs $ "Could not find " <> refTable.name <> "." <> refFieldName <> " referenced by a foreign key constraint. Make sure that there is no typo in the foreign key constraint")

-- | Generate a hasql decoder expression for a column based on its PostgresType and nullability
-- Note: Generated columns are treated as nullable in the Haskell type (even if notNull=True)
-- because they're not included in INSERT statements and are computed by the database.
-- Primary key and foreign key columns are wrapped with Id.
hasqlColumnDecoder :: (?schema :: Schema) => CreateTable -> Column -> Text
hasqlColumnDecoder table column@Column { name, columnType, notNull, generator } =
    "Decoders.column (" <> nullability <> " " <> decoder <> ")"
    where
        -- Match the logic in haskellType: if not notNull OR has a generator, treat as nullable
        isNullable = not notNull || isJust generator
        nullability = if isNullable then "Decoders.nullable" else "Decoders.nonNullable"

        -- Check if this column should be wrapped with Id
        isPrimaryKey = [name] == primaryKeyColumnNames table.primaryKeyConstraint
        isForeignKey = isJust (findForeignKeyConstraint table column)
        needsIdWrapper = isPrimaryKey || isForeignKey

        baseDecoder = hasqlValueDecoder columnType
        decoder = if needsIdWrapper then "(Id <$> " <> baseDecoder <> ")" else baseDecoder

-- | Map a PostgresType to its hasql value decoder expression
hasqlValueDecoder :: PostgresType -> Text
hasqlValueDecoder = \case
    PUUID -> "Decoders.uuid"
    PText -> "Decoders.text"
    PSmallInt -> "(fromIntegral <$> Decoders.int2)"
    PInt -> "(fromIntegral <$> Decoders.int4)"
    PBigInt -> "(fromIntegral <$> Decoders.int8)"
    PSerial -> "(fromIntegral <$> Decoders.int4)"
    PBigserial -> "(fromIntegral <$> Decoders.int8)"
    PBoolean -> "Decoders.bool"
    PReal -> "Decoders.float4"
    PDouble -> "Decoders.float8"
    PTimestampWithTimezone -> "Decoders.timestamptz"
    PTimestamp -> "Decoders.timestamp"
    PDate -> "Decoders.date"
    PTime -> "Decoders.time"
    (PNumeric _ _) -> "Decoders.numeric"
    PJSONB -> "Decoders.jsonb"
    PBinary -> "(Database.PostgreSQL.Simple.Types.Binary <$> Decoders.bytea)"
    (PVaryingN _) -> "Decoders.text"
    (PCharacterN _) -> "Decoders.text"
    (PInterval _) -> "Mapping.decoder"
    PPoint -> "Mapping.decoder"
    PPolygon -> "Mapping.decoder"
    PInet -> "Mapping.decoder"
    PTSVector -> "Mapping.decoder"
    PArray innerType -> "(Decoders.listArray (" <> hasqlArrayElementDecoder innerType <> "))"
    PCustomType _ -> "Mapping.decoder"
    PSingleChar -> "Decoders.char"
    PTrigger -> "Decoders.text"  -- Trigger types shouldn't appear in table columns
    PEventTrigger -> "Decoders.text"  -- Event trigger types shouldn't appear in table columns

-- | For array elements, we need to specify nullability (assuming non-nullable elements)
hasqlArrayElementDecoder :: PostgresType -> Text
hasqlArrayElementDecoder innerType = "Decoders.nonNullable " <> hasqlValueDecoder innerType

compileBuild :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileBuild table@(CreateTable { name, columns }) =
    let
        constructor = qualifiedConstructorNameFromTableName name
        qbDefaults = if ?compilerOptions.compileRelationSupport
            then columnsReferencingTable name |> map (const "def") |> unwords
            else ""
    in
        "instance Record " <> constructor <> " where\n"
        <> "    {-# INLINE newRecord #-}\n"
        <> "    newRecord = " <> constructor <> " " <> unwords (map toDefaultValueExpr columns) <> " " <> qbDefaults <> " def\n"


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

compileHasTableNameInstance :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileHasTableNameInstance table@(CreateTable { name }) =
    "type instance GetTableName (" <> tableNameToModelName name <> "'" <> spacePrefix (unwords (map (const "_") (dataTypeArguments table))) <>  ") = " <> tshow name <> "\n"
    <> "type instance GetModelByTableName " <> tshow name <> " = " <> tableNameToModelName name <> "\n"

compilePrimaryKeyInstance :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
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

compileFilterPrimaryKeyInstance :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
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

compileTableInstance :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileTableInstance table@(CreateTable { name, columns, constraints }) = cs [i|
instance #{instanceHead} where
    tableName = \"#{name}\"
    columnNames = #{columnNames}
    primaryKeyColumnNames = #{primaryKeyColumnNames}
|]
    where
        instanceHead :: Text
        instanceHead = "IHP.ModelSupport.Table (" <> compileTypePattern table <> ")"

        primaryKeyColumnNames :: [Text]
        primaryKeyColumnNames = primaryKeyColumns table |> map (.name)

        columnNames = columns
                |> map (.name)
                |> tshow

compileGetModelName :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileGetModelName table@(CreateTable { name }) = "type instance GetModelName (" <> tableNameToModelName name <> "'" <> spacePrefix (unwords (map (const "_") (dataTypeArguments table))) <>  ") = " <> tshow (tableNameToModelName name) <> "\n"

compileDataTypePattern :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileDataTypePattern table@(CreateTable { name }) = tableNameToModelName name <> " " <> unwords (table |> dataFields |> map fst)

compileTypePattern :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileTypePattern table@(CreateTable { name }) = tableNameToModelName name <> "'" <> spacePrefix (unwords (dataTypeArguments table))

compileInclude :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
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


compileSetFieldInstances :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
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

compileUpdateFieldInstances :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
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
                compileTypePattern' name = tableNameToModelName table.name <> "'" <> spacePrefix (unwords (map (\f -> if f == name then name <> "'" else f) (dataTypeArguments table)))

compileHasFieldId :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
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

-- Statement Module Generation
-- ===========================

-- | Generate all statement modules for all tables (Create, Update, FetchById per table)
statementModules :: (?compilerOptions :: CompilerOptions) => Schema -> [(OsPath, Text)]
statementModules schema =
    let ?schema = schema
    in schema.statements
        |> concatMap \case
            StatementCreateTable table | tableHasPrimaryKey table ->
                statementModulesForTable table
            _ -> []

statementModulesForTable :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> [(OsPath, Text)]
statementModulesForTable table =
    let modelName = tableNameToModelName table.name
        mkPath suffix = (OsPath.</>) "build/Generated/Statements" (either (error . show) id (encodeUtf (cs (suffix <> modelName) <> ".hs")))
    in [ (mkPath "Create", compileCreateStatement table)
       , (mkPath "Update", compileUpdateStatement table)
       , (mkPath "Fetch", compileFetchByIdStatement table)
       ]

-- | Barrel module that re-exports all statement modules
compileStatementsIndex :: (?compilerOptions :: CompilerOptions) => Schema -> Text
compileStatementsIndex schema@(Schema statements) =
    let tableNames = statements
            |> mapMaybe \case
                StatementCreateTable table | tableHasPrimaryKey table -> Just (tableNameToModelName table.name)
                _ -> Nothing
        imports = tableNames >>= \modelName ->
            [ "import qualified Generated.Statements.Create" <> modelName
            , "import qualified Generated.Statements.Update" <> modelName
            , "import qualified Generated.Statements.Fetch" <> modelName
            ]
    in Text.unlines
        [ "-- This file is auto generated and will be overriden regulary."
        , "module Generated.Statements ("
        , Text.intercalate ",\n" (map (\imp -> "    module " <> Text.drop (length ("import qualified " :: Text)) imp) imports)
        , ") where"
        , Text.unlines imports
        ]

-- | Core imports needed by all statement modules (Fetch, Create, Update)
statementModuleBaseImports :: Text
statementModuleBaseImports =
    Text.unlines
        [ "import Generated.ActualTypes"
        , "import Generated.Enums"
        , "import IHP.ModelSupport.Types (Id'(..), MetaBag(..))"
        , "import qualified Hasql.Statement as Statement"
        , "import qualified Hasql.Decoders as Decoders"
        , "import qualified Hasql.Encoders as Encoders"
        , "import qualified Hasql.Mapping.IsScalar as Mapping"
        , "import Hasql.PostgresqlTypes ()"
        , "import Data.Functor.Contravariant (contramap, (>$<))"
        , "import Data.Default (def)"
        , "import qualified Data.Dynamic"
        , "import Data.UUID (UUID)"
        , "import Data.Text (Text)"
        , "import Data.Int (Int16, Int32, Int64)"
        , "import Data.Time.Clock (UTCTime)"
        , "import Data.Time.LocalTime (LocalTime, TimeOfDay)"
        , "import qualified Data.Time.Calendar"
        , "import Data.Scientific (Scientific)"
        , "import qualified Data.Aeson"
        , "import qualified Database.PostgreSQL.Simple.Types"
        , "import PostgresqlTypes.Point (Point)"
        , "import PostgresqlTypes.Polygon (Polygon)"
        , "import PostgresqlTypes.Inet (Inet)"
        , "import PostgresqlTypes.Tsvector (Tsvector)"
        , "import PostgresqlTypes.Interval (Interval)"
        ]

-- | Additional imports needed by Create/Update statement modules (dynamic SQL)
statementModuleDynamicImports :: Text
statementModuleDynamicImports =
    Text.unlines
        [ "import qualified Data.Set as Set"
        , "import Data.Maybe (catMaybes)"
        , "import qualified Data.Text as Text"
        ]

-- | Map a PostgresType to its hasql value encoder expression
hasqlValueEncoder :: PostgresType -> Text
hasqlValueEncoder = \case
    PUUID -> "Encoders.uuid"
    PText -> "Encoders.text"
    PSmallInt -> "(fromIntegral >$< Encoders.int2)"
    PInt -> "(fromIntegral >$< Encoders.int4)"
    PBigInt -> "(fromIntegral >$< Encoders.int8)"
    PSerial -> "(fromIntegral >$< Encoders.int4)"
    PBigserial -> "(fromIntegral >$< Encoders.int8)"
    PBoolean -> "Encoders.bool"
    PReal -> "Encoders.float4"
    PDouble -> "Encoders.float8"
    PTimestampWithTimezone -> "Encoders.timestamptz"
    PTimestamp -> "Encoders.timestamp"
    PDate -> "Encoders.date"
    PTime -> "Encoders.time"
    (PNumeric _ _) -> "Encoders.numeric"
    PJSONB -> "Encoders.jsonb"
    PBinary -> "((\\ (Database.PostgreSQL.Simple.Types.Binary bs) -> bs) >$< Encoders.bytea)"
    (PVaryingN _) -> "Encoders.text"
    (PCharacterN _) -> "Encoders.text"
    (PInterval _) -> "Mapping.encoder"
    PPoint -> "Mapping.encoder"
    PPolygon -> "Mapping.encoder"
    PInet -> "Mapping.encoder"
    PTSVector -> "Mapping.encoder"
    PArray innerType -> "(Encoders.foldableArray (Encoders.nonNullable " <> hasqlValueEncoder innerType <> "))"
    PCustomType _ -> "Mapping.encoder"
    PSingleChar -> "Encoders.char"
    PTrigger -> error "hasqlValueEncoder: PTrigger not supported"
    PEventTrigger -> error "hasqlValueEncoder: PEventTrigger not supported"

-- | Format a list of encoder expressions into a mconcat block
formatEncoderBlock :: [Text] -> Text
formatEncoderBlock [] = "        mempty"
formatEncoderBlock encoderLines =
    "        mconcat\n            [ " <> intercalate "\n            , " encoderLines <> "\n            ]"

-- | Generate one encoder expression for a column, accessing a field from the unwrapped record.
-- The expression assumes the record is in scope.
hasqlColumnEncoder :: (?schema :: Schema) => CreateTable -> Column -> Text
hasqlColumnEncoder table column@Column { name, columnType, notNull, generator }
    | isJust generator = error $ "hasqlColumnEncoder: cannot encode generated column " <> cs name
    | otherwise = "(." <> fieldName <> ") >$< Encoders.param (" <> nullability <> " " <> encoder <> ")"
    where
        fieldName = columnNameToFieldName name
        -- For encoders, columns that are nullable in Haskell (not notNull) use nullable encoder
        isNullable = not notNull
        nullability = if isNullable then "Encoders.nullable" else "Encoders.nonNullable"
        isPrimaryKey = [name] == primaryKeyColumnNames table.primaryKeyConstraint
        isForeignKey = isJust (findForeignKeyConstraint table column)
        needsIdWrapper = isPrimaryKey || isForeignKey
        baseEncoder = hasqlValueEncoder columnType
        encoder = if needsIdWrapper then "((\\ (Id pk) -> pk) >$< " <> baseEncoder <> ")" else baseEncoder

-- | Generate a conditional SQL entry for a column.
-- When @alwaysConditional@ is True (Update), all columns are conditional on touchedFields.
-- When False (Create), only columns with DB defaults are conditional.
compileSqlEntry :: Bool -> Column -> Text
compileSqlEntry alwaysConditional col
    | alwaysConditional || hasExplicitOrImplicitDefault col =
        let fieldName = columnNameToFieldName col.name
        in "if Set.member " <> tshow fieldName <> " touchedFields then Just " <> tshow col.name <> " else Nothing"
    | otherwise = "Just " <> tshow col.name

-- | Generate a conditional encoder entry for a column.
-- Same conditional logic as 'compileSqlEntry'.
compileEncoderEntry :: (?schema :: Schema) => Bool -> CreateTable -> Column -> Text
compileEncoderEntry alwaysConditional table col
    | alwaysConditional || hasExplicitOrImplicitDefault col =
        let fieldName = columnNameToFieldName col.name
        in "if Set.member " <> tshow fieldName <> " touchedFields then Just (" <> hasqlColumnEncoder table col <> ") else Nothing"
    | otherwise = "Just (" <> hasqlColumnEncoder table col <> ")"

-- | Returns True when any writable column has a DB default (meaning Create needs dynamic SQL)
hasAnyDefaults :: [Column] -> Bool
hasAnyDefaults = any hasExplicitOrImplicitDefault

-- | Generate the Create statement module for a table
--
-- When no writable columns have DB defaults, generates a static SQL statement
-- (no @Set Text@ parameter). Otherwise generates a dynamic @statement :: Set Text -> ...@
-- that omits untouched defaulted columns from INSERT, letting PostgreSQL use real DEFAULT.
compileCreateStatement :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileCreateStatement table@(CreateTable { name, columns }) =
    let modelName = tableNameToModelName name
        moduleName = "Generated.Statements.Create" <> modelName
        qualifiedModelName = qualifiedConstructorNameFromTableName name
        writableColumns = onlyWritableColumns columns
        allColumnNames = commaSep (map (.name) columns)
        isDynamic = hasAnyDefaults writableColumns

        decoderBlock = compileStatementDecoder modelName table columns
    in if isDynamic
        then compileDynamicCreateStatement moduleName qualifiedModelName name writableColumns allColumnNames table columns decoderBlock
        else compileStaticCreateStatement moduleName qualifiedModelName name writableColumns allColumnNames table columns decoderBlock

-- | Create statement with static SQL (no columns have defaults)
compileStaticCreateStatement :: (?schema :: Schema) => Text -> Text -> Text -> [Column] -> Text -> CreateTable -> [Column] -> Text -> Text
compileStaticCreateStatement moduleName qualifiedModelName tableName writableColumns allColumnNames table columns decoderBlock =
    let writableColumnNames = commaSep (map (.name) writableColumns)
        placeholders = commaSep ["$" <> tshow i | i <- [1 .. length writableColumns]]
        sql = "INSERT INTO " <> tableName <> " (" <> writableColumnNames <> ") VALUES (" <> placeholders <> ") RETURNING " <> allColumnNames
        encoderLines = map (hasqlColumnEncoder table) writableColumns
    in Text.unlines
        [ "-- This file is auto generated and will be overriden regulary."
        , "{-# OPTIONS_GHC -Wno-unused-imports -Wno-dodgy-imports -Wno-unused-matches #-}"
        , "module " <> moduleName <> " (statement) where"
        , ""
        , statementModuleBaseImports
        , "statement :: Statement.Statement " <> qualifiedModelName <> " " <> qualifiedModelName
        , "statement = Statement.preparable sql encoder decoder"
        , ""
        , "sql :: Text"
        , "sql = " <> tshow sql
        , ""
        , "encoder :: Encoders.Params " <> qualifiedModelName
        , "encoder ="
        , formatEncoderBlock encoderLines
        , ""
        , "decoder :: Decoders.Result " <> qualifiedModelName
        , "decoder = " <> decoderBlock
        ]

-- | Create statement with dynamic SQL (some columns have defaults)
compileDynamicCreateStatement :: (?schema :: Schema) => Text -> Text -> Text -> [Column] -> Text -> CreateTable -> [Column] -> Text -> Text
compileDynamicCreateStatement moduleName qualifiedModelName tableName writableColumns allColumnNames table columns decoderBlock =
    let sqlEntries = map (compileSqlEntry False) writableColumns
        sqlBody = Text.unlines
            [ "sql :: Set.Set Text -> Text"
            , "sql touchedFields ="
            , "    let entries = catMaybes"
            , "            [ " <> Text.intercalate "\n            , " sqlEntries
            , "            ]"
            , "        columns = Text.intercalate \", \" entries"
            , "        placeholders = Text.intercalate \", \" [\"$\" <> Text.pack (show i) | i <- [1 .. length entries]]"
            , "    in if null entries"
            , "        then \"INSERT INTO " <> tableName <> " DEFAULT VALUES RETURNING " <> allColumnNames <> "\""
            , "        else \"INSERT INTO " <> tableName <> " (\" <> columns <> \") VALUES (\" <> placeholders <> \") RETURNING " <> allColumnNames <> "\""
            ]

        encoderEntries = map (compileEncoderEntry False table) writableColumns
        encoderBody = Text.unlines
            [ "encoder :: Set.Set Text -> Encoders.Params " <> qualifiedModelName
            , "encoder touchedFields = mconcat $ catMaybes"
            , "    [ " <> Text.intercalate "\n    , " encoderEntries
            , "    ]"
            ]
    in Text.unlines
        [ "-- This file is auto generated and will be overriden regulary."
        , "{-# OPTIONS_GHC -Wno-unused-imports -Wno-dodgy-imports -Wno-unused-matches #-}"
        , "module " <> moduleName <> " (statement) where"
        , ""
        , statementModuleBaseImports
        , statementModuleDynamicImports
        , "statement :: Set.Set Text -> Statement.Statement " <> qualifiedModelName <> " " <> qualifiedModelName
        , "statement touchedFields = Statement.preparable (sql touchedFields) (encoder touchedFields) decoder"
        , ""
        , sqlBody
        , ""
        , encoderBody
        , ""
        , "decoder :: Decoders.Result " <> qualifiedModelName
        , "decoder = " <> decoderBlock
        ]

-- | Generate the Update statement module for a table
--
-- Generates a @statement :: Set Text -> Statement.Statement Model Model@ function.
-- Only touched columns appear in the SET clause.
compileUpdateStatement :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileUpdateStatement table@(CreateTable { name, columns }) =
    let modelName = tableNameToModelName name
        moduleName = "Generated.Statements.Update" <> modelName
        qualifiedModelName = qualifiedConstructorNameFromTableName name
        writableColumns = onlyWritableColumns columns
        pkColumns = primaryKeyColumns table
        allColumnNames = commaSep (map (.name) columns)

        -- Build the sql function body
        sqlEntries = map (compileSqlEntry True) writableColumns

        sqlBody = Text.unlines
            [ "sql :: Set.Set Text -> Text"
            , "sql touchedFields ="
            , "    let setEntries = catMaybes"
            , "            [ " <> Text.intercalate "\n            , " sqlEntries
            , "            ]"
            , "        setClauses = [col <> \" = $\" <> Text.pack (show i) | (i, col) <- zip [1..] setEntries]"
            , "        pkIdx = length setEntries + 1"
            , "        whereClause = " <> compileUpdateWhereClause pkColumns
            , "    in \"UPDATE " <> name <> " SET \" <> Text.intercalate \", \" setClauses <> \" WHERE \" <> whereClause pkIdx <> \" RETURNING " <> allColumnNames <> "\""
            ]

        -- Build the encoder function body
        encoderEntries = map (compileEncoderEntry True table) writableColumns
        pkEncoders = map (hasqlColumnEncoder table) pkColumns
        encoderBody = Text.unlines
            [ "encoder :: Set.Set Text -> Encoders.Params " <> qualifiedModelName
            , "encoder touchedFields = mconcat (catMaybes"
            , "    [ " <> Text.intercalate "\n    , " encoderEntries
            , "    ])"
            , "    <> " <> case pkEncoders of
                    [e] -> e
                    es -> "mconcat [" <> Text.intercalate ", " es <> "]"
            ]

        decoderBlock = compileStatementDecoder modelName table columns
    in Text.unlines
        [ "-- This file is auto generated and will be overriden regulary."
        , "{-# OPTIONS_GHC -Wno-unused-imports -Wno-dodgy-imports -Wno-unused-matches #-}"
        , "module " <> moduleName <> " (statement) where"
        , ""
        , statementModuleBaseImports
        , statementModuleDynamicImports
        , "statement :: Set.Set Text -> Statement.Statement " <> qualifiedModelName <> " " <> qualifiedModelName
        , "statement touchedFields = Statement.preparable (sql touchedFields) (encoder touchedFields) decoder"
        , ""
        , sqlBody
        , ""
        , encoderBody
        , ""
        , "decoder :: Decoders.Result " <> qualifiedModelName
        , "decoder = " <> decoderBlock
        ]
    where
        -- Generate WHERE clause builder that handles single and composite PKs
        compileUpdateWhereClause :: [Column] -> Text
        compileUpdateWhereClause [col] = "\\startIdx -> " <> tshow col.name <> " <> \" = $\" <> Text.pack (show startIdx)"
        compileUpdateWhereClause cols =
            let parts = zipWith (\col i ->
                    tshow col.name <> " <> \" = $\" <> Text.pack (show (startIdx + " <> tshow i <> "))"
                    ) cols [(0 :: Int)..]
            in "\\startIdx -> Text.intercalate \" AND \" [" <> Text.intercalate ", " parts <> "]"

-- | Generate the FetchById statement module for a table
--
-- Static SQL — no touched fields needed. @statement@ is a value, not a function.
compileFetchByIdStatement :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileFetchByIdStatement table@(CreateTable { name, columns }) =
    let modelName = tableNameToModelName name
        moduleName = "Generated.Statements.Fetch" <> modelName
        qualifiedModelName = qualifiedConstructorNameFromTableName name
        pkColumns = primaryKeyColumns table
        allColumnNames = commaSep (map (.name) columns)

        -- WHERE clause
        (whereClauses, _) = foldl' (\(acc, idx) col -> (col.name <> " = $" <> tshow idx : acc, idx + 1)) ([], 1 :: Int) pkColumns
        whereClauseSql = intercalate " AND " (reverse whereClauses)

        sql = "SELECT " <> allColumnNames <> " FROM " <> name <> " WHERE " <> whereClauseSql <> " LIMIT 1"

    in Text.unlines
        [ "-- This file is auto generated and will be overriden regulary."
        , "{-# OPTIONS_GHC -Wno-unused-imports -Wno-dodgy-imports -Wno-unused-matches #-}"
        , "module " <> moduleName <> " (statement) where"
        , ""
        , statementModuleBaseImports
        , "statement :: Statement.Statement (Id' " <> tshow name <> ") (Maybe " <> qualifiedModelName <> ")"
        , "statement = Statement.preparable sql encoder decoder"
        , ""
        , "sql :: Text"
        , "sql = " <> tshow sql
        , ""
        , "encoder :: Encoders.Params (Id' " <> tshow name <> ")"
        , "encoder = " <> fetchByIdEncoder table
        , ""
        , "decoder :: Decoders.Result (Maybe " <> qualifiedModelName <> ")"
        , "decoder = Decoders.rowMaybe (" <> compileStatementRowDecoder modelName table columns <> ")"
        ]

-- | Encoder for FetchById - handles single and composite primary keys
fetchByIdEncoder :: (?schema :: Schema) => CreateTable -> Text
fetchByIdEncoder table = case primaryKeyColumns table of
    [col] ->
        let baseEncoder = hasqlValueEncoder col.columnType
        in "(\\ (Id pk) -> pk) >$< Encoders.param (Encoders.nonNullable " <> baseEncoder <> ")"
    cols ->
        let encoders = zipWith (\col idx ->
                let baseEncoder = hasqlValueEncoder col.columnType
                    accessor = "(\\(Id (" <> commaSep (map (\i -> if i == idx then "v" else "_") [0..length cols - 1]) <> ")) -> v)"
                in accessor <> " >$< Encoders.param (Encoders.nonNullable " <> baseEncoder <> ")"
                ) cols [0..]
        in "mconcat [" <> intercalate ", " encoders <> "]"

-- | Generate a decoder block for singleRow that constructs a model record
compileStatementDecoder :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => Text -> CreateTable -> [Column] -> Text
compileStatementDecoder modelName table columns =
    "Decoders.singleRow (" <> compileStatementRowDecoder modelName table columns <> ")"

-- | Shared helper for building a row decoder body (lambda + applicative chain).
-- The @compileQueryBuilderField@ parameter controls how one-to-many QueryBuilder fields are compiled:
-- - 'compileFromRowHasqlInstance' passes the real QueryBuilder filter expression
-- - 'compileStatementRowDecoder' passes @\\_ _ -> "def"@ (no QueryBuilder available in statement decoders)
-- The @decoderIndent@ parameter controls indentation of the @<$>@ / @<*>@ lines.
compileRowDecoderBody :: (?schema :: Schema, ?compilerOptions :: CompilerOptions)
    => CreateTable -> [Column] -> ((Text, Text) -> Text) -> Text -> Text
compileRowDecoderBody table columns compileSetQueryBuilder decoderIndent =
    let columnNames = map (columnNameToFieldName . (.name)) columns
        referencing = columnsReferencingTable table.name
        referencingWithFieldNames = zip (map fst (compileQueryBuilderFields referencing)) referencing

        compileField (fieldName, _)
            | isColumn fieldName = fieldName
            | isOneToManyField fieldName = let (Just (_, ref)) = find (\(name, _) -> name == fieldName) referencingWithFieldNames in compileSetQueryBuilder ref
            | fieldName == "meta" = "def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }"
            | otherwise = "def"

        isColumn colName = colName `elem` columnNames
        isOneToManyField fieldName = fieldName `elem` (map fst referencingWithFieldNames)

        decoderExprs = map (hasqlColumnDecoder table) columns

        lambdaArgs = intercalate " " columnNames
        constructorArgs = intercalate " " (map compileField (dataFields table))
        lambdaBody = "(\\" <> lambdaArgs <> " -> let theRecord = " <> qualifiedConstructorNameFromTableName table.name <> " " <> constructorArgs <> " in theRecord)"
    in case decoderExprs of
        [] -> error "compileRowDecoderBody: table has no columns"
        (first:rest) -> lambdaBody <> "\n" <> decoderIndent <> "<$> " <> first <> concatMap (\d -> "\n" <> decoderIndent <> "<*> " <> d) rest

-- | Generate a row decoder expression that constructs a model record (for statement modules)
compileStatementRowDecoder :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => Text -> CreateTable -> [Column] -> Text
compileStatementRowDecoder _modelName table columns =
    compileRowDecoderBody table columns (\_ -> "def") "            "
