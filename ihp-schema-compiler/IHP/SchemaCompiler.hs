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
, compileCreateManyStatement
, compileRowDecoderModule
, Schema(..)
) where

import ClassyPrelude
import Data.Bits (bit)
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
            {-# OPTIONS_GHC -Wno-unused-imports -Wno-dodgy-imports -Wno-unused-matches -Wno-ambiguous-fields #-}
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
            [ "import qualified Generated.Statements.RowDecoder" <> modelName
            , "import qualified Generated.Statements.Create" <> modelName
            , "import qualified Generated.Statements.Update" <> modelName
            , "import qualified Generated.Statements.CreateMany" <> modelName
            ]
        prelude = [trimming|
            -- This file is auto generated and will be overriden regulary. Please edit `Application/Schema.sql` to change the Types\n"
            {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, TypeFamilies, DataKinds, TypeOperators, UndecidableInstances, ConstraintKinds, StandaloneDeriving  #-}
            {-# OPTIONS_GHC -Wno-unused-imports -Wno-dodgy-imports -Wno-unused-matches -Wno-ambiguous-fields #-}
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
tableModuleBody options table = Text.unlines $ filter (not . Text.null)
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
    , compileFieldBitInstances table
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
                    Just fk@(ForeignKeyConstraint { referenceTable, referenceColumn })
                        | isForeignKeyReferencingPK fk -> "(" <> primaryKeyTypeName referenceTable <> ")"
                        | otherwise ->
                            -- FK references a non-PK column; use the referenced column's actual type
                            case referenceColumn >>= \refCol -> findTableByName referenceTable >>= \t -> find (\c -> c.name == refCol) t.columns of
                                Just refColumn -> atomicType refColumn.columnType
                                Nothing -> atomicType columnType
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
    import IHP.Hasql.Encoders ()
    import qualified Hasql.Mapping.IsScalar as Mapping
    import Hasql.PostgresqlTypes ()
    import Data.Bits ((.&.), (.|.))
    import Control.Monad (unless)
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
dataFields table@(CreateTable { name }) = columnFields <> queryBuilderFields <> [("meta", "MetaBag")]
    where
        columns = allColumnsIncludingInherited table
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

fieldBitPositions :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> [(Text, Integer)]
fieldBitPositions table =
    let columns = allColumnsIncludingInherited table
    in zip (map (columnNameToFieldName . (.name)) columns) (map bit [0..])

columnsWithBitIndices :: [Column] -> [Column] -> [(Column, Int)]
columnsWithBitIndices allColumns subset =
    let subsetNames = setFromList (map (.name) subset) :: Set Text
    in [(col, idx) | (col, idx) <- zip allColumns [0..], col.name `member` subsetNames]

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
            AddConstraint { tableName, constraint = fk@ForeignKeyConstraint { columnName, referenceTable } } | referenceTable == theTableName && isForeignKeyReferencingPK fk -> Just (tableName, columnName)
            _ -> Nothing

variableAttributes :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> [Column]
variableAttributes table = filter (isVariableAttribute table) (allColumnsIncludingInherited table)

isVariableAttribute :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Column -> Bool
isVariableAttribute table column
    | not ?compilerOptions.compileRelationSupport = False
    | otherwise = isRefCol table column


-- | Returns @True@ when the column references another table's primary key via foreign key constraint.
-- FK constraints that reference a non-PK column (e.g., REFERENCES users(email)) return @False@,
-- because the relation machinery (Include, fetchRelated, QueryBuilder) only works for PK-based FKs.
isRefCol :: (?schema :: Schema) => CreateTable -> Column -> Bool
isRefCol table column = case findForeignKeyConstraint table column of
    Just fk -> isForeignKeyReferencingPK fk
    Nothing -> False

-- | Returns the foreign key constraint bound on the given column.
-- For inherited columns, recursively checks ancestor table constraints.
findForeignKeyConstraint :: (?schema :: Schema) => CreateTable -> Column -> Maybe Constraint
findForeignKeyConstraint table@CreateTable { name, inherits } column =
        case find (isFkConstraint name) statements of
            Just (AddConstraint { constraint }) -> Just constraint
            _ -> case inherits >>= findTableByName of
                Just parentTable -> findForeignKeyConstraint parentTable column
                Nothing -> Nothing
    where
        isFkConstraint tableName (AddConstraint { tableName = tName, constraint = ForeignKeyConstraint { columnName }}) = tName == tableName && columnName == column.name
        isFkConstraint _ _ = False

        (Schema statements) = ?schema

-- | Finds a table by name in the schema
findTableByName :: (?schema :: Schema) => Text -> Maybe CreateTable
findTableByName tableName =
    let (Schema statements) = ?schema
    in statements
        |> mapMaybe (\case
            StatementCreateTable table@CreateTable { name } | name == tableName -> Just table
            _ -> Nothing)
        |> headMay

-- | Returns @True@ when a FK constraint references the primary key of the target table.
-- FK constraints pointing at non-PK columns (e.g., REFERENCES users(email)) return @False@.
isForeignKeyReferencingPK :: (?schema :: Schema) => Constraint -> Bool
isForeignKeyReferencingPK ForeignKeyConstraint { referenceTable, referenceColumn } =
    case referenceColumn of
        Just refCol -> refCol `elem` referencedPKColumns
        Nothing -> True
    where
        referencedPKColumns = case findTableByName referenceTable of
            Just t -> primaryKeyColumnNames t.primaryKeyConstraint
            Nothing -> []
isForeignKeyReferencingPK _ = False

-- | Returns all columns including inherited columns from parent tables.
-- Inherited columns that are not overridden in the child table are appended.
allColumnsIncludingInherited :: (?schema :: Schema) => CreateTable -> [Column]
allColumnsIncludingInherited CreateTable { columns, inherits = Nothing } = columns
allColumnsIncludingInherited CreateTable { columns, inherits = Just parentName } =
    case findTableByName parentName of
        Nothing -> columns
        Just parentTable ->
            let parentCols = allColumnsIncludingInherited parentTable
                childNames = map (.name) columns
                inherited = filter (\pc -> pc.name `notElem` childNames) parentCols
            in columns <> inherited

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

--
-- Trigger types don't have encoders as they're not real data columns.
hasqlSupportsColumnType :: PostgresType -> Bool
hasqlSupportsColumnType = \case
    PTrigger -> False
    PEventTrigger -> False
    (PArray inner) -> hasqlSupportsColumnType inner
    _ -> True

compileCreate :: (?schema :: Schema) => CreateTable -> Text
compileCreate table@(CreateTable { name }) =
    let
        columns = allColumnsIncludingInherited table
        writableColumns = onlyWritableColumns columns
        modelName = qualifiedConstructorNameFromTableName name
        funcName = tableNameToModelName name

        -- Hasql bodies
        isDynamic = hasAnyDefaults writableColumns
        hasqlCreateBody = if isDynamic
            then "let pool = ?modelContext.hasqlPool\n"
                <> "let touched = model.meta.touchedFields\n"
                <> "sqlStatementHasql pool model (Generated.Statements.Create" <> funcName <> ".statement touched)"
            else "let pool = ?modelContext.hasqlPool\n"
                <> "sqlStatementHasql pool model Generated.Statements.Create" <> funcName <> ".statement"
        hasqlCreateManyBody = "let pool = ?modelContext.hasqlPool\n"
                <> "sqlStatementHasql pool models (Generated.Statements.CreateMany" <> funcName <> ".statement (List.length models))"
        hasqlCreateDiscardBody = if isDynamic
            then "let pool = ?modelContext.hasqlPool\n"
                <> "let touched = model.meta.touchedFields\n"
                <> "sqlStatementHasql pool model (Generated.Statements.Create" <> funcName <> ".discardResultStatement touched)"
            else "let pool = ?modelContext.hasqlPool\n"
                <> "sqlStatementHasql pool model Generated.Statements.Create" <> funcName <> ".discardResultStatement"
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

-- Avoids trailing spaces when type arguments are empty.
spacePrefix :: Text -> Text
spacePrefix "" = ""
spacePrefix t = " " <> t

toBinding :: Text -> Column -> Text
toBinding modelName Column { name } = "let " <> modelName <> "{" <> columnNameToFieldName name <> "} = model in " <> columnNameToFieldName name

onlyWritableColumns columns = columns |> filter (\Column { generator } -> isNothing generator)

compileUpdate :: (?schema :: Schema) => CreateTable -> Text
compileUpdate table@(CreateTable { name }) =
    let
        columns = allColumnsIncludingInherited table
        modelName = qualifiedConstructorNameFromTableName name
        funcName = tableNameToModelName name

        stmtModule = "Generated.Statements.Update" <> funcName
    in
        -- Instance block: delegate to top-level functions
        "instance CanUpdate " <> modelName <> " where\n"
        <> indent ("updateRecord = updateRecord" <> funcName <> "\n")
        <> indent ("updateRecordDiscardResult = updateRecordDiscardResult" <> funcName <> "\n")
        -- updateRecord<Model>
        <> "\n"
        <> "updateRecord" <> funcName <> " :: (?modelContext :: ModelContext) => " <> modelName <> " -> IO " <> modelName <> "\n"
        <> "updateRecord" <> funcName <> " model = do\n"
        <> "    let touched = model.meta.touchedFields\n"
        <> "    if touched == 0 then pure model else do\n"
        <> "        let pool = ?modelContext.hasqlPool\n"
        <> "        sqlStatementHasql pool model (" <> stmtModule <> ".statement touched)\n"
        -- updateRecordDiscardResult<Model>
        <> "\n"
        <> "updateRecordDiscardResult" <> funcName <> " :: (?modelContext :: ModelContext) => " <> modelName <> " -> IO ()\n"
        <> "updateRecordDiscardResult" <> funcName <> " model = do\n"
        <> "    let touched = model.meta.touchedFields\n"
        <> "    unless (touched == 0) $ do\n"
        <> "        let pool = ?modelContext.hasqlPool\n"
        <> "        sqlStatementHasql pool model (" <> stmtModule <> ".discardResultStatement touched)\n"

compileFromRowInstance :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileFromRowInstance table@(CreateTable { name }) = cs [i|instance FromRow #{modelName} where
    fromRow = do
#{unsafeInit . indent . indent . unlines $ map columnBinding columnNames}
        let theRecord = #{modelName} #{intercalate " " (map compileField (dataFields table))}
        pure theRecord
|]
    where
        modelName = qualifiedConstructorNameFromTableName name
        columns = allColumnsIncludingInherited table
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

-- | Generates a 'FromRowHasql' instance that delegates to the RowDecoder statement module.
compileFromRowHasqlInstance :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileFromRowHasqlInstance table@(CreateTable { name, columns }) =
    let modelName = qualifiedConstructorNameFromTableName name
        rowDecoderModule = "Generated.Statements.RowDecoder" <> tableNameToModelName name
    in cs [i|instance FromRowHasql #{modelName} where
    hasqlRowDecoder = #{rowDecoderModule}.rowDecoder
|]

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

-- Note: Generated columns are treated as nullable in the Haskell type (even if notNull=True)
-- because they're not included in INSERT statements and are computed by the database.
-- Primary key and foreign key columns are wrapped with Id.
hasqlColumnDecoder :: (?schema :: Schema) => CreateTable -> Column -> Text
hasqlColumnDecoder table column@Column { name, columnType, notNull, generator } =
    "Decoders.column (" <> nullability <> " " <> decoder <> ")"
    where
        -- Id columns (primary keys and foreign keys) use Mapping.decoder which
        -- goes through the IsScalar instance for Id'
        isPrimaryKey = [name] == primaryKeyColumnNames table.primaryKeyConstraint

        -- Match the logic in haskellType: primary keys are always nonNullable (even without
        -- explicit NOT NULL), otherwise nullable if not notNull or has a generator
        isNullable = not isPrimaryKey && (not notNull || isJust generator)
        nullability = if isNullable then "Decoders.nullable" else "Decoders.nonNullable"
        isForeignKey = isJust (findForeignKeyConstraint table column)
        needsIdWrapper = isPrimaryKey || isForeignKey

        baseDecoder = hasqlValueDecoder columnType
        decoder = if needsIdWrapper then "Mapping.decoder" else baseDecoder

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

hasqlArrayElementDecoder :: PostgresType -> Text
hasqlArrayElementDecoder innerType = "Decoders.nonNullable " <> hasqlValueDecoder innerType

compileBuild :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileBuild table@(CreateTable { name }) =
    let
        columns = allColumnsIncludingInherited table
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
compileTableInstance table@(CreateTable { name, constraints }) = cs [i|
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

        columnNames = allColumnsIncludingInherited table
                |> map (.name)
                |> tshow

compileGetModelName :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileGetModelName table@(CreateTable { name }) = "type instance GetModelName (" <> tableNameToModelName name <> "'" <> spacePrefix (unwords (map (const "_") (dataTypeArguments table))) <>  ") = " <> tshow (tableNameToModelName name) <> "\n"

compileDataTypePattern :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileDataTypePattern table@(CreateTable { name }) = tableNameToModelName name <> " " <> unwords (table |> dataFields |> map fst)

compileTypePattern :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileTypePattern table@(CreateTable { name }) = tableNameToModelName name <> "'" <> spacePrefix (unwords (dataTypeArguments table))

compileInclude :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileInclude table@(CreateTable { name }) = (belongsToIncludes <> hasManyIncludes) |> unlines
    where
        columns = allColumnsIncludingInherited table
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
        fieldBitMap = fieldBitPositions table
        compileSetField (fieldName, fieldType) =
            "instance SetField " <> tshow fieldName <> " (" <> compileTypePattern table <>  ") " <> fieldType <> " where\n" <>
            "    {-# INLINE setField #-}\n" <>
            "    setField newValue record = record" <> recordUpdate
            where
                recordUpdate = case lookup fieldName fieldBitMap of
                    Just bitVal ->
                        " { " <> fieldName <> " = newValue" <>
                        ", meta = record.meta { touchedFields = record.meta.touchedFields .|. " <> tshow bitVal <> " }" <>
                        " }"
                    Nothing ->
                        " { " <> fieldName <> " = newValue }"

compileUpdateFieldInstances :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileUpdateFieldInstances table@(CreateTable { name, columns }) = unlines (map compileField (dataFields table))
    where
        modelName = tableNameToModelName name
        typeArgs = dataTypeArguments table
        fieldBitMap = fieldBitPositions table

        compileField (fieldName, fieldType)
            | fieldName `elem` typeArgs = compilePolymorphic fieldName fieldType
            | otherwise = compileSimple fieldName fieldType

        -- Non-polymorphic: model type doesn't change, use record update
        compileSimple fieldName fieldType =
            "instance UpdateField " <> tshow fieldName <> " (" <> compileTypePattern table <> ") (" <> compileTypePattern table <> ") " <> fieldType <> " " <> fieldType <> " where\n" <>
            "    {-# INLINE updateField #-}\n" <>
            "    updateField newValue record = record" <> recordUpdate
            where
                recordUpdate = case lookup fieldName fieldBitMap of
                    Just bitVal ->
                        " { " <> fieldName <> " = newValue" <>
                        ", meta = record.meta { touchedFields = record.meta.touchedFields .|. " <> tshow bitVal <> " }" <>
                        " }"
                    Nothing ->
                        " { " <> fieldName <> " = newValue }"

        -- Polymorphic: type changes, must use full pattern match
        compilePolymorphic fieldName fieldType =
            "instance UpdateField " <> tshow fieldName <> " (" <> compileTypePattern table <> ") (" <> compileTypePattern' fieldName <> ") " <> fieldName <> " " <> fieldName <> "'" <> " where\n" <>
            "    {-# INLINE updateField #-}\n" <>
            "    updateField newValue (" <> compileDataTypePattern table <> ") = " <> modelName <> " " <> (unwords (map compileAttribute (table |> dataFields |> map fst)))
            where
                compileAttribute name'
                    | name' == fieldName = "newValue"
                    | name' == "meta" = case lookup fieldName fieldBitMap of
                        Just bitVal -> "(meta { touchedFields = touchedFields meta .|. " <> tshow bitVal <> " })"
                        Nothing -> "meta"
                    | otherwise = name'

        compileTypePattern' :: Text -> Text
        compileTypePattern' name = tableNameToModelName table.name <> "'" <> spacePrefix (unwords (map (\f -> if f == name then name <> "'" else f) typeArgs))

compileFieldBitInstances :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileFieldBitInstances table@(CreateTable { name }) = unlines (map compileInstance (fieldBitPositions table))
    where
        typePattern = compileTypePattern table
        compileInstance (fieldName, bitVal) =
            "instance FieldBit " <> tshow fieldName <> " (" <> typePattern <> ") where fieldBit = " <> tshow bitVal

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
needsHasFieldId CreateTable { columns, primaryKeyConstraint } =
  case primaryKeyColumnNames primaryKeyConstraint of
    [] -> False
    ["id"] -> False
    pkCols
      | any (\col -> col.name == "id") columns -> False
      | otherwise -> True

primaryKeyColumns :: CreateTable -> [Column]
primaryKeyColumns CreateTable { name, columns, primaryKeyConstraint } =
    map getColumn (primaryKeyColumnNames primaryKeyConstraint)
  where
    getColumn columnName = case find ((==) columnName . (.name)) columns of
      Just c -> c
      Nothing -> error ("Missing column " <> cs columnName <> " used in primary key for " <> cs name)

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

hasExplicitOrImplicitDefault :: Column -> Bool
hasExplicitOrImplicitDefault column = case column of
        Column { defaultValue = Just _ } -> True
        Column { columnType = PSerial } -> True
        Column { columnType = PBigserial } -> True
        _ -> False

-- Statement Module Generation
-- ===========================

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
    in [ (mkPath "RowDecoder", compileRowDecoderModule table)
       , (mkPath "Create", compileCreateStatement table)
       , (mkPath "Update", compileUpdateStatement table)
       , (mkPath "Fetch", compileFetchByIdStatement table)
       , (mkPath "CreateMany", compileCreateManyStatement table)
       ]

compileStatementsIndex :: (?compilerOptions :: CompilerOptions) => Schema -> Text
compileStatementsIndex schema@(Schema statements) =
    let tableNames = statements
            |> mapMaybe \case
                StatementCreateTable table | tableHasPrimaryKey table -> Just (tableNameToModelName table.name)
                _ -> Nothing
        imports = tableNames >>= \modelName ->
            [ "import qualified Generated.Statements.RowDecoder" <> modelName
            , "import qualified Generated.Statements.Create" <> modelName
            , "import qualified Generated.Statements.Update" <> modelName
            , "import qualified Generated.Statements.Fetch" <> modelName
            , "import qualified Generated.Statements.CreateMany" <> modelName
            ]
    in Text.unlines
        [ "-- This file is auto generated and will be overriden regulary."
        , "module Generated.Statements ("
        , Text.intercalate ",\n" (map (\imp -> "    module " <> Text.drop (length ("import qualified " :: Text)) imp) imports)
        , ") where"
        , Text.unlines imports
        ]

statementModuleBaseImports :: Text
statementModuleBaseImports =
    Text.unlines
        [ "import Prelude (($), (.), (<$>), (<*>), (<>), (+), (*), (-), show, fromIntegral, length, null, zip, mconcat, (++), Maybe(..), (!!), map, Bool(..), Int, Integer, pure)"
        , "import Generated.ActualTypes"
        , "import Generated.Enums"
        , "import IHP.ModelSupport.Types (Id'(..), MetaBag(..))"
        , "import qualified Hasql.Statement as Statement"
        , "import qualified Hasql.Decoders as Decoders"
        , "import qualified Hasql.Encoders as Encoders"
        , "import qualified Hasql.Mapping.IsScalar as Mapping"
        , "import Hasql.PostgresqlTypes ()"
        , "import IHP.Job.Queue ()"
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

statementModuleDynamicImports :: Text
statementModuleDynamicImports =
    Text.unlines
        [ "import Data.Bits (testBit)"
        , "import Data.Maybe (catMaybes)"
        , "import qualified Data.Text as Text"
        ]

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

formatEncoderBlock :: [Text] -> Text
formatEncoderBlock [] = "        mconcat []"
formatEncoderBlock encoderLines =
    "        mconcat\n            [ " <> intercalate "\n            , " encoderLines <> "\n            ]"

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
        encoder = if needsIdWrapper then "Mapping.encoder" else baseEncoder

compileSqlEntry :: Bool -> Int -> Column -> Text
compileSqlEntry alwaysConditional bitIndex col
    | alwaysConditional || hasExplicitOrImplicitDefault col =
        "if testBit touchedFields " <> tshow bitIndex <> " then Just " <> tshow col.name <> " else Nothing"
    | otherwise = "Just " <> tshow col.name

compileEncoderEntry :: (?schema :: Schema) => Bool -> Int -> CreateTable -> Column -> Text
compileEncoderEntry alwaysConditional bitIndex table col
    | alwaysConditional || hasExplicitOrImplicitDefault col =
        "if testBit touchedFields " <> tshow bitIndex <> " then Just (" <> hasqlColumnEncoder table col <> ") else Nothing"
    | otherwise = "Just (" <> hasqlColumnEncoder table col <> ")"

hasAnyDefaults :: [Column] -> Bool
hasAnyDefaults = any hasExplicitOrImplicitDefault

compileRowDecoderModule :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileRowDecoderModule table@(CreateTable { name }) =
    let columns = allColumnsIncludingInherited table
        modelName = tableNameToModelName name
        moduleName = "Generated.Statements.RowDecoder" <> modelName
        qualifiedModelName = qualifiedConstructorNameFromTableName name
        extraImports = Text.unlines
            [ "import qualified IHP.QueryBuilder as QueryBuilder"
            , "import GHC.Records"
            ]

        columnNames = map (columnNameToFieldName . (.name)) columns
        referencing = columnsReferencingTable table.name
        referencingWithFieldNames = zip (map fst (compileQueryBuilderFields referencing)) referencing

        compileField (fieldName, _)
            | isColumn fieldName = fieldName
            | isOneToManyField fieldName = let (Just (_, ref)) = find (\(n, _) -> n == fieldName) referencingWithFieldNames in compileFromRowQueryBuilder table ref
            | fieldName == "meta" = "def { originalDatabaseRecord = Just (Data.Dynamic.toDyn theRecord) }"
            | otherwise = "def"

        isColumn colName = colName `elem` columnNames
        isOneToManyField fieldName = fieldName `elem` (map fst referencingWithFieldNames)

        columnBindings = map (\col -> "    " <> columnNameToFieldName col.name <> " <- " <> hasqlColumnDecoder table col) columns
        constructorArgs = intercalate " " (map compileField (dataFields table))
        -- The recursive let (theRecord references itself via toDyn) must be inside
        -- the pure expression, not as a do-block let statement, because GHC's
        -- ApplicativeDo cannot desugar recursive do-block lets and Decoders.Row
        -- has no Monad instance. A let-in inside pure(...) is just a pure Haskell
        -- expression that ApplicativeDo doesn't need to analyze.
        pureExpr = "    pure (let theRecord = " <> qualifiedConstructorNameFromTableName name <> " " <> constructorArgs <> " in theRecord)"
    in "{-# LANGUAGE ApplicativeDo, OverloadedLabels, TypeApplications, ScopedTypeVariables #-}\n"
        <> statementModuleHeader moduleName ["rowDecoder"] extraImports
        <> Text.unlines
        ([ "rowDecoder :: Decoders.Row " <> qualifiedModelName
         , "rowDecoder = do"
         ] <> columnBindings <>
         [ pureExpr
         ])

compileCreateStatement :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileCreateStatement table@(CreateTable { name }) =
    let columns = allColumnsIncludingInherited table
        modelName = tableNameToModelName name
        moduleName = "Generated.Statements.Create" <> modelName
        qualifiedModelName = qualifiedConstructorNameFromTableName name
        writableColumns = onlyWritableColumns columns
        allColumnNames = commaSep (map (.name) columns)
        isDynamic = hasAnyDefaults writableColumns
        rowDecoderImport = "import qualified Generated.Statements.RowDecoder" <> modelName <> " as RowDecoder\n"
    in if isDynamic
        then compileDynamicCreateStatement moduleName qualifiedModelName name writableColumns allColumnNames table columns rowDecoderImport
        else compileStaticCreateStatement moduleName qualifiedModelName name writableColumns allColumnNames table columns rowDecoderImport

compileStaticCreateStatement :: (?schema :: Schema) => Text -> Text -> Text -> [Column] -> Text -> CreateTable -> [Column] -> Text -> Text
compileStaticCreateStatement moduleName qualifiedModelName tableName writableColumns allColumnNames table columns rowDecoderImport =
    let writableColumnNames = commaSep (map (.name) writableColumns)
        placeholders = commaSep ["$" <> tshow i | i <- [1 .. length writableColumns]]
        encoderLines = map (hasqlColumnEncoder table) writableColumns
    in statementModuleHeader moduleName ["statement", "discardResultStatement"] rowDecoderImport
        <> Text.unlines
        [ "statement :: Statement.Statement " <> qualifiedModelName <> " " <> qualifiedModelName
        , "statement = Statement.preparable sqlReturningResult encoder decoder"
        , ""
        , "discardResultStatement :: Statement.Statement " <> qualifiedModelName <> " ()"
        , "discardResultStatement = Statement.preparable sqlDiscardResult encoder Decoders.noResult"
        , ""
        , "sql :: Bool -> Text"
        , "sql returning = \"INSERT INTO " <> tableName <> " (" <> writableColumnNames <> ") VALUES (" <> placeholders <> ")\""
        , "    <> if returning then \" RETURNING " <> allColumnNames <> "\" else \"\""
        , ""
        , "sqlReturningResult :: Text"
        , "sqlReturningResult = sql True"
        , ""
        , "sqlDiscardResult :: Text"
        , "sqlDiscardResult = sql False"
        , ""
        , "encoder :: Encoders.Params " <> qualifiedModelName
        , "encoder ="
        , formatEncoderBlock encoderLines
        , ""
        , "decoder :: Decoders.Result " <> qualifiedModelName
        , "decoder = Decoders.singleRow RowDecoder.rowDecoder"
        ]

compileDynamicCreateStatement :: (?schema :: Schema) => Text -> Text -> Text -> [Column] -> Text -> CreateTable -> [Column] -> Text -> Text
compileDynamicCreateStatement moduleName qualifiedModelName tableName writableColumns allColumnNames table columns rowDecoderImport =
    let columnBitIndices = columnsWithBitIndices columns writableColumns
        sqlEntries = map (\(col, bitIdx) -> compileSqlEntry False bitIdx col) columnBitIndices
        sqlBody = Text.unlines
            [ "sql :: Integer -> Bool -> Text"
            , "sql touchedFields returning ="
            , "    let entries = catMaybes"
            , "            [ " <> Text.intercalate "\n            , " sqlEntries
            , "            ]"
            , "        columns = Text.intercalate \", \" entries"
            , "        placeholders = Text.intercalate \", \" [\"$\" <> Text.pack (show i) | i <- [1 .. length entries]]"
            , "        returningClause = if returning then \" RETURNING " <> allColumnNames <> "\" else \"\""
            , "    in if null entries"
            , "        then \"INSERT INTO " <> tableName <> " DEFAULT VALUES\" <> returningClause"
            , "        else \"INSERT INTO " <> tableName <> " (\" <> columns <> \") VALUES (\" <> placeholders <> \")\" <> returningClause"
            ]

        encoderEntries = map (\(col, bitIdx) -> compileEncoderEntry False bitIdx table col) columnBitIndices
        encoderBody = Text.unlines
            [ "encoder :: Integer -> Encoders.Params " <> qualifiedModelName
            , "encoder touchedFields = mconcat $ catMaybes"
            , "    [ " <> Text.intercalate "\n    , " encoderEntries
            , "    ]"
            ]
    in dynamicStatementModule moduleName qualifiedModelName sqlBody encoderBody rowDecoderImport

compileUpdateStatement :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileUpdateStatement table@(CreateTable { name }) =
    let columns = allColumnsIncludingInherited table
        modelName = tableNameToModelName name
        qualifiedModelName = qualifiedConstructorNameFromTableName name
        allWritableColumns = onlyWritableColumns columns
        pkColumns = primaryKeyColumns table
        pkColumnNames = map (.name) pkColumns
        writableColumns = filter (\col -> col.name `notElem` pkColumnNames) allWritableColumns
        allColumnNames = commaSep (map (.name) columns)

        columnBitIndices = columnsWithBitIndices columns writableColumns
        sqlEntries = map (\(col, bitIdx) -> compileSqlEntry True bitIdx col) columnBitIndices

        sqlBody = Text.unlines
            [ "sql :: Integer -> Bool -> Text"
            , "sql touchedFields returning ="
            , "    let setEntries = catMaybes"
            , "            [ " <> Text.intercalate "\n            , " sqlEntries
            , "            ]"
            , "        setClauses = [col <> \" = $\" <> Text.pack (show i) | (i, col) <- zip [1..] setEntries]"
            , "        pkIdx = length setEntries + 1"
            , "        whereClause = " <> compileUpdateWhereClause pkColumns
            , "        returningClause = if returning then \" RETURNING " <> allColumnNames <> "\" else \"\""
            , "    in \"UPDATE " <> name <> " SET \" <> Text.intercalate \", \" setClauses <> \" WHERE \" <> whereClause pkIdx <> returningClause"
            ]

        encoderEntries = map (\(col, bitIdx) -> compileEncoderEntry True bitIdx table col) columnBitIndices
        pkEncoders = map (hasqlColumnEncoder table) pkColumns
        encoderBody = Text.unlines
            [ "encoder :: Integer -> Encoders.Params " <> qualifiedModelName
            , "encoder touchedFields = mconcat (catMaybes"
            , "    [ " <> Text.intercalate "\n    , " encoderEntries
            , "    ])"
            , "    <> " <> case pkEncoders of
                    [e] -> "(" <> e <> ")"
                    es -> "mconcat [" <> Text.intercalate ", " es <> "]"
            ]

        moduleName = "Generated.Statements.Update" <> modelName
        rowDecoderImport = "import qualified Generated.Statements.RowDecoder" <> modelName <> " as RowDecoder\n"
    in dynamicStatementModule moduleName qualifiedModelName sqlBody encoderBody rowDecoderImport
    where
        compileUpdateWhereClause :: [Column] -> Text
        compileUpdateWhereClause [col] = "\\startIdx -> " <> tshow col.name <> " <> \" = $\" <> Text.pack (show startIdx)"
        compileUpdateWhereClause cols =
            let parts = zipWith (\col i ->
                    tshow col.name <> " <> \" = $\" <> Text.pack (show (startIdx + " <> tshow i <> "))"
                    ) cols [(0 :: Int)..]
            in "\\startIdx -> Text.intercalate \" AND \" [" <> Text.intercalate ", " parts <> "]"

dynamicStatementModule :: Text -> Text -> Text -> Text -> Text -> Text
dynamicStatementModule moduleName qualifiedModelName sqlBody encoderBody rowDecoderImport =
    statementModuleHeader moduleName ["statement", "discardResultStatement"] (rowDecoderImport <> statementModuleDynamicImports)
    <> Text.unlines
        [ "statement :: Integer -> Statement.Statement " <> qualifiedModelName <> " " <> qualifiedModelName
        , "statement touchedFields = Statement.preparable (sql touchedFields True) (encoder touchedFields) decoder"
        , ""
        , "discardResultStatement :: Integer -> Statement.Statement " <> qualifiedModelName <> " ()"
        , "discardResultStatement touchedFields = Statement.preparable (sql touchedFields False) (encoder touchedFields) Decoders.noResult"
        , ""
        , sqlBody
        , ""
        , encoderBody
        , ""
        , "decoder :: Decoders.Result " <> qualifiedModelName
        , "decoder = Decoders.singleRow RowDecoder.rowDecoder"
        ]

statementModuleHeader :: Text -> [Text] -> Text -> Text
statementModuleHeader moduleName exports extraImports =
    Text.unlines
        [ "-- This file is auto generated and will be overriden regulary."
        , "{-# OPTIONS_GHC -Wno-unused-imports -Wno-dodgy-imports -Wno-unused-matches #-}"
        , "module " <> moduleName <> " (" <> intercalate ", " exports <> ") where"
        , ""
        , statementModuleBaseImports
        ]
    <> extraImports

compileFetchByIdStatement :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileFetchByIdStatement table@(CreateTable { name }) =
    let columns = allColumnsIncludingInherited table
        modelName = tableNameToModelName name
        moduleName = "Generated.Statements.Fetch" <> modelName
        qualifiedModelName = qualifiedConstructorNameFromTableName name
        pkColumns = primaryKeyColumns table
        allColumnNames = commaSep (map (.name) columns)

        -- WHERE clause
        (whereClauses, _) = foldl' (\(acc, idx) col -> (col.name <> " = $" <> tshow idx : acc, idx + 1)) ([], 1 :: Int) pkColumns
        whereClauseSql = intercalate " AND " (reverse whereClauses)

        sql = "SELECT " <> allColumnNames <> " FROM " <> name <> " WHERE " <> whereClauseSql <> " LIMIT 1"

        rowDecoderImport = "import qualified Generated.Statements.RowDecoder" <> modelName <> " as RowDecoder\n"

    in statementModuleHeader moduleName ["statement"] rowDecoderImport
        <> Text.unlines
        [ "statement :: Statement.Statement (Id' " <> tshow name <> ") (Maybe " <> qualifiedModelName <> ")"
        , "statement = Statement.preparable sql encoder decoder"
        , ""
        , "sql :: Text"
        , "sql = " <> tshow sql
        , ""
        , "encoder :: Encoders.Params (Id' " <> tshow name <> ")"
        , "encoder = " <> fetchByIdEncoder table
        , ""
        , "decoder :: Decoders.Result (Maybe " <> qualifiedModelName <> ")"
        , "decoder = Decoders.rowMaybe RowDecoder.rowDecoder"
        ]

fetchByIdEncoder :: (?schema :: Schema) => CreateTable -> Text
fetchByIdEncoder table = case primaryKeyColumns table of
    [col] ->
        "Encoders.param (Encoders.nonNullable Mapping.encoder)"
    cols ->
        let encoders = zipWith (\col idx ->
                let baseEncoder = hasqlValueEncoder col.columnType
                    accessor = "(\\(Id (" <> commaSep (map (\i -> if i == idx then "v" else "_") [0..length cols - 1]) <> ")) -> v)"
                in accessor <> " >$< Encoders.param (Encoders.nonNullable " <> baseEncoder <> ")"
                ) cols [0..]
        in "mconcat [" <> intercalate ", " encoders <> "]"

compileCreateManyStatement :: (?schema :: Schema, ?compilerOptions :: CompilerOptions) => CreateTable -> Text
compileCreateManyStatement table@(CreateTable { name }) =
    let columns = allColumnsIncludingInherited table
        modelName = tableNameToModelName name
        moduleName = "Generated.Statements.CreateMany" <> modelName
        qualifiedModelName = qualifiedConstructorNameFromTableName name
        writableColumns = onlyWritableColumns columns
        writableColumnNames = commaSep (map (.name) writableColumns)
        allColumnNames = commaSep (map (.name) columns)
        numCols = length writableColumns

        encoderLines = map (hasqlColumnEncoder table) writableColumns
        singleEncoderBlock = formatEncoderBlock encoderLines

        rowDecoderImport = "import qualified Generated.Statements.RowDecoder" <> modelName <> " as RowDecoder\n"

    in statementModuleHeader moduleName ["statement"] (rowDecoderImport <> "import qualified Data.Text as Text\n")
        <> Text.unlines
        [ "statement :: Int -> Statement.Statement [" <> qualifiedModelName <> "] [" <> qualifiedModelName <> "]"
        , "statement count = Statement.unpreparable (sql count) (encoder count) decoder"
        , ""
        , "sql :: Int -> Text"
        , "sql count = \"INSERT INTO " <> name <> " (" <> writableColumnNames <> ") VALUES \""
        , "    <> Text.intercalate \", \" [valueGroup (i * " <> tshow numCols <> ") | i <- [0..count - 1]]"
        , "    <> \" RETURNING " <> allColumnNames <> "\""
        , "  where"
        , "    valueGroup offset = \"(\" <> Text.intercalate \", \" [\"$\" <> Text.pack (show (offset + j)) | j <- [1.." <> tshow numCols <> "]] <> \")\""
        , ""
        , "encoder :: Int -> Encoders.Params [" <> qualifiedModelName <> "]"
        , "encoder count = mconcat [contramap (!! i) singleEncoder | i <- [0..count - 1]]"
        , ""
        , "singleEncoder :: Encoders.Params " <> qualifiedModelName
        , "singleEncoder ="
        , singleEncoderBlock
        , ""
        , "decoder :: Decoders.Result [" <> qualifiedModelName <> "]"
        , "decoder = Decoders.rowList RowDecoder.rowDecoder"
        ]


