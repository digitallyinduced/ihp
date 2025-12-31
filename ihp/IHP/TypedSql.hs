{-# LANGUAGE ImplicitParams #-} -- allow ?modelContext implicit parameter
{-# LANGUAGE NamedFieldPuns #-} -- allow record field puns like {field}
{-# LANGUAGE QuasiQuotes #-} -- enable [typedSql|...|] quasiquoter syntax
{-# LANGUAGE RecordWildCards #-} -- allow {..} to bind all record fields
{-# LANGUAGE TemplateHaskell #-} -- allow compile-time code generation via TH

-- Module implementing the typed SQL quasiquoter and its supporting helpers.
module IHP.TypedSql
    ( typedSql -- expose the SQL quasiquoter entry point
    , TypedQuery (..) -- expose the query container and its fields
    , runTyped -- execute a typed query returning all rows
    , runTypedOne -- execute a typed query expecting exactly one row
    ) where

import IHP.Prelude -- IHP's standard prelude with common utilities
import           Control.Monad (guard) -- guard for Maybe-based checks
import qualified Data.Aeson as Aeson -- JSON parsing for stub metadata
import           Data.Aeson ((.:), (.:?), (.!=)) -- JSON field operators used in FromJSON instances
import qualified Data.ByteString as BS -- byte string for raw SQL and file IO
import qualified Data.List as List -- list helpers like groupBy
import           Data.Maybe (mapMaybe) -- mapMaybe for grouping columns safely
import qualified Data.Map.Strict as Map -- strict map used for metadata lookup
import qualified Data.Set as Set -- set used for OID collection
import qualified Data.String.Conversions as CS -- convert between Text/String/ByteString
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef) -- mutable cache for stubs
import Data.Scientific (Scientific) -- numeric type for Postgres numeric
import Data.Time (LocalTime, TimeOfDay, UTCTime) -- time types for mapping
import Data.Time.Calendar (Day) -- date-only type
import Data.UUID (UUID) -- UUID mapping
import Data.Word (Word32) -- raw OID storage in stub files
import qualified Database.PostgreSQL.LibPQ as PQ -- low-level libpq for describe/prepare
import qualified Database.PostgreSQL.Simple as PG -- high-level postgres-simple API
import qualified Database.PostgreSQL.Simple.FromRow as PGFR -- row parser typeclass
import qualified Database.PostgreSQL.Simple.ToField as PGTF -- parameter encoding
import qualified Database.PostgreSQL.Simple.ToRow as PGTR -- ToRow instance for PreparedRow
import qualified Database.PostgreSQL.Simple.Types as PG -- extra postgres-simple types (Binary, Oid, etc.)
import qualified Language.Haskell.Meta.Parse as HaskellMeta -- parse placeholder expressions as Haskell AST
import qualified Language.Haskell.TH as TH -- Template Haskell core types
import qualified Language.Haskell.TH.Quote as TH -- QuasiQuoter type
import Net.IP (IP) -- inet type mapping

import IHP.FrameworkConfig (defaultDatabaseUrl) -- resolve DB URL for describe
import IHP.ModelSupport (Id', ModelContext, measureTimeIfLogging, withDatabaseConnection, withRLSParams) -- query helpers and Id type
import IHP.NameSupport (tableNameToModelName) -- convert SQL table names to Haskell type names
import qualified IHP.Postgres.Point as PGPoint -- Postgres point mapping
import qualified IHP.Postgres.Polygon as PGPolygon -- Postgres polygon mapping
import qualified IHP.Postgres.TSVector as PGTs -- tsvector mapping
import qualified IHP.Postgres.TimeParser as PGTime -- interval mapping
import System.Environment (lookupEnv) -- read stub path env var
import System.IO.Unsafe (unsafePerformIO) -- top-level cache with IORef

-- | Prepared query with a custom row parser.
-- High-level: this is the runtime value produced by the typed SQL quasiquoter.
data TypedQuery result = TypedQuery
    { tqQuery :: !PG.Query -- low-level: SQL text packaged for postgres-simple
    , tqParams :: ![PGTF.Action] -- low-level: encoded parameter values for the query
    , tqRowParser :: !(PGFR.RowParser result) -- low-level: how to decode each row into result
    }

-- Wrapper to turn a list of Action into a ToRow instance.
newtype PreparedRow = PreparedRow [PGTF.Action] -- low-level: container to satisfy ToRow
instance PGTR.ToRow PreparedRow where
    toRow (PreparedRow params) = params -- low-level: pass through the parameter list

-- | Run a typed query and return all rows.
-- High-level: delegates to postgres-simple with logging and RLS params.
runTyped :: (?modelContext :: ModelContext) => TypedQuery result -> IO [result]
runTyped TypedQuery { tqQuery, tqParams, tqRowParser } =
    withDatabaseConnection \connection -> -- obtain a DB connection from the model context
        withRLSParams -- apply row-level security parameters if configured
            (\query params ->
                measureTimeIfLogging "🔍" connection -- measure/log query runtime with a label
                    (PG.queryWith tqRowParser connection query (PreparedRow params)) -- execute and parse rows
                    query -- log the SQL query
                    (PreparedRow params) -- log the query parameters
            )
            tqQuery -- the SQL to execute
            (PreparedRow tqParams) -- wrap params to match ToRow

-- | Run a typed query that is expected to return a single row.
-- High-level: enforces exactly-one-row semantics.
runTypedOne :: (?modelContext :: ModelContext) => TypedQuery result -> IO result
runTypedOne typed = do
    rows <- runTyped typed -- execute query and collect rows
    case rows of
        [row] -> pure row -- success: exactly one row
        [] -> error "runTypedOne: expected exactly one row but got none" -- error on zero rows
        _ -> error ("runTypedOne: expected a single row but got " <> show (length rows)) -- error on too many rows

-- * Template Haskell quasiquoter

-- | QuasiQuoter entry point for typed SQL.
-- High-level: produces a TH expression that builds a TypedQuery at compile time.
typedSql :: TH.QuasiQuoter
typedSql =
    TH.QuasiQuoter
        { TH.quoteExp = typedSqlExp -- allow use in expressions
        , TH.quotePat = \_ -> fail "typedSql: not supported in patterns" -- disallow pattern contexts
        , TH.quoteType = \_ -> fail "typedSql: not supported in types" -- disallow type contexts
        , TH.quoteDec = \_ -> fail "typedSql: not supported at top-level" -- disallow declaration contexts
        }

-- | Result of describing a statement.
-- High-level: captures everything we need to infer Haskell types.
data DescribeResult = DescribeResult
    { drParams :: ![PQ.Oid] -- low-level: parameter OIDs in order
    , drColumns :: ![DescribeColumn] -- low-level: column metadata in order
    , drTables :: !(Map.Map PQ.Oid TableMeta) -- low-level: table metadata by OID
    , drTypes :: !(Map.Map PQ.Oid PgTypeInfo) -- low-level: type metadata by OID
    }

-- Metadata for a column in the result set.
data DescribeColumn = DescribeColumn
    { dcName :: !BS.ByteString -- low-level: column name as bytes
    , dcType :: !PQ.Oid -- low-level: column type OID
    , dcTable :: !PQ.Oid -- low-level: originating table OID (0 if none)
    , dcAttnum :: !(Maybe Int) -- low-level: attribute number inside table, if known
    }

-- Column details extracted from pg_attribute.
data ColumnMeta = ColumnMeta
    { cmAttnum :: !Int -- low-level: attribute number within table
    , cmName :: !Text -- low-level: column name
    , cmTypeOid :: !PQ.Oid -- low-level: type OID for this column
    , cmNotNull :: !Bool -- low-level: whether the column is NOT NULL
    }

-- Table metadata, including columns and key relationships.
data TableMeta = TableMeta
    { tmOid :: !PQ.Oid -- low-level: table OID
    , tmName :: !Text -- low-level: table name
    , tmColumns :: !(Map.Map Int ColumnMeta) -- low-level: columns keyed by attnum
    , tmColumnOrder :: ![Int] -- low-level: column order as defined in table
    , tmPrimaryKeys :: !(Set.Set Int) -- low-level: attribute numbers of primary keys
    , tmForeignKeys :: !(Map.Map Int PQ.Oid) -- low-level: attnum -> referenced table oid
    }

-- Postgres type metadata we need for Haskell mapping.
data PgTypeInfo = PgTypeInfo
    { ptiOid :: !PQ.Oid -- low-level: type OID
    , ptiName :: !Text -- low-level: type name (typname)
    , ptiElem :: !(Maybe PQ.Oid) -- low-level: element type for arrays
    , ptiType :: !(Maybe Char) -- low-level: typtype code (e.g. 'e' for enum)
    , ptiNamespace :: !(Maybe Text) -- low-level: namespace name
    }

-- Convert libpq Oid to postgres-simple Oid.
toPGOid :: PQ.Oid -> PG.Oid
toPGOid (PQ.Oid w) = PG.Oid w -- low-level: wrap the same numeric value

-- Convert postgres-simple Oid to libpq Oid.
toPQOid :: PG.Oid -> PQ.Oid
toPQOid (PG.Oid w) = PQ.Oid w -- low-level: wrap the same numeric value

-- Build the TH expression for a typed SQL quasiquote.
typedSqlExp :: String -> TH.ExpQ
typedSqlExp rawSql = do
    -- Replace ${expr} placeholders with $1, $2, ... and collect expressions.
    let (sqlText, placeholderExprs) = substitutePlaceholders rawSql
    parsedExprs <- mapM parseExpr placeholderExprs -- parse each placeholder as Haskell code

    stubPath <- TH.runIO (lookupEnv "IHP_TYPED_SQL_STUB") -- optional path to a stub file
    describeResult <- TH.runIO $ case stubPath of
        Just path -> describeUsingStub path sqlText -- offline: use stub metadata
        Nothing -> describeStatement (CS.cs sqlText) -- online: ask Postgres to describe

    let DescribeResult { drParams, drColumns, drTables, drTypes } = describeResult -- unpack metadata
    when (length drParams /= length parsedExprs) $ -- make sure counts match
        fail (CS.cs ("typedSql: placeholder count mismatch. SQL expects " <> show (length drParams) <> " parameters but found " <> show (length parsedExprs) <> " ${..} expressions."))

    paramTypes <- mapM (hsTypeForParam drTypes) drParams -- map param OIDs to Haskell types

    let annotatedParams =
            zipWith (\expr paramTy -> TH.SigE expr paramTy) parsedExprs paramTypes -- add type sigs to args

    resultType <- hsTypeForColumns drTypes drTables drColumns -- compute result type from columns

    let sqlLiteral = TH.SigE (TH.LitE (TH.StringL sqlText)) (TH.ConT ''String)
        queryExpr = TH.AppE (TH.ConE 'PG.Query) (TH.AppE (TH.VarE 'CS.cs) sqlLiteral)
        rowParserExpr = if length drColumns == 1 then TH.VarE 'PGFR.field else TH.VarE 'PGFR.fromRow
        typedQueryExpr =
            TH.RecConE
                'TypedQuery
                [ (TH.mkName "tqQuery", queryExpr) -- build query text
                , (TH.mkName "tqParams", TH.ListE (map (TH.AppE (TH.VarE 'PGTF.toField)) annotatedParams)) -- encode params
                , (TH.mkName "tqRowParser", rowParserExpr) -- parse single column or full row
                ]

    pure (TH.SigE typedQueryExpr (TH.AppT (TH.ConT ''TypedQuery) resultType)) -- add overall type signature

-- | Replace ${expr} placeholders with PostgreSQL-style $1 placeholders.
-- High-level: turns a templated SQL string into a PG-ready SQL string plus expr list.
substitutePlaceholders :: String -> (String, [String])
substitutePlaceholders = go 1 "" [] where
    go _ acc exprs [] = (reverse acc, reverse exprs) -- done: reverse accumulators
    go n acc exprs ('$':'{':rest) =
        let (expr, after) = breakOnClosing 0 "" rest -- parse until matching }
        in go (n + 1) (reverse ('$' : CS.cs (show n)) <> acc) (expr : exprs) after -- replace with $n
    go n acc exprs (c:rest) = go n (c : acc) exprs rest -- copy non-placeholder chars

    breakOnClosing depth acc [] = (reverse acc, []) -- no closing brace found
    breakOnClosing depth acc ('{':xs) = breakOnClosing (depth + 1) ('{':acc) xs -- nested { increases depth
    breakOnClosing depth acc ('}':xs)
        | depth == 0 = (reverse acc, xs) -- close the current placeholder
        | otherwise = breakOnClosing (depth - 1) ('}':acc) xs -- close a nested brace
    breakOnClosing depth acc (x:xs) = breakOnClosing depth (x:acc) xs -- accumulate placeholder chars

-- Parse a placeholder expression into TH.
parseExpr :: String -> TH.ExpQ
parseExpr exprText =
    case HaskellMeta.parseExp exprText of
        Left err -> fail ("typedSql: failed to parse expression {" <> exprText <> "}: " <> err) -- parse error
        Right expr -> pure expr -- success: return parsed TH expression

-- | Describe a statement using libpq, and fetch the additional metadata needed to map to Haskell types.
-- High-level: loads a DescribeResult from a stub JSON file.
describeUsingStub :: FilePath -> String -> IO DescribeResult
describeUsingStub path sqlText = do
    entries <- loadStubEntries path -- load and cache stub entries
    let key = CS.cs sqlText -- use the SQL text as the lookup key
    maybe (fail ("typedSql: no stub entry for SQL: " <> sqlText)) pure (Map.lookup key entries) -- lookup or fail

-- Describe a statement by asking a real Postgres server.
describeStatement :: BS.ByteString -> IO DescribeResult
describeStatement sql = do
    dbUrl <- defaultDatabaseUrl -- read database URL
    conn <- PQ.connectdb dbUrl -- open libpq connection
    status <- PQ.status conn -- check connection state
    unless (status == PQ.ConnectionOk) do
        err <- PQ.errorMessage conn -- fetch error message
        fail ("typedSql: could not connect to database: " <> CS.cs (fromMaybe "" err)) -- abort compile

    let statementName = "ihp_typed_sql_stmt" -- use a fixed prepared statement name
    _ <- ensureOk "prepare" =<< PQ.prepare conn statementName sql Nothing -- prepare the statement
    desc <- ensureOk "describe" =<< PQ.describePrepared conn statementName -- ask for metadata

    paramCount <- PQ.nparams desc -- number of parameters
    paramTypes <- mapM (PQ.paramtype desc) [0 .. paramCount - 1] -- each parameter type OID

    columnCount <- PQ.nfields desc -- number of result columns
    let PQ.Col columnCountCInt = columnCount
    let columnCountInt = fromIntegral columnCountCInt :: Int
    columns <- mapM (\i -> do
        let colIndex = PQ.Col (fromIntegral i)
        name <- fromMaybe "" <$> PQ.fname desc colIndex -- column name
        colType <- PQ.ftype desc colIndex -- column type OID
        tableOid <- PQ.ftable desc colIndex -- table OID for the column
        attnumRaw <- PQ.ftablecol desc colIndex -- attribute number in table
        let PQ.Col attnumCInt = attnumRaw
        let attnum = if tableOid == PQ.Oid 0 then Nothing else Just (fromIntegral attnumCInt) -- ignore attnum when table is 0
        pure DescribeColumn { dcName = name, dcType = colType, dcTable = tableOid, dcAttnum = attnum } -- build column meta
        ) [0 .. columnCountInt - 1]

    -- Load metadata for referenced tables and types.
    let tableOids = Set.fromList (map dcTable columns) |> Set.delete (PQ.Oid 0) -- collect referenced table OIDs
        typeOids = Set.fromList paramTypes <> Set.fromList (map dcType columns) -- collect referenced type OIDs

    pgConn <- PG.connectPostgreSQL dbUrl -- open postgres-simple connection for catalog queries
    tables <- loadTableMeta pgConn (Set.toList tableOids) -- load table metadata
    types <- loadTypeInfo pgConn (Set.toList typeOids) -- load type metadata
    PG.close pgConn -- close postgres-simple connection

    _ <- PQ.exec conn ("DEALLOCATE " <> statementName) -- release prepared statement
    PQ.finish conn -- close libpq connection

    pure DescribeResult { drParams = paramTypes, drColumns = columns, drTables = tables, drTypes = types } -- return full metadata

-- Ensure libpq returned a successful result.
ensureOk :: String -> Maybe PQ.Result -> IO PQ.Result
ensureOk actionName = \case
    Nothing -> fail ("typedSql: " <> actionName <> " returned no result") -- missing libpq result
    Just res -> do
        status <- PQ.resultStatus res -- inspect result status
        case status of
            PQ.CommandOk -> pure res -- OK for command
            PQ.TuplesOk -> pure res -- OK for tuples
            _ -> do
                msg <- PQ.resultErrorMessage res -- read error message
                fail ("typedSql: " <> actionName <> " failed: " <> CS.cs (fromMaybe "" msg)) -- abort

-- | Load table metadata for all referenced tables.
-- High-level: read pg_catalog to map table/column info.
loadTableMeta :: PG.Connection -> [PQ.Oid] -> IO (Map.Map PQ.Oid TableMeta)
loadTableMeta _ [] = pure mempty -- no tables requested
loadTableMeta conn tableOids = do
    rows <- PG.query conn -- fetch column info for each requested table
        (mconcat
            [ "SELECT c.oid, c.relname, a.attnum, a.attname, a.atttypid, a.attnotnull "
            , "FROM pg_class c "
            , "JOIN pg_namespace ns ON ns.oid = c.relnamespace "
            , "JOIN pg_attribute a ON a.attrelid = c.oid "
            , "WHERE c.oid = ANY(?) AND a.attnum > 0 AND NOT a.attisdropped "
            , "ORDER BY c.oid, a.attnum"
            ])
        (PG.Only (PG.PGArray (map toPGOid tableOids)) :: PG.Only (PG.PGArray PG.Oid)) -- parameterize the OID list

    primaryKeys <- PG.query conn -- fetch primary key columns for each table
        "SELECT conrelid, unnest(conkey) as attnum FROM pg_constraint WHERE contype = 'p' AND conrelid = ANY(?)"
        (PG.Only (PG.PGArray (map toPGOid tableOids)) :: PG.Only (PG.PGArray PG.Oid)) -- parameterize the OID list

    foreignKeys <- PG.query conn -- fetch simple (single-column) foreign keys
        (mconcat
            [ "SELECT conrelid, conkey[1] as attnum, confrelid "
            , "FROM pg_constraint "
            , "WHERE contype = 'f' AND array_length(conkey,1) = 1 AND conrelid = ANY(?)"
            ])
        (PG.Only (PG.PGArray (map toPGOid tableOids)) :: PG.Only (PG.PGArray PG.Oid)) -- parameterize the OID list

    let pkMap = primaryKeys
            |> foldl' (\acc (relid :: PG.Oid, att :: Int) ->
                    Map.insertWith Set.union (toPQOid relid) (Set.singleton att) acc
                ) mempty -- build map of table -> primary key attnums

        fkMap = foreignKeys
            |> foldl' (\acc (relid :: PG.Oid, att :: Int, ref :: PG.Oid) ->
                    Map.insertWith Map.union (toPQOid relid) (Map.singleton att (toPQOid ref)) acc
                ) mempty -- build map of table -> foreign key attnum -> referenced table

        tableGroups =
            rows
                |> map (\(relid :: PG.Oid, name :: Text, attnum :: Int, attname :: Text, atttypid :: PG.Oid, attnotnull :: Bool) ->
                        (toPQOid relid, ColumnMeta { cmAttnum = attnum, cmName = attname, cmTypeOid = toPQOid atttypid, cmNotNull = attnotnull }, name)
                    ) -- annotate each column row with its table
                |> List.groupBy (\(l, _, _) (r, _, _) -> l == r) -- group by table OID

    pure $ tableGroups
        |> foldl'
            (\acc group ->
                case group of
                    [] -> acc -- no columns, keep accumulator
                    (relid, _, tableName) : _ ->
                        let cols = group
                                |> map (\(_, col, _) -> (cmAttnum col, col))
                                |> Map.fromList -- map attnum -> ColumnMeta
                            order = group |> map (\(_, ColumnMeta { cmAttnum }, _) -> cmAttnum) -- preserve column order
                            pks = Map.findWithDefault mempty relid pkMap -- lookup primary keys
                            fks = Map.findWithDefault mempty relid fkMap -- lookup foreign keys
                            meta = TableMeta
                                { tmOid = relid
                                , tmName = tableName
                                , tmColumns = cols
                                , tmColumnOrder = order
                                , tmPrimaryKeys = pks
                                , tmForeignKeys = fks
                                } -- build TableMeta
                        in Map.insert relid meta acc -- add to result map
            )
            mempty -- start with empty map

-- | Load type information for the given OIDs.
-- High-level: fetch pg_type metadata recursively for arrays.
loadTypeInfo :: PG.Connection -> [PQ.Oid] -> IO (Map.Map PQ.Oid PgTypeInfo)
loadTypeInfo _ [] = pure mempty -- no types requested
loadTypeInfo conn typeOids = do
    let requested = Set.fromList typeOids -- track requested OIDs
    rows <- PG.query conn -- fetch pg_type rows for requested OIDs
        (mconcat
            [ "SELECT oid, typname, typelem, typtype, typnamespace::regnamespace::text "
            , "FROM pg_type "
            , "WHERE oid = ANY(?)"
            ])
        (PG.Only (PG.PGArray (map toPGOid typeOids)) :: PG.Only (PG.PGArray PG.Oid)) -- parameterize the OID list
    let (typeMap, missing) =
            rows
                |> foldl'
                    (\(acc, missingAcc) (oid :: PG.Oid, name :: Text, elemOid :: PG.Oid, typtype :: Maybe Text, nsp :: Maybe Text) ->
                        let thisOid = toPQOid oid -- convert to libpq Oid type
                            elemOid' = if elemOid == PG.Oid 0 then Nothing else Just (toPQOid elemOid) -- ignore 0 elem
                            nextMissing = case elemOid' of
                                Just o | o `Set.notMember` requested -> o : missingAcc -- queue missing element types
                                _ -> missingAcc
                        in ( Map.insert thisOid PgTypeInfo
                                { ptiOid = thisOid
                                , ptiName = name
                                , ptiElem = elemOid'
                                , ptiType = typtype >>= (listToMaybe . CS.cs) -- extract single char typtype
                                , ptiNamespace = nsp
                                }
                                acc
                           , nextMissing
                           )
                    )
                    (mempty, []) -- start with empty map and missing list
    extras <- loadTypeInfo conn missing -- recursively load missing element types
    pure (typeMap <> extras) -- merge base and extra type info

-- Stub metadata -------------------------------------------------------

-- Cache mapping stub file path to parsed SQL metadata.
type StubCache = Map.Map FilePath (Map.Map Text DescribeResult)

{-# NOINLINE describeStubCache #-} -- ensure the IORef is shared and not duplicated
-- Global cache for stub metadata (safe because file contents are immutable).
describeStubCache :: IORef StubCache
describeStubCache = unsafePerformIO (newIORef mempty) -- initialize to empty

-- Load stub entries from cache or parse if missing.
loadStubEntries :: FilePath -> IO (Map.Map Text DescribeResult)
loadStubEntries path = do
    cache <- readIORef describeStubCache -- read current cache
    case Map.lookup path cache of
        Just entries -> pure entries -- cache hit
        Nothing -> do
            entries <- parseStubFile path -- parse the stub file
            atomicModifyIORef' describeStubCache (\m -> (Map.insert path entries m, ())) -- store in cache
            pure entries

-- Parse a stub file on disk into DescribeResult values.
parseStubFile :: FilePath -> IO (Map.Map Text DescribeResult)
parseStubFile path = do
    bytes <- BS.readFile path -- read JSON file
    stubFile <- either (fail . ("typedSql: failed to parse stub file: " <>) ) pure (Aeson.eitherDecodeStrict' bytes :: Either String DescribeStubFile) -- decode JSON
    pure (buildStubEntries stubFile) -- build lookup map

-- Convert a DescribeStubFile into a map keyed by SQL text.
buildStubEntries :: DescribeStubFile -> Map.Map Text DescribeResult
buildStubEntries DescribeStubFile { stubFileQueries } =
    foldl'
        (\acc entry ->
            Map.insert (stubEntrySql entry) (stubEntryToDescribe entry) acc
        )
        mempty
        stubFileQueries

-- Top-level JSON structure for stub metadata.
data DescribeStubFile = DescribeStubFile
    { stubFileQueries :: ![DescribeStubEntry] -- list of stubbed statements
    }

instance Aeson.FromJSON DescribeStubFile where
    parseJSON = Aeson.withObject "DescribeStubFile" \obj ->
        DescribeStubFile <$> obj .:? "queries" .!= [] -- default to empty list

-- One stubbed SQL statement entry.
data DescribeStubEntry = DescribeStubEntry
    { stubEntrySql :: !Text -- SQL text used as lookup key
    , stubEntryParams :: ![Word32] -- parameter type OIDs
    , stubEntryColumns :: ![DescribeStubColumn] -- result columns
    , stubEntryTables :: ![DescribeStubTable] -- table metadata
    , stubEntryTypes :: ![DescribeStubType] -- type metadata
    }

instance Aeson.FromJSON DescribeStubEntry where
    parseJSON = Aeson.withObject "DescribeStubEntry" \obj ->
        DescribeStubEntry
            <$> obj .: "sql" -- required SQL string
            <*> obj .:? "params" .!= [] -- optional params list
            <*> obj .:? "columns" .!= [] -- optional columns list
            <*> obj .:? "tables" .!= [] -- optional tables list
            <*> obj .:? "types" .!= [] -- optional types list

-- Column description inside a stub entry.
data DescribeStubColumn = DescribeStubColumn
    { stubColumnName :: !Text -- column name
    , stubColumnType :: !Word32 -- type OID
    , stubColumnTable :: !Word32 -- table OID (0 if none)
    , stubColumnAttnum :: !(Maybe Int) -- attribute number, if known
    }

instance Aeson.FromJSON DescribeStubColumn where
    parseJSON = Aeson.withObject "DescribeStubColumn" \obj ->
        DescribeStubColumn
            <$> obj .: "name" -- required name
            <*> obj .: "typeOid" -- required type OID
            <*> obj .:? "tableOid" .!= 0 -- default to 0 (no table)
            <*> obj .:? "attnum" -- optional attnum

-- Table description inside a stub entry.
data DescribeStubTable = DescribeStubTable
    { stubTableOid :: !Word32 -- table OID
    , stubTableName :: !Text -- table name
    , stubTableColumns :: ![DescribeStubTableColumn] -- table columns
    , stubTablePrimaryKeys :: ![Int] -- primary key attnums
    , stubTableForeignKeys :: ![DescribeStubForeignKey] -- foreign keys
    }

instance Aeson.FromJSON DescribeStubTable where
    parseJSON = Aeson.withObject "DescribeStubTable" \obj ->
        DescribeStubTable
            <$> obj .: "oid" -- required OID
            <*> obj .: "name" -- required table name
            <*> obj .:? "columns" .!= [] -- optional columns list
            <*> obj .:? "primaryKeys" .!= [] -- optional PK list
            <*> obj .:? "foreignKeys" .!= [] -- optional FK list

-- Column description inside a stubbed table.
data DescribeStubTableColumn = DescribeStubTableColumn
    { stubTableColumnAttnum :: !Int -- attribute number in table
    , stubTableColumnName :: !Text -- column name
    , stubTableColumnType :: !Word32 -- type OID
    , stubTableColumnNotNull :: !Bool -- NOT NULL flag
    }

instance Aeson.FromJSON DescribeStubTableColumn where
    parseJSON = Aeson.withObject "DescribeStubTableColumn" \obj ->
        DescribeStubTableColumn
            <$> obj .: "attnum" -- required attribute number
            <*> obj .: "name" -- required column name
            <*> obj .: "typeOid" -- required type OID
            <*> obj .:? "notNull" .!= False -- default false for nullable

-- Foreign key description inside a stubbed table.
data DescribeStubForeignKey = DescribeStubForeignKey
    { stubForeignKeyAttnum :: !Int -- local column attnum
    , stubForeignKeyReferences :: !Word32 -- referenced table OID
    }

instance Aeson.FromJSON DescribeStubForeignKey where
    parseJSON = Aeson.withObject "DescribeStubForeignKey" \obj ->
        DescribeStubForeignKey
            <$> obj .: "attnum" -- required local attnum
            <*> obj .: "references" -- required referenced table OID

-- Type description inside a stub entry.
data DescribeStubType = DescribeStubType
    { stubTypeOid :: !Word32 -- type OID
    , stubTypeName :: !Text -- type name
    , stubTypeElemOid :: !(Maybe Word32) -- element type OID for arrays
    , stubTypeTyptype :: !(Maybe Text) -- typtype as text
    , stubTypeNamespace :: !(Maybe Text) -- namespace name
    }

instance Aeson.FromJSON DescribeStubType where
    parseJSON = Aeson.withObject "DescribeStubType" \obj ->
        DescribeStubType
            <$> obj .: "oid" -- required OID
            <*> obj .: "name" -- required type name
            <*> obj .:? "elemOid" -- optional element OID
            <*> obj .:? "typtype" -- optional typtype
            <*> obj .:? "namespace" -- optional namespace

-- Convert a stub entry into a DescribeResult.
stubEntryToDescribe :: DescribeStubEntry -> DescribeResult
stubEntryToDescribe DescribeStubEntry { .. } =
    DescribeResult
        { drParams = map oidFromWord stubEntryParams -- convert param OIDs
        , drColumns = map stubColumnToDescribe stubEntryColumns -- convert columns
        , drTables = Map.fromList (map stubTableToDescribe stubEntryTables) -- convert tables
        , drTypes = Map.fromList (map stubTypeToDescribe stubEntryTypes) -- convert types
        }

-- Convert a stub column into a DescribeColumn.
stubColumnToDescribe :: DescribeStubColumn -> DescribeColumn
stubColumnToDescribe DescribeStubColumn { .. } =
    DescribeColumn
        { dcName = CS.cs stubColumnName -- convert Text to ByteString
        , dcType = oidFromWord stubColumnType -- convert type OID
        , dcTable = oidFromWord stubColumnTable -- convert table OID
        , dcAttnum = stubColumnAttnum -- keep attribute number
        }

-- Convert a stub table into TableMeta.
stubTableToDescribe :: DescribeStubTable -> (PQ.Oid, TableMeta)
stubTableToDescribe DescribeStubTable { .. } =
    ( tableOid
    , TableMeta
        { tmOid = tableOid
        , tmName = stubTableName
        , tmColumns = Map.fromList (map (\col -> (stubTableColumnAttnum col, stubColumnMeta col)) stubTableColumns)
        , tmColumnOrder = map stubTableColumnAttnum stubTableColumns
        , tmPrimaryKeys = Set.fromList stubTablePrimaryKeys
        , tmForeignKeys = Map.fromList (map (\fk -> (stubForeignKeyAttnum fk, oidFromWord (stubForeignKeyReferences fk))) stubTableForeignKeys)
        }
    )
  where
    tableOid = oidFromWord stubTableOid -- convert table OID
    stubColumnMeta DescribeStubTableColumn { .. } = ColumnMeta
        { cmAttnum = stubTableColumnAttnum
        , cmName = stubTableColumnName
        , cmTypeOid = oidFromWord stubTableColumnType
        , cmNotNull = stubTableColumnNotNull
        }

-- Convert a stub type into PgTypeInfo.
stubTypeToDescribe :: DescribeStubType -> (PQ.Oid, PgTypeInfo)
stubTypeToDescribe DescribeStubType { .. } =
    ( oid
    , PgTypeInfo
        { ptiOid = oid
        , ptiName = stubTypeName
        , ptiElem = stubTypeElemOid >>= nonZeroOid
        , ptiType = stubTypeTyptype >>= extractTyptype
        , ptiNamespace = stubTypeNamespace
        }
    )
  where
    oid = oidFromWord stubTypeOid -- convert type OID
    extractTyptype text = case CS.cs text :: String of
        (c:_) -> Just c -- take the first character
        _ -> Nothing -- empty or invalid typtype
    nonZeroOid w = if w == 0 then Nothing else Just (oidFromWord w) -- ignore 0 sentinel

-- Convert a Word32 from JSON into a libpq Oid.
oidFromWord :: Word32 -> PQ.Oid
oidFromWord = PQ.Oid . fromIntegral -- convert numeric width

-- | Build the Haskell type for a parameter, based on its OID.
-- High-level: map a PG type OID into a TH Type.
hsTypeForParam :: Map.Map PQ.Oid PgTypeInfo -> PQ.Oid -> TH.TypeQ
hsTypeForParam typeInfo oid = maybe (fail (CS.cs unknown)) (hsTypeForPg typeInfo False) (Map.lookup oid typeInfo)
  where
    unknown = "typedSql: missing type information for parameter oid " <> show oid -- error message

-- | Build the result type for the described columns.
-- High-level: pick a model type for table.* or a tuple type for ad-hoc select lists.
hsTypeForColumns :: Map.Map PQ.Oid PgTypeInfo -> Map.Map PQ.Oid TableMeta -> [DescribeColumn] -> TH.TypeQ
hsTypeForColumns typeInfo tables cols = do
    case detectFullTable tables cols of
        Just tableName ->
            pure (TH.ConT (TH.mkName (CS.cs ("Generated.ActualTypes." <> tableNameToModelName tableName)))) -- use model type
        Nothing -> do
            hsCols <- mapM (hsTypeForColumn typeInfo tables) cols -- map each column to a type
            case hsCols of
                [single] -> pure single -- single column: no tuple
                _ -> pure $ foldl TH.AppT (TH.TupleT (length hsCols)) hsCols -- multiple columns: tuple

-- | Detect whether the columns represent a full table selection (table.* with all columns in order).
-- High-level: if yes, we can return the model type directly.
detectFullTable :: Map.Map PQ.Oid TableMeta -> [DescribeColumn] -> Maybe Text
detectFullTable tables cols = do
    guard (not (null cols)) -- need at least one column
    let grouped =
            cols
                |> List.groupBy (\a b -> dcTable a == dcTable b) -- group by table OID
                |> mapMaybe (\group -> case List.uncons group of
                        Just (first, _) -> Just (dcTable first, group)
                        Nothing -> Nothing
                   ) -- pair table OID with group
    case grouped of
        [(tableOid, colGroup)] | tableOid /= PQ.Oid 0 -> do
            TableMeta { tmColumnOrder } <- Map.lookup tableOid tables -- look up column order
            let attnums = mapMaybe dcAttnum colGroup -- extract attnums, ignoring unknown
            guard (attnums == tmColumnOrder) -- must match exact order
            TableMeta { tmName } <- Map.lookup tableOid tables -- fetch table name
            pure tmName -- signal full-table selection
        _ -> Nothing -- not a full table selection

-- Map a single column into a Haskell type, with key-aware rules.
hsTypeForColumn :: Map.Map PQ.Oid PgTypeInfo -> Map.Map PQ.Oid TableMeta -> DescribeColumn -> TH.TypeQ
hsTypeForColumn typeInfo tables DescribeColumn { dcType, dcTable, dcAttnum } =
    case (Map.lookup dcTable tables, dcAttnum) of
        (Just TableMeta { tmName = tableName, tmPrimaryKeys, tmForeignKeys, tmColumns }, Just attnum) -> do
            let baseType = Map.lookup attnum tmColumns >>= \ColumnMeta { cmTypeOid } -> Map.lookup cmTypeOid typeInfo -- base PG type
            let nullable = maybe True (not . cmNotNull) (Map.lookup attnum tmColumns) -- default to nullable if unknown
            case () of
                _ | attnum `Set.member` tmPrimaryKeys ->
                    pure (wrapNull nullable (idType tableName)) -- primary key becomes Id
                  | Just refTable <- Map.lookup attnum tmForeignKeys ->
                    case Map.lookup refTable tables of
                        Just TableMeta { tmName = refName } ->
                            pure (wrapNull nullable (idType refName)) -- foreign key becomes Id of referenced table
                        Nothing ->
                            maybe (fail (CS.cs missingType)) (hsTypeForPg typeInfo nullable) baseType -- fallback
                  | otherwise ->
                    maybe (fail (CS.cs missingType)) (hsTypeForPg typeInfo nullable) baseType -- normal column mapping
          where
            missingType = "typedSql: missing type info for column " <> show attnum <> " of table " <> tableName
        _ ->
            maybe (fail (CS.cs ("typedSql: missing type info for column oid " <> show dcType))) (hsTypeForPg typeInfo True) (Map.lookup dcType typeInfo) -- unknown table info

-- Wrap a type in Maybe when nullable.
wrapNull :: Bool -> TH.Type -> TH.Type
wrapNull nullable ty = if nullable then TH.AppT (TH.ConT ''Maybe) ty else ty -- add Maybe for nullable columns

-- Build the Id' type for a table name.
idType :: Text -> TH.Type
idType tableName = TH.AppT (TH.ConT ''Id') (TH.LitT (TH.StrTyLit (CS.cs tableName))) -- Id' "table"

-- Map Postgres type metadata to a Haskell type.
hsTypeForPg :: Map.Map PQ.Oid PgTypeInfo -> Bool -> PgTypeInfo -> TH.TypeQ
hsTypeForPg typeInfo nullable PgTypeInfo { ptiName, ptiElem, ptiType } = do
    base <- case () of
        _ | Just elemOid <- ptiElem -> do
            elemInfo <- maybe (fail (CS.cs ("typedSql: missing array element type for " <> ptiName))) pure (Map.lookup elemOid typeInfo) -- lookup element
            elemTy <- hsTypeForPg typeInfo False elemInfo -- map element type
            pure (TH.AppT TH.ListT elemTy) -- array maps to list
        _ | ptiName `elem` ["int2", "int4"] -> pure (TH.ConT ''Int) -- small/regular int
        _ | ptiName == "int8" -> pure (TH.ConT ''Integer) -- 64-bit int
        _ | ptiName `elem` ["text", "varchar", "bpchar", "citext"] -> pure (TH.ConT ''Text) -- text types
        _ | ptiName == "bool" -> pure (TH.ConT ''Bool) -- boolean
        _ | ptiName == "uuid" -> pure (TH.ConT ''UUID) -- UUID
        _ | ptiName == "timestamptz" -> pure (TH.ConT ''UTCTime) -- timestamp with time zone
        _ | ptiName == "timestamp" -> pure (TH.ConT ''LocalTime) -- timestamp without time zone
        _ | ptiName == "date" -> pure (TH.ConT ''Day) -- date
        _ | ptiName == "time" -> pure (TH.ConT ''TimeOfDay) -- time
        _ | ptiName `elem` ["json", "jsonb"] -> pure (TH.ConT ''Aeson.Value) -- JSON value
        _ | ptiName == "bytea" -> pure (TH.AppT (TH.ConT ''PG.Binary) (TH.ConT ''BS.ByteString)) -- byte array
        _ | ptiName == "float4" -> pure (TH.ConT ''Float) -- 32-bit float
        _ | ptiName == "float8" -> pure (TH.ConT ''Double) -- 64-bit float
        _ | ptiName == "numeric" -> pure (TH.ConT ''Scientific) -- arbitrary precision numeric
        _ | ptiName == "point" -> pure (TH.ConT ''PGPoint.Point) -- point type
        _ | ptiName == "polygon" -> pure (TH.ConT ''PGPolygon.Polygon) -- polygon type
        _ | ptiName == "inet" -> pure (TH.ConT ''IP) -- inet type
        _ | ptiName == "tsvector" -> pure (TH.ConT ''PGTs.TSVector) -- full-text search vector
        _ | ptiName == "interval" -> pure (TH.ConT ''PGTime.PGInterval) -- interval type
        _ | ptiType == Just 'e' ->
            pure (TH.ConT (TH.mkName (CS.cs ("Generated.Enums." <> tableNameToModelName ptiName)))) -- enum type
        _ | ptiType == Just 'c' ->
            pure (TH.ConT (TH.mkName (CS.cs ("Generated.ActualTypes." <> tableNameToModelName ptiName)))) -- composite type
        _ -> pure (TH.ConT (TH.mkName (CS.cs ("Generated.ActualTypes." <> tableNameToModelName ptiName)))) -- fallback to generated type
    pure (wrapNull nullable base) -- apply nullability wrapper
