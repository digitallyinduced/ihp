{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- Module implementing the typed SQL quasiquoter and its supporting helpers.
module IHP.TypedSql
    ( typedSql -- expose the SQL quasiquoter entry point
    , TypedQuery (..) -- expose the query container and its fields
    , runTyped -- execute a typed query returning all rows
    , runTypedOne -- execute a typed query expecting exactly one row
    ) where

import           Control.Exception                  (bracket_)
import           Control.Monad                      (guard, when)
import qualified Data.Aeson                         as Aeson
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Char8              as BS8
import qualified Data.Char                          as Char
import           Data.Coerce                        (coerce)
import qualified Data.List                          as List
import qualified Data.Map.Strict                    as Map
import           Data.Maybe                         (catMaybes, mapMaybe)
import           Data.Scientific                    (Scientific)
import qualified Data.Set                           as Set
import qualified Data.String.Conversions            as CS
import           Data.Time                          (LocalTime, TimeOfDay,
                                                     UTCTime)
import           Data.Time.Calendar                 (Day)
import           Data.UUID                          (UUID)
import qualified Database.PostgreSQL.LibPQ          as PQ
import qualified Database.PostgreSQL.Simple         as PG
import qualified Database.PostgreSQL.Simple.FromRow as PGFR
import qualified Database.PostgreSQL.Simple.ToField as PGTF
import qualified Database.PostgreSQL.Simple.ToRow   as PGTR
import qualified Database.PostgreSQL.Simple.Types   as PG
import           IHP.Prelude
import qualified Language.Haskell.Meta.Parse        as HaskellMeta
import qualified Language.Haskell.TH                as TH
import qualified Language.Haskell.TH.Quote          as TH
import           Net.IP                             (IP)

import           IHP.FrameworkConfig                (defaultDatabaseUrl)
import           IHP.ModelSupport                   (Id', ModelContext,
                                                     measureTimeIfLogging,
                                                     withDatabaseConnection,
                                                     withRLSParams)
import           IHP.NameSupport                    (tableNameToModelName)
import qualified IHP.Postgres.Point                 as PGPoint
import qualified IHP.Postgres.Polygon               as PGPolygon
import qualified IHP.Postgres.TimeParser            as PGTime
import qualified IHP.Postgres.TSVector              as PGTs
import           System.Directory                   (canonicalizePath,
                                                     createDirectoryIfMissing,
                                                     doesDirectoryExist,
                                                     doesFileExist,
                                                     findExecutable,
                                                     removeDirectoryRecursive)
import           System.Environment                 (lookupEnv)
import           System.FilePath                    (isRelative, takeDirectory,
                                                     takeFileName, (</>))
import           System.IO                          (Handle, hIsEOF)
import           System.IO.Temp                     (withSystemTempDirectory)
import qualified System.Process                     as Process

-- | Prepared query with a custom row parser.
-- High-level: this is the runtime value produced by the typed SQL quasiquoter.
data TypedQuery result = TypedQuery
    { tqQuery     :: !PG.Query -- low-level: SQL text packaged for postgres-simple
    , tqParams    :: ![PGTF.Action] -- low-level: encoded parameter values for the query
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
    { drParams  :: ![PQ.Oid] -- low-level: parameter OIDs in order
    , drColumns :: ![DescribeColumn] -- low-level: column metadata in order
    , drTables  :: !(Map.Map PQ.Oid TableMeta) -- low-level: table metadata by OID
    , drTypes   :: !(Map.Map PQ.Oid PgTypeInfo) -- low-level: type metadata by OID
    }

-- Metadata for a column in the result set.
data DescribeColumn = DescribeColumn
    { dcName   :: !BS.ByteString -- low-level: column name as bytes
    , dcType   :: !PQ.Oid -- low-level: column type OID
    , dcTable  :: !PQ.Oid -- low-level: originating table OID (0 if none)
    , dcAttnum :: !(Maybe Int) -- low-level: attribute number inside table, if known
    }

-- Column details extracted from pg_attribute.
data ColumnMeta = ColumnMeta
    { cmAttnum  :: !Int -- low-level: attribute number within table
    , cmName    :: !Text -- low-level: column name
    , cmTypeOid :: !PQ.Oid -- low-level: type OID for this column
    , cmNotNull :: !Bool -- low-level: whether the column is NOT NULL
    }

-- Table metadata, including columns and key relationships.
data TableMeta = TableMeta
    { tmOid         :: !PQ.Oid -- low-level: table OID
    , tmName        :: !Text -- low-level: table name
    , tmColumns     :: !(Map.Map Int ColumnMeta) -- low-level: columns keyed by attnum
    , tmColumnOrder :: ![Int] -- low-level: column order as defined in table
    , tmPrimaryKeys :: !(Set.Set Int) -- low-level: attribute numbers of primary keys
    , tmForeignKeys :: !(Map.Map Int PQ.Oid) -- low-level: attnum -> referenced table oid
    }

-- Postgres type metadata we need for Haskell mapping.
data PgTypeInfo = PgTypeInfo
    { ptiOid       :: !PQ.Oid -- low-level: type OID
    , ptiName      :: !Text -- low-level: type name (typname)
    , ptiElem      :: !(Maybe PQ.Oid) -- low-level: element type for arrays
    , ptiType      :: !(Maybe Char) -- low-level: typtype code (e.g. 'e' for enum)
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
    -- Replace ${expr} placeholders with $1, $2, ... for describe and ? for runtime.
    let PlaceholderPlan { ppDescribeSql, ppRuntimeSql, ppExprs } = planPlaceholders rawSql
    parsedExprs <- mapM parseExpr ppExprs -- parse each placeholder as Haskell code

    bootstrapEnv <- TH.runIO (lookupEnv "IHP_TYPED_SQL_BOOTSTRAP") -- optional bootstrap mode
    loc <- TH.location
    let useBootstrap = isBootstrapEnabled bootstrapEnv
    describeResult <- TH.runIO $
        if useBootstrap
            then describeUsingBootstrap (TH.loc_filename loc) ppDescribeSql -- bootstrap DB from schema
            else describeStatement (CS.cs ppDescribeSql) -- online: ask Postgres to describe

    let DescribeResult { drParams, drColumns, drTables, drTypes } = describeResult -- unpack metadata
    when (length drParams /= length parsedExprs) $ -- make sure counts match
        fail (CS.cs ("typedSql: placeholder count mismatch. SQL expects " <> show (length drParams) <> " parameters but found " <> show (length parsedExprs) <> " ${..} expressions."))

    paramTypes <- mapM (hsTypeForParam drTypes) drParams -- map param OIDs to Haskell types

    let annotatedParams =
            zipWith
                (\expr paramTy -> TH.SigE (TH.AppE (TH.VarE 'coerce) expr) paramTy)
                parsedExprs
                paramTypes -- coerce placeholder expressions into expected param types

    resultType <- hsTypeForColumns drTypes drTables drColumns -- compute result type from columns

    let sqlLiteral = TH.SigE (TH.LitE (TH.StringL ppRuntimeSql)) (TH.ConT ''String)
        queryExpr = TH.AppE (TH.ConE 'PG.Query) (TH.AppE (TH.VarE 'CS.cs) sqlLiteral)
        isCompositeColumn =
            case drColumns of
                [DescribeColumn { dcType }] ->
                    case Map.lookup dcType drTypes of
                        Just PgTypeInfo { ptiType = Just 'c' } -> True
                        _ -> False
                _ -> False
    when (length drColumns == 1 && isCompositeColumn) $
        fail
            ("typedSql: composite columns must be expanded (use SELECT table.* "
                <> "or list columns explicitly)")
    let rowParserExpr = if length drColumns == 1 then TH.VarE 'PGFR.field else TH.VarE 'PGFR.fromRow
        typedQueryExpr =
            TH.AppE
                (TH.AppE
                    (TH.AppE
                        (TH.ConE 'TypedQuery)
                        queryExpr
                    )
                    (TH.ListE (map (TH.AppE (TH.VarE 'PGTF.toField)) annotatedParams))
                )
                rowParserExpr

    pure (TH.SigE typedQueryExpr (TH.AppT (TH.ConT ''TypedQuery) resultType)) -- add overall type signature

data PlaceholderPlan = PlaceholderPlan
    { ppDescribeSql :: !String
    , ppRuntimeSql  :: !String
    , ppExprs       :: ![String]
    }

-- | Replace ${expr} placeholders with PostgreSQL-style $1 for describe and ? for runtime.
-- High-level: turns a templated SQL string into SQL strings plus expr list.
planPlaceholders :: String -> PlaceholderPlan
planPlaceholders = go 1 "" "" [] where
    go _ accDescribe accRuntime exprs [] =
        PlaceholderPlan
            { ppDescribeSql = reverse accDescribe
            , ppRuntimeSql = reverse accRuntime
            , ppExprs = reverse exprs
            }
    go n accDescribe accRuntime exprs ('$':'{':rest) =
        let (expr, after) = breakOnClosing 0 "" rest -- parse until matching }
            describeToken = reverse ('$' : CS.cs (show n))
        in go (n + 1)
              (describeToken <> accDescribe)
              ('?' : accRuntime)
              (expr : exprs)
              after
    go n accDescribe accRuntime exprs (c:rest) =
        go n (c : accDescribe) (c : accRuntime) exprs rest

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

isBootstrapEnabled :: Maybe String -> Bool
isBootstrapEnabled = \case
    Nothing -> False
    Just raw ->
        let value = map Char.toLower raw
        in not (value `elem` ["", "0", "false", "no", "off"])

data BootstrapConfig = BootstrapConfig
    { bcAppSchemaPath :: !FilePath
    , bcIhpSchemaPath :: !(Maybe FilePath)
    }

data PgTools = PgTools
    { pgInitdb   :: !FilePath
    , pgPostgres :: !FilePath
    , pgCreatedb :: !FilePath
    , pgPsql     :: !FilePath
    }

describeUsingBootstrap :: FilePath -> String -> IO DescribeResult
describeUsingBootstrap sourcePath sqlText = do
    config <- resolveBootstrapConfig sourcePath
    withBootstrapDatabase config \dbUrl ->
        describeStatementWith dbUrl (CS.cs sqlText)

resolveBootstrapConfig :: FilePath -> IO BootstrapConfig
resolveBootstrapConfig sourcePath = do
    sourceDir <- canonicalizePath (takeDirectory sourcePath)
    appSchemaPath <- resolveSchemaPath sourceDir
    ihpSchemaPath <- resolveIhpSchemaPath sourceDir
    pure BootstrapConfig
        { bcAppSchemaPath = appSchemaPath
        , bcIhpSchemaPath = ihpSchemaPath
        }

resolveSchemaPath :: FilePath -> IO FilePath
resolveSchemaPath sourceDir = do
    envSchema <- lookupEnv "IHP_TYPED_SQL_SCHEMA"
    case envSchema of
        Just path -> resolveRelativePath sourceDir path >>= ensureFileExists "IHP_TYPED_SQL_SCHEMA"
        Nothing -> do
            findUpwards sourceDir ("Application" </> "Schema.sql") >>= \case
                Just found -> pure found
                Nothing ->
                    fail "typedSql: could not find Application/Schema.sql. Set IHP_TYPED_SQL_SCHEMA to an absolute path."

resolveIhpSchemaPath :: FilePath -> IO (Maybe FilePath)
resolveIhpSchemaPath sourceDir = do
    envSchema <- lookupEnv "IHP_TYPED_SQL_IHP_SCHEMA"
    case envSchema of
        Just path -> Just <$> (resolveRelativePath sourceDir path >>= ensureFileExists "IHP_TYPED_SQL_IHP_SCHEMA")
        Nothing -> do
            envLib <- lookupEnv "IHP_LIB"
            fromLib <- case envLib of
                Just libPath -> do
                    let candidate = libPath </> "IHPSchema.sql"
                    exists <- doesFileExist candidate
                    pure (if exists then Just candidate else Nothing)
                Nothing -> pure Nothing
            case fromLib of
                Just _ -> pure fromLib
                Nothing -> findUpwards sourceDir ("ihp-ide" </> "data" </> "IHPSchema.sql")

resolveRelativePath :: FilePath -> FilePath -> IO FilePath
resolveRelativePath baseDir path = do
    let resolved = if isRelative path then baseDir </> path else path
    canonicalizePath resolved

ensureFileExists :: String -> FilePath -> IO FilePath
ensureFileExists label path = do
    exists <- doesFileExist path
    if exists
        then pure path
        else fail ("typedSql: " <> label <> " points to missing file: " <> path)

findUpwards :: FilePath -> FilePath -> IO (Maybe FilePath)
findUpwards startDir relativePath = go startDir
  where
    go current = do
        let candidate = current </> relativePath
        exists <- doesFileExist candidate
        if exists
            then Just <$> canonicalizePath candidate
            else do
                let parent = takeDirectory current
                if parent == current
                    then pure Nothing
                    else go parent

withBootstrapDatabase :: BootstrapConfig -> (BS.ByteString -> IO a) -> IO a
withBootstrapDatabase BootstrapConfig { bcAppSchemaPath, bcIhpSchemaPath } action = do
    PgTools { pgInitdb, pgPostgres, pgCreatedb, pgPsql } <- resolvePgTools
    withSystemTempDirectory "ihp-typed-sql" \tempDir -> do
        let dataDir = tempDir </> "state"
        let socketDir = "/tmp" </> takeFileName tempDir
        let cleanupSocket = do
                exists <- doesDirectoryExist socketDir
                when exists (removeDirectoryRecursive socketDir)
        bracket_ (createDirectoryIfMissing True socketDir) cleanupSocket do
            Process.callProcess pgInitdb [dataDir, "--no-locale", "--encoding", "UTF8"]

            let params =
                    (Process.proc pgPostgres ["-D", dataDir, "-k", socketDir, "-c", "listen_addresses="])
                        { Process.std_in = Process.CreatePipe
                        , Process.std_out = Process.CreatePipe
                        , Process.std_err = Process.CreatePipe
                        }
            Process.withCreateProcess params \_ _ stderrHandle processHandle -> do
                errHandle <- maybe (fail "typedSql: unable to read postgres logs") pure stderrHandle
                let stop = do
                        Process.terminateProcess processHandle
                        _ <- Process.waitForProcess processHandle
                        pure ()
                let start = do
                        waitUntilReady errHandle
                        Process.callProcess pgCreatedb ["app", "-h", socketDir]
                        let loadSchema file = Process.callProcess pgPsql ["-h", socketDir, "-d", "app", "-v", "ON_ERROR_STOP=1", "-f", file]
                        forM_ (catMaybes [bcIhpSchemaPath, Just bcAppSchemaPath]) loadSchema
                bracket_ start stop do
                    let dbUrl = CS.cs ("postgresql:///app?host=" <> socketDir)
                    action dbUrl

resolvePgTools :: IO PgTools
resolvePgTools = do
    pgPostgres <- requireExecutable "postgres"
    let binDir = takeDirectory pgPostgres
    pgInitdb <- findInBinOrPath binDir "initdb"
    pgCreatedb <- findInBinOrPath binDir "createdb"
    pgPsql <- findInBinOrPath binDir "psql"
    pure PgTools { pgInitdb, pgPostgres, pgCreatedb, pgPsql }

findInBinOrPath :: FilePath -> String -> IO FilePath
findInBinOrPath binDir name = do
    let candidate = binDir </> name
    exists <- doesFileExist candidate
    if exists then pure candidate else requireExecutable name

requireExecutable :: String -> IO FilePath
requireExecutable name =
    findExecutable name >>= \case
        Just path -> pure path
        Nothing -> fail ("typedSql: bootstrap requires '" <> name <> "' in PATH")

waitUntilReady :: Handle -> IO ()
waitUntilReady handle = do
    done <- hIsEOF handle
    if done
        then fail "typedSql: postgres exited before it was ready"
        else do
            line <- BS8.hGetLine handle
            if "database system is ready to accept connections" `BS8.isInfixOf` line
                then pure ()
                else waitUntilReady handle

-- Describe a statement by asking a real Postgres server.
describeStatement :: BS.ByteString -> IO DescribeResult
describeStatement sql = do
    dbUrl <- defaultDatabaseUrl -- read database URL
    describeStatementWith dbUrl sql

describeStatementWith :: BS.ByteString -> BS.ByteString -> IO DescribeResult
describeStatementWith dbUrl sql = do
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
        let attnumInt = fromIntegral attnumCInt :: Int
        let attnum =
                if tableOid == PQ.Oid 0 || attnumInt <= 0
                    then Nothing -- no reliable column info for composites/expressions
                    else Just attnumInt
        pure DescribeColumn { dcName = name, dcType = colType, dcTable = tableOid, dcAttnum = attnum } -- build column meta
        ) [0 .. columnCountInt - 1]

    -- Load metadata for referenced tables and types.
    let tableOids = Set.fromList (map dcTable columns) |> Set.delete (PQ.Oid 0) -- collect referenced table OIDs
        typeOids = Set.fromList paramTypes <> Set.fromList (map dcType columns) -- collect referenced type OIDs

    pgConn <- PG.connectPostgreSQL dbUrl -- open postgres-simple connection for catalog queries
    tables <- loadTableMeta pgConn (Set.toList tableOids) -- load table metadata
    let referencedOids =
            tables
                |> Map.elems
                |> foldl'
                    (\acc TableMeta { tmForeignKeys } ->
                        acc <> Set.fromList (Map.elems tmForeignKeys)
                    )
                    mempty
    let missingRefs = referencedOids `Set.difference` Map.keysSet tables
    extraTables <- loadTableMeta pgConn (Set.toList missingRefs)
    let tables' = tables <> extraTables
    types <- loadTypeInfo pgConn (Set.toList typeOids) -- load type metadata
    PG.close pgConn -- close postgres-simple connection

    _ <- PQ.exec conn ("DEALLOCATE " <> statementName) -- release prepared statement
    PQ.finish conn -- close libpq connection

    pure DescribeResult { drParams = paramTypes, drColumns = columns, drTables = tables', drTypes = types } -- return full metadata

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
            pure (TH.ConT (TH.mkName (CS.cs (tableNameToModelName tableName)))) -- use model type
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
                        Nothing         -> Nothing
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
            pure (TH.ConT (TH.mkName (CS.cs (tableNameToModelName ptiName)))) -- enum type
        _ | ptiType == Just 'c' ->
            pure (TH.ConT (TH.mkName (CS.cs (tableNameToModelName ptiName)))) -- composite type
        _ -> pure (TH.ConT (TH.mkName (CS.cs (tableNameToModelName ptiName)))) -- fallback to generated type
    pure (wrapNull nullable base) -- apply nullability wrapper
