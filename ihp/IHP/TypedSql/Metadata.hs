{-# LANGUAGE NamedFieldPuns #-}

module IHP.TypedSql.Metadata
    ( DescribeResult (..)
    , DescribeColumn (..)
    , ColumnMeta (..)
    , TableMeta (..)
    , PgTypeInfo (..)
    , toOidInt32
    , fromOidInt32
    , describeStatement
    , describeStatementWith
    ) where

import           Control.Exception             (bracket)
import           Control.Monad                 (guard)
import           Data.Int                      (Int32)
import qualified Data.ByteString               as BS
import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (catMaybes, mapMaybe)
import qualified Data.Set                      as Set
import qualified Data.String.Conversions       as CS
import qualified Data.Text                     as Text
import qualified Database.PostgreSQL.LibPQ     as PQ
import qualified Hasql.Connection              as HasqlConnection
import qualified Hasql.Connection.Settings     as HasqlSettings
import qualified Hasql.Decoders                as HasqlDecoders
import qualified Hasql.Encoders                as HasqlEncoders
import qualified Hasql.Session                 as HasqlSession
import qualified Hasql.Statement               as HasqlStatement
import           IHP.FrameworkConfig           (defaultDatabaseUrl)
import           IHP.Prelude

-- | Result of describing a statement.
-- High-level: this is the central metadata bundle for typedSql inference.
data DescribeResult = DescribeResult
    { drParams  :: ![PQ.Oid]
    , drColumns :: ![DescribeColumn]
    , drTables  :: !(Map.Map PQ.Oid TableMeta)
    , drTypes   :: !(Map.Map PQ.Oid PgTypeInfo)
    }

-- | Metadata for a column in the result set.
-- This drives result type inference and row parser selection.
data DescribeColumn = DescribeColumn
    { dcName   :: !BS.ByteString
    , dcType   :: !PQ.Oid
    , dcTable  :: !PQ.Oid
    , dcAttnum :: !(Maybe Int)
    }

-- | Column details extracted from pg_attribute.
-- These are used to map columns to IHP Id' and nullable types.
data ColumnMeta = ColumnMeta
    { cmAttnum  :: !Int
    , cmName    :: !Text
    , cmTypeOid :: !PQ.Oid
    , cmNotNull :: !Bool
    }

-- | Table metadata, including columns and key relationships.
-- This is used for table.* detection and key-aware typing.
data TableMeta = TableMeta
    { tmOid         :: !PQ.Oid
    , tmName        :: !Text
    , tmColumns     :: !(Map.Map Int ColumnMeta)
    , tmColumnOrder :: ![Int]
    , tmPrimaryKeys :: !(Set.Set Int)
    , tmForeignKeys :: !(Map.Map Int PQ.Oid)
    }

-- | Postgres type metadata needed for Haskell mapping.
-- Used to convert OIDs to concrete Haskell types.
data PgTypeInfo = PgTypeInfo
    { ptiOid       :: !PQ.Oid
    , ptiName      :: !Text
    , ptiElem      :: !(Maybe PQ.Oid)
    , ptiType      :: !(Maybe Char)
    , ptiNamespace :: !(Maybe Text)
    }

-- | Convert libpq Oid to Int32 for Hasql parameter encoding.
toOidInt32 :: PQ.Oid -> Int32
toOidInt32 (PQ.Oid oid) = fromIntegral oid

-- | Convert Hasql-decoded Oid value back to libpq Oid.
fromOidInt32 :: Int32 -> PQ.Oid
fromOidInt32 oid = PQ.Oid (fromIntegral oid)

-- | Describe a statement by asking a real Postgres server.
-- typedSql uses this when not running in bootstrap mode.
describeStatement :: BS.ByteString -> IO DescribeResult
describeStatement sql = do
    dbUrl <- defaultDatabaseUrl
    describeStatementWith dbUrl sql

-- | Describe a statement using an explicit database URL.
-- This is the core path for metadata lookup in typedSql.
describeStatementWith :: BS.ByteString -> BS.ByteString -> IO DescribeResult
describeStatementWith dbUrl sql = do
    conn <- PQ.connectdb dbUrl
    status <- PQ.status conn
    unless (status == PQ.ConnectionOk) do
        err <- PQ.errorMessage conn
        fail ("typedSql: could not connect to database: " <> CS.cs (fromMaybe "" err))

    let statementName = "ihp_typed_sql_stmt"
    _ <- ensureOk "prepare" =<< PQ.prepare conn statementName sql Nothing
    desc <- ensureOk "describe" =<< PQ.describePrepared conn statementName

    paramCount <- PQ.nparams desc
    paramTypes <- mapM (PQ.paramtype desc) [0 .. paramCount - 1]

    columnCount <- PQ.nfields desc
    let PQ.Col columnCountCInt = columnCount
    let columnCountInt = fromIntegral columnCountCInt :: Int
    columns <- mapM (\i -> do
        let colIndex = PQ.Col (fromIntegral i)
        name <- fromMaybe "" <$> PQ.fname desc colIndex
        colType <- PQ.ftype desc colIndex
        tableOid <- PQ.ftable desc colIndex
        attnumRaw <- PQ.ftablecol desc colIndex
        let PQ.Col attnumCInt = attnumRaw
        let attnumInt = fromIntegral attnumCInt :: Int
        let attnum =
                if tableOid == PQ.Oid 0 || attnumInt <= 0
                    then Nothing
                    else Just attnumInt
        pure DescribeColumn { dcName = name, dcType = colType, dcTable = tableOid, dcAttnum = attnum }
        ) [0 .. columnCountInt - 1]

    let tableOids = Set.fromList (map dcTable columns) |> Set.delete (PQ.Oid 0)
        typeOids = Set.fromList paramTypes <> Set.fromList (map dcType columns)

    tables <- loadTableMeta dbUrl (Set.toList tableOids)
    let referencedOids =
            tables
                |> Map.elems
                |> foldl'
                    (\acc TableMeta { tmForeignKeys } ->
                        acc <> Set.fromList (Map.elems tmForeignKeys)
                    )
                    mempty
    let missingRefs = referencedOids `Set.difference` Map.keysSet tables
    extraTables <- loadTableMeta dbUrl (Set.toList missingRefs)
    let tables' = tables <> extraTables
    types <- loadTypeInfo dbUrl (Set.toList typeOids)

    _ <- PQ.exec conn ("DEALLOCATE " <> statementName)
    PQ.finish conn

    pure DescribeResult { drParams = paramTypes, drColumns = columns, drTables = tables', drTypes = types }

-- | Ensure libpq returned a successful result.
-- Errors here surface as typedSql compile-time failures.
ensureOk :: String -> Maybe PQ.Result -> IO PQ.Result
ensureOk actionName = \case
    Nothing -> fail ("typedSql: " <> actionName <> " returned no result")
    Just res -> do
        status <- PQ.resultStatus res
        case status of
            PQ.CommandOk -> pure res
            PQ.TuplesOk -> pure res
            _ -> do
                msg <- PQ.resultErrorMessage res
                fail ("typedSql: " <> actionName <> " failed: " <> CS.cs (fromMaybe "" msg))

-- | Run a Hasql session for metadata queries (pg_catalog).
-- This keeps metadata lookups on the same hasql stack as typedSql execution.
runHasqlMetadataSession :: BS.ByteString -> HasqlSession.Session a -> IO a
runHasqlMetadataSession dbUrl session = do
    let settings = HasqlSettings.connectionString (CS.cs dbUrl)
    hasqlConnection <-
        HasqlConnection.acquire settings >>= \case
            Left connectionError ->
                fail (CS.cs ("typedSql: could not connect to database: " <> tshow connectionError))
            Right connection ->
                pure connection
    bracket (pure hasqlConnection) HasqlConnection.release \connection ->
        HasqlConnection.use connection session >>= \case
            Left sessionError ->
                fail (CS.cs ("typedSql: metadata query failed: " <> tshow sessionError))
            Right result ->
                pure result

-- | Encoder for passing OID arrays into pg_catalog queries.
oidArrayParamsEncoder :: HasqlEncoders.Params [Int32]
oidArrayParamsEncoder =
    HasqlEncoders.param
        (HasqlEncoders.nonNullable
            (HasqlEncoders.foldableArray
                (HasqlEncoders.nonNullable HasqlEncoders.oid)
            )
        )

-- | Query to load column metadata for a set of table OIDs.
tableColumnsStatement :: HasqlStatement.Statement [Int32] [(Int32, Text, Int32, Text, Int32, Bool)]
tableColumnsStatement =
    HasqlStatement.preparable
        (mconcat
            [ "SELECT c.oid::int4, c.relname::text, a.attnum::int4, a.attname::text, a.atttypid::int4, a.attnotnull "
            , "FROM pg_class c "
            , "JOIN pg_namespace ns ON ns.oid = c.relnamespace "
            , "JOIN pg_attribute a ON a.attrelid = c.oid "
            , "WHERE c.oid = ANY($1) AND a.attnum > 0 AND NOT a.attisdropped "
            , "ORDER BY c.oid, a.attnum"
            ])
        oidArrayParamsEncoder
        (HasqlDecoders.rowList
            ((,,,,,)
                <$> HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.int4)
                <*> HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.text)
                <*> HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.int4)
                <*> HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.text)
                <*> HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.int4)
                <*> HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.bool)
            ))

-- | Query to load primary key columns for a set of tables.
primaryKeysStatement :: HasqlStatement.Statement [Int32] [(Int32, Int32)]
primaryKeysStatement =
    HasqlStatement.preparable
        "SELECT conrelid::int4, unnest(conkey)::int4 as attnum FROM pg_constraint WHERE contype = 'p' AND conrelid = ANY($1)"
        oidArrayParamsEncoder
        (HasqlDecoders.rowList
            ((,)
                <$> HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.int4)
                <*> HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.int4)
            ))

-- | Query to load single-column foreign keys for a set of tables.
foreignKeysStatement :: HasqlStatement.Statement [Int32] [(Int32, Int32, Int32)]
foreignKeysStatement =
    HasqlStatement.preparable
        (mconcat
            [ "SELECT conrelid::int4, conkey[1]::int4 as attnum, confrelid::int4 "
            , "FROM pg_constraint "
            , "WHERE contype = 'f' AND array_length(conkey,1) = 1 AND conrelid = ANY($1)"
            ])
        oidArrayParamsEncoder
        (HasqlDecoders.rowList
            ((,,)
                <$> HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.int4)
                <*> HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.int4)
                <*> HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.int4)
            ))

-- | Query to load type information for a set of type OIDs.
typeInfoStatement :: HasqlStatement.Statement [Int32] [(Int32, Text, Int32, Maybe Text, Maybe Text)]
typeInfoStatement =
    HasqlStatement.preparable
        (mconcat
            [ "SELECT oid::int4, typname::text, typelem::int4, typtype::text, typnamespace::regnamespace::text "
            , "FROM pg_type "
            , "WHERE oid = ANY($1)"
            ])
        oidArrayParamsEncoder
        (HasqlDecoders.rowList
            ((,,,,)
                <$> HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.int4)
                <*> HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.text)
                <*> HasqlDecoders.column (HasqlDecoders.nonNullable HasqlDecoders.int4)
                <*> HasqlDecoders.column (HasqlDecoders.nullable HasqlDecoders.text)
                <*> HasqlDecoders.column (HasqlDecoders.nullable HasqlDecoders.text)
            ))

-- | Load table metadata for all referenced tables.
-- High-level: read pg_catalog to map table/column info for typedSql inference.
loadTableMeta :: BS.ByteString -> [PQ.Oid] -> IO (Map.Map PQ.Oid TableMeta)
loadTableMeta _ [] = pure mempty
loadTableMeta dbUrl tableOids = do
    let tableOidParams = map toOidInt32 tableOids
    rows <- runHasqlMetadataSession dbUrl (HasqlSession.statement tableOidParams tableColumnsStatement)
    primaryKeys <- runHasqlMetadataSession dbUrl (HasqlSession.statement tableOidParams primaryKeysStatement)
    foreignKeys <- runHasqlMetadataSession dbUrl (HasqlSession.statement tableOidParams foreignKeysStatement)

    let pkMap = primaryKeys
            |> foldl' (\acc (relid, attnum) ->
                    Map.insertWith Set.union (fromOidInt32 relid) (Set.singleton (fromIntegral attnum)) acc
                ) mempty

        fkMap = foreignKeys
            |> foldl' (\acc (relid, attnum, ref) ->
                    Map.insertWith Map.union (fromOidInt32 relid) (Map.singleton (fromIntegral attnum) (fromOidInt32 ref)) acc
                ) mempty

        tableGroups =
            rows
                |> map (\(relid, name, attnum, attname, atttypid, attnotnull) ->
                        ( fromOidInt32 relid
                        , ColumnMeta
                            { cmAttnum = fromIntegral attnum
                            , cmName = attname
                            , cmTypeOid = fromOidInt32 atttypid
                            , cmNotNull = attnotnull
                            }
                        , name
                        )
                    )
                |> List.groupBy (\(l, _, _) (r, _, _) -> l == r)

    pure $ tableGroups
        |> foldl'
            (\acc group ->
                case group of
                    [] -> acc
                    (tableOid, _, tableName):_ ->
                        let cols = group
                                |> map (\(_, column, _) -> (cmAttnum column, column))
                                |> Map.fromList
                            order = group |> map (\(_, column, _) -> cmAttnum column)
                            pks = Map.findWithDefault mempty tableOid pkMap
                            fks = Map.findWithDefault mempty tableOid fkMap
                            meta = TableMeta
                                { tmOid = tableOid
                                , tmName = tableName
                                , tmColumns = cols
                                , tmColumnOrder = order
                                , tmPrimaryKeys = pks
                                , tmForeignKeys = fks
                                }
                        in Map.insert tableOid meta acc
            )
            mempty

-- | Load type information for the given OIDs.
-- High-level: fetch pg_type metadata recursively for arrays.
loadTypeInfo :: BS.ByteString -> [PQ.Oid] -> IO (Map.Map PQ.Oid PgTypeInfo)
loadTypeInfo _ [] = pure mempty
loadTypeInfo dbUrl typeOids = do
    let requested = Set.fromList typeOids
    rows <- runHasqlMetadataSession dbUrl (HasqlSession.statement (map toOidInt32 typeOids) typeInfoStatement)
    let (typeMap, missing) =
            rows
                |> foldl'
                    (\(acc, missingAcc) (oid, name, elemOid, typtype, nsp) ->
                        let thisOid = fromOidInt32 oid
                            elemOid' = if elemOid == 0 then Nothing else Just (fromOidInt32 elemOid)
                            nextMissing = case elemOid' of
                                Just o | o `Set.notMember` requested -> o : missingAcc
                                _ -> missingAcc
                        in ( Map.insert thisOid PgTypeInfo
                                { ptiOid = thisOid
                                , ptiName = name
                                , ptiElem = elemOid'
                                , ptiType = typtype >>= (listToMaybe . CS.cs)
                                , ptiNamespace = nsp
                                }
                                acc
                           , nextMissing
                           )
                    )
                    (mempty, [])
    extras <- loadTypeInfo dbUrl missing
    pure (typeMap <> extras)
