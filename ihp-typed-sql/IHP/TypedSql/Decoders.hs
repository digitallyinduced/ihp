{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module IHP.TypedSql.Decoders
    ( resultDecoderForColumns
    ) where

import qualified Data.Map.Strict                  as Map
import qualified Data.Set                         as Set
import qualified Data.String.Conversions          as CS
import qualified Database.PostgreSQL.LibPQ        as PQ
import qualified Hasql.Decoders                   as HasqlDecoders
import qualified Language.Haskell.TH              as TH
import           IHP.Hasql.FromRow                as HasqlFromRow
import           IHP.ModelSupport.Types           (Id' (..))
import           IHP.Prelude

import           IHP.TypedSql.Metadata            (ColumnMeta (..), DescribeColumn (..), PgTypeInfo (..), TableMeta (..))
import           IHP.TypedSql.TypeMapping        (detectFullTable)

-- | Build a hasql result decoder for the described SQL columns.
-- For full-table selections we reuse FromRowHasql; otherwise we decode a scalar/tuple.
resultDecoderForColumns :: Map.Map PQ.Oid PgTypeInfo -> Map.Map PQ.Oid TableMeta -> Set.Set PQ.Oid -> [DescribeColumn] -> TH.ExpQ
resultDecoderForColumns typeInfo tables joinNullableOids columns = do
    case detectFullTable tables columns of
        Just _ ->
            pure (TH.VarE 'HasqlFromRow.hasqlRowDecoder)
        Nothing -> do
            rowDecoder <- case columns of
                [] -> pure (TH.AppE (TH.VarE 'pure) (TH.ConE '()))
                [column] -> rowDecoderForColumn typeInfo tables joinNullableOids column
                _ -> tupleRowDecoderForColumns typeInfo tables joinNullableOids columns
            pure rowDecoder

tupleRowDecoderForColumns :: Map.Map PQ.Oid PgTypeInfo -> Map.Map PQ.Oid TableMeta -> Set.Set PQ.Oid -> [DescribeColumn] -> TH.ExpQ
tupleRowDecoderForColumns typeInfo tables joinNullableOids columns = do
    columnDecoders <- mapM (rowDecoderForColumn typeInfo tables joinNullableOids) columns
    case columnDecoders of
        [] -> pure (TH.AppE (TH.VarE 'pure) (TH.ConE '()))
        firstDecoder:restDecoders -> do
            let tupleConstructor = TH.ConE (TH.tupleDataName (length columnDecoders))
            let withFirst = TH.AppE (TH.AppE (TH.VarE '(<$>)) tupleConstructor) firstDecoder
            pure (foldl (\acc decoder -> TH.AppE (TH.AppE (TH.VarE '(<*>)) acc) decoder) withFirst restDecoders)

rowDecoderForColumn :: Map.Map PQ.Oid PgTypeInfo -> Map.Map PQ.Oid TableMeta -> Set.Set PQ.Oid -> DescribeColumn -> TH.ExpQ
rowDecoderForColumn typeInfo tables joinNullableOids DescribeColumn { dcType, dcTable, dcAttnum } =
    case (Map.lookup dcTable tables, dcAttnum) of
        (Just TableMeta { tmPrimaryKeys, tmForeignKeys, tmColumns }, Just attnum)
            | attnum `Set.member` tmPrimaryKeys -> do
                let joinNullable = dcTable `Set.member` joinNullableOids
                let nullable = joinNullable || maybe True (not . cmNotNull) (Map.lookup attnum tmColumns)
                columnTypeOid <- maybe (failText (missingColumnType attnum dcTable)) (pure . cmTypeOid) (Map.lookup attnum tmColumns)
                decodeIdColumn typeInfo nullable columnTypeOid
            | attnum `Map.member` tmForeignKeys -> do
                let joinNullable = dcTable `Set.member` joinNullableOids
                let nullable = joinNullable || maybe True (not . cmNotNull) (Map.lookup attnum tmColumns)
                columnTypeOid <- maybe (failText (missingColumnType attnum dcTable)) (pure . cmTypeOid) (Map.lookup attnum tmColumns)
                decodeIdColumn typeInfo nullable columnTypeOid
            | otherwise -> do
                let joinNullable = dcTable `Set.member` joinNullableOids
                let nullable = joinNullable || maybe True (not . cmNotNull) (Map.lookup attnum tmColumns)
                columnTypeOid <- maybe (failText (missingColumnType attnum dcTable)) (pure . cmTypeOid) (Map.lookup attnum tmColumns)
                decodeColumnByOid typeInfo nullable columnTypeOid
        _ ->
            decodeColumnByOid typeInfo True dcType
  where
    missingColumnType attnum tableOid =
        "typedSql: missing column metadata for attnum " <> show attnum <> " on table oid " <> show tableOid

decodeIdColumn :: Map.Map PQ.Oid PgTypeInfo -> Bool -> PQ.Oid -> TH.ExpQ
decodeIdColumn typeInfo nullable oid = do
    baseDecoder <- decodeColumnByOid typeInfo nullable oid
    if nullable
        then pure (TH.AppE (TH.AppE (TH.VarE 'fmap) (TH.AppE (TH.VarE 'fmap) (TH.ConE 'Id))) baseDecoder)
        else pure (TH.AppE (TH.AppE (TH.VarE 'fmap) (TH.ConE 'Id)) baseDecoder)

decodeColumnByOid :: Map.Map PQ.Oid PgTypeInfo -> Bool -> PQ.Oid -> TH.ExpQ
decodeColumnByOid typeInfo nullable oid =
    case Map.lookup oid typeInfo of
        Nothing -> failText ("typedSql: missing type information for column oid " <> show oid)
        Just pgTypeInfo -> decodeColumnByTypeInfo typeInfo nullable pgTypeInfo

decodeColumnByTypeInfo :: Map.Map PQ.Oid PgTypeInfo -> Bool -> PgTypeInfo -> TH.ExpQ
decodeColumnByTypeInfo typeInfo nullable PgTypeInfo { ptiName, ptiElem } =
    case ptiElem of
        Just elementOid -> decodeArrayColumn typeInfo nullable elementOid
        Nothing -> decodeScalarColumn nullable ptiName

decodeArrayColumn :: Map.Map PQ.Oid PgTypeInfo -> Bool -> PQ.Oid -> TH.ExpQ
decodeArrayColumn typeInfo nullable elementOid =
    case Map.lookup elementOid typeInfo of
        Nothing -> failText ("typedSql: missing array element type for oid " <> show elementOid)
        Just elementType ->
            case ptiName elementType of
                "int2" -> decodeIntArray nullable
                "int4" -> decodeIntArray nullable
                "int8" -> decodeIntegerArray nullable
                "text" -> decodeSimpleArray nullable (TH.VarE 'HasqlDecoders.text)
                "varchar" -> decodeSimpleArray nullable (TH.VarE 'HasqlDecoders.text)
                "bpchar" -> decodeSimpleArray nullable (TH.VarE 'HasqlDecoders.text)
                "citext" -> decodeSimpleArray nullable (TH.VarE 'HasqlDecoders.text)
                "bool" -> decodeSimpleArray nullable (TH.VarE 'HasqlDecoders.bool)
                "uuid" -> decodeSimpleArray nullable (TH.VarE 'HasqlDecoders.uuid)
                "float4" -> decodeSimpleArray nullable (TH.VarE 'HasqlDecoders.float4)
                "float8" -> decodeSimpleArray nullable (TH.VarE 'HasqlDecoders.float8)
                "numeric" -> decodeSimpleArray nullable (TH.VarE 'HasqlDecoders.numeric)
                "json" -> decodeSimpleArray nullable (TH.VarE 'HasqlDecoders.json)
                "jsonb" -> decodeSimpleArray nullable (TH.VarE 'HasqlDecoders.jsonb)
                "bytea" -> decodeByteaArray nullable
                unsupported ->
                    failText ("typedSql: unsupported array element type for hasql decoder: " <> unsupported)

decodeScalarColumn :: Bool -> Text -> TH.ExpQ
decodeScalarColumn nullable typeName =
    case typeName of
        "int2" -> decodeIntScalar nullable
        "int4" -> decodeIntScalar nullable
        "int8" -> decodeIntegerScalar nullable
        "text" -> decodeSimpleScalar nullable (TH.VarE 'HasqlDecoders.text)
        "varchar" -> decodeSimpleScalar nullable (TH.VarE 'HasqlDecoders.text)
        "bpchar" -> decodeSimpleScalar nullable (TH.VarE 'HasqlDecoders.text)
        "citext" -> decodeSimpleScalar nullable (TH.VarE 'HasqlDecoders.text)
        "bool" -> decodeSimpleScalar nullable (TH.VarE 'HasqlDecoders.bool)
        "uuid" -> decodeSimpleScalar nullable (TH.VarE 'HasqlDecoders.uuid)
        "timestamptz" -> decodeSimpleScalar nullable (TH.VarE 'HasqlDecoders.timestamptz)
        "timestamp" -> decodeSimpleScalar nullable (TH.VarE 'HasqlDecoders.timestamp)
        "date" -> decodeSimpleScalar nullable (TH.VarE 'HasqlDecoders.date)
        "time" -> decodeSimpleScalar nullable (TH.VarE 'HasqlDecoders.time)
        "json" -> decodeSimpleScalar nullable (TH.VarE 'HasqlDecoders.json)
        "jsonb" -> decodeSimpleScalar nullable (TH.VarE 'HasqlDecoders.jsonb)
        "float4" -> decodeSimpleScalar nullable (TH.VarE 'HasqlDecoders.float4)
        "float8" -> decodeSimpleScalar nullable (TH.VarE 'HasqlDecoders.float8)
        "numeric" -> decodeSimpleScalar nullable (TH.VarE 'HasqlDecoders.numeric)
        "bytea" -> decodeByteaScalar nullable
        unsupported ->
            failText ("typedSql: unsupported column type for hasql decoder: " <> unsupported)

decodeSimpleScalar :: Bool -> TH.Exp -> TH.ExpQ
decodeSimpleScalar nullable valueDecoder =
    pure (TH.AppE (TH.VarE 'HasqlDecoders.column) (nullabilityWrapper nullable valueDecoder))

decodeSimpleArray :: Bool -> TH.Exp -> TH.ExpQ
decodeSimpleArray nullable valueDecoder =
    pure
        ( TH.AppE
            (TH.VarE 'HasqlDecoders.column)
            ( nullabilityWrapper
                nullable
                ( TH.AppE
                    (TH.VarE 'HasqlDecoders.listArray)
                    (TH.AppE (TH.VarE 'HasqlDecoders.nonNullable) valueDecoder)
                )
            )
        )

decodeIntScalar :: Bool -> TH.ExpQ
decodeIntScalar nullable =
    if nullable
        then pure
            ( TH.AppE
                (TH.AppE (TH.VarE 'fmap) (TH.AppE (TH.VarE 'fmap) (TH.VarE 'fromIntegral)))
                (TH.AppE (TH.VarE 'HasqlDecoders.column) (nullabilityWrapper True (TH.VarE 'HasqlDecoders.int4)))
            )
        else pure
            ( TH.AppE
                (TH.AppE (TH.VarE 'fmap) (TH.VarE 'fromIntegral))
                (TH.AppE (TH.VarE 'HasqlDecoders.column) (nullabilityWrapper False (TH.VarE 'HasqlDecoders.int4)))
            )

decodeIntegerScalar :: Bool -> TH.ExpQ
decodeIntegerScalar nullable =
    if nullable
        then pure
            ( TH.AppE
                (TH.AppE (TH.VarE 'fmap) (TH.AppE (TH.VarE 'fmap) (TH.VarE 'fromIntegral)))
                (TH.AppE (TH.VarE 'HasqlDecoders.column) (nullabilityWrapper True (TH.VarE 'HasqlDecoders.int8)))
            )
        else pure
            ( TH.AppE
                (TH.AppE (TH.VarE 'fmap) (TH.VarE 'fromIntegral))
                (TH.AppE (TH.VarE 'HasqlDecoders.column) (nullabilityWrapper False (TH.VarE 'HasqlDecoders.int8)))
            )

decodeIntArray :: Bool -> TH.ExpQ
decodeIntArray nullable =
    if nullable
        then pure
            ( TH.AppE
                (TH.AppE (TH.VarE 'fmap) (TH.AppE (TH.VarE 'fmap) (TH.AppE (TH.VarE 'map) (TH.VarE 'fromIntegral))))
                ( TH.AppE
                    (TH.VarE 'HasqlDecoders.column)
                    ( nullabilityWrapper
                        True
                        ( TH.AppE
                            (TH.VarE 'HasqlDecoders.listArray)
                            (TH.AppE (TH.VarE 'HasqlDecoders.nonNullable) (TH.VarE 'HasqlDecoders.int4))
                        )
                    )
                )
            )
        else pure
            ( TH.AppE
                (TH.AppE (TH.VarE 'fmap) (TH.AppE (TH.VarE 'map) (TH.VarE 'fromIntegral)))
                ( TH.AppE
                    (TH.VarE 'HasqlDecoders.column)
                    ( nullabilityWrapper
                        False
                        ( TH.AppE
                            (TH.VarE 'HasqlDecoders.listArray)
                            (TH.AppE (TH.VarE 'HasqlDecoders.nonNullable) (TH.VarE 'HasqlDecoders.int4))
                        )
                    )
                )
            )

decodeIntegerArray :: Bool -> TH.ExpQ
decodeIntegerArray nullable =
    if nullable
        then pure
            ( TH.AppE
                (TH.AppE (TH.VarE 'fmap) (TH.AppE (TH.VarE 'fmap) (TH.AppE (TH.VarE 'map) (TH.VarE 'fromIntegral))))
                ( TH.AppE
                    (TH.VarE 'HasqlDecoders.column)
                    ( nullabilityWrapper
                        True
                        ( TH.AppE
                            (TH.VarE 'HasqlDecoders.listArray)
                            (TH.AppE (TH.VarE 'HasqlDecoders.nonNullable) (TH.VarE 'HasqlDecoders.int8))
                        )
                    )
                )
            )
        else pure
            ( TH.AppE
                (TH.AppE (TH.VarE 'fmap) (TH.AppE (TH.VarE 'map) (TH.VarE 'fromIntegral)))
                ( TH.AppE
                    (TH.VarE 'HasqlDecoders.column)
                    ( nullabilityWrapper
                        False
                        ( TH.AppE
                            (TH.VarE 'HasqlDecoders.listArray)
                            (TH.AppE (TH.VarE 'HasqlDecoders.nonNullable) (TH.VarE 'HasqlDecoders.int8))
                        )
                    )
                )
            )

decodeByteaScalar :: Bool -> TH.ExpQ
decodeByteaScalar nullable =
    decodeSimpleScalar nullable (TH.VarE 'HasqlDecoders.bytea)

decodeByteaArray :: Bool -> TH.ExpQ
decodeByteaArray nullable =
    decodeSimpleArray nullable (TH.VarE 'HasqlDecoders.bytea)

nullabilityWrapper :: Bool -> TH.Exp -> TH.Exp
nullabilityWrapper nullable valueDecoder =
    TH.AppE
        (TH.VarE (if nullable then 'HasqlDecoders.nullable else 'HasqlDecoders.nonNullable))
        valueDecoder

failText :: Text -> TH.Q a
failText = fail . CS.cs
