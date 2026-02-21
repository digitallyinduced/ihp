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
import qualified Hasql.Mapping.IsScalar           as Mapping
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
        (Just TableMeta { tmPrimaryKeys, tmForeignKeys, tmColumns }, Just attnum) ->
            let joinNullable = dcTable `Set.member` joinNullableOids
                nullable = joinNullable || maybe True (not . cmNotNull) (Map.lookup attnum tmColumns)
                isIdColumn = attnum `Set.member` tmPrimaryKeys || attnum `Map.member` tmForeignKeys
            in do
                columnTypeOid <- maybe (failText (missingColumnType attnum dcTable)) (pure . cmTypeOid) (Map.lookup attnum tmColumns)
                if isIdColumn
                    then decodeIdColumn typeInfo nullable columnTypeOid
                    else decodeColumnByOid typeInfo nullable columnTypeOid
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
                "int2" -> decodeIntLikeArray nullable (TH.VarE 'HasqlDecoders.int4)
                "int4" -> decodeIntLikeArray nullable (TH.VarE 'HasqlDecoders.int4)
                "int8" -> decodeIntLikeArray nullable (TH.VarE 'HasqlDecoders.int8)
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
                "bytea" -> decodeSimpleArray nullable (TH.VarE 'HasqlDecoders.bytea)
                "timestamptz" -> decodeSimpleArray nullable (TH.VarE 'HasqlDecoders.timestamptz)
                "timestamp" -> decodeSimpleArray nullable (TH.VarE 'HasqlDecoders.timestamp)
                "date" -> decodeSimpleArray nullable (TH.VarE 'HasqlDecoders.date)
                "time" -> decodeSimpleArray nullable (TH.VarE 'HasqlDecoders.time)
                _ -> decodeMappingArray nullable

decodeScalarColumn :: Bool -> Text -> TH.ExpQ
decodeScalarColumn nullable typeName =
    case typeName of
        "int2" -> decodeIntLikeScalar nullable (TH.VarE 'HasqlDecoders.int4)
        "int4" -> decodeIntLikeScalar nullable (TH.VarE 'HasqlDecoders.int4)
        "int8" -> decodeIntLikeScalar nullable (TH.VarE 'HasqlDecoders.int8)
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
        "bytea" -> decodeSimpleScalar nullable (TH.VarE 'HasqlDecoders.bytea)
        "point" -> decodeMappingScalar nullable
        "polygon" -> decodeMappingScalar nullable
        "inet" -> decodeMappingScalar nullable
        "tsvector" -> decodeMappingScalar nullable
        "interval" -> decodeMappingScalar nullable
        _ -> decodeMappingScalar nullable

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

decodeIntLikeScalar :: Bool -> TH.Exp -> TH.ExpQ
decodeIntLikeScalar nullable baseDecoder =
    if nullable
        then pure
            ( TH.AppE
                (TH.AppE (TH.VarE 'fmap) (TH.AppE (TH.VarE 'fmap) (TH.VarE 'fromIntegral)))
                (TH.AppE (TH.VarE 'HasqlDecoders.column) (nullabilityWrapper True baseDecoder))
            )
        else pure
            ( TH.AppE
                (TH.AppE (TH.VarE 'fmap) (TH.VarE 'fromIntegral))
                (TH.AppE (TH.VarE 'HasqlDecoders.column) (nullabilityWrapper False baseDecoder))
            )

decodeIntLikeArray :: Bool -> TH.Exp -> TH.ExpQ
decodeIntLikeArray nullable baseDecoder =
    if nullable
        then pure
            ( TH.AppE
                (TH.AppE (TH.VarE 'fmap) (TH.AppE (TH.VarE 'fmap) (TH.AppE (TH.VarE 'map) (TH.VarE 'fromIntegral))))
                ( TH.AppE
                    (TH.VarE 'HasqlDecoders.column)
                    ( nullabilityWrapper True
                        ( TH.AppE
                            (TH.VarE 'HasqlDecoders.listArray)
                            (TH.AppE (TH.VarE 'HasqlDecoders.nonNullable) baseDecoder)
                        )
                    )
                )
            )
        else pure
            ( TH.AppE
                (TH.AppE (TH.VarE 'fmap) (TH.AppE (TH.VarE 'map) (TH.VarE 'fromIntegral)))
                ( TH.AppE
                    (TH.VarE 'HasqlDecoders.column)
                    ( nullabilityWrapper False
                        ( TH.AppE
                            (TH.VarE 'HasqlDecoders.listArray)
                            (TH.AppE (TH.VarE 'HasqlDecoders.nonNullable) baseDecoder)
                        )
                    )
                )
            )

decodeMappingScalar :: Bool -> TH.ExpQ
decodeMappingScalar nullable =
    decodeSimpleScalar nullable (TH.VarE 'Mapping.decoder)

decodeMappingArray :: Bool -> TH.ExpQ
decodeMappingArray nullable =
    decodeSimpleArray nullable (TH.VarE 'Mapping.decoder)

nullabilityWrapper :: Bool -> TH.Exp -> TH.Exp
nullabilityWrapper nullable valueDecoder =
    TH.AppE
        (TH.VarE (if nullable then 'HasqlDecoders.nullable else 'HasqlDecoders.nonNullable))
        valueDecoder

failText :: Text -> TH.Q a
failText = fail . CS.cs
