{-# LANGUAGE NamedFieldPuns #-}

module IHP.TypedSql.TypeMapping
    ( hsTypeForParam
    , hsTypeForColumns
    , hsTypeForColumn
    , detectFullTable
    ) where

import           Control.Monad            (guard)
import qualified Data.Aeson               as Aeson
import qualified Data.ByteString          as BS
import qualified Data.List                as List
import qualified Data.Map.Strict          as Map
import           Data.Scientific          (Scientific)
import qualified Data.Set                 as Set
import qualified Data.String.Conversions  as CS
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Language.Haskell.TH      as TH
import           IHP.Prelude
import           PostgresqlTypes.Point    (Point)
import           PostgresqlTypes.Polygon  (Polygon)
import           PostgresqlTypes.Inet     (Inet)
import           PostgresqlTypes.Tsvector (Tsvector)
import           PostgresqlTypes.Interval (Interval)

import           IHP.TypedSql.Metadata    (ColumnMeta (..), DescribeColumn (..), PgTypeInfo (..), TableMeta (..))

-- | Build the Haskell type for a parameter, based on its OID.
-- High-level: map a PG type OID into a TH Type.
hsTypeForParam :: Map.Map PQ.Oid PgTypeInfo -> PQ.Oid -> TH.TypeQ
hsTypeForParam typeInfo oid = maybe (fail (CS.cs unknown)) (hsTypeForPg typeInfo False) (Map.lookup oid typeInfo)
  where
    unknown = "typedSql: missing type information for parameter oid " <> show oid

-- | Build the result type for the described columns.
-- High-level: pick a model type for table.* or a tuple type for ad-hoc select lists.
hsTypeForColumns :: Map.Map PQ.Oid PgTypeInfo -> Map.Map PQ.Oid TableMeta -> Set.Set PQ.Oid -> [DescribeColumn] -> TH.TypeQ
hsTypeForColumns typeInfo tables joinNullableOids cols = do
    case detectFullTable tables cols of
        Just tableName ->
            pure (TH.ConT (TH.mkName (CS.cs (tableNameToModelName tableName))))
        Nothing -> do
            hsCols <- mapM (hsTypeForColumn typeInfo tables joinNullableOids) cols
            case hsCols of
                [single] -> pure single
                _ -> pure $ foldl TH.AppT (TH.TupleT (length hsCols)) hsCols

-- | Detect whether the columns represent a full table selection (table.* with all columns in order).
-- High-level: if yes, we can return the model type directly.
detectFullTable :: Map.Map PQ.Oid TableMeta -> [DescribeColumn] -> Maybe Text
detectFullTable tables cols = do
    guard (not (null cols))
    let grouped =
            cols
                |> List.groupBy (\a b -> dcTable a == dcTable b)
                |> mapMaybe (\group -> case List.uncons group of
                        Just (first, _) -> Just (dcTable first, group)
                        Nothing         -> Nothing
                   )
    case grouped of
        [(tableOid, colGroup)] | tableOid /= PQ.Oid 0 -> do
            TableMeta { tmColumnOrder } <- Map.lookup tableOid tables
            let attnums = mapMaybe dcAttnum colGroup
            guard (attnums == tmColumnOrder)
            TableMeta { tmName } <- Map.lookup tableOid tables
            pure tmName
        _ -> Nothing

-- | Map a single column into a Haskell type, with key-aware rules.
hsTypeForColumn :: Map.Map PQ.Oid PgTypeInfo -> Map.Map PQ.Oid TableMeta -> Set.Set PQ.Oid -> DescribeColumn -> TH.TypeQ
hsTypeForColumn typeInfo tables joinNullableOids DescribeColumn { dcType, dcTable, dcAttnum } =
    case (Map.lookup dcTable tables, dcAttnum) of
        (Just TableMeta { tmName = tableName, tmPrimaryKeys, tmForeignKeys, tmColumns }, Just attnum) -> do
            let baseType = Map.lookup attnum tmColumns >>= \ColumnMeta { cmTypeOid } -> Map.lookup cmTypeOid typeInfo
            let joinNullable = dcTable `Set.member` joinNullableOids
            let nullable = joinNullable || maybe True (not . cmNotNull) (Map.lookup attnum tmColumns)
            case () of
                _ | attnum `Set.member` tmPrimaryKeys ->
                    pure (wrapNull nullable (idType tableName))
                  | Just refTable <- Map.lookup attnum tmForeignKeys ->
                    case Map.lookup refTable tables of
                        Just TableMeta { tmName = refName } ->
                            pure (wrapNull nullable (idType refName))
                        Nothing ->
                            maybe (fail (CS.cs missingType)) (hsTypeForPg typeInfo nullable) baseType
                  | otherwise ->
                    maybe (fail (CS.cs missingType)) (hsTypeForPg typeInfo nullable) baseType
          where
            missingType = "typedSql: missing type info for column " <> show attnum <> " of table " <> tableName
        _ ->
            maybe (fail (CS.cs ("typedSql: missing type info for column oid " <> show dcType))) (hsTypeForPg typeInfo True) (Map.lookup dcType typeInfo)

-- | Wrap a type in Maybe when nullable.
wrapNull :: Bool -> TH.Type -> TH.Type
wrapNull nullable ty = if nullable then TH.AppT (TH.ConT ''Maybe) ty else ty

-- | Build the Id' type for a table name.
idType :: Text -> TH.Type
idType tableName = TH.AppT (TH.ConT ''Id') (TH.LitT (TH.StrTyLit (CS.cs tableName)))

-- | Map Postgres type metadata to a Haskell type.
-- This is the core mapping used for both parameters and results.
hsTypeForPg :: Map.Map PQ.Oid PgTypeInfo -> Bool -> PgTypeInfo -> TH.TypeQ
hsTypeForPg typeInfo nullable PgTypeInfo { ptiName, ptiElem, ptiType } = do
    base <- case () of
        _ | Just elemOid <- ptiElem -> do
            elemInfo <- maybe (fail (CS.cs ("typedSql: missing array element type for " <> ptiName))) pure (Map.lookup elemOid typeInfo)
            elemTy <- hsTypeForPg typeInfo False elemInfo
            pure (TH.AppT TH.ListT elemTy)
        _ | ptiName `elem` ["int2", "int4"] -> pure (TH.ConT ''Int)
        _ | ptiName == "int8" -> pure (TH.ConT ''Integer)
        _ | ptiName `elem` ["text", "varchar", "bpchar", "citext"] -> pure (TH.ConT ''Text)
        _ | ptiName == "bool" -> pure (TH.ConT ''Bool)
        _ | ptiName == "uuid" -> pure (TH.ConT ''UUID)
        _ | ptiName == "timestamptz" -> pure (TH.ConT ''UTCTime)
        _ | ptiName == "timestamp" -> pure (TH.ConT ''LocalTime)
        _ | ptiName == "date" -> pure (TH.ConT ''Day)
        _ | ptiName == "time" -> pure (TH.ConT ''TimeOfDay)
        _ | ptiName `elem` ["json", "jsonb"] -> pure (TH.ConT ''Aeson.Value)
        _ | ptiName == "bytea" -> pure (TH.ConT ''BS.ByteString)
        _ | ptiName == "float4" -> pure (TH.ConT ''Float)
        _ | ptiName == "float8" -> pure (TH.ConT ''Double)
        _ | ptiName == "numeric" -> pure (TH.ConT ''Scientific)
        _ | ptiName == "point" -> pure (TH.ConT ''Point)
        _ | ptiName == "polygon" -> pure (TH.ConT ''Polygon)
        _ | ptiName == "inet" -> pure (TH.ConT ''Inet)
        _ | ptiName == "tsvector" -> pure (TH.ConT ''Tsvector)
        _ | ptiName == "interval" -> pure (TH.ConT ''Interval)
        _ | ptiType == 'e' ->
            pure (TH.ConT (TH.mkName (CS.cs (tableNameToModelName ptiName))))
        _ | ptiType == 'c' ->
            pure (TH.ConT (TH.mkName (CS.cs (tableNameToModelName ptiName))))
        _ -> fail (CS.cs ("typedSql: unsupported column type '" <> ptiName <> "' (typtype=" <> cs [ptiType] <> "). Consider filing a feature request."))
    pure (wrapNull nullable base)
