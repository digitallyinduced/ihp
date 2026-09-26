{-# LANGUAGE NamedFieldPuns #-}

module IHP.TypedSql.TypeMapping
    ( hsTypeForParam
    , hsTypeForColumns
    , hsTypeForColumn
    , hsTypesForColumns
    , detectFullTable
    ) where

import           Control.Monad            (guard, zipWithM)
import           Data.Function            ((&))
import qualified Data.Aeson               as Aeson
import qualified Data.ByteString          as BS
import           Data.Int                 (Int64)
import qualified Data.List                as List
import qualified Data.Map.Strict          as Map
import           Data.Maybe                 (mapMaybe)
import           Data.Scientific          (Scientific)
import qualified Data.Set                 as Set
import qualified Data.String.Conversions  as CS
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Language.Haskell.TH      as TH
import           IHP.TypedSql.Id            (Id')
import           Prelude
import qualified Data.Text                  as Text
import           Data.String.Conversions  (cs)
import           Data.Text                (Text)
import           Data.Time.Calendar       (Day)
import           Data.Time.Clock          (UTCTime)
import           Data.Time.LocalTime      (LocalTime, TimeOfDay)
import           Data.UUID                (UUID)
import           Text.Countable           (singularize)
import qualified Text.Inflections         as Inflector
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
hsTypeForColumns :: Map.Map PQ.Oid PgTypeInfo -> Map.Map PQ.Oid TableMeta -> Set.Set PQ.Oid -> Set.Set Int -> [DescribeColumn] -> TH.TypeQ
hsTypeForColumns typeInfo tables joinNullableOids nonNullableColumns cols = do
    case detectFullTable tables cols of
        Just tableName ->
            pure (TH.ConT (TH.mkName (CS.cs (tableNameToModelName tableName))))
        Nothing -> do
            hsCols <- hsTypesForColumns typeInfo tables joinNullableOids nonNullableColumns cols
            case hsCols of
                [single] -> pure single
                _ -> pure $ foldl TH.AppT (TH.TupleT (length hsCols)) hsCols

-- | Compute individual Haskell types for each column.
-- Used by the record type generator which needs per-column types.
hsTypesForColumns :: Map.Map PQ.Oid PgTypeInfo -> Map.Map PQ.Oid TableMeta -> Set.Set PQ.Oid -> Set.Set Int -> [DescribeColumn] -> TH.Q [TH.Type]
hsTypesForColumns typeInfo tables joinNullableOids nonNullableColumns cols =
    zipWithM (\i col -> hsTypeForColumn typeInfo tables joinNullableOids (i `Set.member` nonNullableColumns) col) [0..] cols

-- | Detect whether the columns represent a full table selection (table.* with all columns in order).
-- High-level: if yes, we can return the model type directly.
detectFullTable :: Map.Map PQ.Oid TableMeta -> [DescribeColumn] -> Maybe Text
detectFullTable tables cols = do
    guard (not (null cols))
    let grouped =
            cols
                & List.groupBy (\a b -> dcTable a == dcTable b)
                & mapMaybe (\group -> case List.uncons group of
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

-- | Map a table name to its model name, e.g. @"users"@ to @"User"@.
-- Local copy of IHP's 'IHP.NameSupport.tableNameToModelName', kept here so
-- this package does not depend on @ihp@. Behavior is identical (same
-- singularization and camel-casing rules, including the @"brain_waves"@
-- special case).
tableNameToModelName :: Text -> Text
tableNameToModelName "brain_waves" = "BrainWave"
tableNameToModelName tableName = do
    let singularizedTableName = cs (singularize tableName)
    if "_" `Text.isInfixOf` singularizedTableName
        then unwrapEither tableName $ Inflector.toCamelCased True $ singularizedTableName
        else ucfirst singularizedTableName
{-# INLINABLE tableNameToModelName #-}

unwrapEither :: Show err => Text -> Either err Text -> Text
unwrapEither _ (Right value) = value
unwrapEither input (Left value) = error (CS.cs ("IHP.TypedSql: " <> show value <> " (value to be transformed: " <> show input <> ")"))
{-# INLINABLE unwrapEither #-}

-- | Make a text's first character uppercase.
ucfirst :: Text -> Text
ucfirst = applyFirst Text.toUpper
{-# INLINABLE ucfirst #-}

applyFirst :: (Text -> Text) -> Text -> Text
applyFirst f text =
    let (first, rest) = Text.splitAt 1 text
    in (f first) <> rest
{-# INLINABLE applyFirst #-}

-- | Map a single column into a Haskell type, with key-aware rules.
-- The @forceNonNull@ flag overrides the nullable fallback for computed columns
-- when AST analysis determines the expression is non-nullable (e.g. count()).
hsTypeForColumn :: Map.Map PQ.Oid PgTypeInfo -> Map.Map PQ.Oid TableMeta -> Set.Set PQ.Oid -> Bool -> DescribeColumn -> TH.TypeQ
hsTypeForColumn typeInfo tables joinNullableOids forceNonNull DescribeColumn { dcType, dcTable, dcAttnum } =
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
            missingType = "typedSql: missing type info for column " <> CS.cs (show attnum) <> " of table " <> tableName
        _ ->
            let nullable = not forceNonNull
            in maybe (fail (CS.cs ("typedSql: missing type info for column oid " <> show dcType))) (hsTypeForPg typeInfo nullable) (Map.lookup dcType typeInfo)

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
        _ | ptiName == "int8" -> pure (TH.ConT ''Int64)
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
