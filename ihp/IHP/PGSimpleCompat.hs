{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances, FlexibleInstances, IncoherentInstances, PolyKinds, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Consolidates all postgresql-simple FromField\/ToField\/FromRow orphan instances
-- for IHP model types.
--
-- Note: JobStatus FromField\/ToField instances remain in "IHP.Job.Queue" to
-- avoid a circular dependency through @IHP.Job.Types -> IHP.Prelude -> IHP.ModelSupport@.
module IHP.PGSimpleCompat () where

import Prelude
import Data.String (IsString(..))
import Data.String.Conversions (cs)
import Data.Data (Typeable)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Database.PostgreSQL.Simple.FromField (FromField(..), ResultError(..), returnError)
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(..))
import qualified Database.PostgreSQL.Simple.FromRow as PGFR
import qualified Database.PostgreSQL.Simple.Types as PG
import IHP.ModelSupport.Types (Id'(..), PrimaryKey, LabeledData(..), FieldWithDefault(..), FieldWithUpdate(..))
import IHP.NameSupport (fieldNameToColumnName)

-- Import postgresql-simple-postgresql-types for FromField/ToField instances
-- of all postgresql-types types (Point, Polygon, Inet, Interval, etc.)
import Database.PostgreSQL.Simple.PostgresqlTypes ()

-- Id instances

instance FromField (PrimaryKey model) => FromField (Id' model) where
    {-# INLINE fromField #-}
    fromField value metaData = do
        fieldValue <- fromField value metaData
        pure (Id fieldValue)

instance ToField (PrimaryKey model) => ToField (Id' model) where
    {-# INLINE toField #-}
    toField (Id pk) = toField pk

instance (ToField (Id' a), ToField (Id' b)) => ToField (Id' a, Id' b) where
    {-# INLINE toField #-}
    toField (a, b) = Many [Plain "(", toField a, Plain ",", toField b, Plain ")"]

-- LabeledData instance

instance (FromField label, PGFR.FromRow a) => PGFR.FromRow (LabeledData label a) where
    fromRow = LabeledData <$> PGFR.field <*> PGFR.fromRow

-- FieldWithDefault / FieldWithUpdate instances

instance ToField valueType => ToField (FieldWithDefault valueType) where
    toField Default = Plain "DEFAULT"
    toField (NonDefault a) = toField a

instance (KnownSymbol name, ToField value) => ToField (FieldWithUpdate name value) where
    toField (NoUpdate name) =
        Plain (fromString $ cs $ fieldNameToColumnName $ cs $ symbolVal name)
    toField (Update a) = toField a

-- List instances (PGArray wrappers)

instance ToField value => ToField [value] where
    toField list = toField (PG.PGArray list)

instance (FromField value, Typeable value) => FromField [value] where
    fromField field value = PG.fromPGArray <$> (fromField field value)

