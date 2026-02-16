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
import Data.Word (Word32)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Database.PostgreSQL.Simple.FromField (FromField(..), Field, ResultError(..), returnError, Conversion, typeOid)
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(..))
import qualified Database.PostgreSQL.Simple.FromRow as PGFR
import qualified Database.PostgreSQL.Simple.Types as PG
import Database.PostgreSQL.LibPQ (Oid(..))
import IHP.ModelSupport.Types (Id'(..), PrimaryKey, LabeledData(..), FieldWithDefault(..), FieldWithUpdate(..))
import IHP.NameSupport (fieldNameToColumnName)
import PostgresqlTypes.Algebra (IsScalar(..))
import PostgresqlTypes.Point (Point)
import PostgresqlTypes.Polygon (Polygon)
import PostgresqlTypes.Inet (Inet)
import PostgresqlTypes.Interval (Interval)
import PostgresqlTypes.Tsvector (Tsvector)
import Data.Tagged (Tagged(..), untag)
import Data.ByteString (ByteString)
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text.Encoding as Text
import qualified TextBuilder

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

-- postgresql-types instances via IsScalar

fromFieldViaIsScalar :: forall a. (IsScalar a, Typeable a) => Field -> Maybe ByteString -> Conversion a
fromFieldViaIsScalar f Nothing = returnError UnexpectedNull f ""
fromFieldViaIsScalar f (Just bs) =
    case untag (baseOid :: Tagged a (Maybe Word32)) of
        Just oid | typeOid f /= Oid (fromIntegral oid) -> returnError Incompatible f ""
        _ -> case Attoparsec.parseOnly (textualDecoder <* Attoparsec.endOfInput) (Text.decodeUtf8 bs) of
            Right val -> pure val
            Left err -> returnError ConversionFailed f err

toFieldViaIsScalar :: (IsScalar a) => a -> Action
toFieldViaIsScalar = Escape . Text.encodeUtf8 . TextBuilder.toText . textualEncoder

instance FromField Point where fromField = fromFieldViaIsScalar
instance FromField Polygon where fromField = fromFieldViaIsScalar
instance FromField Inet where fromField = fromFieldViaIsScalar
instance FromField Interval where fromField = fromFieldViaIsScalar
instance FromField Tsvector where fromField = fromFieldViaIsScalar

instance ToField Point where toField = toFieldViaIsScalar
instance ToField Polygon where toField = toFieldViaIsScalar
instance ToField Inet where toField = toFieldViaIsScalar
instance ToField Interval where toField = toFieldViaIsScalar
instance ToField Tsvector where toField = toFieldViaIsScalar
