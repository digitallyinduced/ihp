{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: IHP.TypedSql.Id
Description: Identifier type for typedSql primary-key columns
Copyright: (c) digitally induced GmbH, 2026

The canonical definition of 'Id'' and the open 'PrimaryKey' type family.
@ihp@ re-exports both from "IHP.ModelSupport.Types", so an application
declares each table's key exactly once and typedSql's generated code denotes
the very same type that IHP's model API uses:

> type instance PrimaryKey "users" = UUID

Primary-key columns (and single-column foreign keys) are typed as
@Id' table@, where @table@ is the type-level table name (e.g.
@Id' "users"@).
-}
module IHP.TypedSql.Id
    ( Id' (..)
    , PrimaryKey
    ) where

import           Control.DeepSeq            (NFData)
import           Data.Data                  (Data)
import           Data.Functor.Contravariant (contramap)
import           Data.Functor.Contravariant.Divisible (divide)
import           Data.Hashable              (Hashable)
import           Data.Text                  (Text)
import           GHC.TypeLits               (KnownSymbol, Symbol)
import qualified Hasql.Encoders             as Encoders
import           Hasql.Implicits.Encoders   (DefaultParamEncoder (..))
import qualified Hasql.Mapping.IsScalar     as Mapping
import           Prelude

-- | Provides the primary key type for a given table. The instances are usually
-- declared by the generated haskell code in @Generated.Types@.
--
-- __Example:__ Defining the primary key for a @users@ table
--
-- > type instance PrimaryKey "users" = UUID
--
-- __Example:__ Defining the primary key for a table with a SERIAL pk
--
-- > type instance PrimaryKey "projects" = Int
type family PrimaryKey (table :: Symbol)

-- | A primary-key (or foreign-key) reference to a row in @table@.
newtype Id' (table :: Symbol) = Id (PrimaryKey table)

deriving instance Eq (PrimaryKey table) => Eq (Id' table)
deriving instance Ord (PrimaryKey table) => Ord (Id' table)
deriving instance Hashable (PrimaryKey table) => Hashable (Id' table)
deriving instance (KnownSymbol table, Data (PrimaryKey table)) => Data (Id' table)
deriving instance NFData (PrimaryKey table) => NFData (Id' table)

-- | Show the wrapped primary key without the @Id@ constructor, so @show userId@
-- keeps rendering the plain key (moved here from @IHP.ModelSupport@ so the
-- canonical 'Id'' type has a single owner).
instance Show (PrimaryKey table) => Show (Id' table) where
    show (Id primaryKey) = show primaryKey

-- | Encode 'Id' table' for tables with any primary key type that has an 'IsScalar' instance.
instance Mapping.IsScalar (PrimaryKey table) => DefaultParamEncoder (Id' table) where
    defaultParam = Encoders.nonNullable (contramap (\(Id pk) -> pk) Mapping.encoder)

-- | Encode list of 'Id' table' for tables with any encodable primary key type.
instance Mapping.IsScalar (PrimaryKey table) => DefaultParamEncoder [Id' table] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable (contramap (\(Id pk) -> pk) Mapping.encoder)

-- | Encode 'Maybe (Id' table)' for nullable foreign keys with any encodable primary key type.
instance Mapping.IsScalar (PrimaryKey table) => DefaultParamEncoder (Maybe (Id' table)) where
    defaultParam = Encoders.nullable (contramap (\(Id pk) -> pk) Mapping.encoder)

-- | Encode '[Maybe (Id' table)]' for @IN (...)@ queries with nullable foreign keys.
instance Mapping.IsScalar (PrimaryKey table) => DefaultParamEncoder [Maybe (Id' table)] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nullable (contramap (\(Id pk) -> pk) Mapping.encoder)

-- | Encode '(Id' a, Id' b)' as PostgreSQL composite/record type
-- Used for composite primary keys with two Id columns of any scalar PK type
instance (Mapping.IsScalar (PrimaryKey a), Mapping.IsScalar (PrimaryKey b)) => DefaultParamEncoder (Id' a, Id' b) where
    defaultParam = Encoders.nonNullable $ Encoders.composite (Nothing :: Maybe Text) "" $
        divide (\(Id a, Id b) -> (a, b))
            (Encoders.field (Encoders.nonNullable Mapping.encoder))
            (Encoders.field (Encoders.nonNullable Mapping.encoder))

-- | Encode '[(Id' a, Id' b)]' as PostgreSQL array of composite types
-- Used by filterWhereIdIn for tables with two-column composite primary keys
instance (Mapping.IsScalar (PrimaryKey a), Mapping.IsScalar (PrimaryKey b)) => DefaultParamEncoder [(Id' a, Id' b)] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable $ Encoders.composite (Nothing :: Maybe Text) "" $
        divide (\(Id a, Id b) -> (a, b))
            (Encoders.field (Encoders.nonNullable Mapping.encoder))
            (Encoders.field (Encoders.nonNullable Mapping.encoder))

-- | Encode '(Id' a, Id' b, Id' c)' as PostgreSQL composite/record type
-- Used for composite primary keys with three Id columns of any scalar PK type
instance (Mapping.IsScalar (PrimaryKey a), Mapping.IsScalar (PrimaryKey b), Mapping.IsScalar (PrimaryKey c)) => DefaultParamEncoder (Id' a, Id' b, Id' c) where
    defaultParam = Encoders.nonNullable $ Encoders.composite (Nothing :: Maybe Text) "" $
        divide (\(Id a, Id b, Id c) -> (a, (b, c)))
            (Encoders.field (Encoders.nonNullable Mapping.encoder))
            (divide id (Encoders.field (Encoders.nonNullable Mapping.encoder)) (Encoders.field (Encoders.nonNullable Mapping.encoder)))

-- | Encode '[(Id' a, Id' b, Id' c)]' as PostgreSQL array of composite types
-- Used by filterWhereIdIn for tables with three-column composite primary keys
instance (Mapping.IsScalar (PrimaryKey a), Mapping.IsScalar (PrimaryKey b), Mapping.IsScalar (PrimaryKey c)) => DefaultParamEncoder [(Id' a, Id' b, Id' c)] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable $ Encoders.composite (Nothing :: Maybe Text) "" $
        divide (\(Id a, Id b, Id c) -> (a, (b, c)))
            (Encoders.field (Encoders.nonNullable Mapping.encoder))
            (divide id (Encoders.field (Encoders.nonNullable Mapping.encoder)) (Encoders.field (Encoders.nonNullable Mapping.encoder)))

-- | Encode '(Id' a, Id' b, Id' c, Id' d)' as PostgreSQL composite/record type
-- Used for composite primary keys with four Id columns of any scalar PK type
instance (Mapping.IsScalar (PrimaryKey a), Mapping.IsScalar (PrimaryKey b), Mapping.IsScalar (PrimaryKey c), Mapping.IsScalar (PrimaryKey d)) => DefaultParamEncoder (Id' a, Id' b, Id' c, Id' d) where
    defaultParam = Encoders.nonNullable $ Encoders.composite (Nothing :: Maybe Text) "" $
        divide (\(Id a, Id b, Id c, Id d) -> (a, (b, c, d)))
            (Encoders.field (Encoders.nonNullable Mapping.encoder))
            (divide (\(b, c, d) -> (b, (c, d)))
                (Encoders.field (Encoders.nonNullable Mapping.encoder))
                (divide id (Encoders.field (Encoders.nonNullable Mapping.encoder)) (Encoders.field (Encoders.nonNullable Mapping.encoder))))

-- | Encode '[(Id' a, Id' b, Id' c, Id' d)]' as PostgreSQL array of composite types
-- Used by filterWhereIdIn for tables with four-column composite primary keys
instance (Mapping.IsScalar (PrimaryKey a), Mapping.IsScalar (PrimaryKey b), Mapping.IsScalar (PrimaryKey c), Mapping.IsScalar (PrimaryKey d)) => DefaultParamEncoder [(Id' a, Id' b, Id' c, Id' d)] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable $ Encoders.composite (Nothing :: Maybe Text) "" $
        divide (\(Id a, Id b, Id c, Id d) -> (a, (b, c, d)))
            (Encoders.field (Encoders.nonNullable Mapping.encoder))
            (divide (\(b, c, d) -> (b, (c, d)))
                (Encoders.field (Encoders.nonNullable Mapping.encoder))
                (divide id (Encoders.field (Encoders.nonNullable Mapping.encoder)) (Encoders.field (Encoders.nonNullable Mapping.encoder))))
