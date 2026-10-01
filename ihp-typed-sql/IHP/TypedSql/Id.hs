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
import           Data.Hashable              (Hashable)
import           GHC.TypeLits               (KnownSymbol, Symbol)
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

-- NOTE: The 'DefaultParamEncoder' instances for 'Id'' live in
-- "IHP.TypedSql.Encoders", next to every other instance, so that importing
-- this module (e.g. from @IHP.ModelSupport.Types@) does not pull in the
-- encoder machinery.
