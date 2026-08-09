{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module IHP.TypedSql.RowType
    ( SqlRow (..)
    , RowTuple
    , LookupField
    , FieldIndex
    , TupleGet (..)
    , sanitizeColumnName
    , deduplicateNames
    , detectPrimaryTable
    , sqlRowType
    ) where

import           IHP.Prelude
import qualified Data.Char                    as Char
import qualified Data.List                    as List
import qualified Data.Map.Strict              as Map
import qualified Data.Set                     as Set
import qualified Data.String.Conversions      as CS
import qualified Database.PostgreSQL.LibPQ    as PQ
import qualified Language.Haskell.TH          as TH
import           GHC.TypeLits                 (Nat, TypeError, ErrorMessage (..), type (+))
import qualified Prelude

import           IHP.TypedSql.Metadata        (DescribeColumn (..), TableMeta (..))

-- ---------------------------------------------------------------------------
-- SqlRow: a labeled newtype over a tuple
-- ---------------------------------------------------------------------------

-- | A row with type-level field names backed by a tuple.
--
-- @SqlRow '[ '("id", UUID), '("title", Text) ]@ is internally a @(UUID, Text)@
-- but supports @.id@ and @.title@ access via 'OverloadedRecordDot'.
newtype SqlRow (fields :: [(Symbol, Type)]) = SqlRow (RowTuple fields)

deriving instance Show (RowTuple fields) => Show (SqlRow fields)
deriving instance Eq   (RowTuple fields) => Eq   (SqlRow fields)

-- | Map a field list to its tuple representation.
--
-- Supports 2–16 selected columns. Beyond that, a 'TypeError' explains the limit.
-- (See https://github.com/digitallyinduced/ihp/issues/2767.)
type family RowTuple (fields :: [(Symbol, Type)]) where
    RowTuple '[ '(_, a), '(_, b) ] = (a, b)
    RowTuple '[ '(_, a), '(_, b), '(_, c) ] = (a, b, c)
    RowTuple '[ '(_, a), '(_, b), '(_, c), '(_, d) ] = (a, b, c, d)
    RowTuple '[ '(_, a), '(_, b), '(_, c), '(_, d), '(_, e) ] = (a, b, c, d, e)
    RowTuple '[ '(_, a), '(_, b), '(_, c), '(_, d), '(_, e), '(_, f) ] = (a, b, c, d, e, f)
    RowTuple '[ '(_, a), '(_, b), '(_, c), '(_, d), '(_, e), '(_, f), '(_, g) ] = (a, b, c, d, e, f, g)
    RowTuple '[ '(_, a), '(_, b), '(_, c), '(_, d), '(_, e), '(_, f), '(_, g), '(_, h) ] = (a, b, c, d, e, f, g, h)
    RowTuple '[ '(_, a), '(_, b), '(_, c), '(_, d), '(_, e), '(_, f), '(_, g), '(_, h), '(_, i) ] = (a, b, c, d, e, f, g, h, i)
    RowTuple '[ '(_, a), '(_, b), '(_, c), '(_, d), '(_, e), '(_, f), '(_, g), '(_, h), '(_, i), '(_, j) ] = (a, b, c, d, e, f, g, h, i, j)
    RowTuple '[ '(_, a), '(_, b), '(_, c), '(_, d), '(_, e), '(_, f), '(_, g), '(_, h), '(_, i), '(_, j), '(_, k) ] = (a, b, c, d, e, f, g, h, i, j, k)
    RowTuple '[ '(_, a), '(_, b), '(_, c), '(_, d), '(_, e), '(_, f), '(_, g), '(_, h), '(_, i), '(_, j), '(_, k), '(_, l) ] = (a, b, c, d, e, f, g, h, i, j, k, l)
    RowTuple '[ '(_, a), '(_, b), '(_, c), '(_, d), '(_, e), '(_, f), '(_, g), '(_, h), '(_, i), '(_, j), '(_, k), '(_, l), '(_, m) ] = (a, b, c, d, e, f, g, h, i, j, k, l, m)
    RowTuple '[ '(_, a), '(_, b), '(_, c), '(_, d), '(_, e), '(_, f), '(_, g), '(_, h), '(_, i), '(_, j), '(_, k), '(_, l), '(_, m), '(_, n) ] = (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    RowTuple '[ '(_, a), '(_, b), '(_, c), '(_, d), '(_, e), '(_, f), '(_, g), '(_, h), '(_, i), '(_, j), '(_, k), '(_, l), '(_, m), '(_, n), '(_, o) ] = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    RowTuple '[ '(_, a), '(_, b), '(_, c), '(_, d), '(_, e), '(_, f), '(_, g), '(_, h), '(_, i), '(_, j), '(_, k), '(_, l), '(_, m), '(_, n), '(_, o), '(_, p) ] = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
    RowTuple fields = TypeError
        ( 'Text "typedSql supports at most 16 selected columns; this query selects more."
          ':$$: 'Text "Split the query or project fewer columns."
        )

-- | Look up a field's type by name.
type family LookupField (name :: Symbol) (fields :: [(Symbol, Type)]) :: Type where
    LookupField name ('(name, t) ': _)   = t
    LookupField name (_          ': rest) = LookupField name rest
    LookupField name '[] = TypeError ('Text "SqlRow has no field '" ':<>: 'ShowType name ':<>: 'Text "'")

-- | Compute the 0-based index of a field by name.
type family FieldIndex (name :: Symbol) (fields :: [(Symbol, Type)]) :: Nat where
    FieldIndex name ('(name, _) ': _)   = 0
    FieldIndex name (_          ': rest) = 1 + FieldIndex name rest

-- ---------------------------------------------------------------------------
-- TupleGet: O(1) projection from tuples by index
-- ---------------------------------------------------------------------------

class TupleGet (n :: Nat) tuple result | n tuple -> result where
    tupleGet :: tuple -> result

-- Arity 2
instance TupleGet 0 (a,b) a where tupleGet (x,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 1 (a,b) b where tupleGet (_,x) = x; {-# INLINE tupleGet #-}

-- Arity 3
instance TupleGet 0 (a,b,c) a where tupleGet (x,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 1 (a,b,c) b where tupleGet (_,x,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 2 (a,b,c) c where tupleGet (_,_,x) = x; {-# INLINE tupleGet #-}

-- Arity 4
instance TupleGet 0 (a,b,c,d) a where tupleGet (x,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 1 (a,b,c,d) b where tupleGet (_,x,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 2 (a,b,c,d) c where tupleGet (_,_,x,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 3 (a,b,c,d) d where tupleGet (_,_,_,x) = x; {-# INLINE tupleGet #-}

-- Arity 5
instance TupleGet 0 (a,b,c,d,e) a where tupleGet (x,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 1 (a,b,c,d,e) b where tupleGet (_,x,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 2 (a,b,c,d,e) c where tupleGet (_,_,x,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 3 (a,b,c,d,e) d where tupleGet (_,_,_,x,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 4 (a,b,c,d,e) e where tupleGet (_,_,_,_,x) = x; {-# INLINE tupleGet #-}

-- Arity 6
instance TupleGet 0 (a,b,c,d,e,f) a where tupleGet (x,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 1 (a,b,c,d,e,f) b where tupleGet (_,x,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 2 (a,b,c,d,e,f) c where tupleGet (_,_,x,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 3 (a,b,c,d,e,f) d where tupleGet (_,_,_,x,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 4 (a,b,c,d,e,f) e where tupleGet (_,_,_,_,x,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 5 (a,b,c,d,e,f) f where tupleGet (_,_,_,_,_,x) = x; {-# INLINE tupleGet #-}

-- Arity 7
instance TupleGet 0 (a,b,c,d,e,f,g) a where tupleGet (x,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 1 (a,b,c,d,e,f,g) b where tupleGet (_,x,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 2 (a,b,c,d,e,f,g) c where tupleGet (_,_,x,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 3 (a,b,c,d,e,f,g) d where tupleGet (_,_,_,x,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 4 (a,b,c,d,e,f,g) e where tupleGet (_,_,_,_,x,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 5 (a,b,c,d,e,f,g) f where tupleGet (_,_,_,_,_,x,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 6 (a,b,c,d,e,f,g) g where tupleGet (_,_,_,_,_,_,x) = x; {-# INLINE tupleGet #-}

-- Arity 8
instance TupleGet 0 (a,b,c,d,e,f,g,h) a where tupleGet (x,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 1 (a,b,c,d,e,f,g,h) b where tupleGet (_,x,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 2 (a,b,c,d,e,f,g,h) c where tupleGet (_,_,x,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 3 (a,b,c,d,e,f,g,h) d where tupleGet (_,_,_,x,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 4 (a,b,c,d,e,f,g,h) e where tupleGet (_,_,_,_,x,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 5 (a,b,c,d,e,f,g,h) f where tupleGet (_,_,_,_,_,x,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 6 (a,b,c,d,e,f,g,h) g where tupleGet (_,_,_,_,_,_,x,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 7 (a,b,c,d,e,f,g,h) h where tupleGet (_,_,_,_,_,_,_,x) = x; {-# INLINE tupleGet #-}

-- Arity 9
instance TupleGet 0 (a,b,c,d,e,f,g,h,i) a where tupleGet (x,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 1 (a,b,c,d,e,f,g,h,i) b where tupleGet (_,x,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 2 (a,b,c,d,e,f,g,h,i) c where tupleGet (_,_,x,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 3 (a,b,c,d,e,f,g,h,i) d where tupleGet (_,_,_,x,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 4 (a,b,c,d,e,f,g,h,i) e where tupleGet (_,_,_,_,x,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 5 (a,b,c,d,e,f,g,h,i) f where tupleGet (_,_,_,_,_,x,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 6 (a,b,c,d,e,f,g,h,i) g where tupleGet (_,_,_,_,_,_,x,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 7 (a,b,c,d,e,f,g,h,i) h where tupleGet (_,_,_,_,_,_,_,x,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 8 (a,b,c,d,e,f,g,h,i) i where tupleGet (_,_,_,_,_,_,_,_,x) = x; {-# INLINE tupleGet #-}

-- Arity 10
instance TupleGet 0 (a,b,c,d,e,f,g,h,i,j) a where tupleGet (x,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 1 (a,b,c,d,e,f,g,h,i,j) b where tupleGet (_,x,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 2 (a,b,c,d,e,f,g,h,i,j) c where tupleGet (_,_,x,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 3 (a,b,c,d,e,f,g,h,i,j) d where tupleGet (_,_,_,x,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 4 (a,b,c,d,e,f,g,h,i,j) e where tupleGet (_,_,_,_,x,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 5 (a,b,c,d,e,f,g,h,i,j) f where tupleGet (_,_,_,_,_,x,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 6 (a,b,c,d,e,f,g,h,i,j) g where tupleGet (_,_,_,_,_,_,x,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 7 (a,b,c,d,e,f,g,h,i,j) h where tupleGet (_,_,_,_,_,_,_,x,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 8 (a,b,c,d,e,f,g,h,i,j) i where tupleGet (_,_,_,_,_,_,_,_,x,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 9 (a,b,c,d,e,f,g,h,i,j) j where tupleGet (_,_,_,_,_,_,_,_,_,x) = x; {-# INLINE tupleGet #-}

-- Arity 11
instance TupleGet 0 (a,b,c,d,e,f,g,h,i,j,k) a where tupleGet (x,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 1 (a,b,c,d,e,f,g,h,i,j,k) b where tupleGet (_,x,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 2 (a,b,c,d,e,f,g,h,i,j,k) c where tupleGet (_,_,x,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 3 (a,b,c,d,e,f,g,h,i,j,k) d where tupleGet (_,_,_,x,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 4 (a,b,c,d,e,f,g,h,i,j,k) e where tupleGet (_,_,_,_,x,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 5 (a,b,c,d,e,f,g,h,i,j,k) f where tupleGet (_,_,_,_,_,x,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 6 (a,b,c,d,e,f,g,h,i,j,k) g where tupleGet (_,_,_,_,_,_,x,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 7 (a,b,c,d,e,f,g,h,i,j,k) h where tupleGet (_,_,_,_,_,_,_,x,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 8 (a,b,c,d,e,f,g,h,i,j,k) i where tupleGet (_,_,_,_,_,_,_,_,x,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 9 (a,b,c,d,e,f,g,h,i,j,k) j where tupleGet (_,_,_,_,_,_,_,_,_,x,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 10 (a,b,c,d,e,f,g,h,i,j,k) k where tupleGet (_,_,_,_,_,_,_,_,_,_,x) = x; {-# INLINE tupleGet #-}

-- Arity 12
instance TupleGet 0 (a,b,c,d,e,f,g,h,i,j,k,l) a where tupleGet (x,_,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 1 (a,b,c,d,e,f,g,h,i,j,k,l) b where tupleGet (_,x,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 2 (a,b,c,d,e,f,g,h,i,j,k,l) c where tupleGet (_,_,x,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 3 (a,b,c,d,e,f,g,h,i,j,k,l) d where tupleGet (_,_,_,x,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 4 (a,b,c,d,e,f,g,h,i,j,k,l) e where tupleGet (_,_,_,_,x,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 5 (a,b,c,d,e,f,g,h,i,j,k,l) f where tupleGet (_,_,_,_,_,x,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 6 (a,b,c,d,e,f,g,h,i,j,k,l) g where tupleGet (_,_,_,_,_,_,x,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 7 (a,b,c,d,e,f,g,h,i,j,k,l) h where tupleGet (_,_,_,_,_,_,_,x,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 8 (a,b,c,d,e,f,g,h,i,j,k,l) i where tupleGet (_,_,_,_,_,_,_,_,x,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 9 (a,b,c,d,e,f,g,h,i,j,k,l) j where tupleGet (_,_,_,_,_,_,_,_,_,x,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 10 (a,b,c,d,e,f,g,h,i,j,k,l) k where tupleGet (_,_,_,_,_,_,_,_,_,_,x,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 11 (a,b,c,d,e,f,g,h,i,j,k,l) l where tupleGet (_,_,_,_,_,_,_,_,_,_,_,x) = x; {-# INLINE tupleGet #-}

-- Arity 13
instance TupleGet 0 (a,b,c,d,e,f,g,h,i,j,k,l,m) a where tupleGet (x,_,_,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 1 (a,b,c,d,e,f,g,h,i,j,k,l,m) b where tupleGet (_,x,_,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 2 (a,b,c,d,e,f,g,h,i,j,k,l,m) c where tupleGet (_,_,x,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 3 (a,b,c,d,e,f,g,h,i,j,k,l,m) d where tupleGet (_,_,_,x,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 4 (a,b,c,d,e,f,g,h,i,j,k,l,m) e where tupleGet (_,_,_,_,x,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 5 (a,b,c,d,e,f,g,h,i,j,k,l,m) f where tupleGet (_,_,_,_,_,x,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 6 (a,b,c,d,e,f,g,h,i,j,k,l,m) g where tupleGet (_,_,_,_,_,_,x,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 7 (a,b,c,d,e,f,g,h,i,j,k,l,m) h where tupleGet (_,_,_,_,_,_,_,x,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 8 (a,b,c,d,e,f,g,h,i,j,k,l,m) i where tupleGet (_,_,_,_,_,_,_,_,x,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 9 (a,b,c,d,e,f,g,h,i,j,k,l,m) j where tupleGet (_,_,_,_,_,_,_,_,_,x,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 10 (a,b,c,d,e,f,g,h,i,j,k,l,m) k where tupleGet (_,_,_,_,_,_,_,_,_,_,x,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 11 (a,b,c,d,e,f,g,h,i,j,k,l,m) l where tupleGet (_,_,_,_,_,_,_,_,_,_,_,x,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 12 (a,b,c,d,e,f,g,h,i,j,k,l,m) m where tupleGet (_,_,_,_,_,_,_,_,_,_,_,_,x) = x; {-# INLINE tupleGet #-}

-- Arity 14
instance TupleGet 0 (a,b,c,d,e,f,g,h,i,j,k,l,m,n) a where tupleGet (x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 1 (a,b,c,d,e,f,g,h,i,j,k,l,m,n) b where tupleGet (_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 2 (a,b,c,d,e,f,g,h,i,j,k,l,m,n) c where tupleGet (_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 3 (a,b,c,d,e,f,g,h,i,j,k,l,m,n) d where tupleGet (_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 4 (a,b,c,d,e,f,g,h,i,j,k,l,m,n) e where tupleGet (_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 5 (a,b,c,d,e,f,g,h,i,j,k,l,m,n) f where tupleGet (_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 6 (a,b,c,d,e,f,g,h,i,j,k,l,m,n) g where tupleGet (_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 7 (a,b,c,d,e,f,g,h,i,j,k,l,m,n) h where tupleGet (_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 8 (a,b,c,d,e,f,g,h,i,j,k,l,m,n) i where tupleGet (_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 9 (a,b,c,d,e,f,g,h,i,j,k,l,m,n) j where tupleGet (_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 10 (a,b,c,d,e,f,g,h,i,j,k,l,m,n) k where tupleGet (_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 11 (a,b,c,d,e,f,g,h,i,j,k,l,m,n) l where tupleGet (_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 12 (a,b,c,d,e,f,g,h,i,j,k,l,m,n) m where tupleGet (_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 13 (a,b,c,d,e,f,g,h,i,j,k,l,m,n) n where tupleGet (_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x; {-# INLINE tupleGet #-}

-- Arity 15
instance TupleGet 0 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) a where tupleGet (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 1 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) b where tupleGet (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 2 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) c where tupleGet (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 3 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) d where tupleGet (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 4 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) e where tupleGet (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 5 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) f where tupleGet (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 6 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) g where tupleGet (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 7 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) h where tupleGet (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 8 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) i where tupleGet (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 9 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) j where tupleGet (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 10 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) k where tupleGet (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 11 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) l where tupleGet (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 12 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) m where tupleGet (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 13 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) n where tupleGet (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 14 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) o where tupleGet (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x; {-# INLINE tupleGet #-}

-- Arity 16
instance TupleGet 0 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) a where tupleGet (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 1 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) b where tupleGet (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 2 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) c where tupleGet (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 3 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) d where tupleGet (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 4 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) e where tupleGet (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 5 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) f where tupleGet (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 6 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) g where tupleGet (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 7 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) h where tupleGet (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 8 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) i where tupleGet (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 9 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) j where tupleGet (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 10 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) k where tupleGet (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 11 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) l where tupleGet (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 12 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) m where tupleGet (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 13 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) n where tupleGet (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 14 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) o where tupleGet (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x; {-# INLINE tupleGet #-}
instance TupleGet 15 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) p where tupleGet (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x; {-# INLINE tupleGet #-}

-- ---------------------------------------------------------------------------
-- HasField: wires SqlRow into OverloadedRecordDot
-- ---------------------------------------------------------------------------

instance
    ( idx ~ FieldIndex name fields
    , TupleGet idx (RowTuple fields) result
    , result ~ LookupField name fields
    ) => HasField name (SqlRow fields) result where
    getField (SqlRow tuple) = tupleGet @idx tuple
    {-# INLINE getField #-}

-- ---------------------------------------------------------------------------
-- TH helpers
-- ---------------------------------------------------------------------------

-- | Build the TH type @SqlRow '[ '("col1", T1), '("col2", T2), ... ]@
sqlRowType :: [(String, TH.Type)] -> TH.Type
sqlRowType fields = TH.AppT (TH.ConT ''SqlRow) fieldList
  where
    fieldList = foldr (\(name, ty) acc -> mkCons (mkTuple name ty) acc) TH.PromotedNilT fields
    mkTuple name ty = TH.AppT (TH.AppT (TH.PromotedTupleT 2) (TH.LitT (TH.StrTyLit name))) ty
    mkCons x xs = TH.AppT (TH.AppT TH.PromotedConsT x) xs

-- ---------------------------------------------------------------------------
-- Column name utilities
-- ---------------------------------------------------------------------------

-- | Sanitize a PostgreSQL column name into a valid Haskell identifier.
sanitizeColumnName :: ByteString -> String
sanitizeColumnName raw
    | cleaned == "" || cleaned == "?column?" = "col"
    | otherwise = fixStart (map sanitizeChar cleaned)
  where
    cleaned = CS.cs raw :: String
    sanitizeChar c
        | Char.isAlphaNum c || c == '_' = c
        | otherwise = '_'
    fixStart [] = "col"
    fixStart (c:rest)
        | Char.isDigit c = '_' : c : rest
        | Char.isUpper c = Char.toLower c : rest
        | otherwise = c : rest

-- | Deduplicate field names by appending @_1@, @_2@, etc.
--
-- Tracks all used names (both original and generated suffixes) to avoid
-- collisions like @["name", "name_1", "name"]@ → @["name", "name_1", "name_2"]@.
deduplicateNames :: [String] -> [String]
deduplicateNames names = go Set.empty [] names
  where
    go _ acc [] = reverse acc
    go used acc (n:rest)
        | n `Set.member` used =
            let suffixed = findUnused used n 1
            in go (Set.insert suffixed used) (suffixed : acc) rest
        | otherwise = go (Set.insert n used) (n : acc) rest
    findUnused used base i =
        let candidate = base ++ "_" ++ Prelude.show i
        in if candidate `Set.member` used
            then findUnused used base (i + 1)
            else candidate

-- | Detect the primary table when all columns come from one table.
detectPrimaryTable :: Map.Map PQ.Oid TableMeta -> [DescribeColumn] -> Maybe Text
detectPrimaryTable tables columns =
    case List.nub tableOids of
        [tableOid] -> tmName <$> Map.lookup tableOid tables
        _          -> Nothing
  where
    tableOids = filter (/= PQ.Oid 0) (map dcTable columns)
