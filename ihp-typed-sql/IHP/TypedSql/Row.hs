{-|
Module: IHP.TypedSql.Row
Description: Row decoder contract for full-table typedSql selections
Copyright: (c) digitally induced GmbH, 2026

When a query selects a full table (@SELECT table.*@ with all columns in
order), typedSql infers the result type from the table name (e.g. @User@
for @users@) and decodes rows via 'TypedSqlRow'. Define an instance for
your record type:

> instance TypedSqlRow User where
>     typedSqlRowDecoder = ... -- a 'Hasql.Decoders.Row' for your columns

When used together with @ihp@, the @ihp@ package provides a blanket
instance that reuses IHP's @FromRowHasql@, so IHP models keep working
without changes.
-}
module IHP.TypedSql.Row
    ( TypedSqlRow (..)
    ) where

import qualified Hasql.Decoders as HasqlDecoders

-- | Decode a full-table row. Used by typedSql for @SELECT table.*@ queries.
class TypedSqlRow row where
    typedSqlRowDecoder :: HasqlDecoders.Row row
