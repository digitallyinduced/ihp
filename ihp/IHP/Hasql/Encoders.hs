{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: IHP.Hasql.Encoders
Description: DefaultParamEncoder instances for common types
Copyright: (c) digitally induced GmbH, 2025

This module provides orphan 'DefaultParamEncoder' instances for types that
hasql-implicits doesn't support out of the box.

The instances for 'Int', '[Int]', 'Maybe Int', '[Maybe Int]' and for 'Id'' are
defined in "IHP.TypedSql.Id", and those for the @postgresql-types@ values that
typedSql generates ('Point', 'Polygon', 'Inet', 'Tsvector', 'Interval') in
"IHP.TypedSql.Encoders". Both are merely pulled in here by import, because
@ihp-typed-sql@ needs them standalone too and exactly one package may define
each one.
-}
module IHP.Hasql.Encoders
( ToSnippetParams(..)
, sqlToSnippet
) where

import Prelude
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding as Text
import qualified Hasql.Encoders as Encoders
import Hasql.Implicits.Encoders (DefaultParamEncoder(..))
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import Database.PostgreSQL.Simple (Only(..), (:.)(..))
import Data.Functor.Contravariant (contramap)
import IHP.TypedSql.Id ()
import IHP.TypedSql.Encoders ()
import Database.PostgreSQL.Simple.Types (Binary(..))
import Hasql.PostgresqlTypes ()

-- | Encode 'Binary ByteString' as PostgreSQL bytea
-- IHP wraps bytea columns in Binary, so we need to unwrap before encoding
instance DefaultParamEncoder (Binary ByteString) where
    defaultParam = Encoders.nonNullable (contramap (\(Binary bs) -> bs) Encoders.bytea)

-- | Encode 'Maybe (Binary ByteString)' as nullable PostgreSQL bytea
instance DefaultParamEncoder (Maybe (Binary ByteString)) where
    defaultParam = Encoders.nullable (contramap (\(Binary bs) -> bs) Encoders.bytea)

-- | Converts parameter tuples into a list of hasql 'Snippet' values.
--
-- This mirrors postgresql-simple's 'ToRow' typeclass, allowing @sqlQuery@ and @sqlExec@
-- to use hasql's native parameterized queries instead of 'PG.formatQuery'.
class ToSnippetParams a where
    toSnippetParams :: a -> [Snippet]

instance ToSnippetParams () where
    toSnippetParams () = []

instance DefaultParamEncoder a => ToSnippetParams (Only a) where
    toSnippetParams (Only a) = [Snippet.param a]

instance (DefaultParamEncoder a, DefaultParamEncoder b) => ToSnippetParams (a, b) where
    toSnippetParams (a, b) = [Snippet.param a, Snippet.param b]

instance (DefaultParamEncoder a, DefaultParamEncoder b, DefaultParamEncoder c) => ToSnippetParams (a, b, c) where
    toSnippetParams (a, b, c) = [Snippet.param a, Snippet.param b, Snippet.param c]

instance (DefaultParamEncoder a, DefaultParamEncoder b, DefaultParamEncoder c, DefaultParamEncoder d) => ToSnippetParams (a, b, c, d) where
    toSnippetParams (a, b, c, d) = [Snippet.param a, Snippet.param b, Snippet.param c, Snippet.param d]

instance (DefaultParamEncoder a, DefaultParamEncoder b, DefaultParamEncoder c, DefaultParamEncoder d, DefaultParamEncoder e) => ToSnippetParams (a, b, c, d, e) where
    toSnippetParams (a, b, c, d, e) = [Snippet.param a, Snippet.param b, Snippet.param c, Snippet.param d, Snippet.param e]

instance (DefaultParamEncoder a, DefaultParamEncoder b, DefaultParamEncoder c, DefaultParamEncoder d, DefaultParamEncoder e, DefaultParamEncoder f) => ToSnippetParams (a, b, c, d, e, f) where
    toSnippetParams (a, b, c, d, e, f) = [Snippet.param a, Snippet.param b, Snippet.param c, Snippet.param d, Snippet.param e, Snippet.param f]

instance (DefaultParamEncoder a, DefaultParamEncoder b, DefaultParamEncoder c, DefaultParamEncoder d, DefaultParamEncoder e, DefaultParamEncoder f, DefaultParamEncoder g) => ToSnippetParams (a, b, c, d, e, f, g) where
    toSnippetParams (a, b, c, d, e, f, g) = [Snippet.param a, Snippet.param b, Snippet.param c, Snippet.param d, Snippet.param e, Snippet.param f, Snippet.param g]

instance (DefaultParamEncoder a, DefaultParamEncoder b, DefaultParamEncoder c, DefaultParamEncoder d, DefaultParamEncoder e, DefaultParamEncoder f, DefaultParamEncoder g, DefaultParamEncoder h) => ToSnippetParams (a, b, c, d, e, f, g, h) where
    toSnippetParams (a, b, c, d, e, f, g, h) = [Snippet.param a, Snippet.param b, Snippet.param c, Snippet.param d, Snippet.param e, Snippet.param f, Snippet.param g, Snippet.param h]

instance (DefaultParamEncoder a, DefaultParamEncoder b, DefaultParamEncoder c, DefaultParamEncoder d, DefaultParamEncoder e, DefaultParamEncoder f, DefaultParamEncoder g, DefaultParamEncoder h, DefaultParamEncoder i) => ToSnippetParams (a, b, c, d, e, f, g, h, i) where
    toSnippetParams (a, b, c, d, e, f, g, h, i) = [Snippet.param a, Snippet.param b, Snippet.param c, Snippet.param d, Snippet.param e, Snippet.param f, Snippet.param g, Snippet.param h, Snippet.param i]

instance (DefaultParamEncoder a, DefaultParamEncoder b, DefaultParamEncoder c, DefaultParamEncoder d, DefaultParamEncoder e, DefaultParamEncoder f, DefaultParamEncoder g, DefaultParamEncoder h, DefaultParamEncoder i, DefaultParamEncoder j) => ToSnippetParams (a, b, c, d, e, f, g, h, i, j) where
    toSnippetParams (a, b, c, d, e, f, g, h, i, j) = [Snippet.param a, Snippet.param b, Snippet.param c, Snippet.param d, Snippet.param e, Snippet.param f, Snippet.param g, Snippet.param h, Snippet.param i, Snippet.param j]

-- | Append two parameter lists (mirrors postgresql-simple's ':.' operator)
instance (ToSnippetParams a, ToSnippetParams b) => ToSnippetParams (a :. b) where
    toSnippetParams (a :. b) = toSnippetParams a <> toSnippetParams b

-- | Converts a SQL query with @?@ placeholders and a list of 'Snippet' parameters
-- into a single 'Snippet' with native hasql @$1, $2, ...@ parameterization.
--
-- This mirrors postgresql-simple's @?@ placeholder convention.
--
-- __Example:__
--
-- > sqlToSnippet "SELECT * FROM users WHERE id = ? AND name = ?" [Snippet.param id, Snippet.param name]
-- > -- becomes: Snippet.sql "SELECT * FROM users WHERE id = " <> Snippet.param id <> Snippet.sql " AND name = " <> Snippet.param name
--
sqlToSnippet :: ByteString -> [Snippet] -> Snippet
sqlToSnippet sql params = mconcat (interleave sqlParts params)
  where
    sqlParts = map (Snippet.sql . Text.decodeUtf8) (BS8.split '?' sql)
    interleave (s:ss) (p:ps) = s : p : interleave ss ps
    interleave ss [] = ss
    interleave [] _ = []
{-# INLINE sqlToSnippet #-}
