-- | Shared helpers for building dynamic hasql queries.
--
-- These are used by both @ihp-datasync@ ('IHP.DataSync.DynamicQuery') and
-- @ihp-ide@ ('IHP.IDE.Data.Controller') for executing dynamically-constructed
-- SQL through hasql.
module IHP.QueryBuilder.HasqlHelpers
    ( wrapDynamicQuery
    , quoteIdentifier
    ) where

import IHP.Prelude
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Data.Text as Text

-- | Wraps a SQL query snippet so that each row is returned as a JSON object.
--
-- This is needed because hasql decoders are positional and don't provide column name metadata.
-- By wrapping with @row_to_json@, we get column names in the JSON keys, which we can then
-- decode into a list of fields.
--
-- Uses a CTE (Common Table Expression) which works for both SELECT queries
-- and DML statements (INSERT, UPDATE, DELETE) with RETURNING:
--
-- @
-- WITH _ihp_dynamic_result AS (...original query...) SELECT row_to_json(t)::jsonb FROM _ihp_dynamic_result AS t
-- @
wrapDynamicQuery :: Snippet -> Snippet
wrapDynamicQuery innerQuery =
    Snippet.sql "WITH _ihp_dynamic_result AS (" <> innerQuery <> Snippet.sql ") SELECT row_to_json(t)::jsonb FROM _ihp_dynamic_result AS t"

-- | Quote a SQL identifier (table name, column name) to prevent SQL injection.
--
-- Wraps the identifier in double quotes and escapes any embedded double quotes
-- by doubling them, following the SQL standard.
quoteIdentifier :: Text -> Snippet
quoteIdentifier name = Snippet.sql (cs ("\"" <> Text.replace "\"" "\"\"" name <> "\""))
