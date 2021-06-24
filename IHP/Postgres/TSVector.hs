{-|
Module: IHP.Postgres.TSVector
Description: Adds support for the Postgres tsvector type
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.Postgres.TSVector where

import BasicPrelude

-- | Represents a Postgres tsvector
--
-- See https://www.postgresql.org/docs/current/datatype-textsearch.html
type TSVector = Text
