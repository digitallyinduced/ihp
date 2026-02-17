{-|
Module: IHP.Postgres.TSVector
Description: Re-exports the Postgres tsvector type and provides a backwards-compatible alias
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.Postgres.TSVector
( TSVector
, module PostgresqlTypes.Tsvector
) where

import PostgresqlTypes.Tsvector

-- | Type alias for backwards compatibility
type TSVector = Tsvector
