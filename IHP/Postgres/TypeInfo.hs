{-# LANGUAGE TemplateHaskell #-}
{-|
Module: IHP.Postgres.TypeInfo
Description: Extension Of The Database.PostgreSQL.Simple.TypeInfo Module
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.Postgres.TypeInfo where

import Database.PostgreSQL.Simple.FromField

tsvector :: TypeInfo
tsvector = Basic {
    typoid      = tsvectorOid,
    typcategory = 'U',
    typdelim    = ',',
    typname     = "tsvector"
  }

-- `SELECT oid, typname FROM pg_type WHERE typname ~ 'tsvector';`
tsvectorOid :: Oid
tsvectorOid = Oid 3614
{-# INLINE tsvector #-}
