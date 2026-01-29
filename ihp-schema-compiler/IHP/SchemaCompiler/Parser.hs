{-|
Module: IHP.SchemaCompiler.Parser
Description: IHP-specific schema file utilities
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.SchemaCompiler.Parser
( parseSchemaSql
, schemaFilePath
) where

import IHP.Prelude
import IHP.Postgres.Parser (parseSqlFile)
import IHP.Postgres.Types (Statement)
import System.OsPath (OsPath, osp)

-- | Path to the IHP schema file
schemaFilePath :: OsPath
schemaFilePath = [osp|Application/Schema.sql|]

-- | Parse the IHP schema file at 'Application/Schema.sql'
parseSchemaSql :: IO (Either ByteString [Statement])
parseSchemaSql = parseSqlFile schemaFilePath
