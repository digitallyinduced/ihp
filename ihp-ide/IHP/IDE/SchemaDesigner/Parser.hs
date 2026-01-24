{-|
Module: IHP.IDE.SchemaDesigner.Parser
Description: IHP-specific schema file utilities
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.IDE.SchemaDesigner.Parser
( parseSchemaSql
, schemaFilePath
) where

import IHP.Prelude
import IHP.Postgres.Parser (parseSqlFile)
import IHP.Postgres.Types (Statement)

-- | Path to the IHP schema file
schemaFilePath :: FilePath
schemaFilePath = "Application/Schema.sql"

-- | Parse the IHP schema file at 'Application/Schema.sql'
parseSchemaSql :: IO (Either ByteString [Statement])
parseSchemaSql = parseSqlFile schemaFilePath
