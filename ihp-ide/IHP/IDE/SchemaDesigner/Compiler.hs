{-|
Module: IHP.IDE.SchemaDesigner.Compiler
Description: IHP-specific schema file utilities
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.IDE.SchemaDesigner.Compiler
( writeSchema
) where

import IHP.Prelude
import IHP.Postgres.Compiler (compileSql, compareStatement)
import IHP.Postgres.Types (Statement)
import qualified Data.Text.IO as Text

-- | Write statements to the IHP schema file at 'Application/Schema.sql'
writeSchema :: [Statement] -> IO ()
writeSchema !statements = do
    let sortedStatements = sortBy compareStatement statements
    Text.writeFile "Application/Schema.sql" (compileSql sortedStatements)
