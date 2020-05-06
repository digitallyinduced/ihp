module TurboHaskell.IDE.SchemaDesigner.Controller.Schema where

import TurboHaskell.ControllerPrelude
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.ViewContext

import TurboHaskell.IDE.SchemaDesigner.View.Schema.Code
import TurboHaskell.IDE.SchemaDesigner.View.Schema.Error

import TurboHaskell.IDE.SchemaDesigner.Parser
import TurboHaskell.IDE.SchemaDesigner.Compiler
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.SchemaDesigner.View.Layout (findTableByName, findEnumByName, removeQuotes, replace)
import qualified TurboHaskell.SchemaCompiler as SchemaCompiler
import qualified System.Process as Process
import TurboHaskell.IDE.SchemaDesigner.Parser (schemaFilePath)
import qualified Data.Text.IO as Text

instance Controller SchemaController where
    
    action ShowCodeAction = do
        schema <- Text.readFile schemaFilePath
        error <- getSqlError
        putStrLn (tshow (fromMaybe "" (error)))
        render CodeView { .. }

    action SaveCodeAction = do
        let schema = param "schemaSql"
        Text.writeFile schemaFilePath schema
        redirectTo ShowCodeAction


    -- DB
    action PushToDbAction = do
        Process.system "make db"
        redirectTo TablesAction

readSchema :: _ => _
readSchema = parseSchemaSql >>= \case
        Left error -> do render ErrorView { error }; pure []
        Right statements -> pure statements

getSqlError :: _ => IO (Maybe ByteString)
getSqlError = parseSchemaSql >>= \case
        Left error -> do pure (Just error)
        Right statements -> do pure Nothing

updateSchema :: _ => _
updateSchema updateFn = do
    statements <- readSchema
    let statements' = updateFn statements
    writeSchema statements'