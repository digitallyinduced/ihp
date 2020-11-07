module IHP.IDE.SchemaDesigner.Controller.Schema where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types

import IHP.IDE.SchemaDesigner.View.Schema.Code
import IHP.IDE.SchemaDesigner.View.Schema.Error
import IHP.IDE.SchemaDesigner.View.Schema.GeneratedCode

import IHP.IDE.SchemaDesigner.Parser
import IHP.IDE.SchemaDesigner.Compiler
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.SchemaDesigner.View.Layout (findStatementByName, findStatementByName, removeQuotes, replace)
import qualified IHP.SchemaCompiler as SchemaCompiler
import qualified System.Process as Process
import System.Exit
import IHP.IDE.SchemaDesigner.Parser (schemaFilePath)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

instance Controller SchemaController where
    action ShowCodeAction = do
        schema <- Text.readFile schemaFilePath
        error <- getSqlError
        render CodeView { .. }

    action SaveCodeAction = do
        let schema = param "schemaSql"
        Text.writeFile schemaFilePath schema
        redirectTo ShowCodeAction

    action PushToDbAction = do
        (exitCode, stdOut, stdErr) <- shell "make db"
        let output = stdErr <> "\n\n" <> stdOut
        case exitCode of
            ExitSuccess -> if "ERROR:" `Text.isInfixOf` stdErr
                then setErrorMessage output
                else setSuccessMessage "Recreated DB"
            ExitFailure code -> setErrorMessage output
        redirectTo TablesAction

    action DumpDbAction = do
        Process.system "make dumpdb"
        setSuccessMessage "Database State saved to Application/Fixtures.sql"
        redirectTo TablesAction

    action UpdateDbAction = do
        Process.system "make dumpdb"

        (exitCode, stdOut, stdErr) <- shell "make db"
        let output = stdErr <> "\n\n" <> stdOut
        case exitCode of
            ExitSuccess -> if "ERROR:" `Text.isInfixOf` stdErr
                then setErrorMessage output
                else setSuccessMessage "DB Update successful"
            ExitFailure code -> setErrorMessage output

        redirectTo TablesAction

    action ShowGeneratedCodeAction { statementName } = do
        statements <- readSchema
        let (Just statement) = findStatementByName statementName statements
        let generatedHaskellCode = SchemaCompiler.compileStatementPreview statements statement
        render GeneratedCodeView { .. }

readSchema ::
    ( ?context::ControllerContext
    , ?modelContext::ModelContext
    , ?theAction::controller
    ) => IO [Statement]
readSchema = parseSchemaSql >>= \case
        Left error -> do render ErrorView { error }; pure []
        Right statements -> pure statements

getSqlError :: IO (Maybe ByteString)
getSqlError = parseSchemaSql >>= \case
        Left error -> do pure (Just error)
        Right statements -> do pure Nothing

updateSchema ::
    ( ?context :: ControllerContext
    , ?modelContext::ModelContext
    , ?theAction::controller
    ) => ([Statement] -> [Statement]) -> IO ()
updateSchema updateFn = do
    statements <- readSchema
    let statements' = updateFn statements
    writeSchema statements'

shell :: String -> IO (ExitCode, Text, Text)
shell command = do
    (exitCode, stdOut, stdErr) <- Process.readCreateProcessWithExitCode (Process.shell command) ""
    pure (exitCode, cs stdOut, cs stdErr)
