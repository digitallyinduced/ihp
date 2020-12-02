module IHP.IDE.SchemaDesigner.Controller.Schema where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types

import IHP.IDE.SchemaDesigner.View.Schema.Code
import IHP.IDE.SchemaDesigner.View.Schema.Error
import IHP.IDE.SchemaDesigner.View.Schema.GeneratedCode
import IHP.IDE.SchemaDesigner.View.Schema.SchemaUpdateFailed

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
import IHP.IDE.SchemaDesigner.View.Layout
import IHP.IDE.SchemaDesigner.Controller.Tables
import IHP.IDE.SchemaDesigner.Controller.Helper

instance Controller SchemaController where
    beforeAction = setLayout schemaDesignerLayout

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

        let isError = case exitCode of
                ExitSuccess -> "ERROR:" `Text.isInfixOf` stdErr
                ExitFailure _ -> True

        if isError
            then do
                setModal SchemaUpdateFailedView { .. }
                jumpToAction TablesAction
            else do
                setSuccessMessage "Recreated DB"
                redirectTo TablesAction

    action DumpDbAction = do
        Process.system "make dumpdb"
        setSuccessMessage "Database State saved to Application/Fixtures.sql"
        redirectTo TablesAction

    action UpdateDbAction = do
        Process.system "make dumpdb"

        (exitCode, stdOut, stdErr) <- shell "make db"
        let output = stdErr <> "\n\n" <> stdOut
        let isError = case exitCode of
                ExitSuccess -> "ERROR:" `Text.isInfixOf` stdErr
                ExitFailure _ -> True

        if isError
            then do
                setModal SchemaUpdateFailedView { .. }
                jumpToAction TablesAction
            else do
                setSuccessMessage "DB Update successful"
                redirectTo TablesAction

    action ShowGeneratedCodeAction { statementName } = do
        statements <- readSchema
        let (Just statement) = findStatementByName statementName statements
        let generatedHaskellCode = SchemaCompiler.compileStatementPreview statements statement
        render GeneratedCodeView { .. }

shell :: String -> IO (ExitCode, Text, Text)
shell command = do
    (exitCode, stdOut, stdErr) <- Process.readCreateProcessWithExitCode (Process.shell command) ""
    pure (exitCode, cs stdOut, cs stdErr)
