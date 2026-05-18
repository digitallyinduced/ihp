module Main where

import Test.Hspec
import IHP.Prelude

import qualified IDE.SchemaDesigner.CompilerSpec
import qualified IDE.SchemaDesigner.ParserSpec
import qualified IDE.SchemaDesigner.Controller.EnumValuesSpec
import qualified IDE.SchemaDesigner.Controller.HelperSpec
import qualified IDE.SchemaDesigner.Controller.ValidationSpec
import qualified IDE.SchemaDesigner.SchemaOperationsSpec
import qualified IDE.CodeGeneration.ControllerGenerator
import qualified IDE.CodeGeneration.ViewGenerator
import qualified IDE.CodeGeneration.MailGenerator
import qualified IDE.CodeGeneration.JobGenerator
import qualified IDE.CodeGeneration.MigrationGenerator
import qualified IDE.SplitModeSpec
import qualified IDE.WorkerSignalSpec
import qualified SchemaCompilerSpec
import qualified IDE.ToolServer.MiddlewareSpec
import qualified IDE.Logs.ControllerSpec
import qualified ServerSpec

main :: IO ()
main = hspec do
    IDE.SchemaDesigner.CompilerSpec.tests
    IDE.SchemaDesigner.ParserSpec.tests
    IDE.SchemaDesigner.Controller.EnumValuesSpec.tests
    IDE.SchemaDesigner.Controller.HelperSpec.tests
    IDE.SchemaDesigner.Controller.ValidationSpec.tests
    IDE.CodeGeneration.ControllerGenerator.tests
    IDE.CodeGeneration.ViewGenerator.tests
    IDE.CodeGeneration.MailGenerator.tests
    IDE.CodeGeneration.JobGenerator.tests
    IDE.SchemaDesigner.SchemaOperationsSpec.tests
    IDE.CodeGeneration.MigrationGenerator.tests
    IDE.SplitModeSpec.tests
    IDE.WorkerSignalSpec.tests
    SchemaCompilerSpec.tests
    IDE.ToolServer.MiddlewareSpec.tests
    IDE.Logs.ControllerSpec.tests
    ServerSpec.tests
