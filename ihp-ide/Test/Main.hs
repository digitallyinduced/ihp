module Main where

import Test.Hspec
import IHP.Prelude

import qualified Test.IDE.SchemaDesigner.CompilerSpec
import qualified Test.IDE.SchemaDesigner.ParserSpec
import qualified Test.IDE.SchemaDesigner.Controller.EnumValuesSpec
import qualified Test.IDE.SchemaDesigner.Controller.HelperSpec
import qualified Test.IDE.SchemaDesigner.Controller.ValidationSpec
import qualified Test.IDE.SchemaDesigner.SchemaOperationsSpec
import qualified Test.IDE.CodeGeneration.ControllerGenerator
import qualified Test.IDE.CodeGeneration.ViewGenerator
import qualified Test.IDE.CodeGeneration.MailGenerator
import qualified Test.IDE.CodeGeneration.JobGenerator
import qualified Test.IDE.CodeGeneration.MigrationGenerator
import qualified Test.SchemaCompilerSpec

main :: IO ()
main = hspec do
    Test.IDE.SchemaDesigner.CompilerSpec.tests
    Test.IDE.SchemaDesigner.ParserSpec.tests
    Test.IDE.SchemaDesigner.Controller.EnumValuesSpec.tests
    Test.IDE.SchemaDesigner.Controller.HelperSpec.tests
    Test.IDE.SchemaDesigner.Controller.ValidationSpec.tests
    Test.IDE.CodeGeneration.ControllerGenerator.tests
    Test.IDE.CodeGeneration.ViewGenerator.tests
    Test.IDE.CodeGeneration.MailGenerator.tests
    Test.IDE.CodeGeneration.JobGenerator.tests
    Test.IDE.SchemaDesigner.SchemaOperationsSpec.tests
    Test.IDE.CodeGeneration.MigrationGenerator.tests
    Test.SchemaCompilerSpec.tests