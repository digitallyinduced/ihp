{-|
Module: Test.IDE.SchemaDesigner.CompilerSpec
Description: Entrypoint to the hspec Testsuite
Copyright: (c) digitally induced GmbH, 2020

When in the IHP directory, you can run this file like:

 > nix-shell
 > ghci
 > :l Test/Main.hs
 > main

-}
module Main where

import Test.Hspec
import IHP.Prelude

import qualified Test.IDE.SchemaDesigner.CompilerSpec
import qualified Test.IDE.SchemaDesigner.ParserSpec
import qualified Test.ValidationSupport.ValidateFieldSpec
import qualified Test.IDE.CodeGeneration.ControllerGenerator
import qualified Test.IDE.CodeGeneration.ViewGenerator
import qualified Test.IDE.CodeGeneration.MailGenerator
import qualified Test.IDE.CodeGeneration.JobGenerator
import qualified Test.HtmlSupport.QQSpec
import qualified Test.HtmlSupport.ParserSpec
import qualified Test.NameSupportSpec
import qualified Test.HaskellSupportSpec
import qualified Test.View.CSSFrameworkSpec
import qualified Test.Controller.ContextSpec
import qualified Test.Controller.ParamSpec
import qualified Test.SchemaMigrationSpec
import qualified Test.ModelSupportSpec
import qualified Test.SchemaCompilerSpec

main :: IO ()
main = hspec do
    Test.IDE.SchemaDesigner.CompilerSpec.tests
    Test.IDE.SchemaDesigner.ParserSpec.tests
    Test.ValidationSupport.ValidateFieldSpec.tests
    Test.IDE.CodeGeneration.ControllerGenerator.tests
    Test.IDE.CodeGeneration.ViewGenerator.tests
    Test.IDE.CodeGeneration.MailGenerator.tests
    Test.IDE.CodeGeneration.JobGenerator.tests
    Test.HtmlSupport.QQSpec.tests
    Test.NameSupportSpec.tests
    Test.HaskellSupportSpec.tests
    Test.HtmlSupport.ParserSpec.tests
    Test.View.CSSFrameworkSpec.tests
    Test.Controller.ContextSpec.tests
    Test.Controller.ParamSpec.tests
    Test.SchemaMigrationSpec.tests
    Test.ModelSupportSpec.tests
    Test.SchemaCompilerSpec.tests