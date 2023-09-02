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
import qualified Test.IDE.SchemaDesigner.Controller.EnumValuesSpec
import qualified Test.IDE.SchemaDesigner.Controller.HelperSpec
import qualified Test.IDE.SchemaDesigner.Controller.ValidationSpec
import qualified Test.IDE.SchemaDesigner.SchemaOperationsSpec
import qualified Test.ValidationSupport.ValidateFieldSpec
import qualified Test.IDE.CodeGeneration.ControllerGenerator
import qualified Test.IDE.CodeGeneration.ViewGenerator
import qualified Test.IDE.CodeGeneration.MailGenerator
import qualified Test.IDE.CodeGeneration.JobGenerator
import qualified Test.HSX.QQSpec
import qualified Test.HSX.ParserSpec
import qualified Test.NameSupportSpec
import qualified Test.HaskellSupportSpec
import qualified Test.View.CSSFrameworkSpec
import qualified Test.View.FormSpec
import qualified Test.Controller.ContextSpec
import qualified Test.Controller.ParamSpec
import qualified Test.Controller.CookieSpec
import qualified Test.Controller.AccessDeniedSpec
import qualified Test.Controller.NotFoundSpec
import qualified Test.SchemaMigrationSpec
import qualified Test.ModelSupportSpec
import qualified Test.SchemaCompilerSpec
import qualified Test.QueryBuilderSpec
import qualified Test.RouterSupportSpec
import qualified Test.ViewSupportSpec
import qualified Test.ServerSideComponent.HtmlParserSpec
import qualified Test.ServerSideComponent.HtmlDiffSpec
import qualified Test.FileStorage.MimeTypesSpec
import qualified Test.DataSync.DynamicQueryCompiler
import qualified Test.IDE.CodeGeneration.MigrationGenerator
import qualified Test.PGListenerSpec
import qualified Test.SEO.Sitemap

main :: IO ()
main = hspec do
    Test.IDE.SchemaDesigner.CompilerSpec.tests
    Test.IDE.SchemaDesigner.ParserSpec.tests
    Test.IDE.SchemaDesigner.Controller.EnumValuesSpec.tests
    Test.IDE.SchemaDesigner.Controller.HelperSpec.tests
    Test.IDE.SchemaDesigner.Controller.ValidationSpec.tests
    Test.ValidationSupport.ValidateFieldSpec.tests
    Test.IDE.CodeGeneration.ControllerGenerator.tests
    Test.IDE.CodeGeneration.ViewGenerator.tests
    Test.IDE.CodeGeneration.MailGenerator.tests
    Test.IDE.CodeGeneration.JobGenerator.tests
    Test.HSX.QQSpec.tests
    Test.NameSupportSpec.tests
    Test.HaskellSupportSpec.tests
    Test.HSX.ParserSpec.tests
    Test.View.CSSFrameworkSpec.tests
    Test.View.FormSpec.tests
    Test.Controller.ContextSpec.tests
    Test.Controller.ParamSpec.tests
    Test.Controller.AccessDeniedSpec.tests
    Test.Controller.NotFoundSpec.tests
    Test.SchemaMigrationSpec.tests
    Test.ModelSupportSpec.tests
    Test.SchemaCompilerSpec.tests
    Test.QueryBuilderSpec.tests
    Test.RouterSupportSpec.tests
    Test.ViewSupportSpec.tests
    Test.ServerSideComponent.HtmlParserSpec.tests
    Test.ServerSideComponent.HtmlDiffSpec.tests
    Test.FileStorage.MimeTypesSpec.tests
    Test.DataSync.DynamicQueryCompiler.tests
    Test.IDE.SchemaDesigner.SchemaOperationsSpec.tests
    Test.IDE.CodeGeneration.MigrationGenerator.tests
    Test.Controller.CookieSpec.tests
    Test.PGListenerSpec.tests
    Test.SEO.Sitemap.tests
