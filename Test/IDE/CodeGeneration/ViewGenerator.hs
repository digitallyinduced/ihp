{-|
Module: Test.IDE.CodeGeneration.ViewGenerator
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.IDE.CodeGeneration.ViewGenerator where

import Test.Hspec
import IHP.Prelude
import qualified IHP.IDE.CodeGen.ViewGenerator as ViewGenerator
import IHP.ViewPrelude (cs, plain)
import qualified Text.Megaparsec as Megaparsec
import IHP.IDE.CodeGen.Types
import IHP.IDE.SchemaDesigner.Types
import IHP.NameSupport


tests = do
    describe "View Generator Tests:" do
        let schema = [
                    CreateTable { 
                        name = "pages"
                        , columns = [
                            Column
                                { name = "id"
                                , columnType = PUUID
                                , primaryKey = True
                                , defaultValue = Just (CallExpression "uuid_generate_v4" [])
                                , notNull = True
                                , isUnique = False
                                }
                        ]
                        }
                    ]
        it "should build a view with name \"EditView\"" do
            let viewName = "EditView"
            let rawControllerName = "Pages"
            let controllerName = tableNameToControllerName rawControllerName
            let modelName = tableNameToModelName rawControllerName
            let applicationName = "Web"
            let config = ViewGenerator.ViewConfig { .. }
            let builtPlan = ViewGenerator.buildPlan' schema config

            builtPlan `shouldBe` [
                  EnsureDirectory {directory = "Web/View/Pages"}
                , CreateFile {filePath = "Web/View/Pages/Edit.hs", fileContent = "module Web.View.Pages.Edit where\nimport Web.View.Prelude\n\ndata EditView = EditView { page :: Page }\n\ninstance View EditView ViewContext where\n    html EditView { .. } = [hsx|\n        <nav>\n            <ol class=\"breadcrumb\">\n                <li class=\"breadcrumb-item\"><a href={PagesAction}>Pages</a></li>\n                <li class=\"breadcrumb-item active\">Edit Page</li>\n            </ol>\n        </nav>\n        <h1>Edit Page</h1>\n        {renderForm page}\n    |]\n\nrenderForm :: Page -> Html\nrenderForm page = formFor page [hsx|\n\n    {submitButton}\n|]\n"}
                , AddImport {filePath = "Web/Controller/Pages.hs", fileContent = "import Web.View.Pages.Edit"}
                ]

        it "should build a view with name \"Edit\"" do
            let viewName = "Edit"
            let rawControllerName = "Pages"
            let controllerName = tableNameToControllerName rawControllerName
            let modelName = tableNameToModelName rawControllerName
            let applicationName = "Web"
            let config = ViewGenerator.ViewConfig { .. }
            let builtPlan = ViewGenerator.buildPlan' schema config

            builtPlan `shouldBe` [
                  EnsureDirectory {directory = "Web/View/Pages"}
                , CreateFile {filePath = "Web/View/Pages/Edit.hs", fileContent = "module Web.View.Pages.Edit where\nimport Web.View.Prelude\n\ndata EditView = EditView { page :: Page }\n\ninstance View EditView ViewContext where\n    html EditView { .. } = [hsx|\n        <nav>\n            <ol class=\"breadcrumb\">\n                <li class=\"breadcrumb-item\"><a href={PagesAction}>Pages</a></li>\n                <li class=\"breadcrumb-item active\">Edit Page</li>\n            </ol>\n        </nav>\n        <h1>Edit Page</h1>\n        {renderForm page}\n    |]\n\nrenderForm :: Page -> Html\nrenderForm page = formFor page [hsx|\n\n    {submitButton}\n|]\n"}
                , AddImport {filePath = "Web/Controller/Pages.hs", fileContent = "import Web.View.Pages.Edit"}
                ]

        it "should build a view with name \"Test\"" do
            let viewName = "Test"
            let rawControllerName = "Pages"
            let controllerName = tableNameToControllerName rawControllerName
            let modelName = tableNameToModelName rawControllerName
            let applicationName = "Web"
            let config = ViewGenerator.ViewConfig { .. }
            let builtPlan = ViewGenerator.buildPlan' schema config

            builtPlan `shouldBe` [
                  EnsureDirectory {directory = "Web/View/Pages"}
                , CreateFile {filePath = "Web/View/Pages/Test.hs", fileContent = "module Web.View.Pages.Test where\nimport Web.View.Prelude\n\ndata TestView = TestView\n\ninstance View TestView ViewContext where\n    html TestView { .. } = [hsx|\n        <nav>\n            <ol class=\"breadcrumb\">\n                <li class=\"breadcrumb-item\"><a href={PagesAction}>Tests</a></li>\n                <li class=\"breadcrumb-item active\">TestView</li>\n            </ol>\n        </nav>\n        <h1>TestView</h1>\n    |]\n"}
                , AddImport {filePath = "Web/Controller/Pages.hs", fileContent = "import Web.View.Pages.Test"}
                ]