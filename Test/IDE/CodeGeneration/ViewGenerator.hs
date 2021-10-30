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
                    StatementCreateTable CreateTable {
                        name = "pages"
                        , columns = [
                            Column
                                { name = "id"
                                , columnType = PUUID
                                , defaultValue = Just (CallExpression "uuid_generate_v4" [])
                                , notNull = True
                                , isUnique = False
                                }
                        ]
                        , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        , constraints = []
                        }
                    ]
        it "should build a view with name \"EditView\"" do
            let viewName = "EditView"
            let rawControllerName = "Pages"
            let controllerName = tableNameToControllerName rawControllerName
            let modelName = tableNameToModelName rawControllerName
            let applicationName = "Web"
            let paginationEnabled = False
            let config = ViewGenerator.ViewConfig { .. }
            let builtPlan = ViewGenerator.buildPlan' schema config

            builtPlan `shouldBe`
                [ EnsureDirectory {directory = "Web/View/Pages"},CreateFile {filePath = "Web/View/Pages/Edit.hs", fileContent = "module Web.View.Pages.Edit where\nimport Web.View.Prelude\n\ndata EditView = EditView { page :: Page }\n\ninstance View EditView where\n    html EditView { .. } = [hsx|\n        {breadcrumb}\n        <h1>Edit Page</h1>\n        {renderForm page}\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                [ breadcrumbLink \"Pages\" PagesAction\n                , breadcrumbText \"Edit Page\"\n                ]\n\nrenderForm :: Page -> Html\nrenderForm page = formFor page [hsx|\n    \n    {submitButton}\n\n|]"},AddImport {filePath = "Web/Controller/Pages.hs", fileContent = "import Web.View.Pages.Edit"}
                ]



        it "should build a view with name \"Edit\"" do
            let viewName = "Edit"
            let rawControllerName = "Pages"
            let controllerName = tableNameToControllerName rawControllerName
            let modelName = tableNameToModelName rawControllerName
            let applicationName = "Web"
            let paginationEnabled = False
            let config = ViewGenerator.ViewConfig { .. }
            let builtPlan = ViewGenerator.buildPlan' schema config

            builtPlan `shouldBe`
                [ EnsureDirectory {directory = "Web/View/Pages"},CreateFile {filePath = "Web/View/Pages/Edit.hs", fileContent = "module Web.View.Pages.Edit where\nimport Web.View.Prelude\n\ndata EditView = EditView { page :: Page }\n\ninstance View EditView where\n    html EditView { .. } = [hsx|\n        {breadcrumb}\n        <h1>Edit Page</h1>\n        {renderForm page}\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                [ breadcrumbLink \"Pages\" PagesAction\n                , breadcrumbText \"Edit Page\"\n                ]\n\nrenderForm :: Page -> Html\nrenderForm page = formFor page [hsx|\n    \n    {submitButton}\n\n|]"},AddImport {filePath = "Web/Controller/Pages.hs", fileContent = "import Web.View.Pages.Edit"}
                ]


        it "should build a view with name \"Test\"" do
            let viewName = "Test"
            let rawControllerName = "Pages"
            let controllerName = tableNameToControllerName rawControllerName
            let modelName = tableNameToModelName rawControllerName
            let applicationName = "Web"
            let paginationEnabled = False
            let config = ViewGenerator.ViewConfig { .. }
            let builtPlan = ViewGenerator.buildPlan' schema config

            builtPlan `shouldBe`
                [ EnsureDirectory {directory = "Web/View/Pages"},CreateFile {filePath = "Web/View/Pages/Test.hs", fileContent = "module Web.View.Pages.Test where\nimport Web.View.Prelude\ndata TestView = {TestView}\n\ninstance View TestView where\n    html TestView { .. } = [hsx|\n        {breadcrumb}\n        <h1>TestView</h1>\n        |]\n            where\n                breadcrumb = renderBreadcrumb\n                                [ breadcrumbLink \"Tests\" PagesAction\n                                , breadcrumbText \"TestView\"\n                                ]"},AddImport {filePath = "Web/Controller/Pages.hs", fileContent = "import Web.View.Pages.Test"}
                ]