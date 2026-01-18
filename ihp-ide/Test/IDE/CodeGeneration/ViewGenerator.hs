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
import IHP.Postgres.Types
import IHP.NameSupport
import Test.IDE.SchemaDesigner.ParserSpec (col, table)


tests = do
    describe "View Generator Tests:" do
        let schema = [
                    StatementCreateTable (table "pages") {
                        columns = [
                            (col "id" PUUID) { defaultValue = Just (CallExpression "uuidv7" []), notNull = True }
                        ]
                        , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        }
                    ]
        it "should build a view with name \"EditView\"" do
            let rawViewName = "EditView"
            let viewName = tableNameToViewName rawViewName
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



        it "should build a view with name \"edit_view\"" do
            let rawViewName = "edit_view"
            let viewName = tableNameToViewName rawViewName
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


        it "should build a view with name \"editView\"" do
            let rawViewName = "editView"
            let viewName = tableNameToViewName rawViewName
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
            let rawViewName = "Edit"
            let viewName = tableNameToViewName rawViewName
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
            let rawViewName = "Test"
            let viewName = tableNameToViewName rawViewName
            let rawControllerName = "Pages"
            let controllerName = tableNameToControllerName rawControllerName
            let modelName = tableNameToModelName rawControllerName
            let applicationName = "Web"
            let paginationEnabled = False
            let config = ViewGenerator.ViewConfig { .. }
            let builtPlan = ViewGenerator.buildPlan' schema config

            builtPlan `shouldBe`
                [ EnsureDirectory {directory = "Web/View/Pages"},CreateFile {filePath = "Web/View/Pages/Test.hs", fileContent = "module Web.View.Pages.Test where\nimport Web.View.Prelude\ndata TestView = TestView\n\ninstance View TestView where\n    html TestView { .. } = [hsx|\n        {breadcrumb}\n        <h1>TestView</h1>\n        |]\n            where\n                breadcrumb = renderBreadcrumb\n                                [ breadcrumbLink \"Tests\" PagesAction\n                                , breadcrumbText \"TestView\"\n                                ]"},AddImport {filePath = "Web/Controller/Pages.hs", fileContent = "import Web.View.Pages.Test"}
                ]

        it "should generate type-aware form fields for a rich schema" do
            let richSchema = [
                        StatementCreateTable (table "projects") {
                            columns = [
                                (col "id" PUUID) { defaultValue = Just (CallExpression "uuid_generate_v4" []), notNull = True }
                                , (col "title" PText) { notNull = True }
                                , (col "is_active" PBoolean) { notNull = True }
                                , (col "budget" PInt) { notNull = False }
                                , (col "started_on" PDate) { notNull = False }
                                , (col "user_id" PUUID) { notNull = True }
                            ]
                            , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                            , constraints = [
                                ForeignKeyConstraint { name = Nothing, columnName = "user_id", referenceTable = "users", referenceColumn = Nothing, onDelete = Nothing }
                              ]
                        }
                    ]
            let viewName = "NewView"
            let rawControllerName = "projects"
            let controllerName = tableNameToControllerName rawControllerName
            let modelName = tableNameToModelName rawControllerName
            let applicationName = "Web"
            let paginationEnabled = False
            let config = ViewGenerator.ViewConfig { .. }
            let builtPlan = ViewGenerator.buildPlan' richSchema config
            let (EnsureDirectory {}):((CreateFile { fileContent = newViewContent }):_) = builtPlan
            -- Type-aware form fields
            (cs newViewContent :: String) `shouldContain` "{(textField #title)}"
            (cs newViewContent :: String) `shouldContain` "{(checkboxField #isActive)}"
            (cs newViewContent :: String) `shouldContain` "{(numberField #budget)}"
            (cs newViewContent :: String) `shouldContain` "{(dateField #startedOn)}"
            -- FK field should have a TODO comment
            (cs newViewContent :: String) `shouldContain` "{- userId needs to be a selectField -}"

        it "should generate dl-based show view for a rich schema" do
            let richSchema = [
                        StatementCreateTable (table "projects") {
                            columns = [
                                (col "id" PUUID) { defaultValue = Just (CallExpression "uuid_generate_v4" []), notNull = True }
                                , (col "title" PText) { notNull = True }
                                , (col "is_active" PBoolean) { notNull = True }
                            ]
                            , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        }
                    ]
            let viewName = "ShowView"
            let rawControllerName = "projects"
            let controllerName = tableNameToControllerName rawControllerName
            let modelName = tableNameToModelName rawControllerName
            let applicationName = "Web"
            let paginationEnabled = False
            let config = ViewGenerator.ViewConfig { .. }
            let builtPlan = ViewGenerator.buildPlan' richSchema config
            let (EnsureDirectory {}):((CreateFile { fileContent = showViewContent }):_) = builtPlan
            (cs showViewContent :: String) `shouldContain` "<dl>"
            (cs showViewContent :: String) `shouldContain` "<dt>Title</dt><dd>{project.title}</dd>"
            (cs showViewContent :: String) `shouldContain` "<dt>Is Active</dt><dd>{project.isActive}</dd>"

        it "should generate column-based index view for a rich schema" do
            let richSchema = [
                        StatementCreateTable (table "projects") {
                            columns = [
                                (col "id" PUUID) { defaultValue = Just (CallExpression "uuid_generate_v4" []), notNull = True }
                                , (col "title" PText) { notNull = True }
                                , (col "is_active" PBoolean) { notNull = True }
                            ]
                            , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        }
                    ]
            let viewName = "IndexView"
            let rawControllerName = "projects"
            let controllerName = tableNameToControllerName rawControllerName
            let modelName = tableNameToModelName rawControllerName
            let applicationName = "Web"
            let paginationEnabled = False
            let config = ViewGenerator.ViewConfig { .. }
            let builtPlan = ViewGenerator.buildPlan' richSchema config
            let (EnsureDirectory {}):((CreateFile { fileContent = indexViewContent }):_) = builtPlan
            (cs indexViewContent :: String) `shouldContain` "<th>Title</th>"
            (cs indexViewContent :: String) `shouldContain` "<th>Is Active</th>"
            (cs indexViewContent :: String) `shouldContain` "<td>{project.title}</td>"
            (cs indexViewContent :: String) `shouldContain` "<td>{project.isActive}</td>"