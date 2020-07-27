{-|
Module: Test.IDE.CodeGeneration.ControllerGenerator
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.IDE.CodeGeneration.ControllerGenerator where

import Test.Hspec
import IHP.Prelude
import qualified IHP.IDE.CodeGen.ControllerGenerator as ControllerGenerator
import IHP.ViewPrelude (cs, plain)
import qualified Text.Megaparsec as Megaparsec
import IHP.IDE.CodeGen.Types


tests = do
    describe "Should generate Types" do
        it "should build a controller with single name" do
            builtPlan <- ControllerGenerator.buildPlan "pages" "Web"
            builtPlan `shouldBe` Right [
                CreateFile {filePath = "Web/Controller/Pages.hs", fileContent = "module Web.Controller.Pages where\n\nimport Web.Controller.Prelude\nimport Web.View.Pages.Index\nimport Web.View.Pages.New\nimport Web.View.Pages.Edit\nimport Web.View.Pages.Show\n\ninstance Controller PagesController where\n    action PagesAction = do\n        pages <- query @Page |> fetch\n        render IndexView { .. }\n\n    action NewPageAction = do\n        let page = newRecord\n        render NewView { .. }\n\n    action ShowPageAction { pageId } = do\n        page <- fetch pageId\n        render ShowView { .. }\n\n    action EditPageAction { pageId } = do\n        page <- fetch pageId\n        render EditView { .. }\n\n    action UpdatePageAction { pageId } = do\n        page <- fetch pageId\n        page\n            |> buildPage\n            |> ifValid \\case\n                Left page -> render EditView { .. }\n                Right page -> do\n                    page <- page |> updateRecord\n                    setSuccessMessage \"Page updated\"\n                    redirectTo EditPageAction { .. }\n\n    action CreatePageAction = do\n        let page = newRecord @Page\n        page\n            |> buildPage\n            |> ifValid \\case\n                Left page -> render NewView { .. } \n                Right page -> do\n                    page <- page |> createRecord\n                    setSuccessMessage \"Page created\"\n                    redirectTo PagesAction\n\n    action DeletePageAction { pageId } = do\n        page <- fetch pageId\n        deleteRecord page\n        setSuccessMessage \"Page deleted\"\n        redirectTo PagesAction\n\nbuildPage page = page\n    |> fill @'[]\n"}
                , AppendToFile {filePath = "Web/Routes.hs", fileContent = "instance AutoRoute PagesController\ntype instance ModelControllerMap WebApplication Page = PagesController\n\n"}
                , AppendToFile {filePath = "Web/Types.hs", fileContent = "\ndata PagesController\n    = PagesAction\n    | NewPageAction\n    | ShowPageAction { pageId :: !(Id Page) }\n    | CreatePageAction\n    | EditPageAction { pageId :: !(Id Page) }\n    | UpdatePageAction { pageId :: !(Id Page) }\n    | DeletePageAction { pageId :: !(Id Page) }\n    deriving (Eq, Show, Data)\n"}
                , AppendToMarker {marker = "-- Controller Imports", filePath = "Web/FrontController.hs", fileContent = "import Web.Controller.Pages"}
                , AppendToMarker {marker = "-- Generator Marker", filePath = "Web/FrontController.hs", fileContent = "        , parseRoute @PagesController"}
                , EnsureDirectory {directory = "Web/View/Pages"}
                , CreateFile {filePath = "Web/View/Pages/Index.hs", fileContent = "module Web.View.Pages.Index where\nimport Web.View.Prelude\n\ndata IndexView = IndexView { pages :: [Page] }\n\ninstance View IndexView ViewContext where\n    html IndexView { .. } = [hsx|\n        <nav>\n            <ol class=\"breadcrumb\">\n                <li class=\"breadcrumb-item active\"><a href={PagesAction}>Pages</a></li>\n            </ol>\n        </nav>\n        <h1>Index <a href={pathTo NewPageAction} class=\"btn btn-primary ml-4\">+ New</a></h1>\n        <div class=\"table-responsive\">\n            <table class=\"table\">\n                <thead>\n                    <tr>\n                        <th>Page</th>\n                        <th></th>\n                        <th></th>\n                        <th></th>\n                    </tr>\n                </thead>\n                <tbody>{forEach pages renderPage}</tbody>\n            </table>\n        </div>\n    |]\n\n\nrenderPage page = [hsx|\n    <tr>\n        <td>{page}</td>\n        <td><a href={ShowPageAction (get #id page)}>Show</a></td>\n        <td><a href={EditPageAction (get #id page)} class=\"text-muted\">Edit</a></td>\n        <td><a href={DeletePageAction (get #id page)} class=\"js-delete text-muted\">Delete</a></td>\n    </tr>\n|]\n"}
                , AddImport {filePath = "Web/Controller/Pages.hs", fileContent = "import Web.View.Pages.Index"}
                , EnsureDirectory {directory = "Web/View/Pages"}
                , CreateFile {filePath = "Web/View/Pages/New.hs", fileContent = "module Web.View.Pages.New where\nimport Web.View.Prelude\n\ndata NewView = NewView { page :: Page }\n\ninstance View NewView ViewContext where\n    html NewView { .. } = [hsx|\n        <nav>\n            <ol class=\"breadcrumb\">\n                <li class=\"breadcrumb-item\"><a href={PagesAction}>Pages</a></li>\n                <li class=\"breadcrumb-item active\">New Page</li>\n            </ol>\n        </nav>\n        <h1>New Page</h1>\n        {renderForm page}\n    |]\n\nrenderForm :: Page -> Html\nrenderForm page = formFor page [hsx|\n\n    {submitButton}\n|]\n"}
                , AddImport {filePath = "Web/Controller/Pages.hs", fileContent = "import Web.View.Pages.New"}
                , EnsureDirectory {directory = "Web/View/Pages"}
                , CreateFile {filePath = "Web/View/Pages/Show.hs", fileContent = "module Web.View.Pages.Show where\nimport Web.View.Prelude\n\ndata ShowView = ShowView { page :: Page }\n\ninstance View ShowView ViewContext where\n    html ShowView { .. } = [hsx|\n        <nav>\n            <ol class=\"breadcrumb\">\n                <li class=\"breadcrumb-item\"><a href={PagesAction}>Shows</a></li>\n                <li class=\"breadcrumb-item active\">Show Page</li>\n            </ol>\n        </nav>\n        <h1>Show Page</h1>\n    |]\n"}
                , AddImport {filePath = "Web/Controller/Pages.hs", fileContent = "import Web.View.Pages.Show"}
                , EnsureDirectory {directory = "Web/View/Pages"}
                , CreateFile {filePath = "Web/View/Pages/Edit.hs", fileContent = "module Web.View.Pages.Edit where\nimport Web.View.Prelude\n\ndata EditView = EditView { page :: Page }\n\ninstance View EditView ViewContext where\n    html EditView { .. } = [hsx|\n        <nav>\n            <ol class=\"breadcrumb\">\n                <li class=\"breadcrumb-item\"><a href={PagesAction}>Pages</a></li>\n                <li class=\"breadcrumb-item active\">Edit Page</li>\n            </ol>\n        </nav>\n        <h1>Edit Page</h1>\n        {renderForm page}\n    |]\n\nrenderForm :: Page -> Html\nrenderForm page = formFor page [hsx|\n\n    {submitButton}\n|]\n"}
                , AddImport {filePath = "Web/Controller/Pages.hs", fileContent = "import Web.View.Pages.Edit"}
                ]
                