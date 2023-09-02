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
import IHP.IDE.SchemaDesigner.Types
import IHP.NameSupport

tests = do
    describe "Controller Generator Tests:" do
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
                                , generator = Nothing
                                }
                        ]
                        , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        , constraints = []
                        , unlogged = False
                    },
                    StatementCreateTable CreateTable {
                        name = "people"
                        , columns = [
                            Column
                                { name = "id"
                                , columnType = PUUID
                                , defaultValue = Just (CallExpression "uuid_generate_v4" [])
                                , notNull = True
                                , isUnique = False
                                , generator = Nothing
                                }
                                ,
                            Column
                                { name = "name"
                                , columnType = PText
                                , defaultValue = Nothing
                                , notNull = True
                                , isUnique = False
                                , generator = Nothing
                                }
                                ,
                            Column
                                { name = "email"
                                , columnType = PText
                                , defaultValue = Nothing
                                , notNull = True
                                , isUnique = False
                                , generator = Nothing
                                }
                        ]
                        , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        , constraints = []
                        , unlogged = False
                    }
                ]

        it "should build a controller with name \"pages\"" do
            let rawControllerName = "pages"
            let controllerName = tableNameToControllerName rawControllerName
            let modelName = tableNameToModelName rawControllerName
            let applicationName = "Web"
            let pagination = False
            let builtPlan = ControllerGenerator.buildPlan' schema applicationName controllerName modelName pagination

            builtPlan `shouldBe`
                [ CreateFile {filePath = "Web/Controller/Pages.hs", fileContent = "module Web.Controller.Pages where\n\nimport Web.Controller.Prelude\nimport Web.View.Pages.Index\nimport Web.View.Pages.New\nimport Web.View.Pages.Edit\nimport Web.View.Pages.Show\n\ninstance Controller PagesController where\n    action PagesAction = do\n        pages <- query @Page |> fetch\n        render IndexView { .. }\n\n    action NewPageAction = do\n        let page = newRecord\n        render NewView { .. }\n\n    action ShowPageAction { pageId } = do\n        page <- fetch pageId\n        render ShowView { .. }\n\n    action EditPageAction { pageId } = do\n        page <- fetch pageId\n        render EditView { .. }\n\n    action UpdatePageAction { pageId } = do\n        page <- fetch pageId\n        page\n            |> buildPage\n            |> ifValid \\case\n                Left page -> render EditView { .. }\n                Right page -> do\n                    page <- page |> updateRecord\n                    setSuccessMessage \"Page updated\"\n                    redirectTo EditPageAction { .. }\n\n    action CreatePageAction = do\n        let page = newRecord @Page\n        page\n            |> buildPage\n            |> ifValid \\case\n                Left page -> render NewView { .. } \n                Right page -> do\n                    page <- page |> createRecord\n                    setSuccessMessage \"Page created\"\n                    redirectTo PagesAction\n\n    action DeletePageAction { pageId } = do\n        page <- fetch pageId\n        deleteRecord page\n        setSuccessMessage \"Page deleted\"\n        redirectTo PagesAction\n\nbuildPage page = page\n    |> fill @'[]\n"}
                , AppendToFile {filePath = "Web/Routes.hs", fileContent = "\ninstance AutoRoute PagesController\n\n"}
                , AppendToFile {filePath = "Web/Types.hs", fileContent = "\ndata PagesController\n    = PagesAction\n    | NewPageAction\n    | ShowPageAction { pageId :: !(Id Page) }\n    | CreatePageAction\n    | EditPageAction { pageId :: !(Id Page) }\n    | UpdatePageAction { pageId :: !(Id Page) }\n    | DeletePageAction { pageId :: !(Id Page) }\n    deriving (Eq, Show, Data)\n"}
                , AppendToMarker {marker = "-- Controller Imports", filePath = "Web/FrontController.hs", fileContent = "import Web.Controller.Pages"}
                , AppendToMarker {marker = "-- Generator Marker", filePath = "Web/FrontController.hs", fileContent = "        , parseRoute @PagesController"}
                , EnsureDirectory {directory = "Web/View/Pages"}
                , CreateFile {filePath = "Web/View/Pages/Index.hs", fileContent = "module Web.View.Pages.Index where\nimport Web.View.Prelude\n\ndata IndexView = IndexView { pages :: [Page] }\n\ninstance View IndexView where\n    html IndexView { .. } = [hsx|\n        {breadcrumb}\n\n        <h1>Index<a href={pathTo NewPageAction} class=\"btn btn-primary ms-4\">+ New</a></h1>\n        <div class=\"table-responsive\">\n            <table class=\"table\">\n                <thead>\n                    <tr>\n                        <th>Page</th>\n                        <th></th>\n                        <th></th>\n                        <th></th>\n                    </tr>\n                </thead>\n                <tbody>{forEach pages renderPage}</tbody>\n            </table>\n            \n        </div>\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                [ breadcrumbLink \"Pages\" PagesAction\n                ]\n\nrenderPage :: Page -> Html\nrenderPage page = [hsx|\n    <tr>\n        <td>{page}</td>\n        <td><a href={ShowPageAction page.id}>Show</a></td>\n        <td><a href={EditPageAction page.id} class=\"text-muted\">Edit</a></td>\n        <td><a href={DeletePageAction page.id} class=\"js-delete text-muted\">Delete</a></td>\n    </tr>\n|]"}
                , AddImport {filePath = "Web/Controller/Pages.hs", fileContent = "import Web.View.Pages.Index"}
                , EnsureDirectory {directory = "Web/View/Pages"}
                , CreateFile {filePath = "Web/View/Pages/New.hs", fileContent = "module Web.View.Pages.New where\nimport Web.View.Prelude\n\ndata NewView = NewView { page :: Page }\n\ninstance View NewView where\n    html NewView { .. } = [hsx|\n        {breadcrumb}\n        <h1>New Page</h1>\n        {renderForm page}\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                [ breadcrumbLink \"Pages\" PagesAction\n                , breadcrumbText \"New Page\"\n                ]\n\nrenderForm :: Page -> Html\nrenderForm page = formFor page [hsx|\n    \n    {submitButton}\n\n|]"}
                , AddImport {filePath = "Web/Controller/Pages.hs", fileContent = "import Web.View.Pages.New"}
                , EnsureDirectory {directory = "Web/View/Pages"}
                , CreateFile {filePath = "Web/View/Pages/Show.hs", fileContent = "module Web.View.Pages.Show where\nimport Web.View.Prelude\n\ndata ShowView = ShowView { page :: Page }\n\ninstance View ShowView where\n    html ShowView { .. } = [hsx|\n        {breadcrumb}\n        <h1>Show Page</h1>\n        <p>{page}</p>\n\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                            [ breadcrumbLink \"Pages\" PagesAction\n                            , breadcrumbText \"Show Page\"\n                            ]"}
                , AddImport {filePath = "Web/Controller/Pages.hs", fileContent = "import Web.View.Pages.Show"}
                , EnsureDirectory {directory = "Web/View/Pages"}
                , CreateFile {filePath = "Web/View/Pages/Edit.hs", fileContent = "module Web.View.Pages.Edit where\nimport Web.View.Prelude\n\ndata EditView = EditView { page :: Page }\n\ninstance View EditView where\n    html EditView { .. } = [hsx|\n        {breadcrumb}\n        <h1>Edit Page</h1>\n        {renderForm page}\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                [ breadcrumbLink \"Pages\" PagesAction\n                , breadcrumbText \"Edit Page\"\n                ]\n\nrenderForm :: Page -> Html\nrenderForm page = formFor page [hsx|\n    \n    {submitButton}\n\n|]"}
                , AddImport {filePath = "Web/Controller/Pages.hs", fileContent = "import Web.View.Pages.Edit"}
                ]

        it "should build a controller with name \"page\"" do
            let rawControllerName = "page"
            let controllerName = tableNameToControllerName rawControllerName
            let modelName = tableNameToModelName rawControllerName
            let applicationName = "Web"
            let pagination = False
            let builtPlan = ControllerGenerator.buildPlan' schema applicationName controllerName modelName pagination

            builtPlan `shouldBe`
                [ CreateFile {filePath = "Web/Controller/Page.hs", fileContent = "module Web.Controller.Page where\n\nimport Web.Controller.Prelude\nimport Web.View.Page.Index\nimport Web.View.Page.New\nimport Web.View.Page.Edit\nimport Web.View.Page.Show\n\ninstance Controller PageController where\n    action PagesAction = do\n        page <- query @Page |> fetch\n        render IndexView { .. }\n\n    action NewPageAction = do\n        let page = newRecord\n        render NewView { .. }\n\n    action ShowPageAction { pageId } = do\n        page <- fetch pageId\n        render ShowView { .. }\n\n    action EditPageAction { pageId } = do\n        page <- fetch pageId\n        render EditView { .. }\n\n    action UpdatePageAction { pageId } = do\n        page <- fetch pageId\n        page\n            |> buildPage\n            |> ifValid \\case\n                Left page -> render EditView { .. }\n                Right page -> do\n                    page <- page |> updateRecord\n                    setSuccessMessage \"Page updated\"\n                    redirectTo EditPageAction { .. }\n\n    action CreatePageAction = do\n        let page = newRecord @Page\n        page\n            |> buildPage\n            |> ifValid \\case\n                Left page -> render NewView { .. } \n                Right page -> do\n                    page <- page |> createRecord\n                    setSuccessMessage \"Page created\"\n                    redirectTo PagesAction\n\n    action DeletePageAction { pageId } = do\n        page <- fetch pageId\n        deleteRecord page\n        setSuccessMessage \"Page deleted\"\n        redirectTo PagesAction\n\nbuildPage page = page\n    |> fill @'[]\n"}
                , AppendToFile {filePath = "Web/Routes.hs", fileContent = "\ninstance AutoRoute PageController\n\n"}
                , AppendToFile {filePath = "Web/Types.hs", fileContent = "\ndata PageController\n    = PagesAction\n    | NewPageAction\n    | ShowPageAction { pageId :: !(Id Page) }\n    | CreatePageAction\n    | EditPageAction { pageId :: !(Id Page) }\n    | UpdatePageAction { pageId :: !(Id Page) }\n    | DeletePageAction { pageId :: !(Id Page) }\n    deriving (Eq, Show, Data)\n"}
                , AppendToMarker {marker = "-- Controller Imports", filePath = "Web/FrontController.hs", fileContent = "import Web.Controller.Page"}
                , AppendToMarker {marker = "-- Generator Marker", filePath = "Web/FrontController.hs", fileContent = "        , parseRoute @PageController"}
                , EnsureDirectory {directory = "Web/View/Page"}
                , CreateFile {filePath = "Web/View/Page/Index.hs", fileContent = "module Web.View.Page.Index where\nimport Web.View.Prelude\n\ndata IndexView = IndexView { page :: [Page] }\n\ninstance View IndexView where\n    html IndexView { .. } = [hsx|\n        {breadcrumb}\n\n        <h1>Index<a href={pathTo NewPageAction} class=\"btn btn-primary ms-4\">+ New</a></h1>\n        <div class=\"table-responsive\">\n            <table class=\"table\">\n                <thead>\n                    <tr>\n                        <th>Page</th>\n                        <th></th>\n                        <th></th>\n                        <th></th>\n                    </tr>\n                </thead>\n                <tbody>{forEach page renderPage}</tbody>\n            </table>\n            \n        </div>\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                [ breadcrumbLink \"Pages\" PagesAction\n                ]\n\nrenderPage :: Page -> Html\nrenderPage page = [hsx|\n    <tr>\n        <td>{page}</td>\n        <td><a href={ShowPageAction page.id}>Show</a></td>\n        <td><a href={EditPageAction page.id} class=\"text-muted\">Edit</a></td>\n        <td><a href={DeletePageAction page.id} class=\"js-delete text-muted\">Delete</a></td>\n    </tr>\n|]"}
                , AddImport {filePath = "Web/Controller/Page.hs", fileContent = "import Web.View.Page.Index"}
                , EnsureDirectory {directory = "Web/View/Page"}
                , CreateFile {filePath = "Web/View/Page/New.hs", fileContent = "module Web.View.Page.New where\nimport Web.View.Prelude\n\ndata NewView = NewView { page :: Page }\n\ninstance View NewView where\n    html NewView { .. } = [hsx|\n        {breadcrumb}\n        <h1>New Page</h1>\n        {renderForm page}\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                [ breadcrumbLink \"Pages\" PagesAction\n                , breadcrumbText \"New Page\"\n                ]\n\nrenderForm :: Page -> Html\nrenderForm page = formFor page [hsx|\n    \n    {submitButton}\n\n|]"}
                , AddImport {filePath = "Web/Controller/Page.hs", fileContent = "import Web.View.Page.New"}
                , EnsureDirectory {directory = "Web/View/Page"}
                , CreateFile {filePath = "Web/View/Page/Show.hs", fileContent = "module Web.View.Page.Show where\nimport Web.View.Prelude\n\ndata ShowView = ShowView { page :: Page }\n\ninstance View ShowView where\n    html ShowView { .. } = [hsx|\n        {breadcrumb}\n        <h1>Show Page</h1>\n        <p>{page}</p>\n\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                            [ breadcrumbLink \"Pages\" PagesAction\n                            , breadcrumbText \"Show Page\"\n                            ]"}
                , AddImport {filePath = "Web/Controller/Page.hs", fileContent = "import Web.View.Page.Show"}
                , EnsureDirectory {directory = "Web/View/Page"}
                , CreateFile {filePath = "Web/View/Page/Edit.hs", fileContent = "module Web.View.Page.Edit where\nimport Web.View.Prelude\n\ndata EditView = EditView { page :: Page }\n\ninstance View EditView where\n    html EditView { .. } = [hsx|\n        {breadcrumb}\n        <h1>Edit Page</h1>\n        {renderForm page}\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                [ breadcrumbLink \"Pages\" PagesAction\n                , breadcrumbText \"Edit Page\"\n                ]\n\nrenderForm :: Page -> Html\nrenderForm page = formFor page [hsx|\n    \n    {submitButton}\n\n|]"}
                , AddImport {filePath = "Web/Controller/Page.hs", fileContent = "import Web.View.Page.Edit"}
                ]

        it "should build a controller with name \"page_comment\"" do
            let rawControllerName = "page_comment"
            let controllerName = tableNameToControllerName rawControllerName
            let modelName = tableNameToModelName rawControllerName
            let applicationName = "Web"
            let pagination = False
            let builtPlan = ControllerGenerator.buildPlan' schema applicationName controllerName modelName pagination

            builtPlan `shouldBe`
                [ CreateFile {filePath = "Web/Controller/PageComment.hs", fileContent = "module Web.Controller.PageComment where\n\nimport Web.Controller.Prelude\nimport Web.View.PageComment.Index\nimport Web.View.PageComment.New\nimport Web.View.PageComment.Edit\nimport Web.View.PageComment.Show\n\ninstance Controller PageCommentController where\n    action PageCommentsAction = do\n        pageComment <- query @PageComment |> fetch\n        render IndexView { .. }\n\n    action NewPageCommentAction = do\n        let pageComment = newRecord\n        render NewView { .. }\n\n    action ShowPageCommentAction { pageCommentId } = do\n        pageComment <- fetch pageCommentId\n        render ShowView { .. }\n\n    action EditPageCommentAction { pageCommentId } = do\n        pageComment <- fetch pageCommentId\n        render EditView { .. }\n\n    action UpdatePageCommentAction { pageCommentId } = do\n        pageComment <- fetch pageCommentId\n        pageComment\n            |> buildPageComment\n            |> ifValid \\case\n                Left pageComment -> render EditView { .. }\n                Right pageComment -> do\n                    pageComment <- pageComment |> updateRecord\n                    setSuccessMessage \"PageComment updated\"\n                    redirectTo EditPageCommentAction { .. }\n\n    action CreatePageCommentAction = do\n        let pageComment = newRecord @PageComment\n        pageComment\n            |> buildPageComment\n            |> ifValid \\case\n                Left pageComment -> render NewView { .. } \n                Right pageComment -> do\n                    pageComment <- pageComment |> createRecord\n                    setSuccessMessage \"PageComment created\"\n                    redirectTo PageCommentsAction\n\n    action DeletePageCommentAction { pageCommentId } = do\n        pageComment <- fetch pageCommentId\n        deleteRecord pageComment\n        setSuccessMessage \"PageComment deleted\"\n        redirectTo PageCommentsAction\n\nbuildPageComment pageComment = pageComment\n    |> fill @'[]\n"}
                , AppendToFile {filePath = "Web/Routes.hs", fileContent = "\ninstance AutoRoute PageCommentController\n\n"}
                , AppendToFile {filePath = "Web/Types.hs", fileContent = "\ndata PageCommentController\n    = PageCommentsAction\n    | NewPageCommentAction\n    | ShowPageCommentAction { pageCommentId :: !(Id PageComment) }\n    | CreatePageCommentAction\n    | EditPageCommentAction { pageCommentId :: !(Id PageComment) }\n    | UpdatePageCommentAction { pageCommentId :: !(Id PageComment) }\n    | DeletePageCommentAction { pageCommentId :: !(Id PageComment) }\n    deriving (Eq, Show, Data)\n"}
                , AppendToMarker {marker = "-- Controller Imports", filePath = "Web/FrontController.hs", fileContent = "import Web.Controller.PageComment"}
                , AppendToMarker {marker = "-- Generator Marker", filePath = "Web/FrontController.hs", fileContent = "        , parseRoute @PageCommentController"}
                , EnsureDirectory {directory = "Web/View/PageComment"}
                , CreateFile {filePath = "Web/View/PageComment/Index.hs", fileContent = "module Web.View.PageComment.Index where\nimport Web.View.Prelude\n\ndata IndexView = IndexView { pageComment :: [PageComment] }\n\ninstance View IndexView where\n    html IndexView { .. } = [hsx|\n        {breadcrumb}\n\n        <h1>Index<a href={pathTo NewPageCommentAction} class=\"btn btn-primary ms-4\">+ New</a></h1>\n        <div class=\"table-responsive\">\n            <table class=\"table\">\n                <thead>\n                    <tr>\n                        <th>PageComment</th>\n                        <th></th>\n                        <th></th>\n                        <th></th>\n                    </tr>\n                </thead>\n                <tbody>{forEach pageComment renderPageComment}</tbody>\n            </table>\n            \n        </div>\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                [ breadcrumbLink \"PageComments\" PageCommentsAction\n                ]\n\nrenderPageComment :: PageComment -> Html\nrenderPageComment pageComment = [hsx|\n    <tr>\n        <td>{pageComment}</td>\n        <td><a href={ShowPageCommentAction pageComment.id}>Show</a></td>\n        <td><a href={EditPageCommentAction pageComment.id} class=\"text-muted\">Edit</a></td>\n        <td><a href={DeletePageCommentAction pageComment.id} class=\"js-delete text-muted\">Delete</a></td>\n    </tr>\n|]"}
                , AddImport {filePath = "Web/Controller/PageComment.hs", fileContent = "import Web.View.PageComment.Index"}
                , EnsureDirectory {directory = "Web/View/PageComment"}
                , CreateFile {filePath = "Web/View/PageComment/New.hs", fileContent = "module Web.View.PageComment.New where\nimport Web.View.Prelude\n\ndata NewView = NewView { pageComment :: PageComment }\n\ninstance View NewView where\n    html NewView { .. } = [hsx|\n        {breadcrumb}\n        <h1>New PageComment</h1>\n        {renderForm pageComment}\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                [ breadcrumbLink \"PageComments\" PageCommentsAction\n                , breadcrumbText \"New PageComment\"\n                ]\n\nrenderForm :: PageComment -> Html\nrenderForm pageComment = formFor pageComment [hsx|\n    \n    {submitButton}\n\n|]"}
                , AddImport {filePath = "Web/Controller/PageComment.hs", fileContent = "import Web.View.PageComment.New"}
                , EnsureDirectory {directory = "Web/View/PageComment"}
                , CreateFile {filePath = "Web/View/PageComment/Show.hs", fileContent = "module Web.View.PageComment.Show where\nimport Web.View.Prelude\n\ndata ShowView = ShowView { pageComment :: PageComment }\n\ninstance View ShowView where\n    html ShowView { .. } = [hsx|\n        {breadcrumb}\n        <h1>Show PageComment</h1>\n        <p>{pageComment}</p>\n\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                            [ breadcrumbLink \"PageComments\" PageCommentsAction\n                            , breadcrumbText \"Show PageComment\"\n                            ]"}
                , AddImport {filePath = "Web/Controller/PageComment.hs", fileContent = "import Web.View.PageComment.Show"}
                , EnsureDirectory {directory = "Web/View/PageComment"}
                , CreateFile {filePath = "Web/View/PageComment/Edit.hs", fileContent = "module Web.View.PageComment.Edit where\nimport Web.View.Prelude\n\ndata EditView = EditView { pageComment :: PageComment }\n\ninstance View EditView where\n    html EditView { .. } = [hsx|\n        {breadcrumb}\n        <h1>Edit PageComment</h1>\n        {renderForm pageComment}\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                [ breadcrumbLink \"PageComments\" PageCommentsAction\n                , breadcrumbText \"Edit PageComment\"\n                ]\n\nrenderForm :: PageComment -> Html\nrenderForm pageComment = formFor pageComment [hsx|\n    \n    {submitButton}\n\n|]"}
                , AddImport {filePath = "Web/Controller/PageComment.hs", fileContent = "import Web.View.PageComment.Edit"}
                ]



        it "should build a controller with name \"pageComment\"" do
            let rawControllerName = "pageComment"
            let controllerName = tableNameToControllerName rawControllerName
            let modelName = tableNameToModelName rawControllerName
            let applicationName = "Web"
            let pagination = False
            let builtPlan = ControllerGenerator.buildPlan' schema applicationName controllerName modelName pagination

            builtPlan `shouldBe`
                [ CreateFile {filePath = "Web/Controller/PageComment.hs", fileContent = "module Web.Controller.PageComment where\n\nimport Web.Controller.Prelude\nimport Web.View.PageComment.Index\nimport Web.View.PageComment.New\nimport Web.View.PageComment.Edit\nimport Web.View.PageComment.Show\n\ninstance Controller PageCommentController where\n    action PageCommentsAction = do\n        pageComment <- query @PageComment |> fetch\n        render IndexView { .. }\n\n    action NewPageCommentAction = do\n        let pageComment = newRecord\n        render NewView { .. }\n\n    action ShowPageCommentAction { pageCommentId } = do\n        pageComment <- fetch pageCommentId\n        render ShowView { .. }\n\n    action EditPageCommentAction { pageCommentId } = do\n        pageComment <- fetch pageCommentId\n        render EditView { .. }\n\n    action UpdatePageCommentAction { pageCommentId } = do\n        pageComment <- fetch pageCommentId\n        pageComment\n            |> buildPageComment\n            |> ifValid \\case\n                Left pageComment -> render EditView { .. }\n                Right pageComment -> do\n                    pageComment <- pageComment |> updateRecord\n                    setSuccessMessage \"PageComment updated\"\n                    redirectTo EditPageCommentAction { .. }\n\n    action CreatePageCommentAction = do\n        let pageComment = newRecord @PageComment\n        pageComment\n            |> buildPageComment\n            |> ifValid \\case\n                Left pageComment -> render NewView { .. } \n                Right pageComment -> do\n                    pageComment <- pageComment |> createRecord\n                    setSuccessMessage \"PageComment created\"\n                    redirectTo PageCommentsAction\n\n    action DeletePageCommentAction { pageCommentId } = do\n        pageComment <- fetch pageCommentId\n        deleteRecord pageComment\n        setSuccessMessage \"PageComment deleted\"\n        redirectTo PageCommentsAction\n\nbuildPageComment pageComment = pageComment\n    |> fill @'[]\n"}
                , AppendToFile {filePath = "Web/Routes.hs", fileContent = "\ninstance AutoRoute PageCommentController\n\n"}
                , AppendToFile {filePath = "Web/Types.hs", fileContent = "\ndata PageCommentController\n    = PageCommentsAction\n    | NewPageCommentAction\n    | ShowPageCommentAction { pageCommentId :: !(Id PageComment) }\n    | CreatePageCommentAction\n    | EditPageCommentAction { pageCommentId :: !(Id PageComment) }\n    | UpdatePageCommentAction { pageCommentId :: !(Id PageComment) }\n    | DeletePageCommentAction { pageCommentId :: !(Id PageComment) }\n    deriving (Eq, Show, Data)\n"}
                , AppendToMarker {marker = "-- Controller Imports", filePath = "Web/FrontController.hs", fileContent = "import Web.Controller.PageComment"}
                , AppendToMarker {marker = "-- Generator Marker", filePath = "Web/FrontController.hs", fileContent = "        , parseRoute @PageCommentController"}
                , EnsureDirectory {directory = "Web/View/PageComment"}
                , CreateFile {filePath = "Web/View/PageComment/Index.hs", fileContent = "module Web.View.PageComment.Index where\nimport Web.View.Prelude\n\ndata IndexView = IndexView { pageComment :: [PageComment] }\n\ninstance View IndexView where\n    html IndexView { .. } = [hsx|\n        {breadcrumb}\n\n        <h1>Index<a href={pathTo NewPageCommentAction} class=\"btn btn-primary ms-4\">+ New</a></h1>\n        <div class=\"table-responsive\">\n            <table class=\"table\">\n                <thead>\n                    <tr>\n                        <th>PageComment</th>\n                        <th></th>\n                        <th></th>\n                        <th></th>\n                    </tr>\n                </thead>\n                <tbody>{forEach pageComment renderPageComment}</tbody>\n            </table>\n            \n        </div>\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                [ breadcrumbLink \"PageComments\" PageCommentsAction\n                ]\n\nrenderPageComment :: PageComment -> Html\nrenderPageComment pageComment = [hsx|\n    <tr>\n        <td>{pageComment}</td>\n        <td><a href={ShowPageCommentAction pageComment.id}>Show</a></td>\n        <td><a href={EditPageCommentAction pageComment.id} class=\"text-muted\">Edit</a></td>\n        <td><a href={DeletePageCommentAction pageComment.id} class=\"js-delete text-muted\">Delete</a></td>\n    </tr>\n|]"}
                , AddImport {filePath = "Web/Controller/PageComment.hs", fileContent = "import Web.View.PageComment.Index"}
                , EnsureDirectory {directory = "Web/View/PageComment"}
                , CreateFile {filePath = "Web/View/PageComment/New.hs", fileContent = "module Web.View.PageComment.New where\nimport Web.View.Prelude\n\ndata NewView = NewView { pageComment :: PageComment }\n\ninstance View NewView where\n    html NewView { .. } = [hsx|\n        {breadcrumb}\n        <h1>New PageComment</h1>\n        {renderForm pageComment}\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                [ breadcrumbLink \"PageComments\" PageCommentsAction\n                , breadcrumbText \"New PageComment\"\n                ]\n\nrenderForm :: PageComment -> Html\nrenderForm pageComment = formFor pageComment [hsx|\n    \n    {submitButton}\n\n|]"}
                , AddImport {filePath = "Web/Controller/PageComment.hs", fileContent = "import Web.View.PageComment.New"}
                , EnsureDirectory {directory = "Web/View/PageComment"}
                , CreateFile {filePath = "Web/View/PageComment/Show.hs", fileContent = "module Web.View.PageComment.Show where\nimport Web.View.Prelude\n\ndata ShowView = ShowView { pageComment :: PageComment }\n\ninstance View ShowView where\n    html ShowView { .. } = [hsx|\n        {breadcrumb}\n        <h1>Show PageComment</h1>\n        <p>{pageComment}</p>\n\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                            [ breadcrumbLink \"PageComments\" PageCommentsAction\n                            , breadcrumbText \"Show PageComment\"\n                            ]"}
                , AddImport {filePath = "Web/Controller/PageComment.hs", fileContent = "import Web.View.PageComment.Show"}
                , EnsureDirectory {directory = "Web/View/PageComment"}
                , CreateFile {filePath = "Web/View/PageComment/Edit.hs", fileContent = "module Web.View.PageComment.Edit where\nimport Web.View.Prelude\n\ndata EditView = EditView { pageComment :: PageComment }\n\ninstance View EditView where\n    html EditView { .. } = [hsx|\n        {breadcrumb}\n        <h1>Edit PageComment</h1>\n        {renderForm pageComment}\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                [ breadcrumbLink \"PageComments\" PageCommentsAction\n                , breadcrumbText \"Edit PageComment\"\n                ]\n\nrenderForm :: PageComment -> Html\nrenderForm pageComment = formFor pageComment [hsx|\n    \n    {submitButton}\n\n|]"}
                , AddImport {filePath = "Web/Controller/PageComment.hs", fileContent = "import Web.View.PageComment.Edit"}
                ]

        it "should build a controller with name \"people\"" do
            let rawControllerName = "people"
            let controllerName = tableNameToControllerName rawControllerName
            let modelName = tableNameToModelName rawControllerName
            let applicationName = "Web"
            let pagination = False
            let builtPlan = ControllerGenerator.buildPlan' schema applicationName controllerName modelName pagination

            builtPlan `shouldBe`
                [ CreateFile {filePath = "Web/Controller/People.hs", fileContent = "module Web.Controller.People where\n\nimport Web.Controller.Prelude\nimport Web.View.People.Index\nimport Web.View.People.New\nimport Web.View.People.Edit\nimport Web.View.People.Show\n\ninstance Controller PeopleController where\n    action PeopleAction = do\n        people <- query @Person |> fetch\n        render IndexView { .. }\n\n    action NewPersonAction = do\n        let person = newRecord\n        render NewView { .. }\n\n    action ShowPersonAction { personId } = do\n        person <- fetch personId\n        render ShowView { .. }\n\n    action EditPersonAction { personId } = do\n        person <- fetch personId\n        render EditView { .. }\n\n    action UpdatePersonAction { personId } = do\n        person <- fetch personId\n        person\n            |> buildPerson\n            |> ifValid \\case\n                Left person -> render EditView { .. }\n                Right person -> do\n                    person <- person |> updateRecord\n                    setSuccessMessage \"Person updated\"\n                    redirectTo EditPersonAction { .. }\n\n    action CreatePersonAction = do\n        let person = newRecord @Person\n        person\n            |> buildPerson\n            |> ifValid \\case\n                Left person -> render NewView { .. } \n                Right person -> do\n                    person <- person |> createRecord\n                    setSuccessMessage \"Person created\"\n                    redirectTo PeopleAction\n\n    action DeletePersonAction { personId } = do\n        person <- fetch personId\n        deleteRecord person\n        setSuccessMessage \"Person deleted\"\n        redirectTo PeopleAction\n\nbuildPerson person = person\n    |> fill @'[\"name\", \"email\"]\n"}
                , AppendToFile {filePath = "Web/Routes.hs", fileContent = "\ninstance AutoRoute PeopleController\n\n"}
                , AppendToFile {filePath = "Web/Types.hs", fileContent = "\ndata PeopleController\n    = PeopleAction\n    | NewPersonAction\n    | ShowPersonAction { personId :: !(Id Person) }\n    | CreatePersonAction\n    | EditPersonAction { personId :: !(Id Person) }\n    | UpdatePersonAction { personId :: !(Id Person) }\n    | DeletePersonAction { personId :: !(Id Person) }\n    deriving (Eq, Show, Data)\n"}
                , AppendToMarker {marker = "-- Controller Imports", filePath = "Web/FrontController.hs", fileContent = "import Web.Controller.People"}
                , AppendToMarker {marker = "-- Generator Marker", filePath = "Web/FrontController.hs", fileContent = "        , parseRoute @PeopleController"}
                , EnsureDirectory {directory = "Web/View/People"}
                , CreateFile {filePath = "Web/View/People/Index.hs", fileContent = "module Web.View.People.Index where\nimport Web.View.Prelude\n\ndata IndexView = IndexView { people :: [Person] }\n\ninstance View IndexView where\n    html IndexView { .. } = [hsx|\n        {breadcrumb}\n\n        <h1>Index<a href={pathTo NewPersonAction} class=\"btn btn-primary ms-4\">+ New</a></h1>\n        <div class=\"table-responsive\">\n            <table class=\"table\">\n                <thead>\n                    <tr>\n                        <th>Person</th>\n                        <th></th>\n                        <th></th>\n                        <th></th>\n                    </tr>\n                </thead>\n                <tbody>{forEach people renderPerson}</tbody>\n            </table>\n            \n        </div>\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                [ breadcrumbLink \"People\" PeopleAction\n                ]\n\nrenderPerson :: Person -> Html\nrenderPerson person = [hsx|\n    <tr>\n        <td>{person}</td>\n        <td><a href={ShowPersonAction person.id}>Show</a></td>\n        <td><a href={EditPersonAction person.id} class=\"text-muted\">Edit</a></td>\n        <td><a href={DeletePersonAction person.id} class=\"js-delete text-muted\">Delete</a></td>\n    </tr>\n|]"}
                , AddImport {filePath = "Web/Controller/People.hs", fileContent = "import Web.View.People.Index"}
                , EnsureDirectory {directory = "Web/View/People"}
                , CreateFile {filePath = "Web/View/People/New.hs", fileContent = "module Web.View.People.New where\nimport Web.View.Prelude\n\ndata NewView = NewView { person :: Person }\n\ninstance View NewView where\n    html NewView { .. } = [hsx|\n        {breadcrumb}\n        <h1>New Person</h1>\n        {renderForm person}\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                [ breadcrumbLink \"People\" PeopleAction\n                , breadcrumbText \"New Person\"\n                ]\n\nrenderForm :: Person -> Html\nrenderForm person = formFor person [hsx|\n    {(textField #name)}\n    {(textField #email)}\n    {submitButton}\n\n|]"}
                , AddImport {filePath = "Web/Controller/People.hs", fileContent = "import Web.View.People.New"}
                , EnsureDirectory {directory = "Web/View/People"}
                , CreateFile {filePath = "Web/View/People/Show.hs", fileContent = "module Web.View.People.Show where\nimport Web.View.Prelude\n\ndata ShowView = ShowView { person :: Person }\n\ninstance View ShowView where\n    html ShowView { .. } = [hsx|\n        {breadcrumb}\n        <h1>Show Person</h1>\n        <p>{person}</p>\n\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                            [ breadcrumbLink \"People\" PeopleAction\n                            , breadcrumbText \"Show Person\"\n                            ]"}
                , AddImport {filePath = "Web/Controller/People.hs", fileContent = "import Web.View.People.Show"}
                , EnsureDirectory {directory = "Web/View/People"}
                , CreateFile {filePath = "Web/View/People/Edit.hs", fileContent = "module Web.View.People.Edit where\nimport Web.View.Prelude\n\ndata EditView = EditView { person :: Person }\n\ninstance View EditView where\n    html EditView { .. } = [hsx|\n        {breadcrumb}\n        <h1>Edit Person</h1>\n        {renderForm person}\n    |]\n        where\n            breadcrumb = renderBreadcrumb\n                [ breadcrumbLink \"People\" PeopleAction\n                , breadcrumbText \"Edit Person\"\n                ]\n\nrenderForm :: Person -> Html\nrenderForm person = formFor person [hsx|\n    {(textField #name)}\n    {(textField #email)}\n    {submitButton}\n\n|]"}
                , AddImport {filePath = "Web/Controller/People.hs", fileContent = "import Web.View.People.Edit"}
                ]
