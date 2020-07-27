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
    describe "Should generate Types" do
        it "should build a controller with single name" do
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
            let rawControllerName = "pages"
            let controllerName = tableNameToControllerName rawControllerName
            let modelName = tableNameToModelName rawControllerName
            let applicationName = "Web"
            let viewPlans = []
            let builtPlan = ControllerGenerator.buildPlan' schema applicationName controllerName modelName viewPlans

            builtPlan `shouldBe` [
                  CreateFile {filePath = "Web/Controller/Pages.hs", fileContent = "module Web.Controller.Pages where\n\nimport Web.Controller.Prelude\nimport Web.View.Pages.Index\nimport Web.View.Pages.New\nimport Web.View.Pages.Edit\nimport Web.View.Pages.Show\n\ninstance Controller PagesController where\n    action PagesAction = do\n        pages <- query @Page |> fetch\n        render IndexView { .. }\n\n    action NewPageAction = do\n        let page = newRecord\n        render NewView { .. }\n\n    action ShowPageAction { pageId } = do\n        page <- fetch pageId\n        render ShowView { .. }\n\n    action EditPageAction { pageId } = do\n        page <- fetch pageId\n        render EditView { .. }\n\n    action UpdatePageAction { pageId } = do\n        page <- fetch pageId\n        page\n            |> buildPage\n            |> ifValid \\case\n                Left page -> render EditView { .. }\n                Right page -> do\n                    page <- page |> updateRecord\n                    setSuccessMessage \"Page updated\"\n                    redirectTo EditPageAction { .. }\n\n    action CreatePageAction = do\n        let page = newRecord @Page\n        page\n            |> buildPage\n            |> ifValid \\case\n                Left page -> render NewView { .. } \n                Right page -> do\n                    page <- page |> createRecord\n                    setSuccessMessage \"Page created\"\n                    redirectTo PagesAction\n\n    action DeletePageAction { pageId } = do\n        page <- fetch pageId\n        deleteRecord page\n        setSuccessMessage \"Page deleted\"\n        redirectTo PagesAction\n\nbuildPage page = page\n    |> fill @'[]\n"}
                , AppendToFile {filePath = "Web/Routes.hs", fileContent = "instance AutoRoute PagesController\ntype instance ModelControllerMap WebApplication Page = PagesController\n\n"}
                , AppendToFile {filePath = "Web/Types.hs", fileContent = "\ndata PagesController\n    = PagesAction\n    | NewPageAction\n    | ShowPageAction { pageId :: !(Id Page) }\n    | CreatePageAction\n    | EditPageAction { pageId :: !(Id Page) }\n    | UpdatePageAction { pageId :: !(Id Page) }\n    | DeletePageAction { pageId :: !(Id Page) }\n    deriving (Eq, Show, Data)\n"}
                , AppendToMarker {marker = "-- Controller Imports", filePath = "Web/FrontController.hs", fileContent = "import Web.Controller.Pages"}
                , AppendToMarker {marker = "-- Generator Marker", filePath = "Web/FrontController.hs", fileContent = "        , parseRoute @PagesController"}
                ]
                    