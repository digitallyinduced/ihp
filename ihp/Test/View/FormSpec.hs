{-|
Module: Test.View.FormSpec
Copyright: (c) digitally induced GmbH, 2022
-}
module Test.View.FormSpec where

import Test.Hspec
import IHP.FrameworkConfig as FrameworkConfig
import IHP.Controller.RequestContext
import qualified Text.Blaze.Renderer.Text as Blaze
import IHP.ModelSupport
import qualified Network.Wai as Wai
import IHP.ViewPrelude
import Data.Default
import qualified IHP.QueryBuilder as QueryBuilder


tests = do
    describe "IHP.Form" do
        describe "formFor" do
            let project = newRecord @Project

            it "should render a form" do
                context <- createControllerContext
                let ?context = context

                let form = formFor project [hsx|
                    {textField #title}
                    {submitButton}
                |]
                form `shouldRenderTo` "<form method=\"POST\" action=\"/CreateProject\" id=\"\" class=\"new-form\" data-disable-javascript-submission=\"false\"><div class=\"mb-3\" id=\"form-group-project_title\"><label class=\"form-label\" for=\"project_title\">Title</label><input type=\"text\" name=\"title\" placeholder=\"\" id=\"project_title\" class=\"form-control\"> </div> <button class=\"btn btn-primary\" type=\"submit\">Create Project</button></form>"

            it "should render a form with a GET method" do
                context <- createControllerContext
                let ?context = context

                let options formContext = formContext |> set #formMethod "GET"

                let form = formForWithOptions project options [hsx|
                    {textField #title}
                    {submitButton}
                |]
                form `shouldRenderTo` "<form method=\"GET\" action=\"/CreateProject\" id=\"\" class=\"new-form\" data-disable-javascript-submission=\"false\"><div class=\"mb-3\" id=\"form-group-project_title\"><label class=\"form-label\" for=\"project_title\">Title</label><input type=\"text\" name=\"title\" placeholder=\"\" id=\"project_title\" class=\"form-control\"> </div> <button class=\"btn btn-primary\" type=\"submit\">Create Project</button></form>"

shouldRenderTo renderFunction expectedHtml = Blaze.renderMarkup renderFunction `shouldBe` expectedHtml

createControllerContext :: IO ControllerContext
createControllerContext = do
    frameworkConfig <- FrameworkConfig.buildFrameworkConfig (pure ())
    let requestBody = FormBody { params = [], files = [] }
    let request = Wai.defaultRequest
    let requestContext = RequestContext { request, respond = undefined, requestBody, frameworkConfig = frameworkConfig }
    pure FrozenControllerContext { requestContext, customFields = mempty }

data Project'  = Project {id :: (Id' "projects"), title :: Text, meta :: MetaBag} deriving (Eq, Show)
instance InputValue Project where inputValue = IHP.ModelSupport.recordToInputValue
type Project = Project'

type instance GetTableName (Project' ) = "projects"
type instance GetModelByTableName "projects" = Project
type instance GetModelName (Project' ) = "Project"

type instance PrimaryKey "projects" = UUID

instance Record Project where
    {-# INLINE newRecord #-}
    newRecord = Project def def  def
instance Default (Id' "projects") where def = Id def

instance SetField "id" (Project' ) (Id' "projects") where
    {-# INLINE setField #-}
    setField newValue (Project id title meta) =
        Project newValue title (meta { touchedFields = "id" : touchedFields meta })
instance SetField "title" (Project' ) Text where
    {-# INLINE setField #-}
    setField newValue (Project id title meta) =
        Project id newValue (meta { touchedFields = "title" : touchedFields meta })
instance SetField "meta" (Project' ) MetaBag where
    {-# INLINE setField #-}
    setField newValue (Project id title meta) =
        Project id title newValue
instance UpdateField "id" (Project' ) (Project' ) (Id' "projects") (Id' "projects") where
    {-# INLINE updateField #-}
    updateField newValue (Project id title meta) = Project newValue title (meta { touchedFields = "id" : touchedFields meta })
instance UpdateField "title" (Project' ) (Project' ) Text Text where
    {-# INLINE updateField #-}
    updateField newValue (Project id title meta) = Project id newValue (meta { touchedFields = "title" : touchedFields meta })
instance UpdateField "meta" (Project' ) (Project' ) MetaBag MetaBag where
    {-# INLINE updateField #-}
    updateField newValue (Project id title meta) = Project id title newValue






