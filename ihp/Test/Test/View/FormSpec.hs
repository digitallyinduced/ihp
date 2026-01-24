{-|
Module: Test.View.FormSpec
Copyright: (c) digitally induced GmbH, 2022
-}
module Test.View.FormSpec where

import Test.Hspec
import IHP.FrameworkConfig as FrameworkConfig
import IHP.RequestBodyMiddleware (RequestBody (..))
import qualified Text.Blaze.Renderer.Text as Blaze
import IHP.ModelSupport
import qualified Network.Wai as Wai
import IHP.ViewPrelude
import Data.Default
import qualified IHP.QueryBuilder as QueryBuilder
import qualified Data.Text.Lazy as LT
import qualified Data.Vault.Lazy as Vault
import qualified IHP.RequestVault
import qualified Data.TMap as TypeMap


tests = do
    describe "IHP.Form" do
        describe "formFor" do
            let project = newRecord @Project

            it "should render a form" do
                context <- createControllerContext
                let ?context = context
                let ?request = ?context.request

                let form = formFor project [hsx|
                    {textField #title}
                    {submitButton}
                |]
                form `shouldRenderTo` "<form method=\"POST\" action=\"/CreateProject\" id=\"\" class=\"new-form\" data-disable-javascript-submission=\"false\"><div class=\"mb-3\" id=\"form-group-project_title\"><label class=\"form-label\" for=\"project_title\">Title</label><input type=\"text\" name=\"title\" placeholder=\"\" id=\"project_title\" class=\"form-control\"> </div> <button class=\"btn btn-primary\" type=\"submit\">Create Project</button></form>"

            it "should render a form with a GET method" do
                context <- createControllerContext
                let ?context = context
                let ?request = ?context.request

                let options formContext = formContext |> set #formMethod "GET"

                let form = formForWithOptions project options [hsx|
                    {textField #title}
                    {submitButton}
                |]
                form `shouldRenderTo` "<form method=\"GET\" action=\"/CreateProject\" id=\"\" class=\"new-form\" data-disable-javascript-submission=\"false\"><div class=\"mb-3\" id=\"form-group-project_title\"><label class=\"form-label\" for=\"project_title\">Title</label><input type=\"text\" name=\"title\" placeholder=\"\" id=\"project_title\" class=\"form-control\"> </div> <button class=\"btn btn-primary\" type=\"submit\">Create Project</button></form>"

            it "should render a date field with empty value attribute when value is Nothing" do
                context <- createControllerContext
                let ?context = context
                let ?request = ?context.request
                let event = newRecord @Event

                let form = formFor event [hsx|
                    {(dateField #date) { required = True }}
                |]
                
                -- The date input should have value="" (not omit the value attribute)
                -- This is necessary for HTML5 required validation to work properly
                let rendered = Blaze.renderMarkup form
                let renderedText = LT.toStrict rendered
                renderedText `shouldSatisfy` (\t -> "type=\"date\"" `isInfixOf` t)
                renderedText `shouldSatisfy` (\t -> "required=\"required\"" `isInfixOf` t)
                renderedText `shouldSatisfy` (\t -> "value=\"\"" `isInfixOf` t)

            it "should render a datetime field with empty value attribute when value is Nothing" do
                context <- createControllerContext
                let ?context = context
                let ?request = ?context.request
                let event = newRecord @Event

                let form = formFor event [hsx|
                    {(dateTimeField #createdAt) { required = True }}
                |]
                
                -- The datetime input should have value="" (not omit the value attribute)
                let rendered = Blaze.renderMarkup form
                let renderedText = LT.toStrict rendered
                renderedText `shouldSatisfy` (\t -> "type=\"datetime-local\"" `isInfixOf` t)
                renderedText `shouldSatisfy` (\t -> "required=\"required\"" `isInfixOf` t)
                renderedText `shouldSatisfy` (\t -> "value=\"\"" `isInfixOf` t)

shouldRenderTo renderFunction expectedHtml = Blaze.renderMarkup renderFunction `shouldBe` expectedHtml

createControllerContext :: IO ControllerContext
createControllerContext = do
    frameworkConfig <- FrameworkConfig.buildFrameworkConfig (pure ())
    let requestBody = FormBody { params = [], files = [] }
    let request = Wai.defaultRequest { Wai.vault = Vault.insert IHP.RequestVault.frameworkConfigVaultKey frameworkConfig
                                                 $ Vault.insert IHP.RequestVault.requestBodyVaultKey requestBody Vault.empty }
    let customFields = TypeMap.insert request TypeMap.empty
    pure FrozenControllerContext { customFields }

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

-- Event model for testing date fields
data Event' = Event 
    { id :: (Id' "events")
    , date :: Maybe Day
    , createdAt :: Maybe UTCTime
    , meta :: MetaBag
    } deriving (Eq, Show)

instance InputValue Event where inputValue = IHP.ModelSupport.recordToInputValue
type Event = Event'

type instance GetTableName (Event' ) = "events"
type instance GetModelByTableName "events" = Event
type instance GetModelName (Event' ) = "Event"

type instance PrimaryKey "events" = UUID

instance Record Event where
    {-# INLINE newRecord #-}
    newRecord = Event def def def def
instance Default (Id' "events") where def = Id def

instance SetField "id" (Event' ) (Id' "events") where
    {-# INLINE setField #-}
    setField newValue (Event id date createdAt meta) =
        Event newValue date createdAt (meta { touchedFields = "id" : touchedFields meta })
instance SetField "date" (Event' ) (Maybe Day) where
    {-# INLINE setField #-}
    setField newValue (Event id date createdAt meta) =
        Event id newValue createdAt (meta { touchedFields = "date" : touchedFields meta })
instance SetField "createdAt" (Event' ) (Maybe UTCTime) where
    {-# INLINE setField #-}
    setField newValue (Event id date createdAt meta) =
        Event id date newValue (meta { touchedFields = "createdAt" : touchedFields meta })
instance SetField "meta" (Event' ) MetaBag where
    {-# INLINE setField #-}
    setField newValue (Event id date createdAt meta) =
        Event id date createdAt newValue

instance UpdateField "id" (Event' ) (Event' ) (Id' "events") (Id' "events") where
    {-# INLINE updateField #-}
    updateField newValue (Event id date createdAt meta) = Event newValue date createdAt (meta { touchedFields = "id" : touchedFields meta })
instance UpdateField "date" (Event' ) (Event' ) (Maybe Day) (Maybe Day) where
    {-# INLINE updateField #-}
    updateField newValue (Event id date createdAt meta) = Event id newValue createdAt (meta { touchedFields = "date" : touchedFields meta })
instance UpdateField "createdAt" (Event' ) (Event' ) (Maybe UTCTime) (Maybe UTCTime) where
    {-# INLINE updateField #-}
    updateField newValue (Event id date createdAt meta) = Event id date newValue (meta { touchedFields = "createdAt" : touchedFields meta })
instance UpdateField "meta" (Event' ) (Event' ) MetaBag MetaBag where
    {-# INLINE updateField #-}
    updateField newValue (Event id date createdAt meta) = Event id date createdAt newValue






