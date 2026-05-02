{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Controller.TypedActionSpec where

import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bits ((.|.))
import Data.ByteString.Lazy qualified as LBS
import Data.TMap qualified as TypeMap
import Data.Vector qualified as Vector
import Data.Vault.Lazy qualified as Vault
import IHP.AutoRefresh (AutoRefreshWSApp)
import GHC.Generics (Generic)
import IHP.Controller.TypedAction
import IHP.ControllerSupport
import IHP.FrameworkConfig qualified as FrameworkConfig
import IHP.HSX.Markup (renderMarkupText)
import IHP.ModelSupport
import IHP.OpenApiSupport qualified as OpenApiSupport
import IHP.Prelude
import IHP.RequestVault qualified as RequestVault
import "ihp" IHP.Router.Capture (parseCapture, renderCapture)
import "ihp" IHP.Router.DSL (routes)
import IHP.Router.TypedRoute
import IHP.RouterSupport
import IHP.Server qualified as Server
import IHP.ViewPrelude hiding (action)
import Network.HTTP.Types
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WaiTest
import Test.Hspec
import Wai.Request.Params.Middleware (RequestBody (..), requestBodyVaultKey)

data ProjectInput = ProjectInput
    { name :: Text
    , enabled :: Bool
    }
    deriving (Eq, Show, Generic)

instance JSON.FromJSON ProjectInput
instance JSON.ToJSON ProjectInput
instance ToSchema ProjectInput

instance FromMultipartBody ProjectInput where
    parseMultipartBody _ = Right ProjectInput{name = "from multipart", enabled = True}

data GenericFormInput = GenericFormInput
    { genericName :: Text
    , genericEnabled :: Bool
    }
    deriving (Eq, Show, Generic)

data ComplexFormInput = ComplexFormInput
    { textValue :: Text
    , numberValue :: Int
    , urlValue :: Text
    , textareaValue :: Text
    , colorValue :: Text
    , emailValue :: Text
    , dateValue :: Day
    , eventDateTime :: LocalTime
    , passwordValue :: Text
    , hiddenValue :: Text
    , checkboxValue :: Bool
    , selectValue :: Text
    , radioValue :: Text
    , fileValue :: Text
    }

data ProjectRecord = ProjectRecord
    { name :: Text
    , enabled :: Bool
    , meta :: MetaBag
    }
    deriving (Eq, Show)

type instance GetModelName ProjectRecord = "Project"

instance SetField "name" ProjectRecord Text where
    setField value record = record{name = value, meta = record.meta{touchedFields = touchedFields record.meta .|. 1}}

instance SetField "enabled" ProjectRecord Bool where
    setField value record = record{enabled = value, meta = record.meta{touchedFields = touchedFields record.meta .|. 2}}

instance SetField "meta" ProjectRecord MetaBag where
    setField value record = record{meta = value}

data ProjectPayload = ProjectPayload
    { ok :: Bool
    , projectName :: Text
    }
    deriving (Eq, Show, Generic)

instance JSON.ToJSON ProjectPayload
instance ToSchema ProjectPayload

data ProjectView = ProjectView
    { viewProjectName :: Text
    }

instance View ProjectView where
    html ProjectView{..} = [hsx||]

instance JsonView ProjectView where
    type JsonResponse ProjectView = ProjectPayload

    json ProjectView{..} = ProjectPayload{ok = True, projectName = viewProjectName}

data ProjectAction body response where
    ShowProjectAction ::
        { showProjectId :: Int
        , includeArchived :: Maybe Bool
        } ->
        ProjectAction 'NoBody ProjectView
    UpdateProjectAction ::
        { projectId :: Int
        , returnTo :: Maybe Text
        } ->
        ProjectAction ('Body ProjectInput) ProjectView
    ArchiveProjectAction ::
        { archiveProjectId :: Int
        } ->
        ProjectAction ('Body ProjectInput) ProjectView
    UploadProjectLogoAction ::
        { uploadProjectId :: Int
        } ->
        ProjectAction ('BodyWith ProjectInput '[ 'Multipart]) ProjectView

deriving instance Show (ProjectAction body response)
deriving instance Eq (ProjectAction body response)

data BadProjectAction body response where
    BadUpdateProjectAction ::
        { badProjectId :: Int
        } ->
        BadProjectAction ('Body ProjectInput) ProjectView

deriving instance Show (BadProjectAction body response)
deriving instance Eq (BadProjectAction body response)

instance Controller (ProjectAction 'NoBody ProjectView) where
    type ControllerAction (ProjectAction 'NoBody ProjectView) = TypedControllerAction 'NoBody ProjectView

    action ShowProjectAction{..} =
        pure ProjectView{viewProjectName = "show"}

instance Controller (ProjectAction ('Body ProjectInput) ProjectView) where
    type ControllerAction (ProjectAction ('Body ProjectInput) ProjectView) = TypedControllerAction ('Body ProjectInput) ProjectView

    action UpdateProjectAction{..} body =
        pure ProjectView{viewProjectName = bodyParam body #name}

    action ArchiveProjectAction{..} body =
        pure ProjectView{viewProjectName = bodyParam body #name}

instance Controller (ProjectAction ('BodyWith ProjectInput '[ 'Multipart]) ProjectView) where
    type ControllerAction (ProjectAction ('BodyWith ProjectInput '[ 'Multipart]) ProjectView) = TypedControllerAction ('BodyWith ProjectInput '[ 'Multipart]) ProjectView

    action UploadProjectLogoAction{..} body =
        pure ProjectView{viewProjectName = bodyParam body #name}

instance Controller (BadProjectAction ('Body ProjectInput) ProjectView) where
    type ControllerAction (BadProjectAction ('Body ProjectInput) ProjectView) = TypedControllerAction ('Body ProjectInput) ProjectView

    action BadUpdateProjectAction{..} body =
        pure ProjectView{viewProjectName = bodyParam body #name}

$(pure [])

[routes|typedRouteTestRoutes
POST|PATCH /projects/{projectId}?returnTo         UpdateProjectAction
  summary: Update project
  tags: Projects
  success: 201 Project updated
GET        /projects/{showProjectId}?includeArchived ShowProjectAction
  summary: Show project
  tags: Projects
PATCH /projects/{archiveProjectId}/archive        ArchiveProjectAction
  summary: Archive project
  tags: Projects
POST  /projects/{uploadProjectId}/logo            UploadProjectLogoAction
  summary: Upload project logo
  tags: Projects
|]

[routes|badTypedRouteTestRoutes
POST /bad-projects/{badProjectId} BadUpdateProjectAction
  summary: Bad update project
|]

data TypedRouteApplication = TypedRouteApplication
    deriving (Eq, Show)

instance FrontController TypedRouteApplication where
    controllers = typedRouteTestRoutes

instance InitControllerContext TypedRouteApplication

data BadTypedRouteApplication = BadTypedRouteApplication
    deriving (Eq, Show)

instance FrontController BadTypedRouteApplication where
    controllers = badTypedRouteTestRoutes

instance InitControllerContext BadTypedRouteApplication

tests :: Spec
tests = do
    describe "IHP.Controller.TypedAction" do
        describe "typed body helpers" do
            it "reads fields from the decoded typed body" do
                let typedBody = ProjectInput{name = "Acme", enabled = True}

                bodyParam typedBody #name `shouldBe` ("Acme" :: Text)
                bodyParam typedBody #enabled `shouldBe` True

            it "fills a destination record from selected body fields" do
                let typedBody = ProjectInput{name = "Acme", enabled = True}
                let emptyRecord = ProjectRecord{name = "", enabled = False, meta = def}
                let expectedRecord = ProjectRecord{name = "Acme", enabled = True, meta = def{touchedFields = 3}}

                emptyRecord
                    |> fillBody @'["name", "enabled"] typedBody
                    |> shouldBe expectedRecord

        describe "request decoding" do
            it "decodes Body from JSON by default" do
                let payload = JSON.encode (JSON.object ["name" JSON..= ("Acme" :: Text), "enabled" JSON..= True])
                let ?request = requestWithBody [(hContentType, "application/json")] JSONBody{jsonPayload = Just (unsafeDecode payload), rawPayload = payload}

                decodeRequest @('Body ProjectInput) >>= shouldBe (Right ProjectInput{name = "Acme", enabled = True})

            it "accepts JSON content types with parameters and vendor suffixes" do
                let payload = JSON.encode (JSON.object ["name" JSON..= ("Acme" :: Text), "enabled" JSON..= True])

                do
                    let ?request = requestWithBody [(hContentType, "application/json; charset=utf-8")] JSONBody{jsonPayload = Just (unsafeDecode payload), rawPayload = payload}
                    decodeRequest @('Body ProjectInput) >>= shouldBe (Right ProjectInput{name = "Acme", enabled = True})

                do
                    let ?request = requestWithBody [(hContentType, "application/vnd.api+json")] JSONBody{jsonPayload = Just (unsafeDecode payload), rawPayload = payload}
                    decodeRequest @('BodyWith ProjectInput '[ 'Json]) >>= shouldBe (Right ProjectInput{name = "Acme", enabled = True})

            it "decodes Body from form data by default" do
                let ?request = requestWithBody [(hContentType, "application/x-www-form-urlencoded")] FormBody{params = [("name", "Acme"), ("enabled", "on")], files = [], rawPayload = ""}

                decodeRequest @('Body ProjectInput) >>= shouldBe (Right ProjectInput{name = "Acme", enabled = True})

            it "supports generic form decoding only via explicit opt-in" do
                let ?request = requestWithBody [(hContentType, "application/x-www-form-urlencoded")] FormBody{params = [("genericName", "Acme"), ("genericEnabled", "on")], files = [], rawPayload = ""}

                decodeRequest @('BodyWith GenericFormInput '[ 'FormUrlEncoded]) >>= shouldBe (Right GenericFormInput{genericName = "Acme", genericEnabled = True})

            it "decodes multipart bodies with content-type boundaries" do
                let ?request = requestWithBody [(hContentType, "multipart/form-data; boundary=abc")] FormBody{params = [], files = [], rawPayload = ""}

                decodeRequest @('BodyWith ProjectInput '[ 'Multipart]) >>= shouldBe (Right ProjectInput{name = "from multipart", enabled = True})

            it "rejects encodings not listed by BodyWith" do
                let ?request = requestWithBody [(hContentType, "application/x-www-form-urlencoded")] FormBody{params = [("name", "Acme"), ("enabled", "on")], files = [], rawPayload = ""}

                result <- decodeRequest @('BodyWith ProjectInput '[ 'Json])
                case result of
                    Left RequestDecodeError{requestDecodeErrorStatus} ->
                        requestDecodeErrorStatus `shouldBe` status415
                    Right value ->
                        expectationFailure ("expected an unsupported content type error, got " <> cs (show value))

        describe "TypedControllerAction" do
            it "uses plain IO for NoBody actions" do
                let runAction :: TypedControllerAction 'NoBody ProjectView
                    runAction = pure ProjectView{viewProjectName = "Acme"}
                view <- runAction
                view.viewProjectName `shouldBe` "Acme"

        describe "typed routes" do
            it "generates URLs from the typed route spec" do
                pathTo ShowProjectAction{showProjectId = 42, includeArchived = Just True}
                    `shouldBe` "/projects/42?includeArchived=true"

                pathTo UpdateProjectAction{projectId = 42, returnTo = Just "/dashboard"}
                    `shouldBe` "/projects/42?returnTo=%2Fdashboard"

                pathTo ArchiveProjectAction{archiveProjectId = 42}
                    `shouldBe` "/projects/42/archive"

                pathTo UploadProjectLogoAction{uploadProjectId = 42}
                    `shouldBe` "/projects/42/logo"

            it "finds methods for constructors that share body and response indices" do
                typedActionMethods ShowProjectAction{showProjectId = 42, includeArchived = Nothing}
                    `shouldBe` Just [GET, HEAD]

                typedActionMethods UpdateProjectAction{projectId = 42, returnTo = Nothing}
                    `shouldBe` Just [POST, PATCH]

                typedActionMethods ArchiveProjectAction{archiveProjectId = 42}
                    `shouldBe` Just [PATCH]

            it "adds route-owned typed route metadata to OpenAPI" do
                let spec = OpenApiSupport.buildOpenApi TypedRouteApplication
                let Just operation = lookupPathOperation "/projects/{projectId}" "post" spec

                lookupValue "summary" operation `shouldBe` Just (JSON.String "Update project")
                lookupValue "x-ihp-action" operation `shouldBe` Just (JSON.String "UpdateProjectAction")

                let Just projectIdParameter = lookupParameter "projectId" operation
                lookupValue "in" projectIdParameter `shouldBe` Just (JSON.String "path")
                lookupValue "required" projectIdParameter `shouldBe` Just (JSON.Bool True)

                let Just returnToParameter = lookupParameter "returnTo" operation
                lookupValue "in" returnToParameter `shouldBe` Just (JSON.String "query")
                lookupValue "required" returnToParameter `shouldBe` Just (JSON.Bool False)

                ( lookupValue "requestBody" operation
                    >>= lookupValue "content"
                    >>= lookupValue "application/json"
                    )
                    `shouldSatisfy` isJust

                ( lookupValue "requestBody" operation
                    >>= lookupValue "content"
                    >>= lookupValue "application/x-www-form-urlencoded"
                    )
                    `shouldSatisfy` isJust

                let Just uploadOperation = lookupPathOperation "/projects/{uploadProjectId}/logo" "post" spec
                lookupValue "summary" uploadOperation `shouldBe` Just (JSON.String "Upload project logo")
                ( lookupValue "requestBody" uploadOperation
                    >>= lookupValue "content"
                    >>= lookupValue "multipart/form-data"
                    )
                    `shouldSatisfy` isJust

                let Just archiveOperation = lookupPathOperation "/projects/{archiveProjectId}/archive" "patch" spec
                lookupValue "summary" archiveOperation `shouldBe` Just (JSON.String "Archive project")
                lookupValue "x-ihp-action" archiveOperation `shouldBe` Just (JSON.String "ArchiveProjectAction")

                let Just showOperation = lookupPathOperation "/projects/{showProjectId}" "get" spec
                lookupValue "summary" showOperation `shouldBe` Just (JSON.String "Show project")

                let Just includeArchivedParameter = lookupParameter "includeArchived" showOperation
                lookupValue "in" includeArchivedParameter `shouldBe` Just (JSON.String "query")
                lookupValue "required" includeArchivedParameter `shouldBe` Just (JSON.Bool False)

            it "derives OpenAPI path params from the typed route path" do
                let spec = OpenApiSupport.buildOpenApi BadTypedRouteApplication
                let Just operation = lookupPathOperation "/bad-projects/{badProjectId}" "post" spec

                lookupParameter "badProjectId" operation `shouldSatisfy` isJust

            it "renders typed action forms against typed action URLs" do
                context <- createControllerContext
                let ?context = context
                let ?request = ?context.request

                let initialProjectInput = ProjectInput{name = "Acme", enabled = True}
                let targetAction = UpdateProjectAction{projectId = 42, returnTo = Just "/dashboard"}
                let options formContext =
                        formContext
                            |> set #formId "project-settings-form"
                            |> set #formClass "settings-form"
                            |> set #customFormAttributes [("data-controller", "autosave")]

                let form =
                        formForActionWithOptions targetAction initialProjectInput options [hsx|
                            {(textField #name) { fieldLabel = "Project name", placeholder = "Acme billing", required = True, autofocus = True, fieldClass = "input input-lg" }}
                            {(checkboxField #enabled) { fieldLabel = "Enabled?" }}
                            {submitButton { label = "Save project" }}
                        |]

                let rendered = renderMarkupText form
                rendered `shouldSatisfy` ("method=\"POST\"" `isInfixOf`)
                rendered `shouldSatisfy` ("action=\"/projects/42?returnTo=%2Fdashboard\"" `isInfixOf`)
                rendered `shouldSatisfy` ("enctype=\"application/x-www-form-urlencoded\"" `isInfixOf`)
                rendered `shouldSatisfy` ("id=\"project-settings-form\"" `isInfixOf`)
                rendered `shouldSatisfy` ("class=\"settings-form\"" `isInfixOf`)
                rendered `shouldSatisfy` ("data-controller=\"autosave\"" `isInfixOf`)
                rendered `shouldSatisfy` ("Project name" `isInfixOf`)
                rendered `shouldSatisfy` ("placeholder=\"Acme billing\"" `isInfixOf`)

            it "renders method override fields for non-GET/POST typed routes" do
                context <- createControllerContext
                let ?context = context
                let ?request = ?context.request

                let initialProjectInput = ProjectInput{name = "Acme", enabled = True}
                let targetAction = ArchiveProjectAction{archiveProjectId = 42}

                let rendered =
                        renderMarkupText
                            ( formForAction targetAction initialProjectInput [hsx|
                                {hiddenField #name}
                                {submitButton { label = "Archive project" }}
                            |]
                            )

                rendered `shouldSatisfy` ("method=\"POST\"" `isInfixOf`)
                rendered `shouldSatisfy` ("action=\"/projects/42/archive\"" `isInfixOf`)
                rendered `shouldSatisfy` ("name=\"_method\"" `isInfixOf`)
                rendered `shouldSatisfy` ("value=\"PATCH\"" `isInfixOf`)

            it "renders common form field helpers for plain typed inputs" do
                context <- createControllerContext
                let ?context = context
                let ?request = ?context.request
                let ?formContext = complexFormContext ComplexFormInput
                        { textValue = "Title"
                        , numberValue = 42
                        , urlValue = "https://example.com"
                        , textareaValue = "Long body"
                        , colorValue = "#ff0000"
                        , emailValue = "team@example.com"
                        , dateValue = fromGregorian 2026 4 25
                        , eventDateTime = LocalTime (fromGregorian 2026 4 25) (TimeOfDay 12 30 0)
                        , passwordValue = "secret"
                        , hiddenValue = "hidden"
                        , checkboxValue = True
                        , selectValue = "team"
                        , radioValue = "public"
                        , fileValue = ""
                        }
                let simpleOptions :: [(Text, Text)]
                    simpleOptions = [("Team", "team"), ("Public", "public")]

                let rendered =
                        renderMarkupText [hsx|
                            {textField #textValue}
                            {numberField #numberValue}
                            {urlField #urlValue}
                            {textareaField #textareaValue}
                            {colorField #colorValue}
                            {emailField #emailValue}
                            {dateField #dateValue}
                            {dateTimeField #eventDateTime}
                            {passwordField #passwordValue}
                            {hiddenField #hiddenValue}
                            {checkboxField #checkboxValue}
                            {selectFieldOptions #selectValue simpleOptions}
                            {radioFieldOptions #radioValue simpleOptions}
                            {fileField #fileValue}
                        |]

                rendered `shouldSatisfy` ("type=\"text\"" `isInfixOf`)
                rendered `shouldSatisfy` ("type=\"number\"" `isInfixOf`)
                rendered `shouldSatisfy` ("type=\"url\"" `isInfixOf`)
                rendered `shouldSatisfy` ("<textarea" `isInfixOf`)
                rendered `shouldSatisfy` ("type=\"color\"" `isInfixOf`)
                rendered `shouldSatisfy` ("type=\"email\"" `isInfixOf`)
                rendered `shouldSatisfy` ("type=\"date\"" `isInfixOf`)
                rendered `shouldSatisfy` ("type=\"datetime-local\"" `isInfixOf`)
                rendered `shouldSatisfy` ("type=\"password\"" `isInfixOf`)
                rendered `shouldSatisfy` ("type=\"hidden\"" `isInfixOf`)
                rendered `shouldSatisfy` ("type=\"checkbox\"" `isInfixOf`)
                rendered `shouldSatisfy` ("<select" `isInfixOf`)
                rendered `shouldSatisfy` ("type=\"radio\"" `isInfixOf`)
                rendered `shouldSatisfy` ("type=\"file\"" `isInfixOf`)

            it "renders multipart typed action forms with multipart enctype" do
                context <- createControllerContext
                let ?context = context
                let ?request = ?context.request

                let initialProjectInput = ProjectInput{name = "Logo", enabled = True}
                let targetAction = UploadProjectLogoAction{uploadProjectId = 42}

                let form =
                        formForAction targetAction initialProjectInput [hsx|
                            {(fileField #name) { fieldLabel = "Logo file", additionalAttributes = [("accept", "image/png,image/jpeg")] }}
                            {submitButton { label = "Upload logo" }}
                        |]
                let rendered = renderMarkupText form

                rendered `shouldSatisfy` ("action=\"/projects/42/logo\"" `isInfixOf`)
                rendered `shouldSatisfy` ("enctype=\"multipart/form-data\"" `isInfixOf`)
                rendered `shouldSatisfy` ("type=\"file\"" `isInfixOf`)
                rendered `shouldSatisfy` ("accept=\"image/png,image/jpeg\"" `isInfixOf`)

            it "runs typed routes through WAI with JSON bodies" do
                app <- createTypedRouteTestApplication

                response <-
                    WaiTest.runSession
                        (testPostJson "projects/42?returnTo=%2Fdashboard" (JSON.object ["name" JSON..= ("Acme" :: Text), "enabled" JSON..= True]))
                        app

                response.simpleStatus `shouldBe` status201
                JSON.decode response.simpleBody
                    `shouldBe` Just (JSON.object ["ok" JSON..= True, "projectName" JSON..= ("Acme" :: Text)])

            it "dispatches typed routes with the same path by method" do
                app <- createTypedRouteTestApplication

                response <-
                    WaiTest.runSession
                        (testGetJson "projects/42?includeArchived=true")
                        app

                response.simpleStatus `shouldBe` status200
                JSON.decode response.simpleBody
                    `shouldBe` Just (JSON.object ["ok" JSON..= True, "projectName" JSON..= ("show" :: Text)])

            it "dispatches longer typed paths after shorter path prefixes" do
                app <- createTypedRouteTestApplication

                response <-
                    WaiTest.runSession
                        (testJsonWithMethod methodPatch "projects/42/archive" (JSON.object ["name" JSON..= ("Archived" :: Text), "enabled" JSON..= True]))
                        app

                response.simpleStatus `shouldBe` status200
                JSON.decode response.simpleBody
                    `shouldBe` Just (JSON.object ["ok" JSON..= True, "projectName" JSON..= ("Archived" :: Text)])

            it "runs typed routes through WAI with form bodies" do
                app <- createTypedRouteTestApplication

                response <-
                    WaiTest.runSession
                        (testPostForm "projects/42" [("name", "Form Acme"), ("enabled", "on")])
                        app

                response.simpleStatus `shouldBe` status201
                JSON.decode response.simpleBody
                    `shouldBe` Just (JSON.object ["ok" JSON..= True, "projectName" JSON..= ("Form Acme" :: Text)])

            it "rejects invalid typed route request bodies before the handler runs" do
                app <- createTypedRouteTestApplication

                response <-
                    WaiTest.runSession
                        (testPostJson "projects/42" (JSON.object ["enabled" JSON..= True]))
                        app

                response.simpleStatus `shouldBe` status400

requestWithBody :: [Header] -> RequestBody -> Wai.Request
requestWithBody headers body =
    Wai.defaultRequest
        { Wai.requestHeaders = headers
        , Wai.vault = Vault.insert requestBodyVaultKey body Vault.empty
        }

createControllerContext :: IO ControllerContext
createControllerContext = do
    frameworkConfig <- FrameworkConfig.buildFrameworkConfig (pure ())
    let requestBody = FormBody{params = [], files = [], rawPayload = ""}
    let request =
            Wai.defaultRequest
                { Wai.vault =
                    Vault.insert RequestVault.frameworkConfigVaultKey frameworkConfig
                        $ Vault.insert requestBodyVaultKey requestBody Vault.empty
                }
    let customFields = TypeMap.insert request TypeMap.empty
    pure FrozenControllerContext{customFields}

createTypedRouteTestApplication :: IO Wai.Application
createTypedRouteTestApplication = do
    frameworkConfig <- FrameworkConfig.buildFrameworkConfig (pure ())
    let modelContext = notConnectedModelContext frameworkConfig.logger
    middleware <- Server.initMiddlewareStack frameworkConfig modelContext Nothing
    pure (middleware (frontControllerToWAIApp @TypedRouteApplication @AutoRefreshWSApp id TypedRouteApplication typedRouteNotFound))

typedRouteNotFound :: Wai.Application
typedRouteNotFound _ respond =
    respond (Wai.responseLBS status404 [] "not found")

testPostJson :: ByteString -> JSON.Value -> WaiTest.Session WaiTest.SResponse
testPostJson path body =
    WaiTest.srequest (WaiTest.SRequest request (JSON.encode body))
  where
    request =
        WaiTest.setPath
            WaiTest.defaultRequest
                { Wai.requestMethod = methodPost
                , Wai.requestHeaders =
                    [ (hAccept, "application/json")
                    , (hContentType, "application/json")
                    ]
                }
            path

testGetJson :: ByteString -> WaiTest.Session WaiTest.SResponse
testGetJson path =
    WaiTest.srequest (WaiTest.SRequest request "")
  where
    request =
        WaiTest.setPath
            WaiTest.defaultRequest
                { Wai.requestMethod = methodGet
                , Wai.requestHeaders =
                    [ (hAccept, "application/json")
                    ]
                }
            path

testJsonWithMethod :: Method -> ByteString -> JSON.Value -> WaiTest.Session WaiTest.SResponse
testJsonWithMethod method path body =
    WaiTest.srequest (WaiTest.SRequest request (JSON.encode body))
  where
    request =
        WaiTest.setPath
            WaiTest.defaultRequest
                { Wai.requestMethod = method
                , Wai.requestHeaders =
                    [ (hAccept, "application/json")
                    , (hContentType, "application/json")
                    ]
                }
            path

testPostForm :: ByteString -> [(ByteString, ByteString)] -> WaiTest.Session WaiTest.SResponse
testPostForm path params =
    WaiTest.srequest (WaiTest.SRequest request (LBS.fromStrict (renderSimpleQuery False params)))
  where
    request =
        WaiTest.setPath
            WaiTest.defaultRequest
                { Wai.requestMethod = methodPost
                , Wai.requestHeaders =
                    [ (hAccept, "application/json")
                    , (hContentType, "application/x-www-form-urlencoded")
                    ]
                }
            path

complexFormContext :: (?request :: Request) => ComplexFormInput -> FormContext ComplexFormInput
complexFormContext = createInputFormContext

unsafeDecode :: JSON.FromJSON value => LBS.ByteString -> value
unsafeDecode payload =
    case JSON.decode payload of
        Just value -> value
        Nothing -> error "invalid test json"

parameterShape :: ParameterDoc -> (Text, ParameterLocation, Bool)
parameterShape ParameterDoc{parameterName, parameterLocation, parameterRequired} =
    (parameterName, parameterLocation, parameterRequired)

lookupValue :: Text -> JSON.Value -> Maybe JSON.Value
lookupValue key (JSON.Object object) = KeyMap.lookup (Key.fromText key) object
lookupValue _ _ = Nothing

lookupPathOperation :: Text -> Text -> JSON.Value -> Maybe JSON.Value
lookupPathOperation path method spec = do
    paths <- lookupValue "paths" spec
    pathItem <- lookupValue path paths
    lookupValue method pathItem

lookupParameter :: Text -> JSON.Value -> Maybe JSON.Value
lookupParameter name operation = do
    JSON.Array parameters <- lookupValue "parameters" operation
    parameters
        |> Vector.toList
        |> find (\parameter -> lookupValue "name" parameter == Just (JSON.String name))
