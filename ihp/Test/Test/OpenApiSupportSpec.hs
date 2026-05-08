{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.OpenApiSupportSpec where

import ClassyPrelude hiding (find)
import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.OpenApi qualified as OpenApi
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import IHP.TypedControllerPrelude hiding (request)
import IHP.Environment
import IHP.Test.Mocking
import IHP.ViewPrelude hiding (action)
import Network.HTTP.Types
import Network.Wai.Test
import Test.Hspec
import Prelude qualified
import IHP.Router.Capture (parseCapture, renderCapture)
import IHP.Router.DSL (routes)

data WebApplication = WebApplication deriving (Eq, Show)

data BandView = BandView
    { bandId :: !Int
    , page :: !(Maybe Int)
    , bandTags :: ![Text]
    }

data BandPayload = BandPayload
    { bandId :: !Int
    , page :: !(Maybe Int)
    , tags :: ![Text]
    }
    deriving (Eq, Show)

$(deriveApiRecord ''BandPayload)

data AckPayload = AckPayload
    { ok :: !Bool
    }
    deriving (Eq, Show, Generic)

instance JSON.ToJSON AckPayload
instance ToSchema AckPayload

data ApiError = ApiError
    { code :: !Text
    , message :: !Text
    }
    deriving (Eq, Show, Generic)

instance JSON.ToJSON ApiError
instance ToSchema ApiError

data CreateSessionRequest = CreateSessionRequest
    { token :: !Text
    }
    deriving (Eq, Show, Generic)

instance JSON.ToJSON CreateSessionRequest
instance JSON.FromJSON CreateSessionRequest
instance ToSchema CreateSessionRequest

instance View BandView where
    html BandView{..} = [hsx||]

instance JsonView BandView where
    type JsonResponse BandView = BandPayload

    json = bandPayloadFromView

bandPayloadFromView :: BandView -> BandPayload
bandPayloadFromView BandView{..} =
    BandPayload
        { bandId
        , page
        , tags = bandTags
        }

data RawJsonValueView = RawJsonValueView
    deriving (Eq, Show)

instance JsonView RawJsonValueView where
    type JsonResponse RawJsonValueView = JSON.Value

    json RawJsonValueView =
        JSON.object ["legacy" JSON..= True]

data WrongBandView = WrongBandView
    { wrongBandId :: !Int
    }
    deriving (Eq, Show)

instance JsonView WrongBandView where
    type JsonResponse WrongBandView = BandPayload

    json WrongBandView{..} =
        BandPayload{bandId = wrongBandId, page = Nothing, tags = []}

data AckView = AckView
    { ackOk :: !Bool
    }
    deriving (Eq, Show)

instance JsonView AckView where
    type JsonResponse AckView = AckPayload

    json AckView{..} =
        AckPayload{ok = ackOk}

data ApiErrorView = ApiErrorView
    { errorCode :: !Text
    , errorMessage :: !Text
    }
    deriving (Eq, Show)

instance JsonView ApiErrorView where
    type JsonResponse ApiErrorView = ApiError

    json ApiErrorView{..} =
        ApiError{code = errorCode, message = errorMessage}

data CreatePipeSessionResponse
    = PipeSessionCreated (JsonViewResponse AckView)
    | PipeSessionRejected (JsonViewResponse ApiErrorView)
    | PipeSessionConflict (JsonViewResponse ApiErrorView)

data ApiAction body response where
    ShowBandAction ::
        { bandId :: !Int
        , page :: !(Maybe Int)
        , bandTags :: ![Text]
        } ->
        ApiAction 'NoBody (ViewOrJsonResponse BandView)
    RawJsonValueAction :: ApiAction 'NoBody (JsonViewResponse RawJsonValueView)
    WrongJsonShapeAction :: {wrongBandId :: !Int} -> ApiAction 'NoBody (JsonViewResponse WrongBandView)
    CreateApiSessionAction :: ApiAction ('Body CreateSessionRequest) (JsonViewResponse AckView)
    CreatePipeSessionAction :: ApiAction ('Body CreateSessionRequest) CreatePipeSessionResponse
    ShowApiSessionAction :: ApiAction 'NoBody (JsonViewResponse AckView)
    DeleteApiSessionAction :: ApiAction 'NoBody NoContent
    PlainTextHealthAction :: ApiAction 'NoBody PlainText

deriving instance Show (ApiAction body response)
deriving instance Eq (ApiAction body response)

instance TypedController ApiAction where
    action ShowBandAction{..} () =
        let view = BandView{..}
         in pure (ViewOrJsonResponse view)

    action RawJsonValueAction () =
        pure (JsonViewResponse RawJsonValueView)

    action WrongJsonShapeAction{..} () =
        pure (JsonViewResponse WrongBandView{wrongBandId})

    action CreateApiSessionAction body = do
        let _token = bodyParam body #token
        pure (JsonViewResponse AckView{ackOk = True})

    action CreatePipeSessionAction body =
        case bodyParam body #token of
            "bad" ->
                pure
                    ( PipeSessionRejected
                        (JsonViewResponse ApiErrorView{errorCode = "invalid_token", errorMessage = "Invalid token"})
                    )
            "exists" ->
                pure
                    ( PipeSessionConflict
                        (JsonViewResponse ApiErrorView{errorCode = "session_exists", errorMessage = "Session already exists"})
                    )
            _ ->
                pure (PipeSessionCreated (JsonViewResponse AckView{ackOk = True}))

    action ShowApiSessionAction () =
        pure (JsonViewResponse AckView{ackOk = True})

    action DeleteApiSessionAction () =
        pure NoContent

    action PlainTextHealthAction () =
        pure (PlainText "ok")

$(pure [])

[routes|openApiTestRoutes
GET|HEAD /test/bands/{bandId}?page&tags ShowBandAction { bandTags = #tags }
  summary: Show a band payload
  tags: Bands, Search
GET /test/raw-json                  RawJsonValueAction
  private
GET /test/wrong-json/{wrongBandId}  WrongJsonShapeAction
  summary: Wrong shape route
POST /test/CreateApiSession         CreateApiSessionAction
  summary: Create API session
  description: Creates a JSON API session.
  operationId: createApiSession
POST /test/CreatePipeSession        CreatePipeSessionAction
  summary: Create pipe session
  tags: Sessions
  response PipeSessionCreated: 201 Created response
  response PipeSessionRejected: 400 Invalid session request
  response PipeSessionConflict: 409 Session already exists
GET|HEAD /test/ShowApiSession       ShowApiSessionAction
  summary: Show API session
DELETE /test/DeleteApiSession       DeleteApiSessionAction
  summary: Delete API session
  success: 204 No Content
GET /test/health.txt                PlainTextHealthAction
  summary: Plain text health check
GET /docs                          SwaggerUiAction
GET /docs/openapi.json             OpenApiJsonAction
|]

instance SwaggerUiControllerConfig WebApplication where
    swaggerUiControllerOptions =
        (defaultSwaggerUiOptions @WebApplication)
            { swaggerUiTitle = Just "Band API Docs"
            }
    swaggerUiControllerOpenApiOptions = webApplicationOpenApiOptions

webApplicationOpenApiOptions :: OpenApiOptions
webApplicationOpenApiOptions =
    (defaultOpenApiOptions @WebApplication)
        { openApiOptionsInfo =
            OpenApi.Info
                "Band API"
                (Just "JSON endpoints for bands and sessions")
                (Just "https://example.com/terms")
                (Just (OpenApi.Contact (Just "Band Support") (Just (OpenApi.URL "https://example.com/support")) (Just "support@example.com")))
                (Just (OpenApi.License "MIT" (Just (OpenApi.URL "https://example.com/license"))))
                "2026.1"
        , openApiOptionsServers =
            [ OpenApi.Server "https://api.example.com" (Just "Production") mempty
            ]
        , openApiOptionsTags =
            [ OpenApi.Tag
                "Bands"
                (Just "Band payload endpoints")
                (Just (OpenApi.ExternalDocs (Just "Band guide") (OpenApi.URL "https://docs.example.com/bands")))
            ]
        , openApiOptionsExternalDocs =
            Just (OpenApi.ExternalDocs (Just "Full API documentation") (OpenApi.URL "https://docs.example.com"))
        }

instance FrontController WebApplication where
    controllers = openApiTestRoutes

instance FrontController RootApplication where
    controllers =
        [ mountFrontController WebApplication
        , withPrefix "/api" openApiTestRoutes
        ]

defaultLayout :: Html -> Html
defaultLayout inner = [hsx|{inner}|]

instance InitControllerContext WebApplication where
    initContext = setLayout defaultLayout

instance InitControllerContext RootApplication

config :: ConfigBuilder
config = do
    option Development
    option (AppPort 8000)

testJson :: ByteString -> Session SResponse
testJson url =
    request $
        setPath
            defaultRequest
                { requestMethod = methodGet
                , requestHeaders = [(hAccept, "application/json")]
                }
            url

testPostJson :: ByteString -> JSON.Value -> Session SResponse
testPostJson url body =
    srequest $ SRequest request (JSON.encode body)
  where
    request =
        setPath
            defaultRequest
                { requestMethod = methodPost
                , requestHeaders =
                    [ (hAccept, "application/json")
                    , (hContentType, "application/json")
                    ]
                }
            url

testGet :: ByteString -> Session SResponse
testGet url = request $ setPath defaultRequest{requestMethod = methodGet} url

testDelete :: ByteString -> Session SResponse
testDelete url = request $ setPath defaultRequest{requestMethod = methodDelete} url

assertJsonBody :: JSON.Value -> SResponse -> IO ()
assertJsonBody expected response = do
    response.simpleStatus `shouldBe` status200
    JSON.decode response.simpleBody `shouldBe` Just expected

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

tests :: Spec
tests = aroundAll (withMockContextAndApp RootApplication config) do
    describe "JSON rendering" do
        it "renders ViewOrJsonResponse responses as JSON for API clients" $ withContextAndApp \application -> do
            let expected =
                    JSON.object
                        [ "bandId" JSON..= (12 :: Int)
                        , "page" JSON..= Just (2 :: Int)
                        , "tags" JSON..= (["rock", "jazz"] :: [Text])
                        ]
            runSession (testJson "test/bands/12?page=2&tags=rock&tags=jazz") application >>= assertJsonBody expected

        it "renders ViewOrJsonResponse responses as HTML for browser requests" $ withContextAndApp \application -> do
            response <- runSession (testGet "test/bands/12?page=2") application

            response.simpleStatus `shouldBe` status200
            Prelude.lookup hContentType response.simpleHeaders `shouldBe` Just "text/html; charset=utf-8"
            (JSON.decode response.simpleBody :: Maybe JSON.Value) `shouldBe` Nothing

        it "keeps JSON.Value responses working through JsonViewResponse" $ withContextAndApp \application -> do
            let expected = JSON.object ["legacy" JSON..= True]
            runSession (testJson "test/raw-json") application >>= assertJsonBody expected

        it "renders JSON-only JsonViewResponse payloads" $ withContextAndApp \application -> do
            let expected =
                    JSON.object
                        [ "bandId" JSON..= (12 :: Int)
                        , "page" JSON..= (Nothing :: Maybe Int)
                        , "tags" JSON..= ([] :: [Text])
                        ]
            runSession (testJson "test/wrong-json/12") application >>= assertJsonBody expected

        it "runs typed actions with decoded request bodies" $ withContextAndApp \application -> do
            let expected = JSON.object ["ok" JSON..= True]
            runSession (testPostJson "test/CreateApiSession" (JSON.object ["token" JSON..= ("abc" :: Text)])) application >>= assertJsonBody expected

        it "renders NoContent responses without a body" $ withContextAndApp \application -> do
            response <- runSession (testDelete "test/DeleteApiSession") application
            response.simpleStatus `shouldBe` status204
            response.simpleBody `shouldBe` ""

        it "renders PlainText responses" $ withContextAndApp \application -> do
            response <- runSession (testGet "test/health.txt") application
            response.simpleStatus `shouldBe` status200
            response.simpleBody `shouldBe` "ok"
            Prelude.lookup hContentType response.simpleHeaders `shouldBe` Just "text/plain"

        it "routes typed DSL actions mounted with withPrefix" $ withContextAndApp \application -> do
            let expected =
                    JSON.object
                        [ "bandId" JSON..= (12 :: Int)
                        , "page" JSON..= Just (2 :: Int)
                        , "tags" JSON..= ([] :: [Text])
                        ]
            runSession (testJson "api/test/bands/12?page=2") application >>= assertJsonBody expected

        it "uses route success status for typed actions" $ withContextAndApp \application -> do
            response <- runSession (testPostJson "test/CreatePipeSession" (JSON.object ["token" JSON..= ("abc" :: Text)])) application
            response.simpleStatus `shouldBe` status201
            JSON.decode response.simpleBody `shouldBe` Just (JSON.object ["ok" JSON..= True])

        it "uses route response statuses for typed error outcomes" $ withContextAndApp \application -> do
            badResponse <- runSession (testPostJson "test/CreatePipeSession" (JSON.object ["token" JSON..= ("bad" :: Text)])) application
            badResponse.simpleStatus `shouldBe` status400
            JSON.decode badResponse.simpleBody
                `shouldBe` Just (JSON.object ["code" JSON..= ("invalid_token" :: Text), "message" JSON..= ("Invalid token" :: Text)])

            conflictResponse <- runSession (testPostJson "test/CreatePipeSession" (JSON.object ["token" JSON..= ("exists" :: Text)])) application
            conflictResponse.simpleStatus `shouldBe` status409
            JSON.decode conflictResponse.simpleBody
                `shouldBe` Just (JSON.object ["code" JSON..= ("session_exists" :: Text), "message" JSON..= ("Session already exists" :: Text)])

    describe "OpenAPI generation" do
        it "derives paths, methods, params and response schemas from typed routes" $ withContextAndApp \_ -> do
            let spec = buildOpenApi WebApplication

            lookupValue "openapi" spec `shouldBe` Just (JSON.String "3.0.3")

            let getOperation = lookupPathOperation "/test/bands/{bandId}" "get" spec
            let headOperation = lookupPathOperation "/test/bands/{bandId}" "head" spec

            headOperation `shouldSatisfy` isJust
            getOperation `shouldSatisfy` isJust

            let Just operation = getOperation

            lookupValue "summary" operation `shouldBe` Just (JSON.String "Show a band payload")
            lookupValue "tags" operation `shouldBe` Just (JSON.Array (Vector.fromList [JSON.String "Bands", JSON.String "Search"]))

            let Just bandIdParameter = lookupParameter "bandId" operation
            lookupValue "in" bandIdParameter `shouldBe` Just (JSON.String "path")
            lookupValue "required" bandIdParameter `shouldBe` Just (JSON.Bool True)
            (lookupValue "type" =<< lookupValue "schema" bandIdParameter) `shouldBe` Just (JSON.String "integer")

            let Just pageParameter = lookupParameter "page" operation
            lookupValue "in" pageParameter `shouldBe` Just (JSON.String "query")
            lookupValue "required" pageParameter `shouldBe` Just (JSON.Bool False)

            let Just tagsParameter = lookupParameter "tags" operation
            lookupValue "required" tagsParameter `shouldBe` Just (JSON.Bool False)
            (lookupValue "type" =<< lookupValue "schema" tagsParameter) `shouldBe` Just (JSON.String "array")

            let Just schema =
                    lookupValue "responses" operation
                        >>= lookupValue "200"
                        >>= lookupValue "content"
                        >>= lookupValue "application/json"
                        >>= lookupValue "schema"
            lookupValue "$ref" schema `shouldBe` Just (JSON.String "#/components/schemas/BandPayload")
            ( lookupValue "responses" operation
                >>= lookupValue "200"
                >>= lookupValue "content"
                >>= lookupValue "text/html"
                )
                `shouldSatisfy` isJust

            let Just componentsSchemas = lookupValue "components" spec >>= lookupValue "schemas"
            let Just bandPayloadSchema = lookupValue "BandPayload" componentsSchemas
            lookupValue "type" bandPayloadSchema `shouldBe` Just (JSON.String "object")
            (lookupValue "properties" bandPayloadSchema >>= lookupValue "bandId") `shouldSatisfy` isJust
            lookupValue "required" bandPayloadSchema
                `shouldBe` Just (JSON.Array (Vector.fromList [JSON.String "bandId", JSON.String "tags"]))

        it "omits private typed routes from the generated spec" $ withContextAndApp \_ -> do
            let spec = buildOpenApi WebApplication
            lookupPathOperation "/test/raw-json" "get" spec `shouldBe` Nothing

        it "documents typed DSL routes mounted with withPrefix" $ withContextAndApp \_ -> do
            let spec = buildOpenApi RootApplication
            lookupPathOperation "/api/test/bands/{bandId}" "get" spec `shouldSatisfy` isJust

        it "keeps CreateApi and ShowApi action names unchanged in inferred typed route paths" $ withContextAndApp \_ -> do
            let spec = buildOpenApi WebApplication

            let createApiOperation = lookupPathOperation "/test/CreateApiSession" "post" spec
            createApiOperation `shouldSatisfy` isJust
            (lookupValue "description" =<< createApiOperation) `shouldBe` Just (JSON.String "Creates a JSON API session.")
            (lookupValue "operationId" =<< createApiOperation) `shouldBe` Just (JSON.String "createApiSession")
            lookupPathOperation "/test/CreatePipeSession" "post" spec `shouldSatisfy` isJust
            lookupPathOperation "/test/CreateApiSession" "get" spec `shouldBe` Nothing
            lookupPathOperation "/test/ShowApiSession" "get" spec `shouldSatisfy` isJust
            lookupPathOperation "/test/ShowApiSession" "head" spec `shouldSatisfy` isJust
            lookupPathOperation "/test/ApiSession" "post" spec `shouldBe` Nothing
            lookupPathOperation "/test/ApiSession" "get" spec `shouldBe` Nothing

        it "includes request body schemas and response metadata for typed actions" $ withContextAndApp \_ -> do
            let spec = buildOpenApi WebApplication

            let Just operation = lookupPathOperation "/test/CreatePipeSession" "post" spec
            let Just requestBody =
                    lookupValue "requestBody" operation
                        >>= lookupValue "content"
                        >>= lookupValue "application/json"
                        >>= lookupValue "schema"

            lookupValue "$ref" requestBody `shouldBe` Just (JSON.String "#/components/schemas/CreateSessionRequest")

            let Just formBody =
                    lookupValue "requestBody" operation
                        >>= lookupValue "content"
                        >>= lookupValue "application/x-www-form-urlencoded"
            formBody `shouldSatisfy` const True

            let Just createdResponse =
                    lookupValue "responses" operation
                        >>= lookupValue "201"

            lookupValue "description" createdResponse `shouldBe` Just (JSON.String "Created response")
            (lookupValue "responses" operation >>= lookupValue "200") `shouldBe` Nothing

            let Just rejectedSchema =
                    lookupValue "responses" operation
                        >>= lookupValue "400"
                        >>= lookupValue "content"
                        >>= lookupValue "application/json"
                        >>= lookupValue "schema"
            lookupValue "$ref" rejectedSchema `shouldBe` Just (JSON.String "#/components/schemas/ApiError")

            let Just conflictSchema =
                    lookupValue "responses" operation
                        >>= lookupValue "409"
                        >>= lookupValue "content"
                        >>= lookupValue "application/json"
                        >>= lookupValue "schema"
            lookupValue "$ref" conflictSchema `shouldBe` Just (JSON.String "#/components/schemas/ApiError")

            let Just deleteOperation = lookupPathOperation "/test/DeleteApiSession" "delete" spec
            let Just noContentResponse = lookupValue "responses" deleteOperation >>= lookupValue "204"
            lookupValue "description" noContentResponse `shouldBe` Just (JSON.String "No Content")
            lookupValue "content" noContentResponse `shouldBe` Nothing

            let Just plainTextOperation = lookupPathOperation "/test/health.txt" "get" spec
            ( lookupValue "responses" plainTextOperation
                >>= lookupValue "200"
                >>= lookupValue "content"
                >>= lookupValue "text/plain"
                )
                `shouldSatisfy` isJust

        it "emits typed top-level OpenAPI metadata" $ withContextAndApp \_ -> do
            let spec = buildOpenApiWithOptions webApplicationOpenApiOptions WebApplication

            let Just info = lookupValue "info" spec
            lookupValue "title" info `shouldBe` Just (JSON.String "Band API")
            lookupValue "description" info `shouldBe` Just (JSON.String "JSON endpoints for bands and sessions")
            lookupValue "termsOfService" info `shouldBe` Just (JSON.String "https://example.com/terms")
            lookupValue "version" info `shouldBe` Just (JSON.String "2026.1")

            let Just contact = lookupValue "contact" info
            lookupValue "name" contact `shouldBe` Just (JSON.String "Band Support")
            lookupValue "url" contact `shouldBe` Just (JSON.String "https://example.com/support")
            lookupValue "email" contact `shouldBe` Just (JSON.String "support@example.com")

            let Just license = lookupValue "license" info
            lookupValue "name" license `shouldBe` Just (JSON.String "MIT")
            lookupValue "url" license `shouldBe` Just (JSON.String "https://example.com/license")

            let Just (JSON.Array servers) = lookupValue "servers" spec
            let Just server = Vector.toList servers |> listToMaybe
            lookupValue "url" server `shouldBe` Just (JSON.String "https://api.example.com")
            lookupValue "description" server `shouldBe` Just (JSON.String "Production")

            let Just (JSON.Array tags) = lookupValue "tags" spec
            let Just tag = Vector.toList tags |> listToMaybe
            lookupValue "name" tag `shouldBe` Just (JSON.String "Bands")
            lookupValue "description" tag `shouldBe` Just (JSON.String "Band payload endpoints")

            let Just externalDocs = lookupValue "externalDocs" spec
            lookupValue "description" externalDocs `shouldBe` Just (JSON.String "Full API documentation")
            lookupValue "url" externalDocs `shouldBe` Just (JSON.String "https://docs.example.com")

        it "allows free JSON top-level overrides" $ withContextAndApp \_ -> do
            let options =
                    (defaultOpenApiOptions @RootApplication)
                        |> overrideOpenApiTopLevel
                            ( JSON.object
                                [ "openapi" JSON..= ("3.1.0" :: Text)
                                , "x-logo" JSON..= JSON.object ["url" JSON..= ("/logo.svg" :: Text)]
                                ]
                            )
                spec = buildOpenApiWithOptions options RootApplication

            lookupValue "openapi" spec `shouldBe` Just (JSON.String "3.1.0")
            (lookupValue "x-logo" spec >>= lookupValue "url") `shouldBe` Just (JSON.String "/logo.svg")
            lookupValue "paths" spec `shouldSatisfy` isJust

    describe "Swagger UI" do
        it "serves the generated OpenAPI JSON from the mounted router" $ withContextAndApp \application -> do
            response <- runSession (testGet "docs/openapi.json") application
            response.simpleStatus `shouldBe` status200
            Prelude.lookup hContentType response.simpleHeaders `shouldBe` Just "application/json"
            JSON.decode response.simpleBody `shouldBe` Just (buildOpenApiWithOptions webApplicationOpenApiOptions WebApplication)

        it "serves a Swagger UI page pointing at the generated OpenAPI JSON" $ withContextAndApp \application -> do
            response <- runSession (testGet "docs") application
            response.simpleStatus `shouldBe` status200
            Prelude.lookup hContentType response.simpleHeaders `shouldBe` Just "text/html; charset=utf-8"
            let body = cs response.simpleBody
            Text.isInfixOf "Band API Docs" body `shouldBe` True
            Text.isInfixOf "/docs/openapi.json" body `shouldBe` True
            Text.isInfixOf "SwaggerUIBundle" body `shouldBe` True

        it "uses an OpenAPI URL that works with or without a trailing slash" $ withContextAndApp \application -> do
            response <- runSession (testGet "docs/") application
            response.simpleStatus `shouldBe` status200
            let body = cs response.simpleBody
            Text.isInfixOf "/docs/openapi.json" body `shouldBe` True
