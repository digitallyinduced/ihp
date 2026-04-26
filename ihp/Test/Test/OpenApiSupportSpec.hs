{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.OpenApiSupportSpec where

import ClassyPrelude hiding (handle)
import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Attoparsec.ByteString.Char8 (decimal, endOfInput, string)
import Data.Text qualified as Text
import IHP.Controller.ActionDefinition
import IHP.ControllerPrelude hiding (description, find, get, operationId, request, successResponseDescription, successStatus, summary, tags)
import IHP.Environment
import IHP.Test.Mocking
import IHP.ViewPrelude
import Network.HTTP.Types
import Network.Wai.Test
import Test.Hspec
import Prelude qualified

data Band' = Band {id :: Id' "open_api_bands", meta :: MetaBag} deriving (Eq, Show)
type Band = Band'
type instance GetTableName Band' = "open_api_bands"
type instance GetModelByTableName "open_api_bands" = Band
type instance GetModelName Band' = "Band"
type instance PrimaryKey "open_api_bands" = Integer

data Performance' = Performance {id :: Id' "open_api_performances", meta :: MetaBag} deriving (Eq, Show)
type Performance = Performance'
type instance GetTableName Performance' = "open_api_performances"
type instance GetModelByTableName "open_api_performances" = Performance
type instance GetModelName Performance' = "Performance"
type instance PrimaryKey "open_api_performances" = UUID

data WebApplication = WebApplication deriving (Eq, Show, Data)

data DocumentedController
    = ShowBandAction {bandId :: !(Id Band), page :: !(Maybe Int), tags :: ![Text]}
    | RawJsonValueAction
    | WrongJsonAction
    | WrongJsonShapeAction {bandId :: !(Id Band)}
    deriving (Eq, Show, Data)

data CustomRouteController
    = ListCustomAction
    | ShowCustomAction {performanceId :: !(Id Performance)}
    deriving (Eq, Show, Data)

data DocumentedCustomPathController
    = ShowDocumentedCustomPathAction {bandId :: !(Id Band)}
    deriving (Eq, Show, Data)

data UnsupportedCustomPathApplication = UnsupportedCustomPathApplication deriving (Eq, Show, Data)

data UnsupportedCustomPathController
    = ShowUnsupportedCustomPathAction {bandId :: !(Id Band)}
    deriving (Eq, Show, Data)

data CrudNamedApiController
    = CreateApiSessionAction
    | CreatePipeSessionAction
    | ShowApiSessionAction
    deriving (Eq, Show, Data)

data BandView = BandView
    { bandId :: !(Id Band)
    , page :: !(Maybe Int)
    , tags :: ![Text]
    }

data BandPayload = BandPayload
    { bandId :: !Integer
    , page :: !(Maybe Int)
    , tags :: ![Text]
    }
    deriving (Eq, Show, Generic)

instance JSON.ToJSON BandPayload
instance ToSchema BandPayload

data RawJsonValueView = RawJsonValueView
data WrongJsonShapeView = WrongJsonShapeView {bandId :: !(Id Band)}
data DocumentedCustomPathView = DocumentedCustomPathView {bandId :: !(Id Band)}
data UnsupportedCustomPathView = UnsupportedCustomPathView {bandId :: !(Id Band)}
data AckView = AckView

data AckPayload = AckPayload
    { ok :: !Bool
    }
    deriving (Eq, Show, Generic)

instance JSON.ToJSON AckPayload
instance ToSchema AckPayload

data CreateSessionRequest = CreateSessionRequest
    { token :: !Text
    }
    deriving (Eq, Show, Generic)

instance JSON.ToJSON CreateSessionRequest
instance JSON.FromJSON CreateSessionRequest
instance ToSchema CreateSessionRequest

instance View BandView where
    type JsonResponse BandView = BandPayload

    html BandView{..} = [hsx||]

    json BandView{..} =
        BandPayload
            { bandId = unpackId bandId
            , page
            , tags
            }

instance View RawJsonValueView where
    html RawJsonValueView = [hsx||]

    json RawJsonValueView =
        JSON.object
            [ "legacy" JSON..= True
            ]

instance View WrongJsonShapeView where
    type JsonResponse WrongJsonShapeView = BandPayload

    html WrongJsonShapeView{..} = [hsx||]

    json WrongJsonShapeView{..} =
        BandPayload
            { bandId = unpackId bandId
            , page = Nothing
            , tags = []
            }

instance View DocumentedCustomPathView where
    type JsonResponse DocumentedCustomPathView = BandPayload

    html DocumentedCustomPathView{..} = [hsx||]

    json DocumentedCustomPathView{..} =
        BandPayload
            { bandId = unpackId bandId
            , page = Nothing
            , tags = []
            }

instance View UnsupportedCustomPathView where
    type JsonResponse UnsupportedCustomPathView = BandPayload

    html UnsupportedCustomPathView{..} = [hsx||]

    json UnsupportedCustomPathView{..} =
        BandPayload
            { bandId = unpackId bandId
            , page = Nothing
            , tags = []
            }

instance View AckView where
    type JsonResponse AckView = AckPayload

    html AckView = [hsx||]

    json AckView = AckPayload{ok = True}

instance Controller DocumentedController where
    type ControllerAction DocumentedController = ActionDefinition DocumentedController

    action ShowBandAction{..} =
        endpoint
            |> responseView @BandView
            |> summary "Show a band payload"
            |> handle (pure @IO BandView{..})
    action RawJsonValueAction =
        legacyAction (renderHtmlOrJson RawJsonValueView)
    action WrongJsonAction =
        legacyAction (renderHtmlOrJson RawJsonValueView)
    action WrongJsonShapeAction{..} =
        endpoint
            |> responseView @WrongJsonShapeView
            |> handle (pure @IO WrongJsonShapeView{..})

instance Controller CustomRouteController where
    action ListCustomAction = renderPlain "ListCustomAction"
    action ShowCustomAction{..} = renderPlain (cs (Prelude.show performanceId))

instance Controller DocumentedCustomPathController where
    type ControllerAction DocumentedCustomPathController = ActionDefinition DocumentedCustomPathController

    action ShowDocumentedCustomPathAction{..} =
        endpoint
            |> responseView @DocumentedCustomPathView
            |> handle (pure @IO DocumentedCustomPathView{..})

instance Controller UnsupportedCustomPathController where
    type ControllerAction UnsupportedCustomPathController = ActionDefinition UnsupportedCustomPathController

    action ShowUnsupportedCustomPathAction{..} =
        endpoint
            |> responseView @UnsupportedCustomPathView
            |> handle (pure @IO UnsupportedCustomPathView{..})

instance Controller CrudNamedApiController where
    type ControllerAction CrudNamedApiController = ActionDefinition CrudNamedApiController

    action CreateApiSessionAction =
        endpoint
            |> responseView @AckView
            |> handle \(_ :: CreateSessionRequest) ->
                pure @IO AckView
    action CreatePipeSessionAction =
        endpoint
            |> responseView @AckView
            |> successStatus status201
            |> successResponseDescription "Created response"
            |> handle \(_ :: CreateSessionRequest) ->
                pure @IO AckView
    action ShowApiSessionAction =
        endpoint
            |> responseView @AckView
            |> handle (pure @IO AckView)

instance AutoRoute DocumentedController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id Band))
    applyAction = applyConstr (parseIntegerId @(Id Band))

instance AutoRoute CustomRouteController where
    customRoutes = do
        string "/custom/"
        performanceId <- parseId
        endOfInput
        onlyAllowMethods [GET, HEAD]
        pure ShowCustomAction{performanceId}

    customPathTo ShowCustomAction{performanceId} = Just ("/custom/" <> cs (Prelude.show performanceId))
    customPathTo _ = Nothing

instance AutoRoute DocumentedCustomPathController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id Band))
    applyAction = applyConstr (parseIntegerId @(Id Band))

    customRoutes = do
        string "/bands/"
        bandId <- packId <$> decimal
        endOfInput
        onlyAllowMethods [GET, HEAD]
        pure ShowDocumentedCustomPathAction{bandId}

    customPathTo ShowDocumentedCustomPathAction{bandId} = Just ("/bands/" <> cs (Prelude.show (unpackId bandId)))

instance AutoRoute UnsupportedCustomPathController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id Band))
    applyAction = applyConstr (parseIntegerId @(Id Band))

    customPathTo ShowUnsupportedCustomPathAction{bandId} = Just ("/unsupported-bands/" <> cs (Prelude.show (unpackId bandId)))

instance AutoRoute CrudNamedApiController

instance FrontController WebApplication where
    controllers =
        [ documentRoute @DocumentedController
        , documentRoute @DocumentedCustomPathController
        , documentRoute @CrudNamedApiController
        , parseRoute @CustomRouteController
        , swaggerUiWithOptions ((defaultSwaggerUiOptions @WebApplication){swaggerUiPath = "/docs", swaggerUiTitle = Just "Band API Docs"})
        ]

instance FrontController RootApplication where
    controllers = [mountFrontController WebApplication]

instance FrontController UnsupportedCustomPathApplication where
    controllers = [documentRoute @UnsupportedCustomPathController]

defaultLayout :: Html -> Html
defaultLayout inner = [hsx|{inner}|]

instance InitControllerContext WebApplication where
    initContext = setLayout defaultLayout

instance InitControllerContext RootApplication
instance InitControllerContext UnsupportedCustomPathApplication

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
        |> toList
        |> ClassyPrelude.find (\parameter -> lookupValue "name" parameter == Just (JSON.String name))

config :: ConfigBuilder
config = do
    option Development
    option (AppPort 8000)

tests :: Spec
tests = aroundAll (withMockContextAndApp RootApplication config) do
    describe "JSON rendering" do
        it "renders typed json values through renderHtmlOrJson" $ withContextAndApp \application -> do
            let expected =
                    JSON.object
                        [ "bandId" JSON..= (12 :: Integer)
                        , "page" JSON..= Just (2 :: Int)
                        , "tags" JSON..= (["rock", "jazz"] :: [Text])
                        ]
            runSession (testJson "test/ShowBand?bandId=12&page=2&tags=rock,jazz") application >>= assertJsonBody expected

        it "keeps JSON.Value responses working through json" $ withContextAndApp \application -> do
            let expected = JSON.object ["legacy" JSON..= True]
            runSession (testJson "test/RawJsonValue") application >>= assertJsonBody expected

        it "renders documented actions from json so the JSON shape cannot diverge from the typed response" $ withContextAndApp \application -> do
            let expected =
                    JSON.object
                        [ "bandId" JSON..= (12 :: Integer)
                        , "page" JSON..= (Nothing :: Maybe Int)
                        , "tags" JSON..= ([] :: [Text])
                        ]
            runSession (testJson "test/WrongJsonShape?bandId=12") application >>= assertJsonBody expected

        it "runs inspectable endpoint actions with decoded request bodies" $ withContextAndApp \application -> do
            let expected = JSON.object ["ok" JSON..= True]
            runSession (testPostJson "test/CreateApiSession" (JSON.object ["token" JSON..= ("abc" :: Text)])) application >>= assertJsonBody expected

        it "uses endpoint success status for inspectable actions" $ withContextAndApp \application -> do
            response <- runSession (testPostJson "test/CreatePipeSession" (JSON.object ["token" JSON..= ("abc" :: Text)])) application
            response.simpleStatus `shouldBe` status201
            JSON.decode response.simpleBody `shouldBe` Just (JSON.object ["ok" JSON..= True])

    describe "OpenAPI generation" do
        it "derives AutoRoute paths, methods, params and response schemas from the controller" $ withContextAndApp \_ -> do
            let spec = buildOpenApi RootApplication

            lookupValue "openapi" spec `shouldBe` Just (JSON.String "3.0.3")

            let getOperation = lookupPathOperation "/test/ShowBand" "get" spec
            let headOperation = lookupPathOperation "/test/ShowBand" "head" spec

            headOperation `shouldSatisfy` isJust
            getOperation `shouldSatisfy` isJust

            let Just operation = getOperation

            let Just bandIdParameter = lookupParameter "bandId" operation
            lookupValue "required" bandIdParameter `shouldBe` Just (JSON.Bool True)
            (lookupValue "type" =<< lookupValue "schema" bandIdParameter) `shouldBe` Just (JSON.String "integer")

            let Just pageParameter = lookupParameter "page" operation
            lookupValue "required" pageParameter `shouldBe` Just (JSON.Bool False)

            let Just tagsParameter = lookupParameter "tags" operation
            lookupValue "explode" tagsParameter `shouldBe` Just (JSON.Bool False)
            (lookupValue "type" =<< lookupValue "schema" tagsParameter) `shouldBe` Just (JSON.String "array")

            let Just schema =
                    lookupValue "responses" operation
                        >>= lookupValue "200"
                        >>= lookupValue "content"
                        >>= lookupValue "application/json"
                        >>= lookupValue "schema"
            lookupValue "$ref" schema `shouldBe` Just (JSON.String "#/components/schemas/BandPayload")

            let Just componentsSchemas = lookupValue "components" spec >>= lookupValue "schemas"
            let Just bandPayloadSchema = lookupValue "BandPayload" componentsSchemas
            lookupValue "type" bandPayloadSchema `shouldBe` Just (JSON.String "object")
            (lookupValue "properties" bandPayloadSchema >>= lookupValue "bandId") `shouldSatisfy` isJust

        it "omits undocumented custom routes from the generated spec" $ withContextAndApp \_ -> do
            let spec = buildOpenApi RootApplication
            lookupPathOperation "/test/ShowCustom" "get" spec `shouldBe` Nothing
            lookupPathOperation "/custom/{performanceId}" "get" spec `shouldBe` Nothing

        it "documents basic customPathTo routes verified by customRoutes" $ withContextAndApp \_ -> do
            let spec = buildOpenApi RootApplication
            lookupPathOperation "/test/ShowDocumentedCustomPath" "get" spec `shouldBe` Nothing
            let Just operation = lookupPathOperation "/bands/{bandId}" "get" spec
            let Just bandIdParameter = lookupParameter "bandId" operation
            lookupValue "in" bandIdParameter `shouldBe` Just (JSON.String "path")
            lookupValue "required" bandIdParameter `shouldBe` Just (JSON.Bool True)
            (lookupValue "type" =<< lookupValue "schema" bandIdParameter) `shouldBe` Just (JSON.String "integer")

        it "fails OpenAPI generation when customPathTo is not backed by customRoutes" $ withContextAndApp \_ -> do
            evaluate (buildOpenApi UnsupportedCustomPathApplication) `shouldThrow` \(OpenApiGenerationException message) ->
                "customRoutes does not parse that path" `Text.isInfixOf` message

        it "keeps CreateApi and ShowApi action names unchanged in documented AutoRoute paths" $ withContextAndApp \_ -> do
            let spec = buildOpenApi RootApplication

            lookupPathOperation "/test/CreateApiSession" "post" spec `shouldSatisfy` isJust
            lookupPathOperation "/test/CreatePipeSession" "post" spec `shouldSatisfy` isJust
            lookupPathOperation "/test/CreateApiSession" "get" spec `shouldBe` Nothing
            lookupPathOperation "/test/ShowApiSession" "get" spec `shouldSatisfy` isJust
            lookupPathOperation "/test/ShowApiSession" "head" spec `shouldSatisfy` isJust
            lookupPathOperation "/test/ApiSession" "post" spec `shouldBe` Nothing
            lookupPathOperation "/test/ApiSession" "get" spec `shouldBe` Nothing

        it "includes request body schemas for documented JSON actions" $ withContextAndApp \_ -> do
            let spec = buildOpenApi RootApplication

            let Just operation = lookupPathOperation "/test/CreateApiSession" "post" spec
            let Just requestBody =
                    lookupValue "requestBody" operation
                        >>= lookupValue "content"
                        >>= lookupValue "application/json"
                        >>= lookupValue "schema"

            lookupValue "$ref" requestBody `shouldBe` Just (JSON.String "#/components/schemas/CreateSessionRequest")

            let Just componentsSchemas = lookupValue "components" spec >>= lookupValue "schemas"
            let Just createSessionRequestSchema = lookupValue "CreateSessionRequest" componentsSchemas
            lookupValue "type" createSessionRequestSchema `shouldBe` Just (JSON.String "object")
            (lookupValue "properties" createSessionRequestSchema >>= lookupValue "token") `shouldSatisfy` isJust

        it "supports request body and success metadata setters on action docs" $ withContextAndApp \_ -> do
            let spec = buildOpenApi RootApplication

            let Just operation = lookupPathOperation "/test/CreatePipeSession" "post" spec
            let Just requestBody =
                    lookupValue "requestBody" operation
                        >>= lookupValue "content"
                        >>= lookupValue "application/json"
                        >>= lookupValue "schema"

            lookupValue "$ref" requestBody `shouldBe` Just (JSON.String "#/components/schemas/CreateSessionRequest")

            let Just createdResponse =
                    lookupValue "responses" operation
                        >>= lookupValue "201"

            lookupValue "description" createdResponse `shouldBe` Just (JSON.String "Created response")
            (lookupValue "responses" operation >>= lookupValue "200") `shouldBe` Nothing

    describe "Swagger UI" do
        it "serves the generated OpenAPI JSON from the mounted router" $ withContextAndApp \application -> do
            response <- runSession (testGet "docs/openapi.json") application
            response.simpleStatus `shouldBe` status200
            Prelude.lookup hContentType response.simpleHeaders `shouldBe` Just "application/json"
            JSON.decode response.simpleBody `shouldBe` Just (buildOpenApi WebApplication)

        it "serves a Swagger UI page pointing at the generated OpenAPI JSON" $ withContextAndApp \application -> do
            response <- runSession (testGet "docs") application
            response.simpleStatus `shouldBe` status200
            Prelude.lookup hContentType response.simpleHeaders `shouldBe` Just "text/html; charset=utf-8"
            let body = cs response.simpleBody
            Text.isInfixOf "Band API Docs" body `shouldBe` True
            Text.isInfixOf "./openapi.json" body `shouldBe` True
            Text.isInfixOf "SwaggerUIBundle" body `shouldBe` True
