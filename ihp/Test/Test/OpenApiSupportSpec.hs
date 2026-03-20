{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.OpenApiSupportSpec where

import ClassyPrelude
import qualified Prelude
import Test.Hspec
import IHP.Test.Mocking
import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.RouterSupport hiding (get)
import IHP.ViewPrelude
import IHP.ControllerPrelude hiding (get, request, find)
import Network.Wai.Test
import Network.HTTP.Types
import Data.Attoparsec.ByteString.Char8 (string, endOfInput)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text

data Band' = Band { id :: Id' "open_api_bands", meta :: MetaBag } deriving (Eq, Show)
type Band = Band'
type instance GetTableName Band' = "open_api_bands"
type instance GetModelByTableName "open_api_bands" = Band
type instance GetModelName Band' = "Band"
type instance PrimaryKey "open_api_bands" = Integer

data Performance' = Performance { id :: Id' "open_api_performances", meta :: MetaBag } deriving (Eq, Show)
type Performance = Performance'
type instance GetTableName Performance' = "open_api_performances"
type instance GetModelByTableName "open_api_performances" = Performance
type instance GetModelName Performance' = "Performance"
type instance PrimaryKey "open_api_performances" = UUID

data WebApplication = WebApplication deriving (Eq, Show, Data)

data DocumentedController
    = ShowBandAction { bandId :: !(Id Band), page :: !(Maybe Int), tags :: ![Text] }
    | LegacyJsonAction
    | WrongJsonAction
    | WrongJsonShapeAction { bandId :: !(Id Band) }
    deriving (Eq, Show, Data)

data CustomRouteController
    = ListCustomAction
    | ShowCustomAction { performanceId :: !(Id Performance) }
    deriving (Eq, Show, Data)

data DocumentedCustomPathController
    = ShowDocumentedCustomPathAction { bandId :: !(Id Band) }
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

data LegacyJsonView = LegacyJsonView
data WrongJsonShapeView = WrongJsonShapeView { bandId :: !(Id Band) }
data DocumentedCustomPathView = DocumentedCustomPathView { bandId :: !(Id Band) }

instance View BandView where
    html BandView { .. } = [hsx||]

    type JsonResponse BandView = BandPayload

    jsonTyped BandView { .. } = BandPayload
        { bandId = unpackId bandId
        , page
        , tags
        }

instance View LegacyJsonView where
    html LegacyJsonView = [hsx||]

    json LegacyJsonView = JSON.object
        [ "legacy" JSON..= True
        ]

instance View WrongJsonShapeView where
    html WrongJsonShapeView { .. } = [hsx||]

    type JsonResponse WrongJsonShapeView = BandPayload

    jsonTyped WrongJsonShapeView { .. } = BandPayload
        { bandId = unpackId bandId
        , page = Nothing
        , tags = []
        }

    json WrongJsonShapeView { .. } = JSON.object
        [ "legacyBandId" JSON..= unpackId bandId
        ]

instance View DocumentedCustomPathView where
    html DocumentedCustomPathView { .. } = [hsx||]

    type JsonResponse DocumentedCustomPathView = BandPayload

    jsonTyped DocumentedCustomPathView { .. } = BandPayload
        { bandId = unpackId bandId
        , page = Nothing
        , tags = []
        }

instance Controller DocumentedController where
    action ShowBandAction { .. } = render BandView { .. }
    action LegacyJsonAction = render LegacyJsonView
    action WrongJsonAction = render LegacyJsonView
    action WrongJsonShapeAction { .. } = render WrongJsonShapeView { .. }

instance Controller CustomRouteController where
    action ListCustomAction = renderPlain "ListCustomAction"
    action ShowCustomAction { .. } = renderPlain (cs (Prelude.show performanceId))

instance Controller DocumentedCustomPathController where
    action ShowDocumentedCustomPathAction { .. } = render DocumentedCustomPathView { .. }

instance AutoRoute DocumentedController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id Band))
    applyAction = applyConstr (parseIntegerId @(Id Band))

instance OpenApiController DocumentedController where
    openApiActions =
        [ actionDoc @BandView "ShowBandAction"
            |> setOpenApiSummary "Show a band payload"
        , actionDoc @BandView "WrongJsonAction"
        , actionDoc @WrongJsonShapeView "WrongJsonShapeAction"
        ]

instance AutoRoute CustomRouteController where
    customRoutes = do
        string "/custom/"
        performanceId <- parseId
        endOfInput
        onlyAllowMethods [GET, HEAD]
        pure ShowCustomAction { performanceId }

    customPathTo ShowCustomAction { performanceId } = Just ("/custom/" <> cs (Prelude.show performanceId))
    customPathTo _ = Nothing

instance AutoRoute DocumentedCustomPathController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id Band))
    applyAction = applyConstr (parseIntegerId @(Id Band))

    customPathTo ShowDocumentedCustomPathAction { bandId } = Just ("/bands/" <> cs (Prelude.show (unpackId bandId)))

instance OpenApiController DocumentedCustomPathController where
    openApiActions =
        [ actionDoc @DocumentedCustomPathView "ShowDocumentedCustomPathAction"
        ]

instance FrontController WebApplication where
    controllers =
        [ documentRoute @DocumentedController
        , documentRoute @DocumentedCustomPathController
        , parseRoute @CustomRouteController
        , swaggerUiWithOptions ((defaultSwaggerUiOptions @WebApplication) { swaggerUiPath = "/docs", swaggerUiTitle = Just "Band API Docs" })
        ]

instance FrontController RootApplication where
    controllers = [ mountFrontController WebApplication ]

defaultLayout :: Html -> Html
defaultLayout inner = [hsx|{inner}|]

instance InitControllerContext WebApplication where
    initContext = setLayout defaultLayout

instance InitControllerContext RootApplication

testJson :: ByteString -> Session SResponse
testJson url = request $ setPath defaultRequest
    { requestMethod = methodGet
    , requestHeaders = [(hAccept, "application/json")]
    } url

testGet :: ByteString -> Session SResponse
testGet url = request $ setPath defaultRequest { requestMethod = methodGet } url

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
        it "renders typed jsonTyped values through render" $ withContextAndApp \application -> do
            let expected = JSON.object
                    [ "bandId" JSON..= (12 :: Integer)
                    , "page" JSON..= Just (2 :: Int)
                    , "tags" JSON..= (["rock", "jazz"] :: [Text])
                    ]
            runSession (testJson "test/ShowBand?bandId=12&page=2&tags=rock,jazz") application >>= assertJsonBody expected

        it "keeps legacy json overrides working" $ withContextAndApp \application -> do
            let expected = JSON.object ["legacy" JSON..= True]
            runSession (testJson "test/LegacyJson") application >>= assertJsonBody expected

        it "fails fast when documented actions render the wrong json view" $ withContextAndApp \application -> do
            response <- runSession (testJson "test/WrongJson") application
            response.simpleStatus `shouldBe` status500
            Text.isInfixOf "OpenAPI docs expect view" (cs response.simpleBody) `shouldBe` True

        it "fails fast when documented actions override json with a shape different from jsonTyped" $ withContextAndApp \application -> do
            response <- runSession (testJson "test/WrongJsonShape?bandId=12") application
            response.simpleStatus `shouldBe` status500
            Text.isInfixOf "jsonTyped" (cs response.simpleBody) `shouldBe` True

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

        it "omits documented actions with customPathTo so generated paths cannot diverge" $ withContextAndApp \_ -> do
            let spec = buildOpenApi RootApplication
            lookupPathOperation "/test/ShowDocumentedCustomPath" "get" spec `shouldBe` Nothing
            lookupPathOperation "/bands/{bandId}" "get" spec `shouldBe` Nothing

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
