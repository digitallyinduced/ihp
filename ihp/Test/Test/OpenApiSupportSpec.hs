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
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import IHP.ControllerPrelude hiding (request)
import IHP.Environment
import IHP.Test.Mocking
import IHP.ViewPrelude hiding (action)
import Network.HTTP.Types
import Network.Wai.Test
import Test.Hspec
import Prelude qualified
import "ihp" IHP.Router.Capture (parseCapture, renderCapture)
import "ihp" IHP.Router.DSL (routes)

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

data RawJsonValueView = RawJsonValueView
data WrongJsonShapeView = WrongJsonShapeView {wrongBandId :: !Int}
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
    html BandView{..} = [hsx||]

instance JsonView BandView where
    type JsonResponse BandView = BandPayload

    json BandView{..} =
        BandPayload
            { bandId
            , page
            , tags = bandTags
            }

instance View RawJsonValueView where
    html RawJsonValueView = [hsx||]

instance JsonView RawJsonValueView where
    json RawJsonValueView =
        JSON.object
            [ "legacy" JSON..= True
            ]

instance View WrongJsonShapeView where
    html WrongJsonShapeView{..} = [hsx||]

instance JsonView WrongJsonShapeView where
    type JsonResponse WrongJsonShapeView = BandPayload

    json WrongJsonShapeView{..} =
        BandPayload
            { bandId = wrongBandId
            , page = Nothing
            , tags = []
            }

instance View AckView where
    html AckView = [hsx||]

instance JsonView AckView where
    type JsonResponse AckView = AckPayload

    json AckView = AckPayload{ok = True}

data ApiAction body response where
    ShowBandAction ::
        { bandId :: !Int
        , page :: !(Maybe Int)
        , bandTags :: ![Text]
        } ->
        ApiAction 'NoBody BandView
    RawJsonValueAction :: ApiAction 'NoBody RawJsonValueView
    WrongJsonShapeAction :: {wrongBandId :: !Int} -> ApiAction 'NoBody WrongJsonShapeView
    CreateApiSessionAction :: ApiAction ('Body CreateSessionRequest) AckView
    CreatePipeSessionAction :: ApiAction ('Body CreateSessionRequest) AckView
    ShowApiSessionAction :: ApiAction 'NoBody AckView

deriving instance Show (ApiAction body response)
deriving instance Eq (ApiAction body response)

instance Controller (ApiAction 'NoBody BandView) where
    type ControllerAction (ApiAction 'NoBody BandView) = ActionDef (ApiAction 'NoBody BandView) 'NoBody BandView

    action ShowBandAction{..} =
        typedAction do
            pure BandView{..}

instance Controller (ApiAction 'NoBody RawJsonValueView) where
    type ControllerAction (ApiAction 'NoBody RawJsonValueView) = ActionDef (ApiAction 'NoBody RawJsonValueView) 'NoBody RawJsonValueView

    action RawJsonValueAction =
        typedAction do
            pure RawJsonValueView

instance Controller (ApiAction 'NoBody WrongJsonShapeView) where
    type ControllerAction (ApiAction 'NoBody WrongJsonShapeView) = ActionDef (ApiAction 'NoBody WrongJsonShapeView) 'NoBody WrongJsonShapeView

    action WrongJsonShapeAction{..} =
        typedAction do
            pure WrongJsonShapeView{..}

instance Controller (ApiAction ('Body CreateSessionRequest) AckView) where
    type ControllerAction (ApiAction ('Body CreateSessionRequest) AckView) = ActionDef (ApiAction ('Body CreateSessionRequest) AckView) ('Body CreateSessionRequest) AckView

    action CreateApiSessionAction =
        typedAction do
            let _token = bodyParam #token
            pure AckView

    action CreatePipeSessionAction =
        typedAction do
            let _token = bodyParam #token
            pure AckView

instance Controller (ApiAction 'NoBody AckView) where
    type ControllerAction (ApiAction 'NoBody AckView) = ActionDef (ApiAction 'NoBody AckView) 'NoBody AckView

    action ShowApiSessionAction =
        typedAction do
            pure AckView

$(pure [])

[routes|openApiTestRoutes
GET|HEAD /test/bands/{bandId}?page&tags ShowBandAction { bandTags = #tags }
  summary: Show a band payload
  tags: Bands
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
  success: 201 Created response
GET|HEAD /test/ShowApiSession       ShowApiSessionAction
  summary: Show API session
GET /docs                          SwaggerUiAction
GET /docs/openapi.json             OpenApiJsonAction
|]

instance SwaggerUiControllerConfig WebApplication where
    swaggerUiControllerOptions =
        (defaultSwaggerUiOptions @WebApplication)
            { swaggerUiTitle = Just "Band API Docs"
            }

instance FrontController WebApplication where
    controllers = openApiTestRoutes

instance FrontController RootApplication where
    controllers = [mountFrontController WebApplication]

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
        it "renders typed json values through renderHtmlOrJson" $ withContextAndApp \application -> do
            let expected =
                    JSON.object
                        [ "bandId" JSON..= (12 :: Int)
                        , "page" JSON..= Just (2 :: Int)
                        , "tags" JSON..= (["rock", "jazz"] :: [Text])
                        ]
            runSession (testJson "test/bands/12?page=2&tags=rock&tags=jazz") application >>= assertJsonBody expected

        it "keeps JSON.Value responses working through json" $ withContextAndApp \application -> do
            let expected = JSON.object ["legacy" JSON..= True]
            runSession (testJson "test/raw-json") application >>= assertJsonBody expected

        it "uses typed JsonView.json as the rendered JSON response" $ withContextAndApp \application -> do
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

        it "uses route success status for typed actions" $ withContextAndApp \application -> do
            response <- runSession (testPostJson "test/CreatePipeSession" (JSON.object ["token" JSON..= ("abc" :: Text)])) application
            response.simpleStatus `shouldBe` status201
            JSON.decode response.simpleBody `shouldBe` Just (JSON.object ["ok" JSON..= True])

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
            lookupValue "tags" operation `shouldBe` Just (JSON.Array (Vector.fromList [JSON.String "Bands"]))

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

            let Just componentsSchemas = lookupValue "components" spec >>= lookupValue "schemas"
            let Just bandPayloadSchema = lookupValue "BandPayload" componentsSchemas
            lookupValue "type" bandPayloadSchema `shouldBe` Just (JSON.String "object")
            (lookupValue "properties" bandPayloadSchema >>= lookupValue "bandId") `shouldSatisfy` isJust
            lookupValue "required" bandPayloadSchema
                `shouldBe` Just (JSON.Array (Vector.fromList [JSON.String "bandId", JSON.String "tags"]))

        it "omits private typed routes from the generated spec" $ withContextAndApp \_ -> do
            let spec = buildOpenApi WebApplication
            lookupPathOperation "/test/raw-json" "get" spec `shouldBe` Nothing

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

        it "includes request body schemas and success metadata for typed actions" $ withContextAndApp \_ -> do
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
            Text.isInfixOf "/docs/openapi.json" body `shouldBe` True
            Text.isInfixOf "SwaggerUIBundle" body `shouldBe` True

        it "uses an OpenAPI URL that works with or without a trailing slash" $ withContextAndApp \application -> do
            response <- runSession (testGet "docs/") application
            response.simpleStatus `shouldBe` status200
            let body = cs response.simpleBody
            Text.isInfixOf "/docs/openapi.json" body `shouldBe` True
