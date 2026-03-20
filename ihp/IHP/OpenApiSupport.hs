{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

module IHP.OpenApiSupport
    ( ToSchema (..)
    , NamedSchema (..)
    , Schema
    , toSchema
    , genericDeclareNamedSchema
    , defaultSchemaOptions
    , OpenApiGenerationException (..)
    , OpenApiInfo (..)
    , defaultOpenApiInfo
    , buildOpenApi
    , buildOpenApiWithInfo
    , SwaggerUiOptions (..)
    , defaultSwaggerUiOptions
    , swaggerUi
    , swaggerUiWithOptions
    ) where

import IHP.Prelude
import IHP.RouterSupport
import IHP.Router.Types (UnexpectedMethodException (..))
import IHP.ViewSupport (JsonResponse)
import IHP.ModelSupport
import Data.Attoparsec.ByteString.Char8 (string)
import Data.OpenApi (ToSchema (..), NamedSchema (..), Schema, Definitions, Referenced, declareNamedSchema, declareSchemaRef, toSchema, genericDeclareNamedSchema, defaultSchemaOptions)
import Data.OpenApi.Declare (runDeclare)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as JSON.Key
import qualified Data.Aeson.KeyMap as JSON.KeyMap
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Typeable as Typeable
import qualified Control.Exception as Exception
import qualified Control.Monad.State.Strict as State
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Text.Blaze.Html5 as Html5
import qualified Text.Blaze.Html5.Attributes as Attr
import Data.Data
import Data.Semigroup (Semigroup (..))
import Data.UUID (nil)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (StdMethod (..), parseMethod)
import Network.HTTP.Types.Status (status200)
import Network.Wai (Request, Response, defaultRequest, requestMethod, responseBuilder, responseLBS)

data OpenApiInfo = OpenApiInfo
    { openApiTitle :: Text
    , openApiVersion :: Text
    , openApiDescription :: Maybe Text
    }
    deriving (Eq, Show)

data OpenApiGenerationException = OpenApiGenerationException Text
    deriving (Eq, Show, Typeable.Typeable)

instance Exception.Exception OpenApiGenerationException

data OpenApiDocument = OpenApiDocument
    { pathOperations :: PathOperations
    , componentSchemas :: Definitions Schema
    }

instance Semigroup OpenApiDocument where
    left <> right = OpenApiDocument
        { pathOperations = Map.unionWith Map.union left.pathOperations right.pathOperations
        , componentSchemas = left.componentSchemas <> right.componentSchemas
        }

instance Monoid OpenApiDocument where
    mempty = OpenApiDocument
        { pathOperations = mempty
        , componentSchemas = mempty
        }

defaultOpenApiInfo :: forall application. Typeable.Typeable application => OpenApiInfo
defaultOpenApiInfo = OpenApiInfo
    { openApiTitle = cs (show (Typeable.typeRep (Proxy @application))) <> " API"
    , openApiVersion = "1.0.0"
    , openApiDescription = Nothing
    }

data SwaggerUiOptions = SwaggerUiOptions
    { swaggerUiPath :: ByteString
    , swaggerUiOpenApiPath :: ByteString
    , swaggerUiInfo :: OpenApiInfo
    , swaggerUiTitle :: Maybe Text
    , swaggerUiCssUrl :: Text
    , swaggerUiBundleJsUrl :: Text
    , swaggerUiStandalonePresetJsUrl :: Text
    }
    deriving (Eq, Show)

-- | Default Swagger UI settings for an application.
--
-- This serves the generated OpenAPI JSON at @/api-docs/openapi.json@ and the
-- Swagger UI at @/api-docs@.
defaultSwaggerUiOptions :: forall application. Typeable.Typeable application => SwaggerUiOptions
defaultSwaggerUiOptions =
    let info = defaultOpenApiInfo @application
    in SwaggerUiOptions
        { swaggerUiPath = "/api-docs"
        , swaggerUiOpenApiPath = "/openapi.json"
        , swaggerUiInfo = info
        , swaggerUiTitle = Nothing
        , swaggerUiCssUrl = "https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui.css"
        , swaggerUiBundleJsUrl = "https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui-bundle.js"
        , swaggerUiStandalonePresetJsUrl = "https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui-standalone-preset.js"
        }

buildOpenApi :: forall application. (FrontController application, Typeable.Typeable application) => application -> JSON.Value
buildOpenApi = buildOpenApiWithInfo (defaultOpenApiInfo @application)

buildOpenApiWithInfo :: forall application. FrontController application => OpenApiInfo -> application -> JSON.Value
buildOpenApiWithInfo info application =
    let ?request = defaultRequest
        ?respond = dummyRespond
        ?application = application
    in
        let routeTree = RouteCollection (controllers @application |> map routeInspection)
        in case collectPaths "" routeTree of
            Left errorMessage -> throwOpenApiGenerationException errorMessage
            Right OpenApiDocument { pathOperations, componentSchemas } ->
                JSON.object
                    ( [ Just ("openapi" JSON..= ("3.0.3" :: Text))
                      , Just ("info" JSON..= openApiInfoValue info)
                      , Just ("paths" JSON..= openApiPathsValue pathOperations)
                      , if componentSchemas == mempty then Nothing else Just ("components" JSON..= openApiComponentsValue componentSchemas)
                      ]
                        |> catMaybes
                    )
    where
        dummyRespond _ = error "buildOpenApi: response callback should never be called"

throwOpenApiGenerationException :: Text -> a
throwOpenApiGenerationException = Exception.throw . OpenApiGenerationException
{-# INLINE throwOpenApiGenerationException #-}

-- | Mounts Swagger UI for the current front controller using 'defaultSwaggerUiOptions'.
--
-- This adds two undocumented routes:
--
-- - the Swagger UI HTML at @/api-docs@
-- - the generated OpenAPI document at @/api-docs/openapi.json@
swaggerUi
    :: forall application.
        ( FrontController application
        , Typeable.Typeable application
        , ?application :: application
        )
    => ControllerRoute application
swaggerUi = swaggerUiWithOptions (defaultSwaggerUiOptions @application)

-- | Mounts Swagger UI and the generated OpenAPI JSON for the current front controller.
--
-- The JSON endpoint is derived from the same mounted router tree via
-- 'buildOpenApiWithInfo', so the documentation stays aligned with the
-- application routing.
swaggerUiWithOptions
    :: forall application.
        ( FrontController application
        , ?application :: application
        )
    => SwaggerUiOptions
    -> ControllerRoute application
swaggerUiWithOptions options@SwaggerUiOptions { swaggerUiPath, swaggerUiOpenApiPath, swaggerUiInfo } =
    let normalizedBasePath = normalizeSwaggerUiBasePath swaggerUiPath
        normalizedOpenApiPath = normalizeSwaggerUiChildPath swaggerUiOpenApiPath
        htmlResponse = responseBuilder status200 [(hContentType, "text/html; charset=utf-8")] (Blaze.renderHtmlBuilder (swaggerUiHtml options { swaggerUiPath = normalizedBasePath, swaggerUiOpenApiPath = normalizedOpenApiPath }))
        application = ?application
    in withPrefix normalizedBasePath
        [ getResponseRoute normalizedOpenApiPath (\_ -> responseLBS status200 [(hContentType, "application/json")] (JSON.encode (buildOpenApiWithInfo swaggerUiInfo application)))
        , getResponseRoute "" (\_ -> htmlResponse)
        , getResponseRoute "/" (\_ -> htmlResponse)
        ]

getResponseRoute :: ByteString -> (Request -> Response) -> ControllerRoute application
getResponseRoute path toResponse = rawRoute do
    string path
    pure (\request respond -> handleGet request respond (toResponse request))

handleGet :: Request -> (Response -> IO responseReceived) -> Response -> IO responseReceived
handleGet request respond response =
    case parseMethod (requestMethod request) of
        Right GET -> respond response
        Right HEAD -> respond response
        Right method -> Exception.throw UnexpectedMethodException { allowedMethods = [GET, HEAD], method }
        Left err -> Exception.throwIO (OpenApiGenerationException ("Invalid HTTP method: " <> cs err))

normalizeSwaggerUiBasePath :: ByteString -> ByteString
normalizeSwaggerUiBasePath path
    | ByteString.null path = "/api-docs"
    | ByteString.head path /= '/' = normalizeSwaggerUiBasePath ("/" <> path)
    | ByteString.length path > 1 && ByteString.last path == '/' = ByteString.init path
    | otherwise = path

normalizeSwaggerUiChildPath :: ByteString -> ByteString
normalizeSwaggerUiChildPath path
    | ByteString.null path = "/openapi.json"
    | ByteString.head path /= '/' = "/" <> path
    | otherwise = path

swaggerUiHtml :: SwaggerUiOptions -> Html5.Html
swaggerUiHtml SwaggerUiOptions { swaggerUiOpenApiPath, swaggerUiInfo, swaggerUiTitle, swaggerUiCssUrl, swaggerUiBundleJsUrl, swaggerUiStandalonePresetJsUrl } =
    let title = fromMaybe (swaggerUiInfo.openApiTitle <> " Swagger UI") swaggerUiTitle
        openApiUrlLiteral = jsonStringLiteral (relativeSwaggerUiOpenApiUrl swaggerUiOpenApiPath)
    in Html5.docTypeHtml do
        Html5.head do
            Html5.meta Html5.! Attr.charset "utf-8"
            Html5.meta Html5.! Attr.name "viewport" Html5.! Attr.content "width=device-width, initial-scale=1"
            Html5.title (Html5.toHtml title)
            Html5.link Html5.! Attr.rel "stylesheet" Html5.! Attr.href (Html5.textValue swaggerUiCssUrl)
            Html5.style "html { box-sizing: border-box; overflow-y: scroll; } *, *:before, *:after { box-sizing: inherit; } body { margin: 0; background: #fafafa; }"
        Html5.body do
            Html5.div mempty Html5.! Attr.id "swagger-ui"
            Html5.script mempty Html5.! Attr.src (Html5.textValue swaggerUiBundleJsUrl)
            Html5.script mempty Html5.! Attr.src (Html5.textValue swaggerUiStandalonePresetJsUrl)
            Html5.script (Html5.preEscapedToHtml (swaggerUiBootScript openApiUrlLiteral))

relativeSwaggerUiOpenApiUrl :: ByteString -> Text
relativeSwaggerUiOpenApiUrl openApiPath =
    "./" <> Text.dropWhile (== '/') (Text.decodeUtf8 openApiPath)

swaggerUiBootScript :: Text -> Text
swaggerUiBootScript openApiUrlLiteral =
    Text.unlines
        [ "window.onload = function () {"
        , "  window.ui = SwaggerUIBundle({"
        , "    url: " <> openApiUrlLiteral <> ","
        , "    dom_id: '#swagger-ui',"
        , "    deepLinking: true,"
        , "    presets: [SwaggerUIBundle.presets.apis, SwaggerUIStandalonePreset],"
        , "    layout: 'BaseLayout'"
        , "  });"
        , "};"
        ]

jsonStringLiteral :: Text -> Text
jsonStringLiteral = cs . LazyByteString.toStrict . JSON.encode

openApiInfoValue :: OpenApiInfo -> JSON.Value
openApiInfoValue OpenApiInfo { openApiTitle, openApiVersion, openApiDescription } = JSON.object
    ( [ Just ("title" JSON..= openApiTitle)
      , Just ("version" JSON..= openApiVersion)
      , ("description" JSON..=) <$> openApiDescription
      ]
        |> catMaybes
    )

type PathOperations = Map.Map Text (Map.Map Text JSON.Value)

collectPaths :: Text -> RouteInspection -> Either Text OpenApiDocument
collectPaths currentPrefix = \case
    RouteLeaf UndocumentedRoute -> Right mempty
    RouteLeaf (DocumentedRoute documentedRoute) -> collectDocumentedRoute currentPrefix documentedRoute
    RouteCollection routes -> mconcat <$> mapM (collectPaths currentPrefix) routes
    RoutePrefix routePrefix routes ->
        let prefixedPath = appendPathPrefix currentPrefix (Text.decodeUtf8 routePrefix)
        in mconcat <$> mapM (collectPaths prefixedPath) routes

collectDocumentedRoute :: Text -> DocumentedRouteInfo -> Either Text OpenApiDocument
collectDocumentedRoute _ (AutoRouteControllerInfo { documentedActions = Nothing }) = Right mempty
collectDocumentedRoute currentPrefix (AutoRouteControllerInfo { documentedActions = Just docs }) =
    foldl' (insertActionOperation currentPrefix) (Right mempty) docs

insertActionOperation
    :: forall controller.
        ( AutoRoute controller
        , Data controller
        , Typeable.Typeable controller
        )
    => Text
    -> Either Text OpenApiDocument
    -> ActionDoc controller
    -> Either Text OpenApiDocument
insertActionOperation currentPrefix pathState doc@ActionDoc { actionDocName } = do
    OpenApiDocument { pathOperations, componentSchemas } <- pathState
    constructor <- findControllerConstructor @controller actionDocName
    hasCustomPath <- actionUsesCustomPath @controller constructor
    if hasCustomPath
        then pure OpenApiDocument { pathOperations, componentSchemas }
        else do
            let actionPath = appendPathPrefix currentPrefix (actionPrefixText @controller <> stripActionSuffixText actionDocName)
            parameters <- deriveActionParameters @controller constructor
            let (operation, operationSchemas) = actionDocOperationValue doc parameters
            let methods = allowedMethodsForAction @controller (Text.encodeUtf8 actionDocName)
            pure OpenApiDocument
                { pathOperations = foldl' (insertMethod actionPath operation) pathOperations methods
                , componentSchemas = componentSchemas <> operationSchemas
                }

findControllerConstructor :: forall controller. Data controller => Text -> Either Text Constr
findControllerConstructor actionName =
    dataTypeConstrs (dataTypeOf (undefined :: controller))
        |> find (\constructor -> cs (showConstr constructor) == actionName)
        |> \case
            Just constructor -> Right constructor
            Nothing -> Left ("OpenAPI docs reference unknown action " <> actionName)

insertMethod :: Text -> JSON.Value -> PathOperations -> StdMethod -> PathOperations
insertMethod actionPath operation paths method = Map.alter updatePath actionPath paths
    where
        methodName = httpMethodName method
        updatePath Nothing = Just (Map.singleton methodName operation)
        updatePath (Just operations) = Just (Map.insert methodName operation operations)

httpMethodName :: StdMethod -> Text
httpMethodName = \case
    GET -> "get"
    POST -> "post"
    PUT -> "put"
    DELETE -> "delete"
    OPTIONS -> "options"
    HEAD -> "head"
    PATCH -> "patch"
    TRACE -> "trace"
    CONNECT -> error "OpenAPI does not support CONNECT routes"

appendPathPrefix :: Text -> Text -> Text
appendPathPrefix currentPrefix nextPrefix =
    let normalize text
            | Text.null text = ""
            | "/" `Text.isPrefixOf` text = text
            | otherwise = "/" <> text
        trimTrailingSlash text
            | Text.length text > 1 && "/" `Text.isSuffixOf` text = Text.dropEnd 1 text
            | otherwise = text
        normalizedCurrent = currentPrefix |> normalize |> trimTrailingSlash
        normalizedNext = normalize nextPrefix
    in case (normalizedCurrent, normalizedNext) of
        ("", next) -> next
        (current, "/") -> current <> "/"
        (current, next) -> current <> next

openApiPathsValue :: PathOperations -> JSON.Value
openApiPathsValue paths =
    paths
        |> Map.toList
        |> map (\(path, operations) ->
                ( JSON.Key.fromText path
                , operations
                    |> Map.toList
                    |> map (\(method, operation) -> (JSON.Key.fromText method, operation))
                    |> JSON.KeyMap.fromList
                    |> JSON.Object
                )
            )
        |> JSON.KeyMap.fromList
        |> JSON.Object

openApiComponentsValue :: Definitions Schema -> JSON.Value
openApiComponentsValue schemas = JSON.object
    [ "schemas" JSON..= schemas
    ]

actionDocOperationValue :: forall controller. ActionDoc controller -> [QueryParameterDocumentation] -> (JSON.Value, Definitions Schema)
actionDocOperationValue ActionDoc { actionDocName, actionDocSummary, actionDocDescription, actionDocTags, actionDocOperationId, actionDocView } parameters =
    let SchemaDocumentation { documentedSchema, documentedDefinitions } = responseSchemaValue actionDocView
        parameterDefinitions = parameters |> map (\QueryParameterDocumentation { parameterDefinitions } -> parameterDefinitions) |> mconcat
    in
        ( JSON.object
            ( [ Just ("parameters" JSON..= map queryParameterValue parameters)
              , Just ("responses" JSON..= JSON.object ["200" JSON..= successResponseValue documentedSchema])
              , ("summary" JSON..=) <$> actionDocSummary
              , ("description" JSON..=) <$> actionDocDescription
              , if null actionDocTags then Nothing else Just ("tags" JSON..= actionDocTags)
              , ("operationId" JSON..=) <$> actionDocOperationId
              , Just ("x-ihp-action" JSON..= actionDocName)
              ]
                |> catMaybes
            )
        , documentedDefinitions <> parameterDefinitions
        )

responseSchemaValue :: forall view. ToSchema (JsonResponse view) => Proxy view -> SchemaDocumentation
responseSchemaValue _ = declareSchemaDocumentation (Proxy @(JsonResponse view))

successResponseValue :: Referenced Schema -> JSON.Value
successResponseValue schema = JSON.object
    [ "description" JSON..= ("Successful response" :: Text)
    , "content" JSON..= JSON.object
        [ "application/json" JSON..= JSON.object
            [ "schema" JSON..= schema
            ]
        ]
    ]

data QueryParameterDocumentation = QueryParameterDocumentation
    { parameterName :: Text
    , parameterRequired :: Bool
    , parameterSchema :: Referenced Schema
    , parameterDefinitions :: Definitions Schema
    , parameterExplode :: Maybe Bool
    }

queryParameterValue :: QueryParameterDocumentation -> JSON.Value
queryParameterValue QueryParameterDocumentation { parameterName, parameterRequired, parameterSchema, parameterExplode } = JSON.object
    ( [ Just ("name" JSON..= parameterName)
      , Just ("in" JSON..= ("query" :: Text))
      , Just ("required" JSON..= parameterRequired)
      , Just ("schema" JSON..= parameterSchema)
      , if isJust parameterExplode then Just ("style" JSON..= ("form" :: Text)) else Nothing
      , ("explode" JSON..=) <$> parameterExplode
      ]
        |> catMaybes
    )

data SchemaDocumentation = SchemaDocumentation
    { documentedSchema :: Referenced Schema
    , documentedDefinitions :: Definitions Schema
    }

declareSchemaDocumentation :: forall schema. ToSchema schema => Proxy schema -> SchemaDocumentation
declareSchemaDocumentation proxy =
    let (definitions, schema) = runDeclare (declareSchemaRef proxy) mempty
    in SchemaDocumentation
        { documentedSchema = schema
        , documentedDefinitions = definitions
        }

queryParameterDocumentation :: forall field. Data field => Text -> Maybe QueryParameterDocumentation
queryParameterDocumentation parameterName =
    directParameterDocumentation @field parameterName
        <|> wrappedIdParameterDocumentation @field parameterName

directParameterDocumentation :: forall field. Data field => Text -> Maybe QueryParameterDocumentation
directParameterDocumentation parameterName =
    asum
        [ eqT @field @Text |> fmap (\Refl -> requiredParameter @Text parameterName)
        , eqT @field @Int |> fmap (\Refl -> requiredParameter @Int parameterName)
        , eqT @field @Integer |> fmap (\Refl -> requiredParameter @Integer parameterName)
        , eqT @field @UUID |> fmap (\Refl -> requiredParameter @UUID parameterName)
        , eqT @field @(Maybe Text) |> fmap (\Refl -> optionalParameter @Text parameterName)
        , eqT @field @(Maybe Int) |> fmap (\Refl -> optionalParameter @Int parameterName)
        , eqT @field @(Maybe Integer) |> fmap (\Refl -> optionalParameter @Integer parameterName)
        , eqT @field @[Text] |> fmap (\Refl -> listParameter @Text parameterName)
        , eqT @field @[Int] |> fmap (\Refl -> listParameter @Int parameterName)
        , eqT @field @[Integer] |> fmap (\Refl -> listParameter @Integer parameterName)
        , eqT @field @[UUID] |> fmap (\Refl -> listParameter @UUID parameterName)
        ]

requiredParameter :: forall a. ToSchema a => Text -> QueryParameterDocumentation
requiredParameter parameterName =
    let SchemaDocumentation { documentedSchema, documentedDefinitions } = declareSchemaDocumentation (Proxy @a)
    in QueryParameterDocumentation
        { parameterName
        , parameterRequired = True
        , parameterSchema = documentedSchema
        , parameterDefinitions = documentedDefinitions
        , parameterExplode = Nothing
        }

optionalParameter :: forall a. ToSchema a => Text -> QueryParameterDocumentation
optionalParameter parameterName =
    let SchemaDocumentation { documentedSchema, documentedDefinitions } = declareSchemaDocumentation (Proxy @a)
    in QueryParameterDocumentation
        { parameterName
        , parameterRequired = False
        , parameterSchema = documentedSchema
        , parameterDefinitions = documentedDefinitions
        , parameterExplode = Nothing
        }

listParameter :: forall a. ToSchema [a] => Text -> QueryParameterDocumentation
listParameter parameterName =
    let SchemaDocumentation { documentedSchema, documentedDefinitions } = declareSchemaDocumentation (Proxy @[a])
    in QueryParameterDocumentation
        { parameterName
        , parameterRequired = False
        , parameterSchema = documentedSchema
        , parameterDefinitions = documentedDefinitions
        , parameterExplode = Just False
        }

wrappedIdParameterDocumentation :: forall field. Data field => Text -> Maybe QueryParameterDocumentation
wrappedIdParameterDocumentation parameterName
    | dataTypeName (dataTypeOf (undefined :: field)) /= "IHP.ModelSupport.Types.Id'" = Nothing
    | otherwise =
        dataTypeConstrs (dataTypeOf (undefined :: field))
            |> listToMaybe
            >>= \constructor -> either (const Nothing) Just (deriveWrappedIdParameter @field constructor parameterName)

deriveWrappedIdParameter :: forall field. Data field => Constr -> Text -> Either Text QueryParameterDocumentation
deriveWrappedIdParameter constructor parameterName =
    let nextField :: forall inner. Data inner => State.StateT (Maybe QueryParameterDocumentation) (Either Text) inner
        nextField = do
            parameter <- case queryParameterDocumentation @inner parameterName of
                Just queryParameter -> pure queryParameter
                Nothing -> State.lift (Left unsupportedInnerTypeMessage)
            State.put (Just parameter)
            case dummyValueForFieldType @inner of
                Right dummyValue -> pure dummyValue
                Left errorMessage -> State.lift (Left errorMessage)
        unsupportedInnerTypeMessage =
            "OpenAPI does not support the inner primary key type of "
            <> parameterName
    in case State.runStateT (fromConstrM nextField constructor :: State.StateT (Maybe QueryParameterDocumentation) (Either Text) field) Nothing of
        Right (_, Just parameter) -> Right parameter
        Right (_, Nothing) -> Left ("OpenAPI could not derive the inner primary key type of " <> parameterName)
        Left errorMessage -> Left errorMessage

dummyValueForFieldType :: forall field. Data field => Either Text field
dummyValueForFieldType =
    fromMaybe
        (Left unsupportedDummyTypeMessage)
        (directDummyValue @field <|> wrappedIdDummyValue @field)
    where
        unsupportedDummyTypeMessage =
            "OpenAPI dummy value is not implemented for type "
            <> cs (dataTypeName (dataTypeOf (undefined :: field)))

directDummyValue :: forall field. Data field => Maybe (Either Text field)
directDummyValue = asum
    [ eqT @field @Text |> fmap (\Refl -> Right "")
    , eqT @field @Int |> fmap (\Refl -> Right 0)
    , eqT @field @Integer |> fmap (\Refl -> Right 0)
    , eqT @field @UUID |> fmap (\Refl -> Right nil)
    , eqT @field @(Maybe Text) |> fmap (\Refl -> Right Nothing)
    , eqT @field @(Maybe Int) |> fmap (\Refl -> Right Nothing)
    , eqT @field @(Maybe Integer) |> fmap (\Refl -> Right Nothing)
    , eqT @field @[Text] |> fmap (\Refl -> Right [])
    , eqT @field @[Int] |> fmap (\Refl -> Right [])
    , eqT @field @[Integer] |> fmap (\Refl -> Right [])
    , eqT @field @[UUID] |> fmap (\Refl -> Right [])
    ]

wrappedIdDummyValue :: forall field. Data field => Maybe (Either Text field)
wrappedIdDummyValue
    | dataTypeName (dataTypeOf (undefined :: field)) /= "IHP.ModelSupport.Types.Id'" = Nothing
    | otherwise =
        dataTypeConstrs (dataTypeOf (undefined :: field))
            |> listToMaybe
            |> fmap deriveWrappedIdDummyValue

deriveWrappedIdDummyValue :: forall field. Data field => Constr -> Either Text field
deriveWrappedIdDummyValue constructor =
    let nextField :: forall inner. Data inner => State.StateT () (Either Text) inner
        nextField =
            case dummyValueForFieldType @inner of
                Right dummyValue -> pure dummyValue
                Left errorMessage -> State.lift (Left errorMessage)
    in fst <$> State.runStateT (fromConstrM nextField constructor :: State.StateT () (Either Text) field) ()

buildDummyAction :: forall controller. Data controller => Constr -> Either Text controller
buildDummyAction constructor =
    let nextField :: forall field. Data field => State.StateT () (Either Text) field
        nextField = State.lift (dummyValueForFieldType @field)
    in fst <$> State.runStateT (fromConstrM nextField constructor :: State.StateT () (Either Text) controller) ()

actionUsesCustomPath :: forall controller. (AutoRoute controller, Data controller) => Constr -> Either Text Bool
actionUsesCustomPath constructor = isJust . customPathTo <$> buildDummyAction @controller constructor

deriveActionParameters :: forall controller. Data controller => Constr -> Either Text [QueryParameterDocumentation]
deriveActionParameters constr =
    let initialState = (map cs (constrFields constr), [])
        nextField :: forall field. Data field => State.StateT ([Text], [QueryParameterDocumentation]) (Either Text) field
        nextField = do
            (remainingFields, parameters) <- State.get
            case remainingFields of
                [] -> State.lift (Left ("OpenAPI field derivation failed for action " <> cs (showConstr constr)))
                (fieldName:restFields) ->
                    case queryParameterDocumentation @field fieldName of
                        Just parameter -> do
                            State.put (restFields, parameters <> [parameter])
                            case dummyValueForFieldType @field of
                                Right dummyValue -> pure dummyValue
                                Left errorMessage -> State.lift (Left errorMessage)
                        Nothing -> State.lift (Left unsupportedTypeMessage)
                    where
                        unsupportedTypeMessage =
                            "OpenAPI does not support the AutoRoute field "
                            <> fieldName
                            <> " with type "
                            <> cs (dataTypeName (dataTypeOf (undefined :: field)))
    in case State.runStateT
            (fromConstrM nextField constr :: State.StateT ([Text], [QueryParameterDocumentation]) (Either Text) controller)
            initialState of
        Left errorMessage -> Left errorMessage
        Right (_, ([], parameters)) -> Right parameters
        Right (_, (remainingFields, _)) ->
            Left ("OpenAPI field derivation did not consume all fields for action " <> cs (showConstr constr) <> ": " <> cs (show remainingFields) :: Text)

instance {-# OVERLAPPABLE #-} (KnownSymbol table, ToSchema (PrimaryKey table)) => ToSchema (Id' table) where
    declareNamedSchema _ = declareNamedSchema (Proxy @(PrimaryKey table))
