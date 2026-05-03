{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

module IHP.OpenApiSupport (
    ToSchema (..),
    NamedSchema (..),
    Schema,
    toSchema,
    genericDeclareNamedSchema,
    deriveApiRecord,
    deriveToSchema,
    defaultSchemaOptions,
    OpenApiGenerationException (..),
    OpenApiInfo (..),
    defaultOpenApiInfo,
    buildOpenApi,
    buildOpenApiWithInfo,
    SwaggerUiOptions (..),
    defaultSwaggerUiOptions,
    SwaggerUiController (..),
    SwaggerUiControllerConfig (..),
    swaggerUi,
    swaggerUiWithOptions,
) where

import Control.Exception qualified as Exception
import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as JSON.Key
import Data.Aeson.KeyMap qualified as JSON.KeyMap
import Data.Aeson.TH qualified as AesonTH
import Data.Attoparsec.ByteString.Char8 (string)
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Dynamic (fromDynamic)
import Data.Map.Strict qualified as Map
import Data.OpenApi (Definitions, NamedSchema (..), OpenApiType (..), Referenced, Schema (..), ToSchema (..), declareNamedSchema, declareSchemaRef, defaultSchemaOptions, genericDeclareNamedSchema, toSchema)
import Data.OpenApi.Declare (Declare, runDeclare)
import Data.Semigroup (Semigroup (..))
import Data.String qualified as String
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Typeable qualified as Typeable
import GHC.Exts qualified as Exts
import IHP.Controller.Context qualified as Context
import IHP.Controller.Response (respondWith)
import IHP.Controller.TypedAction (FromFormBody (..))
import IHP.Controller.TypedAction qualified as TypedAction
import IHP.ControllerSupport (Controller (..))
import IHP.ModelSupport
import IHP.Prelude
import IHP.Router.Types (UnexpectedMethodException (..))
import IHP.RouterSupport
import IHP.ViewSupport qualified as ViewSupport
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (StdMethod (..), parseMethod)
import Network.HTTP.Types.Status (status200, statusCode)
import Network.Wai (Request, Response, defaultRequest, requestMethod, responseBuilder, responseLBS)
import Language.Haskell.TH qualified as TH
import Prelude qualified
import Text.Blaze.Html.Renderer.Utf8 qualified as Blaze
import Text.Blaze.Html5 qualified as Html5
import Text.Blaze.Html5.Attributes qualified as Attr

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
    left <> right =
        OpenApiDocument
            { pathOperations = Map.unionWith Map.union left.pathOperations right.pathOperations
            , componentSchemas = left.componentSchemas <> right.componentSchemas
            }

instance Monoid OpenApiDocument where
    mempty =
        OpenApiDocument
            { pathOperations = mempty
            , componentSchemas = mempty
            }

{- | Generates 'JSON.FromJSON', 'JSON.ToJSON', 'FromFormBody' and 'ToSchema'
instances for a plain API record without using 'Generic'.

@
data PostInput = PostInput { title :: Text, body :: Text }
$(deriveApiRecord ''PostInput)
@

Use this for request/response DTOs that should be accepted as JSON, returned as
JSON, accepted as @application/x-www-form-urlencoded@, and included in OpenAPI
schemas.
-}
deriveApiRecord :: TH.Name -> TH.DecsQ
deriveApiRecord typeName = do
    jsonDecs <- AesonTH.deriveJSON JSON.defaultOptions typeName
    formDecs <- deriveFromFormBody typeName
    schemaDecs <- deriveToSchema typeName
    pure (jsonDecs <> formDecs <> schemaDecs)

deriveFromFormBody :: TH.Name -> TH.DecsQ
deriveFromFormBody typeName = do
    RecordInfo{recordConstructorName, recordFields} <- reifyRecordInfo typeName
    requestName <- TH.newName "request"

    fieldVariableNames <-
        recordFields
            |> mapM \(fieldName, _) -> TH.newName (TH.nameBase fieldName)

    fieldStatements <-
        recordFields
            |> zip fieldVariableNames
            |> mapM \(fieldVariableName, (fieldName, _)) ->
                TH.bindS
                    (TH.varP fieldVariableName)
                    [| TypedAction.formBodyParam (String.fromString $(TH.stringE (TH.nameBase fieldName))) $(TH.varE requestName) |]

    let recordExpression =
            TH.RecConE recordConstructorName
                ( recordFields
                    |> zip fieldVariableNames
                    |> map \(fieldVariableName, (fieldName, _)) ->
                        (fieldName, TH.VarE fieldVariableName)
                )

    finalStatement <- TH.noBindS [| pure $(pure recordExpression) |]

    pure
        [ TH.InstanceD
            Nothing
            []
            (TH.AppT (TH.ConT ''FromFormBody) (TH.ConT typeName))
            [ TH.FunD
                'parseFormBody
                [ TH.Clause [TH.VarP requestName] (TH.NormalB (TH.DoE Nothing (fieldStatements <> [finalStatement]))) []
                ]
            ]
        ]

{- | Generates a 'ToSchema' instance for a plain record without using 'Generic'.

This is intentionally small and aimed at request/response DTOs:

@
data PostInput = PostInput { title :: Text, body :: Text }
$(deriveToSchema ''PostInput)
@

The generated schema marks every field as required except fields whose type is
'Maybe'.
-}
deriveToSchema :: TH.Name -> TH.DecsQ
deriveToSchema typeName = do
    RecordInfo{recordTypeName, recordFields} <- reifyRecordInfo typeName

    schemaVariableNames <-
        recordFields
            |> zip [(1 :: Int) ..]
            |> mapM \(index, _) -> TH.newName ("fieldSchema" <> Prelude.show index)

    schemaStatements <-
        recordFields
            |> zip schemaVariableNames
            |> mapM \(schemaVariableName, (_, fieldType)) -> do
                proxyExpression <- TH.sigE (TH.conE 'Proxy) (TH.appT (TH.conT ''Proxy) (pure fieldType))
                TH.bindS (TH.varP schemaVariableName) [| recordSchemaField $(pure proxyExpression) |]

    let propertyExpressions =
            recordFields
                |> zip schemaVariableNames
                |> map \(schemaVariableName, (fieldName, _)) ->
                    [| ($(TH.stringE (TH.nameBase fieldName)), $(TH.varE schemaVariableName)) |]

    let requiredExpressions =
            recordFields
                |> filter (not . isMaybeType . snd)
                |> map (TH.stringE . TH.nameBase . fst)

    finalStatement <-
        TH.noBindS
            [| pure (recordNamedSchema $(TH.stringE recordTypeName) $(TH.listE propertyExpressions) $(TH.listE requiredExpressions)) |]

    let body = TH.DoE Nothing (schemaStatements <> [finalStatement])

    pure
        [ TH.InstanceD
            Nothing
            []
            (TH.AppT (TH.ConT ''ToSchema) (TH.ConT typeName))
            [ TH.FunD
                'declareNamedSchema
                [ TH.Clause [TH.WildP] (TH.NormalB body) []
                ]
            ]
        ]

data RecordInfo = RecordInfo
    { recordTypeName :: String
    , recordConstructorName :: TH.Name
    , recordFields :: [(TH.Name, TH.Type)]
    }

reifyRecordInfo :: TH.Name -> TH.Q RecordInfo
reifyRecordInfo typeName = do
    info <- TH.reify typeName
    reifyRecordInfoFromType typeName info

reifyRecordInfoFromType :: TH.Name -> TH.Info -> TH.Q RecordInfo
reifyRecordInfoFromType requestedName (TH.TyConI declaration) =
    case declaration of
        TH.DataD _ typeName typeVariables _ constructors _ ->
            recordInfoFor requestedName typeName typeVariables constructors
        TH.NewtypeD _ typeName typeVariables _ constructor _ ->
            recordInfoFor requestedName typeName typeVariables [constructor]
        _ ->
            fail ("deriveToSchema: expected a data or newtype declaration for " <> TH.nameBase requestedName)
reifyRecordInfoFromType requestedName _ =
    fail ("deriveToSchema: expected a type constructor, but got " <> TH.nameBase requestedName)

recordInfoFor :: TH.Name -> TH.Name -> [TH.TyVarBndr TH.BndrVis] -> [TH.Con] -> TH.Q RecordInfo
recordInfoFor requestedName typeName typeVariables constructors = do
    unless (null typeVariables) do
        fail ("deriveToSchema: type parameters are not supported yet for " <> TH.nameBase requestedName)

    case constructors of
        [constructor] -> do
            (constructorName, fields) <- constructorRecordFields requestedName constructor
            pure
                RecordInfo
                    { recordTypeName = TH.nameBase typeName
                    , recordConstructorName = constructorName
                    , recordFields = fields
                    }
        _ ->
            fail ("deriveToSchema: only single-constructor records are supported for " <> TH.nameBase requestedName)

constructorRecordFields :: TH.Name -> TH.Con -> TH.Q (TH.Name, [(TH.Name, TH.Type)])
constructorRecordFields requestedName = \case
    TH.RecC constructorName fields -> do
        recordFields <- mapM recordField fields
        pure (constructorName, recordFields)
    TH.RecGadtC [constructorName] fields _ -> do
        recordFields <- mapM recordField fields
        pure (constructorName, recordFields)
    TH.RecGadtC _ _ _ ->
        fail ("deriveToSchema: only single-constructor records are supported for " <> TH.nameBase requestedName)
    TH.ForallC _ _ constructor ->
        constructorRecordFields requestedName constructor
    _ ->
        fail ("deriveToSchema: only record constructors are supported for " <> TH.nameBase requestedName)

recordField :: TH.VarBangType -> TH.Q (TH.Name, TH.Type)
recordField (fieldName, _, fieldType) =
    pure (fieldName, fieldType)

isMaybeType :: TH.Type -> Bool
isMaybeType = \case
    TH.AppT (TH.ConT maybeName) _ | maybeName == ''Maybe -> True
    TH.SigT fieldType _ -> isMaybeType fieldType
    TH.ParensT fieldType -> isMaybeType fieldType
    _ -> False

recordSchemaField :: forall schema. ToSchema schema => Proxy schema -> Declare (Definitions Schema) (Referenced Schema)
recordSchemaField = declareSchemaRef

recordNamedSchema :: String -> [(String, Referenced Schema)] -> [String] -> NamedSchema
recordNamedSchema schemaName propertySchemas requiredFields =
    NamedSchema (Just (Text.pack schemaName)) schema
  where
    schema =
        mempty
            { _schemaType = Just OpenApiObject
            , _schemaRequired = map Text.pack requiredFields
            , _schemaProperties =
                propertySchemas
                    |> map (\(propertyName, propertySchema) -> (Text.pack propertyName, propertySchema))
                    |> Exts.fromList
            }

defaultOpenApiInfo :: forall application. (Typeable.Typeable application) => OpenApiInfo
defaultOpenApiInfo =
    OpenApiInfo
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

data SwaggerUiController application
    = SwaggerUiAction
    | OpenApiJsonAction
    deriving (Eq, Show)

{- | Configuration used by 'SwaggerUiController'.

When 'SwaggerUiController' is routed via @[routes|...|]@, the
@swaggerUiPath@ and @swaggerUiOpenApiPath@ fields are ignored: the routes DSL
is the source of truth for both URLs. The remaining fields configure the
document metadata and Swagger UI assets.
-}
class (Typeable.Typeable application) => SwaggerUiControllerConfig application where
    swaggerUiControllerOptions :: SwaggerUiOptions

instance {-# OVERLAPPABLE #-} (Typeable.Typeable application) => SwaggerUiControllerConfig application where
    swaggerUiControllerOptions = defaultSwaggerUiOptions @application

instance
    ( FrontController application
    , HasPath (SwaggerUiController application)
    , SwaggerUiControllerConfig application
    ) =>
    Controller (SwaggerUiController application)
    where
    action SwaggerUiAction =
        let options = swaggerUiControllerOptions @application
            openApiUrl = pathTo (OpenApiJsonAction @application)
         in respondWith (swaggerUiHtmlResponse options openApiUrl)

    action OpenApiJsonAction =
        do
            application <- Context.fromContext @application
            let options = swaggerUiControllerOptions @application
            respondWith (openApiJsonResponse options.swaggerUiInfo application)

{- | Default Swagger UI settings for an application.

This serves the generated OpenAPI JSON at @/api-docs/openapi.json@ and the
Swagger UI at @/api-docs@.
-}
defaultSwaggerUiOptions :: forall application. (Typeable.Typeable application) => SwaggerUiOptions
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

buildOpenApiWithInfo :: forall application. (FrontController application) => OpenApiInfo -> application -> JSON.Value
buildOpenApiWithInfo info application =
    let ?request = defaultRequest
        ?respond = dummyRespond
        ?application = application
     in let routeTree = RouteCollection (controllers @application |> map routeInspection)
         in case collectPaths "" routeTree of
                Left errorMessage -> throwOpenApiGenerationException errorMessage
                Right OpenApiDocument{pathOperations, componentSchemas} ->
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

{- | Mounts Swagger UI for the current front controller using 'defaultSwaggerUiOptions'.

This adds two undocumented routes:

- the Swagger UI HTML at @/api-docs@
- the generated OpenAPI document at @/api-docs/openapi.json@
-}
swaggerUi ::
    forall application.
    ( FrontController application
    , Typeable.Typeable application
    , ?application :: application
    ) =>
    ControllerRoute application
swaggerUi = swaggerUiWithOptions (defaultSwaggerUiOptions @application)

{- | Mounts Swagger UI and the generated OpenAPI JSON for the current front controller.

The JSON endpoint is derived from the same mounted router tree via
'buildOpenApiWithInfo', so the documentation stays aligned with the
application routing.
-}
swaggerUiWithOptions ::
    forall application.
    ( FrontController application
    , ?application :: application
    ) =>
    SwaggerUiOptions ->
    ControllerRoute application
swaggerUiWithOptions options@SwaggerUiOptions{swaggerUiPath, swaggerUiOpenApiPath, swaggerUiInfo} =
    let normalizedBasePath = normalizeSwaggerUiBasePath swaggerUiPath
        normalizedOpenApiPath = normalizeSwaggerUiChildPath swaggerUiOpenApiPath
        openApiUrl = swaggerUiOpenApiUrl normalizedBasePath normalizedOpenApiPath
        htmlResponse = swaggerUiHtmlResponse options{swaggerUiPath = normalizedBasePath, swaggerUiOpenApiPath = normalizedOpenApiPath} openApiUrl
        application = ?application
     in withPrefix
            normalizedBasePath
            [ getResponseRoute normalizedOpenApiPath (\_ -> openApiJsonResponse swaggerUiInfo application)
            , getResponseRoute "" (\_ -> htmlResponse)
            , getResponseRoute "/" (\_ -> htmlResponse)
            ]

openApiJsonResponse :: forall application. (FrontController application) => OpenApiInfo -> application -> Response
openApiJsonResponse info application =
    responseLBS status200 [(hContentType, "application/json")] (JSON.encode (buildOpenApiWithInfo info application))

swaggerUiHtmlResponse :: SwaggerUiOptions -> Text -> Response
swaggerUiHtmlResponse options openApiUrl =
    responseBuilder status200 [(hContentType, "text/html; charset=utf-8")] (Blaze.renderHtmlBuilder (swaggerUiHtml options openApiUrl))

getResponseRoute :: ByteString -> (Request -> Response) -> ControllerRoute application
getResponseRoute path toResponse = rawRoute do
    string path
    pure (\request respond -> handleGet request respond (toResponse request))

handleGet :: Request -> (Response -> IO responseReceived) -> Response -> IO responseReceived
handleGet request respond response =
    case parseMethod (requestMethod request) of
        Right GET -> respond response
        Right HEAD -> respond response
        Right method -> Exception.throw UnexpectedMethodException{allowedMethods = [GET, HEAD], method}
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

swaggerUiHtml :: SwaggerUiOptions -> Text -> Html5.Html
swaggerUiHtml SwaggerUiOptions{swaggerUiInfo, swaggerUiTitle, swaggerUiCssUrl, swaggerUiBundleJsUrl, swaggerUiStandalonePresetJsUrl} openApiUrl =
    let title = fromMaybe (swaggerUiInfo.openApiTitle <> " Swagger UI") swaggerUiTitle
        openApiUrlLiteral = jsonStringLiteral openApiUrl
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

swaggerUiOpenApiUrl :: ByteString -> ByteString -> Text
swaggerUiOpenApiUrl basePath openApiPath =
    let base = Text.dropWhileEnd (== '/') (Text.decodeUtf8 basePath)
        child = Text.dropWhile (== '/') (Text.decodeUtf8 openApiPath)
     in (if Text.null base then "" else base) <> "/" <> child

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
openApiInfoValue OpenApiInfo{openApiTitle, openApiVersion, openApiDescription} =
    JSON.object
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
collectDocumentedRoute currentPrefix (TypedRouteControllerInfo{typedRouteDocuments}) =
    typedRouteDocuments
        |> mapMaybe (fromDynamic @TypedAction.TypedRouteDocument)
        |> foldl' (insertTypedRouteOperation currentPrefix) (Right mempty)

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
        |> map
            ( \(path, operations) ->
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
openApiComponentsValue schemas =
    JSON.object
        [ "schemas" JSON..= schemas
        ]

insertTypedRouteOperation ::
    Text ->
    Either Text OpenApiDocument ->
    TypedAction.TypedRouteDocument ->
    Either Text OpenApiDocument
insertTypedRouteOperation currentPrefix pathState routeDocument@TypedAction.TypedRouteDocument{typedRouteDocumentName, typedRouteDocumentPath, typedRouteDocumentMethods, typedRouteDocumentParameters} = do
    OpenApiDocument{pathOperations, componentSchemas} <- pathState
    let actionPath = appendPathPrefix currentPrefix typedRouteDocumentPath
    validateTypedRoutePathParameters typedRouteDocumentName actionPath typedRouteDocumentParameters
    let (operation, operationSchemas) = typedRouteDocumentOperationValue routeDocument
    pure
        OpenApiDocument
            { pathOperations = foldl' (insertMethod actionPath operation) pathOperations typedRouteDocumentMethods
            , componentSchemas = componentSchemas <> operationSchemas
            }

validateTypedRoutePathParameters :: Text -> Text -> [TypedAction.ParameterDoc] -> Either Text ()
validateTypedRoutePathParameters routeName actionPath routeParameters =
    let routePathParams = pathParameterNames actionPath
        documentedPathParams = routePathParameterNames routeParameters
        undocumentedPathParams = filter (`notElem` documentedPathParams) routePathParams
        paramsMissingFromRoute = filter (`notElem` routePathParams) documentedPathParams
     in case (undocumentedPathParams, paramsMissingFromRoute) of
            ([], []) -> Right ()
            (missingDocs, []) ->
                Left
                    ( "OpenAPI docs for "
                        <> routeName
                        <> " are missing route path-parameter docs for route path "
                        <> actionPath
                        <> ": "
                        <> Text.intercalate ", " missingDocs
                    )
            ([], missingRouteParams) ->
                Left
                    ( "OpenAPI docs for "
                        <> routeName
                        <> " document path params not present in route path "
                        <> actionPath
                        <> ": "
                        <> Text.intercalate ", " missingRouteParams
                    )
            (missingDocs, missingRouteParams) ->
                Left
                    ( "OpenAPI docs for "
                        <> routeName
                        <> " disagree with route path "
                        <> actionPath
                        <> "; missing route path-parameter docs for: "
                        <> Text.intercalate ", " missingDocs
                        <> "; documented params not present in route path: "
                        <> Text.intercalate ", " missingRouteParams
                    )
{-# INLINE validateTypedRoutePathParameters #-}

routePathParameterNames :: [TypedAction.ParameterDoc] -> [Text]
routePathParameterNames parameters =
    parameters
        |> mapMaybe
            ( \case
                TypedAction.ParameterDoc name TypedAction.PathParameter _ _ _ -> Just name
                _ -> Nothing
            )
{-# INLINE routePathParameterNames #-}

pathParameterNames :: Text -> [Text]
pathParameterNames path =
    case Text.breakOn "{" path of
        (_, "") -> []
        (_, rest) ->
            let afterOpen = Text.drop 1 rest
                (name, afterName) = Text.breakOn "}" afterOpen
             in if Text.null afterName
                    then []
                    else name : pathParameterNames (Text.drop 1 afterName)
{-# INLINE pathParameterNames #-}

typedRouteDocumentOperationValue :: TypedAction.TypedRouteDocument -> (JSON.Value, Definitions Schema)
typedRouteDocumentOperationValue
    TypedAction.TypedRouteDocument
        { typedRouteDocumentName
        , typedRouteDocumentParameters
        , typedRouteDocumentSummary
        , typedRouteDocumentDescription
        , typedRouteDocumentTags
        , typedRouteDocumentOperationId
        , typedRouteDocumentRequestBody
        , typedRouteDocumentResponse
        , typedRouteDocumentSuccessStatus
        , typedRouteDocumentSuccessResponseDescription
        } =
    let SchemaDocumentation{documentedSchema, documentedDefinitions} = responseSchemaValue typedRouteDocumentResponse
        parameterDocumentation = map typedParameterDocumentation typedRouteDocumentParameters
        parameterDefinitions = parameterDocumentation |> map (\QueryParameterDocumentation{parameterDefinitions} -> parameterDefinitions) |> mconcat
        requestBodyDocumentation = typedRequestBodySchemaValue <$> typedRouteDocumentRequestBody
        requestBodyDefinitions = requestBodyDocumentation |> fmap (.documentedDefinitions) |> fromMaybe mempty
        requestBodyValue =
            case (typedRouteDocumentRequestBody, requestBodyDocumentation) of
                (Just requestBodyDoc, Just schemaDocumentation) -> Just ("requestBody" JSON..= typedRequestBodyValue requestBodyDoc schemaDocumentation.documentedSchema)
                _ -> Nothing
     in ( JSON.object
            ( [ Just ("parameters" JSON..= map queryParameterValue parameterDocumentation)
              , Just ("responses" JSON..= JSON.object [cs (show (statusCode typedRouteDocumentSuccessStatus)) JSON..= successResponseValue typedRouteDocumentSuccessResponseDescription documentedSchema])
              , requestBodyValue
              , ("summary" JSON..=) <$> typedRouteDocumentSummary
              , ("description" JSON..=) <$> typedRouteDocumentDescription
              , if null typedRouteDocumentTags then Nothing else Just ("tags" JSON..= typedRouteDocumentTags)
              , ("operationId" JSON..=) <$> typedRouteDocumentOperationId
              , Just ("x-ihp-action" JSON..= typedRouteDocumentName)
              ]
                |> catMaybes
            )
        , documentedDefinitions <> parameterDefinitions <> requestBodyDefinitions
        )

typedParameterDocumentation :: TypedAction.ParameterDoc -> QueryParameterDocumentation
typedParameterDocumentation (TypedAction.ParameterDoc name location required schema _) =
    let SchemaDocumentation{documentedSchema, documentedDefinitions} = declareSchemaDocumentation schema
     in QueryParameterDocumentation
            { parameterName = name
            , parameterLocation = typedParameterLocationValue location
            , parameterRequired = required
            , parameterSchema = documentedSchema
            , parameterDefinitions = documentedDefinitions
            , parameterExplode = Nothing
            }

typedParameterLocationValue :: TypedAction.ParameterLocation -> ParameterLocation
typedParameterLocationValue = \case
    TypedAction.PathParameter -> PathParameter
    TypedAction.QueryParameter -> QueryParameter

typedRequestBodySchemaValue :: TypedAction.TypedRequestBodyDoc -> SchemaDocumentation
typedRequestBodySchemaValue (TypedAction.TypedRequestBodyDoc schema _ _) =
    declareSchemaDocumentation schema

typedRequestBodyValue :: TypedAction.TypedRequestBodyDoc -> Referenced Schema -> JSON.Value
typedRequestBodyValue (TypedAction.TypedRequestBodyDoc _ _ encodings) schema =
    JSON.object
        [ "required" JSON..= True
        , "content" JSON..= typedRequestBodyContentValue encodings schema
        ]

typedRequestBodyContentValue :: [TypedAction.BodyEncoding] -> Referenced Schema -> JSON.Value
typedRequestBodyContentValue encodings schema =
    encodings
        |> map
            ( \encoding ->
                ( JSON.Key.fromText (TypedAction.encodingMediaType encoding)
                , JSON.object ["schema" JSON..= schema]
                )
            )
        |> JSON.KeyMap.fromList
        |> JSON.Object

responseSchemaValue :: forall view. (ViewSupport.JsonView view, ToSchema (ViewSupport.JsonResponse view)) => Proxy view -> SchemaDocumentation
responseSchemaValue _ = declareSchemaDocumentation (Proxy @(ViewSupport.JsonResponse view))

successResponseValue :: Text -> Referenced Schema -> JSON.Value
successResponseValue responseDescription schema =
    JSON.object
        [ "description" JSON..= responseDescription
        , "content"
            JSON..= JSON.object
                [ "application/json"
                    JSON..= JSON.object
                        [ "schema" JSON..= schema
                        ]
                ]
        ]

data ParameterLocation
    = QueryParameter
    | PathParameter

data QueryParameterDocumentation = QueryParameterDocumentation
    { parameterName :: Text
    , parameterLocation :: ParameterLocation
    , parameterRequired :: Bool
    , parameterSchema :: Referenced Schema
    , parameterDefinitions :: Definitions Schema
    , parameterExplode :: Maybe Bool
    }

queryParameterValue :: QueryParameterDocumentation -> JSON.Value
queryParameterValue QueryParameterDocumentation{parameterName, parameterLocation, parameterRequired, parameterSchema, parameterExplode} =
    JSON.object
        ( [ Just ("name" JSON..= parameterName)
          , Just ("in" JSON..= parameterLocationValue parameterLocation)
          , Just ("required" JSON..= parameterRequired)
          , Just ("schema" JSON..= parameterSchema)
          , if isJust parameterExplode then Just ("style" JSON..= ("form" :: Text)) else Nothing
          , ("explode" JSON..=) <$> parameterExplode
          ]
            |> catMaybes
        )

parameterLocationValue :: ParameterLocation -> Text
parameterLocationValue QueryParameter = "query"
parameterLocationValue PathParameter = "path"

data SchemaDocumentation = SchemaDocumentation
    { documentedSchema :: Referenced Schema
    , documentedDefinitions :: Definitions Schema
    }

declareSchemaDocumentation :: forall schema. (ToSchema schema) => Proxy schema -> SchemaDocumentation
declareSchemaDocumentation proxy =
    let (definitions, schema) = runDeclare (declareSchemaRef proxy) mempty
     in SchemaDocumentation
            { documentedSchema = schema
            , documentedDefinitions = definitions
            }

instance {-# OVERLAPPABLE #-} (KnownSymbol table, ToSchema (PrimaryKey table)) => ToSchema (Id' table) where
    declareNamedSchema _ = declareNamedSchema (Proxy @(PrimaryKey table))
