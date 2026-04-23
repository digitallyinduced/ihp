{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module IHP.RouterSupport (
    CanRoute (..),
    HasPath (..),
    AutoRoute (..),
    RouteInspection (..),
    RouteDocumentation (..),
    DocumentedRouteInfo (..),
    OpenApiController (..),
    documentRoute,
    ActionDoc (..),
    OpenApiRequestBodyDoc (..),
    HasOpenApiRequestBody (..),
    actionDoc,
    actionDocFor,
    actionDocForRequestBody,
    setOpenApiSummary,
    setOpenApiDescription,
    setOpenApiTags,
    setOpenApiOperationId,
    setOpenApiRequestBody,
    setOpenApiRequestBodyRequired,
    setOpenApiSuccessStatus,
    setOpenApiSuccessResponseDescription,
    decodeActionRequestBody,
    runAction,
    get,
    post,
    startPage,
    frontControllerToWAIApp,
    withPrefix,
    FrontController (..),
    defaultRouter,
    parseRoute,
    catchAll,
    mountFrontController,
    createAction,
    updateAction,
    urlTo,
    actionPrefixText,
    parseUUID,
    parseId,
    parseIntegerId,
    remainingText,
    parseText,
    webSocketApp,
    webSocketAppWithCustomPath,
    webSocketAppWithHTTPFallback,
    onlyAllowMethods,
    getMethod,
    routeParam,
    withImplicits,
    rawRoute,
    validateOpenApiRenderedView,
    applyConstr,
    ControllerRoute (..),
    findInRouteMaps,
    buildAutoRouteMap,
    stripActionSuffixText,
) where

import Control.Applicative (empty, (<|>))
import Control.Exception (evaluate)
import Control.Exception qualified as Exception
import Control.Exception.Safe (SomeException, catch, throwIO)
import Control.Monad (join, unless)
import Control.Monad.State.Strict qualified as State
import Data.Aeson qualified as JSON
import Data.Attoparsec.ByteString.Char8 (Parser, choice, endOfInput, parseOnly, string, take, takeByteString, takeTill)
import Data.Attoparsec.ByteString.Char8 qualified as Attoparsec
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.Data
import Data.Dynamic
import Data.HashMap.Strict qualified as HashMap
import Data.Kind
import Data.List (find, isPrefixOf)
import Data.List qualified as List
import Data.Maybe (fromMaybe, mapMaybe)
import Data.OpenApi (ToSchema)
import Data.String.Conversions (ConvertibleStrings (convertString), cs)
import Data.TMap qualified as TypeMap
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Typeable qualified as Typeable
import Data.UUID
import Data.Vault.Lazy qualified as Vault
import GHC.TypeLits
import GHC.TypeLits as T
import IHP.Controller.Context
import IHP.Controller.Param
import IHP.Controller.Response (ResponseException(..))
import IHP.ControllerSupport
import IHP.ErrorController qualified as ErrorController
import IHP.FrameworkConfig
import IHP.HaskellSupport hiding (get)
import IHP.ModelSupport qualified as ModelSupport
import IHP.Router.Types
import IHP.Router.UrlGenerator
import IHP.ViewSupport qualified as ViewSupport
import IHP.WebSocket (WSApp)
import IHP.WebSocket qualified as WS
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (Status, status200, status400, status500, statusCode)
import Network.HTTP.Types.URI
import Network.URI.Encode qualified as URI
import Network.Wai
import Network.Wai.Middleware.EarlyReturn (earlyReturnMiddleware)
import System.IO.Unsafe (unsafePerformIO)
import Text.Blaze.Html5 qualified as Html5
import Text.Read (readMaybe)
import Unsafe.Coerce
import Prelude hiding (take)

{- | Binds @?request@ and @?respond@ from WAI arguments, then runs the given action.

This avoids repeating @let ?request = waiRequest; let ?respond = waiRespond@ at each call site.
-}
{-# INLINE withImplicits #-}
withImplicits :: ((?request :: Request, ?respond :: Respond) => Application) -> Application
withImplicits action waiRequest waiRespond =
    let ?request = waiRequest
        ?respond = waiRespond
     in action waiRequest waiRespond

runAction' ::
    forall application controller.
    ( Controller controller
    , InitControllerContext application
    , ?application :: application
    , Typeable application
    , Typeable controller
    ) =>
    controller -> Application
runAction' controller waiRequest waiRespond =
    earlyReturnMiddleware
        ( \request respond -> do
            context <- setupActionContext @application (Typeable.typeOf controller) request respond
            let ?context = context
            let ?respond = respond
            let ?request = context.request
            let ?modelContext = ?request.modelContext
            runAction controller
        )
        waiRequest
        waiRespond
{-# INLINE runAction' #-}

-- | Catches exceptions from routing and rethrows them wrapped in
-- 'RouterException' so the error handler middleware can distinguish
-- routing failures from action failures.
wrapRouterException :: IO a -> IO a
wrapRouterException action = action `catch` \(e :: SomeException) -> throwIO (ErrorController.RouterException e)

data ActionDoc controller where
    ActionDoc ::
        forall controller view.
        ( ViewSupport.View view
        , ViewSupport.JsonView view
        , Typeable.Typeable view
        , JSON.ToJSON (ViewSupport.JsonResponse view)
        , ToSchema (ViewSupport.JsonResponse view)
        ) =>
        { actionDocName :: Text
        , actionDocSummary :: Maybe Text
        , actionDocDescription :: Maybe Text
        , actionDocTags :: [Text]
        , actionDocOperationId :: Maybe Text
        , actionDocView :: Proxy view
        , actionDocTypedJson :: view -> JSON.Value
        , actionDocRequestBody :: Maybe OpenApiRequestBodyDoc
        , actionDocSuccessStatus :: Int
        , actionDocSuccessResponseDescription :: Text
        } ->
        ActionDoc controller

data OpenApiRequestBodyDoc where
    OpenApiRequestBodyDoc ::
        forall body.
        ( ToSchema body
        , Typeable.Typeable body
        ) =>
        { requestBodyRequired :: Bool
        , requestBodySchema :: Proxy body
        , requestBodyTypeRep :: Typeable.TypeRep
        } ->
        OpenApiRequestBodyDoc

class (JSON.FromJSON (OpenApiRequestBody controller actionName), ToSchema (OpenApiRequestBody controller actionName)) => HasOpenApiRequestBody controller (actionName :: Symbol) where
    type OpenApiRequestBody controller actionName :: Type
    openApiRequestBodyRequired :: Bool
    openApiRequestBodyRequired = True

actionDoc ::
    forall view controller.
    ( ViewSupport.View view
    , ViewSupport.JsonView view
    , Typeable.Typeable view
    , JSON.ToJSON (ViewSupport.JsonResponse view)
    , ToSchema (ViewSupport.JsonResponse view)
    ) =>
    Text -> ActionDoc controller
actionDoc actionName =
    ActionDoc
        { actionDocName = actionName
        , actionDocSummary = Nothing
        , actionDocDescription = Nothing
        , actionDocTags = []
        , actionDocOperationId = Nothing
        , actionDocView = Proxy @view
        , actionDocTypedJson = JSON.toJSON . ViewSupport.jsonTyped
        , actionDocRequestBody = Nothing
        , actionDocSuccessStatus = statusCode status200
        , actionDocSuccessResponseDescription = "Successful response"
        }
{-# INLINE actionDoc #-}

actionDocFor ::
    forall actionName view controller.
    ( KnownSymbol actionName
    , ViewSupport.View view
    , Typeable.Typeable view
    , JSON.ToJSON (ViewSupport.JsonResponse view)
    , ToSchema (ViewSupport.JsonResponse view)
    ) =>
    ActionDoc controller
actionDocFor =
    ActionDoc
        { actionDocName = cs (symbolVal (Proxy @actionName))
        , actionDocSummary = Nothing
        , actionDocDescription = Nothing
        , actionDocTags = []
        , actionDocOperationId = Nothing
        , actionDocView = Proxy @view
        , actionDocTypedJson = JSON.toJSON . ViewSupport.jsonTyped
        , actionDocRequestBody = Nothing
        , actionDocSuccessStatus = statusCode status200
        , actionDocSuccessResponseDescription = "Successful response"
        }
{-# INLINE actionDocFor #-}

actionDocForRequestBody ::
    forall actionName view controller.
    ( KnownSymbol actionName
    , HasOpenApiRequestBody controller actionName
    , ViewSupport.View view
    , Typeable.Typeable view
    , JSON.ToJSON (ViewSupport.JsonResponse view)
    , ToSchema (ViewSupport.JsonResponse view)
    ) =>
    ActionDoc controller
actionDocForRequestBody =
    ActionDoc
        { actionDocName = cs (symbolVal (Proxy @actionName))
        , actionDocSummary = Nothing
        , actionDocDescription = Nothing
        , actionDocTags = []
        , actionDocOperationId = Nothing
        , actionDocView = Proxy @view
        , actionDocTypedJson = JSON.toJSON . ViewSupport.jsonTyped
        , actionDocRequestBody =
            Just
                OpenApiRequestBodyDoc
                    { requestBodyRequired = openApiRequestBodyRequired @controller @actionName
                    , requestBodySchema = Proxy @(OpenApiRequestBody controller actionName)
                    , requestBodyTypeRep = Typeable.typeRep (Proxy @(OpenApiRequestBody controller actionName))
                    }
        , actionDocSuccessStatus = statusCode status200
        , actionDocSuccessResponseDescription = "Successful response"
        }
{-# INLINE actionDocForRequestBody #-}

setOpenApiSummary :: Text -> ActionDoc controller -> ActionDoc controller
setOpenApiSummary summary ActionDoc{actionDocName, actionDocDescription, actionDocTags, actionDocOperationId, actionDocView, actionDocTypedJson, actionDocRequestBody, actionDocSuccessStatus, actionDocSuccessResponseDescription} =
    ActionDoc
        { actionDocName
        , actionDocSummary = Just summary
        , actionDocDescription
        , actionDocTags
        , actionDocOperationId
        , actionDocView
        , actionDocTypedJson
        , actionDocRequestBody
        , actionDocSuccessStatus
        , actionDocSuccessResponseDescription
        }
{-# INLINE setOpenApiSummary #-}

setOpenApiDescription :: Text -> ActionDoc controller -> ActionDoc controller
setOpenApiDescription description ActionDoc{actionDocName, actionDocSummary, actionDocTags, actionDocOperationId, actionDocView, actionDocTypedJson, actionDocRequestBody, actionDocSuccessStatus, actionDocSuccessResponseDescription} =
    ActionDoc
        { actionDocName
        , actionDocSummary
        , actionDocDescription = Just description
        , actionDocTags
        , actionDocOperationId
        , actionDocView
        , actionDocTypedJson
        , actionDocRequestBody
        , actionDocSuccessStatus
        , actionDocSuccessResponseDescription
        }
{-# INLINE setOpenApiDescription #-}

setOpenApiTags :: [Text] -> ActionDoc controller -> ActionDoc controller
setOpenApiTags tags ActionDoc{actionDocName, actionDocSummary, actionDocDescription, actionDocOperationId, actionDocView, actionDocTypedJson, actionDocRequestBody, actionDocSuccessStatus, actionDocSuccessResponseDescription} =
    ActionDoc
        { actionDocName
        , actionDocSummary
        , actionDocDescription
        , actionDocTags = tags
        , actionDocOperationId
        , actionDocView
        , actionDocTypedJson
        , actionDocRequestBody
        , actionDocSuccessStatus
        , actionDocSuccessResponseDescription
        }
{-# INLINE setOpenApiTags #-}

setOpenApiOperationId :: Text -> ActionDoc controller -> ActionDoc controller
setOpenApiOperationId operationId ActionDoc{actionDocName, actionDocSummary, actionDocDescription, actionDocTags, actionDocView, actionDocTypedJson, actionDocRequestBody, actionDocSuccessStatus, actionDocSuccessResponseDescription} =
    ActionDoc
        { actionDocName
        , actionDocSummary
        , actionDocDescription
        , actionDocTags
        , actionDocOperationId = Just operationId
        , actionDocView
        , actionDocTypedJson
        , actionDocRequestBody
        , actionDocSuccessStatus
        , actionDocSuccessResponseDescription
        }
{-# INLINE setOpenApiOperationId #-}

-- | Documents the JSON request body schema for an action.
setOpenApiRequestBody ::
    forall body controller.
    ( ToSchema body
    , Typeable.Typeable body
    ) =>
    ActionDoc controller ->
    ActionDoc controller
setOpenApiRequestBody ActionDoc{actionDocName, actionDocSummary, actionDocDescription, actionDocTags, actionDocOperationId, actionDocView, actionDocTypedJson, actionDocSuccessStatus, actionDocSuccessResponseDescription} =
    ActionDoc
        { actionDocName
        , actionDocSummary
        , actionDocDescription
        , actionDocTags
        , actionDocOperationId
        , actionDocView
        , actionDocTypedJson
        , actionDocRequestBody =
            Just
                OpenApiRequestBodyDoc
                    { requestBodyRequired = True
                    , requestBodySchema = Proxy @body
                    , requestBodyTypeRep = Typeable.typeRep (Proxy @body)
                    }
        , actionDocSuccessStatus
        , actionDocSuccessResponseDescription
        }
{-# INLINE setOpenApiRequestBody #-}

-- | Overrides whether the documented request body is required.
setOpenApiRequestBodyRequired :: Bool -> ActionDoc controller -> ActionDoc controller
setOpenApiRequestBodyRequired required ActionDoc{actionDocName, actionDocSummary, actionDocDescription, actionDocTags, actionDocOperationId, actionDocView, actionDocTypedJson, actionDocRequestBody, actionDocSuccessStatus, actionDocSuccessResponseDescription} =
    ActionDoc
        { actionDocName
        , actionDocSummary
        , actionDocDescription
        , actionDocTags
        , actionDocOperationId
        , actionDocView
        , actionDocTypedJson
        , actionDocRequestBody = setRequired <$> actionDocRequestBody
        , actionDocSuccessStatus
        , actionDocSuccessResponseDescription
        }
  where
    setRequired OpenApiRequestBodyDoc{requestBodySchema, requestBodyTypeRep} =
        OpenApiRequestBodyDoc
            { requestBodyRequired = required
            , requestBodySchema
            , requestBodyTypeRep
            }
{-# INLINE setOpenApiRequestBodyRequired #-}

-- | Overrides the documented success response status for an action.
setOpenApiSuccessStatus :: Status -> ActionDoc controller -> ActionDoc controller
setOpenApiSuccessStatus status ActionDoc{actionDocName, actionDocSummary, actionDocDescription, actionDocTags, actionDocOperationId, actionDocView, actionDocTypedJson, actionDocRequestBody, actionDocSuccessResponseDescription} =
    ActionDoc
        { actionDocName
        , actionDocSummary
        , actionDocDescription
        , actionDocTags
        , actionDocOperationId
        , actionDocView
        , actionDocTypedJson
        , actionDocRequestBody
        , actionDocSuccessStatus = statusCode status
        , actionDocSuccessResponseDescription
        }
{-# INLINE setOpenApiSuccessStatus #-}

-- | Overrides the documented success response description for an action.
setOpenApiSuccessResponseDescription :: Text -> ActionDoc controller -> ActionDoc controller
setOpenApiSuccessResponseDescription description ActionDoc{actionDocName, actionDocSummary, actionDocDescription, actionDocTags, actionDocOperationId, actionDocView, actionDocTypedJson, actionDocRequestBody, actionDocSuccessStatus} =
    ActionDoc
        { actionDocName
        , actionDocSummary
        , actionDocDescription
        , actionDocTags
        , actionDocOperationId
        , actionDocView
        , actionDocTypedJson
        , actionDocRequestBody
        , actionDocSuccessStatus
        , actionDocSuccessResponseDescription = description
        }
{-# INLINE setOpenApiSuccessResponseDescription #-}

decodeActionRequestBody ::
    forall body controller.
    ( OpenApiController controller
    , JSON.FromJSON body
    , Typeable.Typeable body
    , Data controller
    , ?request :: Request
    , ?theAction :: controller
    ) =>
    IO (Either String body)
decodeActionRequestBody = do
    let currentActionName = cs (showConstr (toConstr ?theAction))
    let maybeRequestBodyDoc =
            openApiActions @controller
                |> find (\ActionDoc{actionDocName} -> actionDocName == currentActionName)
                >>= (.actionDocRequestBody)
    case maybeRequestBodyDoc of
        Nothing ->
            fail ("decodeActionRequestBody found no registered request body for action " <> cs currentActionName)
        Just OpenApiRequestBodyDoc{requestBodyTypeRep} ->
            unless (requestBodyTypeRep == Typeable.typeRep (Proxy @body)) do
                fail ("decodeActionRequestBody expected " <> show requestBodyTypeRep <> " for action " <> cs currentActionName <> " but handler requested " <> show (Typeable.typeRep (Proxy @body)))
    JSON.eitherDecode <$> getRequestBody
{-# INLINE decodeActionRequestBody #-}

class (AutoRoute controller) => OpenApiController controller where
    openApiActions :: [ActionDoc controller]

data DocumentedRouteInfo where
    AutoRouteControllerInfo ::
        forall controller.
        ( AutoRoute controller
        , Data controller
        , Typeable.Typeable controller
        ) =>
        { documentedActions :: Maybe [ActionDoc controller]
        } ->
        DocumentedRouteInfo

data RouteDocumentation
    = UndocumentedRoute
    | DocumentedRoute !DocumentedRouteInfo

data RouteInspection
    = RouteLeaf !RouteDocumentation
    | RouteCollection [RouteInspection]
    | RoutePrefix ByteString [RouteInspection]

data ControllerRoute application
    = ControllerRouteMap
        { routeMap :: !(HashMap.HashMap ByteString (application -> Application))
        , routeParser :: !(Parser Application)
        , routeInspection :: !RouteInspection
        }
    | ControllerRouteParser
        { routeParser :: !(Parser Application)
        , routeInspection :: !RouteInspection
        }

rawRoute :: Parser Application -> ControllerRoute application
rawRoute parser = ControllerRouteParser{routeParser = parser, routeInspection = RouteLeaf UndocumentedRoute}
{-# INLINE rawRoute #-}

compileControllerRoute :: ControllerRoute application -> Parser Application
compileControllerRoute ControllerRouteMap{routeParser} = routeParser
compileControllerRoute ControllerRouteParser{routeParser} = routeParser
{-# INLINE compileControllerRoute #-}

class FrontController application where
    controllers ::
        (?application :: application, ?request :: Request, ?respond :: Respond) =>
        [ControllerRoute application]

    router ::
        (?application :: application, ?request :: Request, ?respond :: Respond) =>
        [ControllerRoute application] -> Parser Application
    router = defaultRouter
    {-# INLINEABLE router #-}

defaultRouter ::
    (?application :: application, ?request :: Request, ?respond :: Respond, FrontController application) =>
    [ControllerRoute application] -> Parser Application
defaultRouter additionalRoutes = do
    let allRoutes = controllers <> additionalRoutes
        path = rawPathInfo ?request
    -- Fast path: check auto-route HashMaps directly (no Attoparsec string matching)
    case findInRouteMaps path allRoutes of
        Just handler -> takeByteString *> pure (handler ?application)
        Nothing -> do
            -- Slow path: fall back to Attoparsec parsers for custom/dynamic routes
            let parsers = concatMap getRouteParsers allRoutes
            choice (map (<* endOfInput) parsers)
{-# INLINEABLE defaultRouter #-}

{- | Scan 'ControllerRouteMap' entries for a matching path.
Returns as soon as a HashMap contains the path. Skips 'ControllerRouteParser' entries.
-}
findInRouteMaps :: ByteString -> [ControllerRoute application] -> Maybe (application -> Application)
findInRouteMaps _ [] = Nothing
findInRouteMaps path (ControllerRouteMap{routeMap} : rest) =
    case HashMap.lookup path routeMap of
        Just handler -> Just handler
        Nothing -> findInRouteMaps path rest
findInRouteMaps path (_ : rest) = findInRouteMaps path rest

-- | Extract fallback parsers from controller routes.
getRouteParsers :: ControllerRoute application -> [Parser Application]
getRouteParsers ControllerRouteMap{routeParser} = [routeParser]
getRouteParsers ControllerRouteParser{routeParser} = [routeParser]

data DocumentedRenderExpectation = DocumentedRenderExpectation
    { expectedViewTypeName :: Text
    , expectedTypedJson :: Dynamic -> Maybe JSON.Value
    }

openApiRenderExpectationKey :: Vault.Key DocumentedRenderExpectation
openApiRenderExpectationKey = unsafePerformIO Vault.newKey
{-# NOINLINE openApiRenderExpectationKey #-}

throwOpenApiRenderMismatch :: Text -> IO a
throwOpenApiRenderMismatch message =
    Exception.throwIO
        (ResponseException (responseLBS status500 [(hContentType, "text/plain")] (cs message)))
{-# INLINE throwOpenApiRenderMismatch #-}

validateOpenApiRenderedView
    :: forall view.
        ( Typeable.Typeable view
        , ?request :: Request
        )
    => view
    -> JSON.Value
    -> IO ()
validateOpenApiRenderedView view actualJson =
    case Vault.lookup openApiRenderExpectationKey ?request.vault of
        Nothing -> pure ()
        Just DocumentedRenderExpectation{expectedViewTypeName, expectedTypedJson} ->
            let renderedViewTypeName = cs (show (Typeable.typeRep (Proxy @view)))
             in unless
                    (renderedViewTypeName == expectedViewTypeName)
                    (throwOpenApiRenderMismatch ("OpenAPI docs expect view " <> cs expectedViewTypeName <> ", but render produced " <> cs renderedViewTypeName))
                    >> case expectedTypedJson (toDyn view) of
                        Nothing ->
                            throwOpenApiRenderMismatch ("OpenAPI docs could not validate the typed JSON for view " <> cs expectedViewTypeName)
                        Just expectedJson ->
                            unless
                                (actualJson == expectedJson)
                                (throwOpenApiRenderMismatch ("OpenAPI docs expect the JSON generated from jsonTyped of view " <> cs expectedViewTypeName <> ", but render produced a different JSON value"))
{-# INLINE validateOpenApiRenderedView #-}

documentedRenderExpectationForAction :: forall controller. (Data controller) => RouteDocumentation -> controller -> Maybe DocumentedRenderExpectation
documentedRenderExpectationForAction UndocumentedRoute _ = Nothing
documentedRenderExpectationForAction (DocumentedRoute (AutoRouteControllerInfo{documentedActions = Nothing})) _ = Nothing
documentedRenderExpectationForAction (DocumentedRoute (AutoRouteControllerInfo{documentedActions = Just docs})) action =
    docs
        |> find (\ActionDoc{actionDocName} -> actionDocName == cs (showConstr (toConstr action)))
        |> fmap
            ( \ActionDoc{actionDocView, actionDocTypedJson} ->
                DocumentedRenderExpectation
                    { expectedViewTypeName = cs (show (Typeable.typeRep actionDocView))
                    , expectedTypedJson = fmap actionDocTypedJson . fromDynamic
                    }
            )

runActionWithRouteDocumentation ::
    forall application controller.
    ( Controller controller
    , InitControllerContext application
    , ?application :: application
    , Typeable application
    , Typeable controller
    , Data controller
    ) =>
    RouteDocumentation ->
    controller ->
    Application
runActionWithRouteDocumentation routeDocumentation action waiRequest waiRespond =
    let renderExpectation = documentedRenderExpectationForAction routeDocumentation action
        waiRequest' = case renderExpectation of
            Just expected -> waiRequest{vault = Vault.insert openApiRenderExpectationKey expected waiRequest.vault}
            Nothing -> waiRequest
     in runAction' @application action waiRequest' waiRespond
{-# INLINE runActionWithRouteDocumentation #-}

{- | Returns the url to a given action.

Uses the baseUrl configured in @Config/Config.hs@. When no @baseUrl@
is configured in development mode, it will automatically detect the
correct @baseUrl@ value.

>>> urlTo UsersAction
"http://localhost:8000/Users"

>>> urlTo ShowUserAction { userId = "a32913dd-ef80-4f3e-9a91-7879e17b2ece" }
"http://localhost:8000/ShowUser?userId=a32913dd-ef80-4f3e-9a91-7879e17b2ece"
-}
urlTo :: (?context :: context, ConfigProvider context, HasPath action) => action -> Text
urlTo action = ?context.frameworkConfig.baseUrl <> pathTo action
{-# INLINE urlTo #-}

class (HasPath controller) => CanRoute controller where
    parseRoute' :: (?request :: Request, ?respond :: Respond) => Parser controller

    {- | Builds a WAI Application parser for this controller.

    The default implementation parses the controller action using 'parseRoute''
    and applies the given callback. The overlappable 'AutoRoute' instance overrides
    this to defer query string parsing and method validation to the Application closure.
    -}
    parseRouteWithAction :: (?request :: Request, ?respond :: Respond) => (controller -> Application) -> Parser Application
    parseRouteWithAction toApp = do
        action <- parseRoute'
        pure (toApp action)
    {-# INLINE parseRouteWithAction #-}

    {- | Build a 'ControllerRoute' for this controller.

    The default wraps the parser in 'ControllerRouteParser'.
    The overlappable 'AutoRoute' instance overrides this to use 'ControllerRouteMap'
    for O(1) HashMap dispatch. This is what 'parseRoute' calls.
    -}
    toControllerRoute ::
        forall application.
        ( ?request :: Request
        , ?respond :: Respond
        , Controller controller
        , InitControllerContext application
        , ?application :: application
        , Typeable application
        , Typeable controller
        ) =>
        ControllerRoute application
    toControllerRoute =
        ControllerRouteParser
            { routeParser = parseRouteWithAction @controller (runAction' @application)
            , routeInspection = RouteLeaf UndocumentedRoute
            }
    {-# INLINEABLE toControllerRoute #-}

{- | Each of these is tried when trying to parse an argument to a controller constructor (i.e. in IHP, an action).
The type @d@ is an the type of the argument, and all we know about this type that its conforms to @Data@.
We cannot cast @d@ to some arbitrary type, since adding additional constraints to @d@ (such as Read)
will break the @fromConstrM@ function which actually constructs the action.

The approach taken here is to make use of the type equality operator @:~:@
to check and see if @d@ happens to be a certain type. If it is,
by matching on Just Refl, we are able to use @d@ as the type we matched it to.

Please consult your doctor before engaging in Haskell type programming.
-}
parseFuncs :: forall d idType. (Data d, Data idType) => (ByteString -> Maybe idType) -> [Maybe ByteString -> Either TypedAutoRouteError d]
parseFuncs parseIdType =
    [ -- Try and parse @Int@ or @Maybe Int@
      \case
        Just queryValue -> case eqT :: Maybe (d :~: Int) of
            Just Refl -> case ByteString.readInt queryValue of
                Just (n, "") -> Right n
                _ -> Left BadType{field = "", value = Just queryValue, expectedType = "Int"}
            Nothing -> case eqT :: Maybe (d :~: Maybe Int) of
                Just Refl -> Right $ case ByteString.readInt queryValue of
                    Just (n, "") -> Just n
                    _ -> Nothing
                Nothing -> Left NotMatched
        Nothing -> case eqT :: Maybe (d :~: Maybe Int) of
            Just Refl -> Right Nothing
            Nothing -> Left NotMatched
    , \case
        Just queryValue -> case eqT :: Maybe (d :~: Integer) of
            Just Refl -> case ByteString.readInteger queryValue of
                Just (n, "") -> Right n
                _ -> Left BadType{field = "", value = Just queryValue, expectedType = "Integer"}
            Nothing -> case eqT :: Maybe (d :~: Maybe Integer) of
                Just Refl -> Right $ case ByteString.readInteger queryValue of
                    Just (n, "") -> Just n
                    _ -> Nothing
                Nothing -> Left NotMatched
        Nothing -> case eqT :: Maybe (d :~: Maybe Integer) of
            Just Refl -> Right Nothing
            Nothing -> Left NotMatched
    , -- Try and parse @Text@ or @Maybe Text@
      \case
        Just queryValue -> case eqT :: Maybe (d :~: Text) of
            Just Refl -> Right $ cs queryValue
            Nothing -> case eqT :: Maybe (d :~: Maybe Text) of
                Just Refl -> Right $ Just $ cs queryValue
                Nothing -> Left NotMatched
        Nothing -> case eqT :: Maybe (d :~: Maybe Text) of
            Just Refl -> Right Nothing
            Nothing -> Left NotMatched
    , \case
        Just queryValue -> case parseIdType queryValue of
            Just idValue -> case eqT :: Maybe (d :~: idType) of
                Just Refl -> Right idValue
                Nothing -> Left NotMatched
            Nothing -> Left NotMatched
        Nothing -> Left NotMatched
    , -- Try and parse @[Text]@. If value is not present then default to empty list.
      \queryValue -> case eqT :: Maybe (d :~: [Text]) of
        Just Refl -> case queryValue of
            Just queryValue -> Right $ Text.splitOn "," (cs queryValue)
            Nothing -> Right []
        Nothing -> Left NotMatched
    , -- Try and parse @[Int]@. If value is not present then default to empty list.
      \queryValue -> case eqT :: Maybe (d :~: [Int]) of
        Just Refl -> case queryValue of
            Just queryValue ->
                ByteString.split ',' queryValue
                    |> mapMaybe (\b -> case ByteString.readInt b of Just (n, "") -> Just n; _ -> Nothing)
                    |> Right
            Nothing -> Right []
        Nothing -> Left NotMatched
    , \queryValue -> case eqT :: Maybe (d :~: [Integer]) of
        Just Refl -> case queryValue of
            Just queryValue ->
                ByteString.split ',' queryValue
                    |> mapMaybe (\b -> case ByteString.readInteger b of Just (n, "") -> Just n; _ -> Nothing)
                    |> Right
            Nothing -> Right []
        Nothing -> Left NotMatched
    , -- Try and parse a raw [UUID]
      \queryValue -> case eqT :: Maybe (d :~: [UUID]) of
        Just Refl -> case queryValue of
            Just queryValue ->
                ByteString.split ',' queryValue
                    |> mapMaybe fromASCIIBytes
                    |> Right
            Nothing -> Right []
        Nothing -> Left NotMatched
    , -- Try and parse a raw UUID
      \queryValue -> case eqT :: Maybe (d :~: UUID) of
        Just Refl -> case queryValue of
            Just queryValue ->
                queryValue
                    |> fromASCIIBytes
                    |> \case
                        Just uuid -> uuid |> Right
                        Nothing -> Left BadType{field = "", value = Just queryValue, expectedType = "UUID"}
            Nothing -> Left NotMatched
        Nothing -> Left NotMatched
    , -- This has to be last parser in the list
      --
      -- Try and parse a UUID wrapped with a Id. In IHP types these are wrapped in a newtype @Id@ such as @Id User@.
      -- Since @Id@ is a newtype wrapping a UUID, it has the same data representation in GHC.
      -- Therefore, we're able to safely cast it to its @Id@ type with @unsafeCoerce@.
      --
      -- We cannot use 'eqT' here for checking the types, as it's wrapped inside the @Id@ type. We expect
      -- that if it looks like a UUID, we can just treat it like an @Id@ type. For that to not overshadow other
      -- parsers, we need to have this last.
      \queryValue -> case queryValue of
        Just queryValue ->
            queryValue
                |> fromASCIIBytes
                |> \case
                    Just uuid -> uuid |> unsafeCoerce |> Right
                    Nothing -> Left BadType{field = "", value = Just queryValue, expectedType = "UUID"}
        Nothing -> Left NotMatched
    ]
{-# NOINLINE parseFuncs #-}

{- | As we fold over a constructor, we want the values parsed from the query string
to be in the same order as they are in the constructor.
This function uses the field labels from the constructor to sort the values from
the query string. As a consequence, constructors with basic record syntax will not work with auto types.

@data MyController = MyAction Text Int@

does not work. Instead use,

@data MyController = MyAction { textArg :: Text, intArg :: Int }@
-}
querySortedByFields :: Query -> Constr -> Query
querySortedByFields query constructor =
    constrFields constructor
        |> map cs
        |> map (\field -> (field, join $ List.lookup field query))
{-# NOINLINE querySortedByFields #-}

{- | Given a constructor and a parsed query string, attempt to construct a value of the constructor's type.
For example, given the controller

@data MyController = MyAction { textArg :: Text, intArg :: Int }@

this function will receive a representation of the @MyAction@ constructor as well as some query string
@[("textArg", "some text"), ("intArg", "123")]@.

By iterating through the query and attempting to match the type of each constructor argument
with some transformation of the query string, we attempt to call @MyAction@.
-}
applyConstr :: (Data controller, Data idType) => (ByteString -> Maybe idType) -> Constr -> Query -> Either TypedAutoRouteError controller
applyConstr parseIdType constructor query =
    let
        -- \| Given some query item (key, optional value), try to parse into the current expected type
        -- by iterating through the available parse functions.
        attemptToParseArg :: forall d. (Data d) => (ByteString, Maybe ByteString) -> [Maybe ByteString -> Either TypedAutoRouteError d] -> State.StateT Query (Either TypedAutoRouteError) d
        attemptToParseArg queryParam@(queryName, queryValue) [] =
            State.lift
                ( Left
                    NoConstructorMatched
                        { field = queryName
                        , value = queryValue
                        , expectedType = (dataTypeOf (Prelude.undefined :: d)) |> dataTypeName |> cs
                        }
                )
        attemptToParseArg queryParam@(k, v) (parseFunc : restFuncs) = case parseFunc v of
            Right result -> pure result
            -- BadType will be returned if, for example, a text is passed to a query parameter typed as int.
            Left badType@BadType{} -> State.lift (Left badType{field = k})
            -- otherwise, safe to assume the match just failed, so recurse on the rest of the functions and try to find one that matches.
            Left _ -> attemptToParseArg queryParam restFuncs

        -- \| Attempt to parse the current expected type, and return its value.
        -- For the example @MyController@ this is called twice by @fromConstrM@.
        -- Once, it is called for @textArg@ where @d :: Text@. Then it is called
        -- for @intArg@ with @d ::: Int@. With both of these values parsed from the query string,
        -- the controller action is able to be created.
        nextField :: forall d. (Data d) => State.StateT Query (Either TypedAutoRouteError) d
        nextField = do
            queryParams <- State.get
            case queryParams of
                [] -> State.lift (Left TooFewArguments)
                (p@(key, value) : rest) -> do
                    State.put rest
                    attemptToParseArg p (parseFuncs parseIdType)
     in
        case State.runStateT (fromConstrM nextField constructor) (querySortedByFields query constructor) of
            Right (x, []) -> pure x
            Right (_) -> Left TooFewArguments
            Left e -> Left e -- runtime type error
{-# NOINLINE applyConstr #-}

class (Data controller) => AutoRoute controller where
    autoRouteWithIdType :: (?request :: Request, ?respond :: Respond, Data idType) => (ByteString -> Maybe idType) -> Parser controller
    autoRouteWithIdType parseIdFunc =
        let
            query :: Query
            query = queryString ?request
         in
            do
                -- routeMatchParser is a CAF (no ?request dependency), computed once per controller type.
                -- It handles the static string matching against URL paths.
                (constr, allowedMethods) <- routeMatchParser @controller
                action <- case applyConstr parseIdFunc constr query of
                    Right parsedAction -> pure parsedAction
                    Left e -> Exception.throw e
                method <- getMethod
                unless (allowedMethods |> includes method) (Exception.throw UnexpectedMethodException{allowedMethods, method})
                pure action
    {-# INLINEABLE autoRouteWithIdType #-}

    autoRoute :: (?request :: Request, ?respond :: Respond) => Parser controller
    autoRoute = autoRouteWithIdType (\_ -> Nothing :: Maybe Integer)
    {-# INLINEABLE autoRoute #-}

    {- | Constructs a controller value from a matched constructor and query string.

    Uses the same id parser as 'autoRoute'. Override this when you override
    'autoRoute' with 'autoRouteWithIdType' to keep them in sync.

    This is used by 'parseRoute' to defer query string parsing to the Application
    closure while keeping path matching in the parser.

    __Example:__

    > instance AutoRoute MyController where
    >     autoRoute = autoRouteWithIdType (parseIntegerId @(Id MyModel))
    >     applyAction = applyConstr (parseIntegerId @(Id MyModel))
    -}
    applyAction :: Constr -> Query -> Either TypedAutoRouteError controller
    applyAction = applyConstr (\_ -> Nothing :: Maybe Integer)
    {-# INLINE applyAction #-}

    {- | Specifies the allowed HTTP methods for a given action

    The default implementation does a smart guess based on the
    usual naming conventions for controllers.

    __Example (for default implementation):__

    >>> allowedMethodsForAction @ProjectsController "DeleteProjectAction"
    [DELETE]

    >>> allowedMethodsForAction @ProjectsController "UpdateProjectAction"
    [POST, PATCH]

    >>> allowedMethodsForAction @ProjectsController "CreateProjectAction"
    [POST]

    >>> allowedMethodsForAction @ProjectsController "ShowProjectAction"
    [GET, HEAD]

    >>> allowedMethodsForAction @ProjectsController "HelloAction"
    [GET, POST, HEAD]
    -}
    allowedMethodsForAction :: ByteString -> [StdMethod]
    allowedMethodsForAction actionName =
        case actionName of
            a | "Delete" `ByteString.isPrefixOf` a -> [DELETE]
            a | "Update" `ByteString.isPrefixOf` a -> [POST, PATCH]
            a | "Create" `ByteString.isPrefixOf` a -> [POST]
            a | "Show" `ByteString.isPrefixOf` a -> [GET, HEAD]
            _ -> [GET, POST, HEAD]
    {-# INLINE allowedMethodsForAction #-}

    {- | Custom route parser for overriding individual action routes.

    Use this to provide custom URL patterns for specific actions while keeping
    the auto-generated routes for all other actions.

    The custom routes are tried first, before the auto-generated routes.
    The auto-generated route for the overridden action still works as a fallback.

    __Example:__

    > instance AutoRoute PostsController where
    >     customRoutes = do
    >         string "/posts/"
    >         postId <- parseId
    >         endOfInput
    >         onlyAllowMethods [GET, HEAD]
    >         pure ShowPostAction { postId }
    -}
    customRoutes :: (?request :: Request, ?respond :: Respond) => Parser controller
    customRoutes = empty
    {-# INLINE customRoutes #-}

    {- | Custom path generation for overriding individual action URLs.

    Use this together with 'customRoutes' to generate custom URLs for specific
    actions while keeping the auto-generated URLs for all other actions.

    Return @Just path@ for actions with custom URLs, or @Nothing@ to fall back
    to the auto-generated URL.

    __Example:__

    > instance AutoRoute PostsController where
    >     customPathTo ShowPostAction { postId } = Just ("/posts/" <> tshow postId)
    >     customPathTo _ = Nothing
    -}
    customPathTo :: controller -> Maybe Text
    customPathTo _ = Nothing
    {-# INLINE customPathTo #-}

{- | Static route-matching parser that becomes a CAF when specialized for a concrete
controller type. Uses a 'HashMap' for O(1) action name lookup after matching the prefix.
Since it doesn't reference @?request@ or @?respond@, GHC can float this out as a
top-level constant.
-}
routeMatchParser :: forall controller. (Data controller, AutoRoute controller) => Parser (Constr, [StdMethod])
routeMatchParser = do
    string prefix
    remaining <- takeByteString
    case HashMap.lookup remaining actionMatchMap of
        Just result -> pure result
        Nothing -> fail "no matching action"
  where
    prefix :: ByteString
    prefix = Text.encodeUtf8 (actionPrefixText @controller)

    actionMatchMap :: HashMap.HashMap ByteString (Constr, [StdMethod])
    actionMatchMap =
        HashMap.fromList
            [ (actionPath, (constr, allowedMethods))
            | constr <- dataTypeConstrs (dataTypeOf (Prelude.undefined :: controller))
            , let actionName = ByteString.pack (showConstr constr)
                  actionPath = stripActionSuffixByteString actionName
                  allowedMethods = allowedMethodsForAction @controller actionName
            ]
{-# NOINLINE routeMatchParser #-}

{- | Returns the url prefix for a controller. The prefix is based on the
module where the controller is defined.

All controllers defined in the `Web/` directory don't have a prefix at all.

E.g. controllers in the `Admin/` directory are prefixed with @/admin/@.
-}
actionPrefixText :: forall (controller :: Type). (Typeable controller) => Text
actionPrefixText
    | "Web." `Text.isPrefixOf` moduleName = "/"
    | "IHP." `Text.isPrefixOf` moduleName = "/"
    | Text.null moduleName = "/"
    | otherwise = "/" <> Text.toLower (getPrefix moduleName) <> "/"
  where
    moduleName :: Text
    moduleName =
        Text.pack $
            Typeable.typeOf (error "unreachable" :: controller)
                |> Typeable.typeRepTyCon
                |> Typeable.tyConModule

    getPrefix :: Text -> Text
    getPrefix t = fst (Text.breakOn "." t)
{-# NOINLINE actionPrefixText #-}

{- | Strips the "Action" suffix from action names

>>> stripActionSuffixByteString "ShowUserAction"
"ShowUser"

>>> stripActionSuffixByteString "UsersAction"
"UsersAction"

>>> stripActionSuffixByteString "User"
"User"
-}
stripActionSuffixByteString :: ByteString -> ByteString
stripActionSuffixByteString actionName = fromMaybe actionName (ByteString.stripSuffix "Action" actionName)
{-# INLINE stripActionSuffixByteString #-}

{- | Strips the "Action" suffix from action names

>>> stripActionSuffixText "ShowUserAction"
"ShowUser"

>>> stripActionSuffixText "UsersAction"
"UsersAction"

>>> stripActionSuffixText "User"
"User"
-}
stripActionSuffixText :: Text -> Text
stripActionSuffixText actionName = fromMaybe actionName (Text.stripSuffix "Action" actionName)
{-# INLINE stripActionSuffixText #-}

{- | Returns the create action for a given controller.
Example: `createAction @UsersController == Just CreateUserAction`
-}
createAction :: forall controller. (AutoRoute controller) => Maybe controller
createAction = fmap fromConstr createConstructor
  where
    createConstructor :: Maybe Constr
    createConstructor = find isCreateConstructor allConstructors

    allConstructors :: [Constr]
    allConstructors = dataTypeConstrs (dataTypeOf (Prelude.undefined :: controller))

    isCreateConstructor :: Constr -> Bool
    isCreateConstructor constructor = "Create" `isPrefixOf` showConstr constructor && Prelude.null (constrFields constructor)
{-# INLINE createAction #-}

{- | Returns the update action when given a controller and id.
Example: `updateAction @UsersController == Just (\id -> UpdateUserAction id)`
-}
updateAction :: forall controller id. (AutoRoute controller) => Maybe (id -> controller)
updateAction =
    case updateConstructor of
        Just constructor -> Just $ \id -> buildInstance constructor id
        Nothing -> Nothing
  where
    updateConstructor :: Maybe Constr
    updateConstructor = find isUpdateConstructor allConstructors

    buildInstance :: Constr -> id -> controller
    buildInstance constructor id =
        State.evalState
            ( ( fromConstrM
                    ( do
                        i <- State.get

                        State.modify (+ 1)
                        pure (unsafeCoerce id)
                    )
              )
                constructor
            )
            0

    allConstructors :: [Constr]
    allConstructors = dataTypeConstrs (dataTypeOf (Prelude.undefined :: controller))

    isUpdateConstructor :: Constr -> Bool
    isUpdateConstructor constructor = "Update" `isPrefixOf` (showConstr constructor) && (length (constrFields constructor) == 1)
{-# INLINE updateAction #-}

instance {-# OVERLAPPABLE #-} (AutoRoute controller, Controller controller) => CanRoute controller where
    parseRoute' = customRoutes <|> autoRoute
    {-# INLINEABLE parseRoute' #-}

    -- \| This is only used as a fallback parser (via lazy thunk in 'ControllerRouteMap').
    -- The primary routing path goes through 'findInRouteMaps' / 'buildAutoRouteMap' instead.
    -- Performance here doesn't matter — keep it simple.
    parseRouteWithAction toApp =
        ( do
            action <- customRoutes @controller
            pure (toApp action)
        )
            <|> ( do
                    (constr, allowedMethods) <- routeMatchParser @controller
                    pure $ \waiRequest waiRespond -> wrapRouterException do
                        case applyAction @controller constr (queryString waiRequest) of
                            Left e -> Exception.throw e
                            Right action -> do
                                case parseMethod (requestMethod waiRequest) of
                                    Right method -> do
                                        unless
                                            (allowedMethods |> includes method)
                                            (Exception.throw UnexpectedMethodException{allowedMethods, method})
                                        toApp action waiRequest waiRespond
                                    Left err -> error ("Invalid HTTP method: " <> ByteString.unpack err)
                )
    {-# INLINEABLE parseRouteWithAction #-}

    -- \| Override to use 'ControllerRouteMap' for O(1) HashMap dispatch.
    toControllerRoute ::
        forall application.
        ( ?request :: Request
        , ?respond :: Respond
        , Controller controller
        , InitControllerContext application
        , ?application :: application
        , Typeable application
        , Typeable controller
        ) =>
        ControllerRoute application
    toControllerRoute =
        ControllerRouteMap
            { routeMap = buildAutoRouteMap @controller @application
            , routeParser = parseRouteWithAction @controller (runAction' @application)
            , routeInspection = RouteLeaf UndocumentedRoute
            }
    {-# INLINEABLE toControllerRoute #-}

{- | Instances of the @QueryParam@ type class can be represented in URLs as query parameters.
Currently this is only Int, Text, and both wrapped in List and Maybe.
IDs also are representable in a URL, but we are unable to match on polymorphic types using reflection,
so we fall back to the default "show" for these.
-}
class (Data a) => QueryParam a where
    showQueryParam :: a -> String

instance QueryParam Text where
    showQueryParam text = Text.unpack text

instance QueryParam Int where
    showQueryParam = show

instance QueryParam Integer where
    showQueryParam = show

instance QueryParam UUID where
    showQueryParam = show

instance (QueryParam a) => QueryParam (Maybe a) where
    showQueryParam (Just val) = showQueryParam val
    showQueryParam Nothing = ""

instance (QueryParam a) => QueryParam [a] where
    showQueryParam = List.intercalate "," . map showQueryParam

instance {-# OVERLAPPABLE #-} (Show controller, AutoRoute controller) => HasPath controller where
    pathTo !action = case customPathTo action of
        Just path -> path
        Nothing ->
            let !ci = constrIndex (toConstr action) - 1
                (!basePath, !fieldNames) = constrInfoCache !! ci
             in case fieldNames of
                    [] -> basePath
                    _ ->
                        let !fieldValues = gmapQ renderFieldForUrl action
                         in basePath <> buildQueryText fieldNames fieldValues
      where
        -- Precomputed per constructor: (basePath, fieldNames).
        -- Does not reference 'action', so GHC floats this out as a CAF
        -- when the instance is specialized for a concrete controller type.
        constrInfoCache :: [(Text, [Text])]
        constrInfoCache = map mkInfo allConstrs
          where
            allConstrs = dataTypeConstrs (dataTypeOf (Prelude.undefined :: controller))
            !appPrefix = actionPrefixText @controller
            mkInfo c =
                let !bp = appPrefix <> stripActionSuffixText (Text.pack (showConstr c))
                    !fns = map Text.pack (constrFields c)
                 in (bp, fns)

        buildQueryText :: [Text] -> [Text] -> Text
        buildQueryText names values =
            zip names values
                |> filter (\(_, v) -> not (Text.null v))
                |> map (\(k, v) -> k <> "=" <> URI.encodeText v)
                |> Text.intercalate "&"
                |> (\q -> if Text.null q then q else Text.cons '?' q)
    {-# NOINLINE pathTo #-}

{- | Render a controller field value as 'Text' for URL query parameter inclusion.

Uses type reflection ('eqT') to match known types (Text, UUID, Int, Integer,
and their Maybe\/List variants). For newtypes like @Id'@ that wrap a known type,
falls back to recursive unwrap via 'gmapQ'.
-}
renderFieldForUrl :: forall d. (Data d) => d -> Text
renderFieldForUrl val
    -- UUID first: most common type after Id newtype unwrap
    | Just Refl <- eqT @d @UUID = toText val
    | Just Refl <- eqT @d @Text = val
    | Just Refl <- eqT @d @Int = Text.pack (show val)
    | Just Refl <- eqT @d @Integer = Text.pack (show val)
    | Just Refl <- eqT @d @(Maybe UUID) = maybe "" toText val
    | Just Refl <- eqT @d @(Maybe Text) = maybe "" id val
    | Just Refl <- eqT @d @(Maybe Int) = maybe "" (Text.pack . show) val
    | Just Refl <- eqT @d @(Maybe Integer) = maybe "" (Text.pack . show) val
    | Just Refl <- eqT @d @[UUID] = Text.intercalate "," (map toText val)
    | Just Refl <- eqT @d @[Text] = Text.intercalate "," (val :: [Text])
    | Just Refl <- eqT @d @[Int] = Text.intercalate "," (map (Text.pack . show) val)
    | Just Refl <- eqT @d @[Integer] = Text.intercalate "," (map (Text.pack . show) val)
    | otherwise =
        -- Unwrap one layer for newtypes (e.g., Id' wrapping UUID, Maybe (Id' table))
        case gmapQ renderFieldForUrl val of
            [inner] -> inner
            _ -> ""
{-# NOINLINE renderFieldForUrl #-}

-- | Parses the HTTP Method from the request and returns it.
getMethod :: (?request :: Request, ?respond :: Respond) => Parser StdMethod
getMethod =
    case parseMethod ?request.requestMethod of
        Left error -> fail (ByteString.unpack error)
        Right method -> pure method
{-# INLINEABLE getMethod #-}

{- | Routes a given path to an action when requested via GET.

__Example:__

> instance FrontController WebApplication where
>     controllers = [
>             get "/my-custom-page" NewSessionAction
>         ]

The request @GET \/my-custom-page@ is now executing NewSessionAction

Also see 'post'.
-}
get ::
    ( Controller action
    , InitControllerContext application
    , ?application :: application
    , Typeable application
    , Typeable action
    ) =>
    ByteString -> action -> ControllerRoute application
get path action = rawRoute do
    string path
    pure $ \waiRequest waiRespond ->
        case parseMethod (requestMethod waiRequest) of
            Right GET -> runAction' action waiRequest waiRespond
            Right HEAD -> runAction' action waiRequest waiRespond
            Right method -> Exception.throw UnexpectedMethodException{allowedMethods = [GET, HEAD], method}
            Left err -> error ("Invalid HTTP method: " <> ByteString.unpack err)
{-# INLINEABLE get #-}

{- | Routes a given path to an action when requested via POST.

__Example:__

> instance FrontController WebApplication where
>     controllers = [
>             post "/do-something" DoSomethingAction
>         ]

The request @POST \/do-something@ is now executing DoSomethingAction

Also see 'get'.
-}
post ::
    ( Controller action
    , InitControllerContext application
    , ?application :: application
    , Typeable application
    , Typeable action
    ) =>
    ByteString -> action -> ControllerRoute application
post path action = rawRoute do
    string path
    pure $ \waiRequest waiRespond ->
        case parseMethod (requestMethod waiRequest) of
            Right POST -> runAction' action waiRequest waiRespond
            Right method -> Exception.throw UnexpectedMethodException{allowedMethods = [POST], method}
            Left err -> error ("Invalid HTTP method: " <> ByteString.unpack err)
{-# INLINEABLE post #-}

{- | Filter methods when writing a custom routing parser

__Example:__

> instance CanRoute ApiController where
>    parseRoute' = do
>        string "/api/"
>        let
>            createRecordAction = do
>                onlyAllowMethods [POST]
>
>                table <- parseText
>                endOfInput
>                pure CreateRecordAction { table }
>
>            updateRecordAction = do
>                onlyAllowMethods [PATCH]
>
>                table <- parseText
>                string "/"
>                id <- parseUUID
>                pure UpdateRecordAction { table, id }
>
> createRecordAction <|> updateRecordAction
-}
onlyAllowMethods :: (?request :: Request, ?respond :: Respond) => [StdMethod] -> Parser ()
onlyAllowMethods methods = do
    method <- getMethod
    unless (method `elem` methods) (fail ("Invalid method, expected one of: " <> show methods))
{-# INLINEABLE onlyAllowMethods #-}

{- | Routes to a given WebSocket app if the path matches the WebSocket app name

__Example:__

> instance FrontController WebApplication where
>     controllers = [
>             webSocketApp @AutoRefreshWSApp
>         ]

The request @\/AutoRefreshWSApp@ will call the AutoRefreshWSApp
-}
webSocketApp ::
    forall webSocketApp application.
    ( WSApp webSocketApp
    , InitControllerContext application
    , ?application :: application
    , Typeable application
    , Typeable webSocketApp
    ) =>
    ControllerRoute application
webSocketApp = webSocketAppWithCustomPath @webSocketApp typeName
  where
    typeName :: ByteString
    typeName =
        Typeable.typeOf (error "unreachable" :: webSocketApp)
            |> show
            |> ByteString.pack
{-# INLINEABLE webSocketApp #-}

webSocketAppWithHTTPFallback ::
    forall webSocketApp application.
    ( WSApp webSocketApp
    , InitControllerContext application
    , ?application :: application
    , Typeable application
    , Typeable webSocketApp
    , Controller webSocketApp
    ) =>
    ControllerRoute application
webSocketAppWithHTTPFallback = webSocketAppWithCustomPathAndHTTPFallback @webSocketApp @application typeName
  where
    typeName :: ByteString
    typeName =
        Typeable.typeOf (error "unreachable" :: webSocketApp)
            |> show
            |> ByteString.pack
{-# INLINEABLE webSocketAppWithHTTPFallback #-}

{- | Routes to a given WebSocket app if the path matches

__Example:__

> instance FrontController WebApplication where
>     controllers = [
>             webSocketAppWithCustomPath @AutoRefreshWSApp "my-ws-app"
>         ]

The request @\/my-ws-app@ will call the AutoRefreshWSApp
-}
webSocketAppWithCustomPath ::
    forall webSocketApp application.
    ( WSApp webSocketApp
    , InitControllerContext application
    , ?application :: application
    , Typeable application
    , Typeable webSocketApp
    ) =>
    ByteString -> ControllerRoute application
webSocketAppWithCustomPath path = rawRoute do
    Attoparsec.char '/'
    string path
    pure $ withImplicits (startWebSocketAppAndFailOnHTTP @webSocketApp @application (WS.initialState @webSocketApp))
{-# INLINEABLE webSocketAppWithCustomPath #-}

webSocketAppWithCustomPathAndHTTPFallback ::
    forall webSocketApp application.
    ( WSApp webSocketApp
    , InitControllerContext application
    , ?application :: application
    , Typeable application
    , Typeable webSocketApp
    , Controller webSocketApp
    ) =>
    ByteString -> ControllerRoute application
webSocketAppWithCustomPathAndHTTPFallback path = rawRoute do
    Attoparsec.char '/'
    string path
    let action = WS.initialState @webSocketApp
    pure $ withImplicits (startWebSocketApp @webSocketApp @application action (runActionWithNewContext action))
{-# INLINEABLE webSocketAppWithCustomPathAndHTTPFallback #-}

-- | Defines the start page for a router (when @\/@ is requested).
startPage :: forall action application. (Controller action, InitControllerContext application, ?application :: application, Typeable application, Typeable action) => action -> ControllerRoute application
startPage action = get (Text.encodeUtf8 (actionPrefixText @action)) action
{-# INLINEABLE startPage #-}

withPrefix :: ByteString -> [ControllerRoute application] -> ControllerRoute application
withPrefix prefix routes =
    ControllerRouteParser
        { routeParser = string prefix >> choice (map (\route -> compileControllerRoute route <* endOfInput) routes)
        , routeInspection = RoutePrefix prefix (map routeInspection routes)
        }
{-# INLINEABLE withPrefix #-}

frontControllerToWAIApp :: forall app (autoRefreshApp :: Type). (FrontController app, WSApp autoRefreshApp, Typeable autoRefreshApp, InitControllerContext ()) => Middleware -> app -> Application -> Application
frontControllerToWAIApp middleware application notFoundAction waiRequest waiRespond = do
    let ?request = waiRequest
    let ?respond = waiRespond

    let autoRefreshWSParser :: Parser Application
        autoRefreshWSParser =
            let ?application = ()
             in let typeName =
                        Typeable.typeOf (error "unreachable" :: autoRefreshApp)
                            |> show
                            |> ByteString.pack
                 in do
                        Attoparsec.char '/'
                        string typeName
                        pure $ withImplicits (startWebSocketAppAndFailOnHTTP @autoRefreshApp @() (WS.initialState @autoRefreshApp))

    let allRoutes =
            let ?application = application
             in ControllerRouteParser{routeParser = autoRefreshWSParser, routeInspection = RouteLeaf UndocumentedRoute} : controllers @app

    let path = waiRequest.rawPathInfo

    -- Fast path: scan auto-route HashMaps directly (no Attoparsec overhead)
    case findInRouteMaps path allRoutes of
        Just handler -> (middleware (handler application)) waiRequest waiRespond
        Nothing -> do
            -- Slow path: Attoparsec for custom/dynamic route parsers only
            -- Wrap any exceptions during routing in RouterException so the error handler
            -- middleware can distinguish them from action exceptions
            let customParsers = concatMap getRouteParsers allRoutes

            routedAction :: Either String Application <-
                ( do
                    res <- evaluate $ parseOnly (choice (map (<* endOfInput) customParsers)) path
                    case res of
                        Left s -> pure $ Left s
                        Right action -> pure $ Right action
                )
                    |> wrapRouterException
            case routedAction of
                Left _ -> notFoundAction waiRequest waiRespond
                Right action -> (middleware action) waiRequest waiRespond
{-# INLINEABLE frontControllerToWAIApp #-}

mountFrontController :: forall frontController application. (?request :: Request, ?respond :: Respond, FrontController frontController) => frontController -> ControllerRoute application
mountFrontController application =
    let ?application = application
     in ControllerRouteParser
            { routeParser = router []
            , routeInspection = RouteCollection (controllers @frontController |> map routeInspection)
            }
{-# INLINEABLE mountFrontController #-}

buildDocumentedRoute ::
    forall controller application.
    ( ?request :: Request
    , ?respond :: Respond
    , Controller controller
    , AutoRoute controller
    , Data controller
    , InitControllerContext application
    , ?application :: application
    , Typeable application
    , Typeable controller
    ) =>
    RouteDocumentation -> ControllerRoute application
buildDocumentedRoute routeDocumentation =
    ControllerRouteMap
        { routeMap = buildAutoRouteMapWithDocumentation @controller @application routeDocumentation
        , routeParser = parseRouteWithAction @controller (runActionWithRouteDocumentation @application routeDocumentation)
        , routeInspection = RouteLeaf routeDocumentation
        }
{-# INLINEABLE buildDocumentedRoute #-}

parseRoute ::
    forall controller application.
    ( ?request :: Request
    , ?respond :: Respond
    , CanRoute controller
    , Controller controller
    , InitControllerContext application
    , ?application :: application
    , Typeable application
    , Typeable controller
    ) =>
    ControllerRoute application
parseRoute = toControllerRoute @controller @application
{-# INLINEABLE parseRoute #-}

{- | Build a HashMap from full paths (prefix + action name) to Application closures.
The Application closures take the application value explicitly and handle query string
parsing, method validation, and controller execution.
Computed once per (controller, application) type pair (NOINLINE CAF).
-}
buildAutoRouteMap ::
    forall controller application.
    ( AutoRoute controller
    , Controller controller
    , InitControllerContext application
    , Typeable application
    , Typeable controller
    ) =>
    HashMap.HashMap ByteString (application -> Application)
buildAutoRouteMap =
    HashMap.fromList
        [ (prefix <> actionPath, handler)
        | constr <- dataTypeConstrs (dataTypeOf (Prelude.undefined :: controller))
        , let actionName = ByteString.pack (showConstr constr)
              actionPath = stripActionSuffixByteString actionName
              allowedMethods = allowedMethodsForAction @controller actionName
              handler app waiRequest waiRespond =
                let ?application = app
                 in wrapRouterException do
                        case parseMethod (requestMethod waiRequest) of
                            Left err -> error ("Invalid HTTP method: " <> ByteString.unpack err)
                            Right method -> do
                                unless
                                    (allowedMethods |> includes method)
                                    (Exception.throw UnexpectedMethodException{allowedMethods, method})
                                case applyAction @controller constr (queryString waiRequest) of
                                    Left e -> Exception.throw e
                                    Right action -> runAction' @application action waiRequest waiRespond
        ]
  where
    prefix :: ByteString
    prefix = Text.encodeUtf8 (actionPrefixText @controller)
{-# NOINLINE buildAutoRouteMap #-}

buildAutoRouteMapWithDocumentation ::
    forall controller application.
    ( AutoRoute controller
    , Controller controller
    , Data controller
    , InitControllerContext application
    , Typeable application
    , Typeable controller
    ) =>
    RouteDocumentation -> HashMap.HashMap ByteString (application -> Application)
buildAutoRouteMapWithDocumentation routeDocumentation =
    HashMap.fromList
        [ (prefix <> actionPath, handler)
        | constr <- dataTypeConstrs (dataTypeOf (Prelude.undefined :: controller))
        , let actionName = ByteString.pack (showConstr constr)
              actionPath = stripActionSuffixByteString actionName
              allowedMethods = allowedMethodsForAction @controller actionName
              handler app waiRequest waiRespond =
                let ?application = app
                 in case parseMethod (requestMethod waiRequest) of
                        Left err -> error ("Invalid HTTP method: " <> ByteString.unpack err)
                        Right method -> do
                            unless
                                (allowedMethods |> includes method)
                                (Exception.throw UnexpectedMethodException{allowedMethods, method})
                            case applyAction @controller constr (queryString waiRequest) of
                                Left e -> waiRespond $ responseLBS status400 [(hContentType, "text/plain")] (cs $ show e)
                                Right action -> runActionWithRouteDocumentation @application routeDocumentation action waiRequest waiRespond
        ]
  where
    prefix :: ByteString
    prefix = Text.encodeUtf8 (actionPrefixText @controller)
{-# NOINLINE buildAutoRouteMapWithDocumentation #-}

documentRoute ::
    forall controller application.
    ( ?request :: Request
    , ?respond :: Respond
    , Controller controller
    , AutoRoute controller
    , OpenApiController controller
    , Data controller
    , InitControllerContext application
    , ?application :: application
    , Typeable application
    , Typeable controller
    ) =>
    ControllerRoute application
documentRoute =
    buildDocumentedRoute @controller @application
        ( DocumentedRoute
            ( AutoRouteControllerInfo
                { documentedActions = Just (openApiActions @controller)
                }
            )
        )
{-# INLINEABLE documentRoute #-}

parseUUIDOrTextId :: ByteString -> Maybe Dynamic
parseUUIDOrTextId queryVal =
    queryVal
        |> fromASCIIBytes
        |> \case
            Just uuid -> uuid |> toDyn |> Just
            Nothing -> Nothing

parseRouteWithId ::
    forall controller application.
    ( ?request :: Request
    , ?respond :: Respond
    , CanRoute controller
    , Controller controller
    , InitControllerContext application
    , ?application :: application
    , Typeable application
    , Typeable controller
    ) =>
    ControllerRoute application
parseRouteWithId = parseRoute @controller @application

catchAll :: forall action application. (Controller action, InitControllerContext application, Typeable action, ?application :: application, Typeable application, Data action) => action -> ControllerRoute application
catchAll action = rawRoute do
    string (Text.encodeUtf8 (actionPrefixText @action))
    _ <- takeByteString
    pure (runAction' @application action)
{-# INLINE catchAll #-}

-- | This instances makes it possible to write @<a href={MyAction}/>@ in HSX
instance {-# OVERLAPPABLE #-} (HasPath action) => ConvertibleStrings action Html5.AttributeValue where
    convertString action = Html5.textValue (pathTo action)
    {-# INLINE convertString #-}

-- | Parses and returns an UUID
parseUUID :: Parser UUID
parseUUID = do
    uuid <- take 36
    case fromASCIIBytes uuid of
        Just theUUID -> pure theUUID
        Nothing -> fail "not uuid"
{-# INLINEABLE parseUUID #-}

-- | Parses an UUID, afterwards wraps it in an Id
parseId :: ((ModelSupport.PrimaryKey table) ~ UUID) => Parser (ModelSupport.Id' table)
parseId = ModelSupport.Id <$> parseUUID
{-# INLINEABLE parseId #-}

-- | Returns all the remaining text until the end of the input
remainingText :: Parser Text
remainingText = Text.decodeUtf8 <$> takeByteString
{-# INLINEABLE remainingText #-}

-- | Parses until the next @/@
parseText :: Parser Text
parseText = Text.decodeUtf8 <$> takeTill ('/' ==)
{-# INLINEABLE parseText #-}

parseIntegerId :: (Data idType) => ByteString -> Maybe idType
parseIntegerId queryVal =
    let
        rawValue :: Maybe Integer = readMaybe (cs queryVal :: String)
     in
        rawValue >>= Just . unsafeCoerce

{- | Parses and returns an integer
parseRational :: (Integral a) => Parser a
parseRational = Attoparsec.decimal
-}

{- | Parses a route query parameter

__Example:__

> let showPost = do
>     string "/post"
>     let postId = routeParam "postId"
>     pure ShowPostAction { .. }
Will parse the `postId` query in `/post?postId=09b545dd-9744-4ef8-87b8-8d227f4faa1e`
-}
routeParam :: (?request :: Request, ?respond :: Respond, ParamReader paramType) => ByteString -> paramType
routeParam paramName =
    let customFields = TypeMap.insert ?request TypeMap.empty
     in let ?context = FrozenControllerContext{customFields}
         in param paramName

{- | Display a better error when the user missed to pass an argument to an action.

E.g. when you forgot to pass a projectId to the ShowProjectAction:

> <a href={ShowProjectAction}>Show project</a>

The correct code would be this:

> <a href={ShowProjectAction projectId}>Show project</a>

See https://github.com/digitallyinduced/ihp/issues/840
-}
instance ((T.TypeError (T.Text "Looks like you forgot to pass a " :<>: (T.ShowType argument) :<>: T.Text " to this " :<>: (T.ShowType controller))), Data argument, Data controller, Data (argument -> controller)) => AutoRoute (argument -> controller)
