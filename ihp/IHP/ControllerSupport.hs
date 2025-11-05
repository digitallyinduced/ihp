{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeFamilies, ConstrainedClassMethods, ScopedTypeVariables, FunctionalDependencies, AllowAmbiguousTypes #-}

module IHP.ControllerSupport
( Action'
, (|>)
, getRequestBody
, getRequestPath
, getRequestPathAndQuery
, getHeader
, RequestContext (RequestContext)
, request
, requestHeaders
, getFiles
, Controller (..)
, runAction
, createRequestContext
, ControllerContext
, InitControllerContext (..)
, runActionWithNewContext
, newContextForAction
, respondAndExit
, jumpToAction
, requestBodyJSON
, startWebSocketApp
, startWebSocketAppAndFailOnHTTP
, setHeader
, getAppConfig
) where

import ClassyPrelude
import IHP.HaskellSupport
import Network.Wai (Request, ResponseReceived, responseLBS, requestHeaders)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai
import IHP.ModelSupport
import Network.Wai.Parse as WaiParse
import qualified Data.ByteString.Lazy
import qualified IHP.Controller.RequestContext as RequestContext
import IHP.Controller.RequestContext (RequestContext, Respond)
import qualified Data.CaseInsensitive
import qualified IHP.ErrorController as ErrorController
import qualified Data.Typeable as Typeable
import IHP.FrameworkConfig (FrameworkConfig (..), ConfigProvider(..))
import qualified IHP.Controller.Context as Context
import IHP.Controller.Context (ControllerContext(ControllerContext), customFieldsRef)
import IHP.Controller.Response
import Network.HTTP.Types.Header
import qualified Data.Aeson as Aeson
import qualified Network.Wai.Handler.WebSockets as WebSockets
import qualified Network.WebSockets as WebSockets
import qualified IHP.WebSocket as WebSockets
import qualified Data.TMap as TypeMap
import IHP.RequestVault

type Action' = IO ResponseReceived

class (Show controller, Eq controller) => Controller controller where
    beforeAction :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller) => IO ()
    beforeAction = pure ()
    {-# INLINABLE beforeAction #-}
    action :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller) => controller -> IO ()

class InitControllerContext application where
    initContext :: (?modelContext :: ModelContext, ?requestContext :: RequestContext, ?context :: ControllerContext) => IO ()
    initContext = pure ()
    {-# INLINABLE initContext #-}

instance InitControllerContext () where
    initContext = pure ()

{-# INLINE runAction #-}
runAction :: forall controller. (Controller controller, ?context :: ControllerContext, ?modelContext :: ModelContext, ?requestContext :: RequestContext) => controller -> IO ResponseReceived
runAction controller = do
    let ?theAction = controller
    let respond = ?context.requestContext.respond

    let doRunAction = do
            authenticatedModelContext <- prepareRLSIfNeeded ?modelContext

            let ?modelContext = authenticatedModelContext
            beforeAction
            (action controller)
            ErrorController.handleNoResponseReturned controller

    let handleResponseException  (ResponseException response) = respond response

    doRunAction `catches` [ Handler handleResponseException, Handler (\exception -> ErrorController.displayException exception controller "")]

{-# INLINE newContextForAction #-}
newContextForAction
    :: forall application controller
     . ( Controller controller
       , ?context :: RequestContext
       , InitControllerContext application
       , ?application :: application
       , Typeable application
       , Typeable controller
       )
    => controller -> IO (Either (IO ResponseReceived) ControllerContext)
newContextForAction controller = do
    let ?modelContext = ?context.request.modelContext
    let ?requestContext = ?context
    controllerContext <- Context.newControllerContext
    let ?context = controllerContext
    Context.putContext ?application
    Context.putContext (Context.ActionType (Typeable.typeOf controller))

    try (initContext @application) >>= \case
        Left (exception :: SomeException) -> do
            pure $ Left $ case fromException exception of
                Just (ResponseException response) ->
                    let respond = ?context.requestContext.respond
                    in respond response
                Nothing -> ErrorController.displayException exception controller " while calling initContext"
        Right _ -> pure $ Right ?context

{-# INLINE runActionWithNewContext #-}
runActionWithNewContext :: forall application controller. (Controller controller, ?context :: RequestContext, InitControllerContext application, ?application :: application, Typeable application, Typeable controller) => controller -> IO ResponseReceived
runActionWithNewContext controller = do
    contextOrResponse <- newContextForAction controller
    case contextOrResponse of
        Left response -> response
        Right context -> do
            let ?modelContext = requestModelContext ?context.request
            let ?requestContext = ?context
            let ?context = context
            runAction controller

-- | If 'IHP.LoginSupport.Helper.Controller.enableRowLevelSecurityIfLoggedIn' was called, this will copy the
-- the prepared RowLevelSecurityContext from the controller context into the ModelContext.
--
-- If row leve security wasn't enabled, this will just return the current model context.
prepareRLSIfNeeded :: (?context :: ControllerContext) => ModelContext -> IO ModelContext
prepareRLSIfNeeded modelContext = do
    rowLevelSecurityContext <- Context.maybeFromContext
    case rowLevelSecurityContext of
        Just context -> pure modelContext { rowLevelSecurity = Just context }
        Nothing -> pure modelContext

{-# INLINE startWebSocketApp #-}
startWebSocketApp :: forall webSocketApp application. (?context :: RequestContext, InitControllerContext application, ?application :: application, Typeable application, WebSockets.WSApp webSocketApp) => webSocketApp -> IO ResponseReceived -> Network.Wai.Application
startWebSocketApp initialState onHTTP request respond = do
    let ?modelContext = requestModelContext ?context.request
    requestContext <- createRequestContext request respond
    let ?requestContext = requestContext

    let handleConnection pendingConnection = do
            connection <- WebSockets.acceptRequest pendingConnection

            controllerContext <- Context.newControllerContext
            let ?context = controllerContext

            Context.putContext ?application

            try (initContext @application) >>= \case
                Left (exception :: SomeException) -> putStrLn $ "Unexpected exception in initContext, " <> tshow exception
                Right context -> do
                    WebSockets.startWSApp initialState connection

    let connectionOptions = WebSockets.connectionOptions @webSocketApp

    request
        |> WebSockets.websocketsApp connectionOptions handleConnection
        |> \case
            Just response -> respond response
            Nothing -> onHTTP
{-# INLINE startWebSocketAppAndFailOnHTTP #-}
startWebSocketAppAndFailOnHTTP :: forall webSocketApp application. (?context :: RequestContext, InitControllerContext application, ?application :: application, Typeable application, WebSockets.WSApp webSocketApp) => webSocketApp -> Network.Wai.Application
startWebSocketAppAndFailOnHTTP initialState = startWebSocketApp @webSocketApp @application initialState (respond $ responseLBS HTTP.status400 [(hContentType, "text/plain")] "This endpoint is only available via a WebSocket")
    where
        respond = ?context.respond


jumpToAction :: forall action. (Controller action, ?context :: ControllerContext, ?modelContext :: ModelContext) => action -> IO ()
jumpToAction theAction = do
    let ?theAction = theAction
    beforeAction @action
    action theAction

{-# INLINE getRequestBody #-}
getRequestBody :: (?context :: ControllerContext) => IO LByteString
getRequestBody =
    case ?context.requestContext.requestBody of
        RequestContext.JSONBody { rawPayload } -> pure rawPayload
        _ -> Network.Wai.lazyRequestBody request

-- | Returns the request path, e.g. @/Users@ or @/CreateUser@
getRequestPath :: (?context :: ControllerContext) => ByteString
getRequestPath = request.rawPathInfo
{-# INLINABLE getRequestPath #-}

-- | Returns the request path and the query params, e.g. @/ShowUser?userId=9bd6b37b-2e53-40a4-bb7b-fdba67d6af42@
getRequestPathAndQuery :: (?context :: ControllerContext) => ByteString
getRequestPathAndQuery = request.rawPathInfo <> request.rawQueryString
{-# INLINABLE getRequestPathAndQuery #-}

-- | Returns a header value for a given header name. Returns Nothing if not found
--
-- The header is looked up in a case insensitive way.
--
-- >>> getHeader "Content-Type"
-- Just "text/html"
--
-- >>> getHeader "X-My-Custom-Header"
-- Nothing
--
getHeader :: (?context :: ControllerContext) => ByteString -> Maybe ByteString
getHeader name = lookup (Data.CaseInsensitive.mk name) request.requestHeaders
{-# INLINABLE getHeader #-}

-- | Set a header value for a given header name.
--
-- >>> setHeader ("Content-Language", "en")
--
setHeader :: (?context :: ControllerContext) => Header -> IO ()
setHeader header = do
    maybeHeaders <- Context.maybeFromContext @[Header]
    let headers = fromMaybe [] maybeHeaders
    Context.putContext (header : headers)
{-# INLINABLE setHeader #-}

-- | Returns the current HTTP request.
--
-- See https://hackage.haskell.org/package/wai-3.2.2.1/docs/Network-Wai.html#t:Request
request :: (?context :: ControllerContext) => Network.Wai.Request
request = requestContext.request
{-# INLINE request #-}

{-# INLINE getFiles #-}
getFiles :: (?context :: ControllerContext) => [File Data.ByteString.Lazy.ByteString]
getFiles =
    case requestContext.requestBody of
        RequestContext.FormBody { files } -> files
        _ -> []

requestContext :: (?context :: ControllerContext) => RequestContext
requestContext = ?context.requestContext
{-# INLINE requestContext #-}

requestBodyJSON :: (?context :: ControllerContext) => Aeson.Value
requestBodyJSON =
    case ?context.requestContext.requestBody of
        RequestContext.JSONBody { jsonPayload = Just value } -> value
        _ -> error "Expected JSON body"

{-# INLINE createRequestContext #-}
createRequestContext :: Request -> Respond -> IO RequestContext
createRequestContext request respond = do
    let frameworkConfig = requestFrameworkConfig request
    let contentType = lookup hContentType (requestHeaders request)
    requestBody <- case contentType of
        "application/json" -> do
            rawPayload <- Network.Wai.lazyRequestBody request
            let jsonPayload = Aeson.decode rawPayload
            pure RequestContext.JSONBody { jsonPayload, rawPayload }
        _ -> do
            (params, files) <- WaiParse.parseRequestBodyEx frameworkConfig.parseRequestBodyOptions WaiParse.lbsBackEnd request
            pure RequestContext.FormBody { .. }

    pure RequestContext.RequestContext { request, respond, requestBody, frameworkConfig }


-- | Returns a custom config parameter
--
-- >>> getAppConfig @StripePublicKey
-- StripePublicKey "pk_test_..."
--
-- Example:
--
-- First you need to define a custom config parameter in Config.hs:
--
-- > -- Config/Config.hs
-- > newtype StripePublicKey = StripePublicKey Text
-- >
-- > config :: ConfigBuilder
-- > config = do
-- >     -- ...
-- >     stripePublicKey <- StripePublicKey <$> env @Text "STRIPE_PUBLIC_KEY"
-- >     option stripePublicKey
--
-- Then you can access it using 'getAppConfig':
--
-- > action MyAction = do
-- >     let (StripePublicKey stripePublicKey) = getAppConfig @StripePublicKey
-- >
-- >     putStrLn ("Stripe public key: " <> stripePublicKey)
--
getAppConfig :: forall configParameter context. (?context :: context, ConfigProvider context, Typeable configParameter) => configParameter
getAppConfig = ?context.frameworkConfig.appConfig
        |> TypeMap.lookup @configParameter
        |> fromMaybe (error ("Could not find " <> (show (Typeable.typeRep (Typeable.Proxy @configParameter))) <>" in config"))
{-# INLINE getAppConfig #-}
