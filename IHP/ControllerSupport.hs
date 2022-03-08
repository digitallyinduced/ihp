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
, respondAndExit
, ResponseException (..)
, jumpToAction
, requestBodyJSON
, startWebSocketApp
, setHeader
, addResponseHeaders
, addResponseHeadersFromContext
, getAppConfig
) where

import ClassyPrelude
import IHP.HaskellSupport
import Network.Wai (Response, Request, ResponseReceived, responseLBS, requestHeaders)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai
import IHP.ModelSupport
import IHP.ApplicationContext (ApplicationContext (..))
import qualified IHP.ApplicationContext as ApplicationContext
import Network.Wai.Parse as WaiParse
import qualified Data.ByteString.Lazy
import qualified IHP.Controller.RequestContext as RequestContext
import IHP.Controller.RequestContext (RequestContext, Respond)
import qualified Data.CaseInsensitive
import qualified Control.Exception as Exception
import qualified IHP.ErrorController as ErrorController
import qualified Data.Typeable as Typeable
import IHP.FrameworkConfig (FrameworkConfig (..), ConfigProvider(..))
import qualified IHP.Controller.Context as Context
import IHP.Controller.Context (ControllerContext)
import Network.HTTP.Types.Header
import qualified Data.Aeson as Aeson
import qualified Network.Wai.Handler.WebSockets as WebSockets
import qualified Network.WebSockets as WebSockets
import qualified IHP.WebSocket as WebSockets
import qualified Data.Typeable as Typeable
import qualified Data.TMap as TypeMap

type Action' = IO ResponseReceived

class (Show controller, Eq controller) => Controller controller where
    beforeAction :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller) => IO ()
    beforeAction = pure ()
    {-# INLINABLE beforeAction #-}
    action :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller) => controller -> IO ()

class InitControllerContext application where
    initContext :: (?modelContext :: ModelContext, ?requestContext :: RequestContext, ?applicationContext :: ApplicationContext, ?context :: ControllerContext) => IO ()
    initContext = pure ()
    {-# INLINABLE initContext #-}

{-# INLINE runAction #-}
runAction :: forall controller. (Controller controller, ?context :: ControllerContext, ?modelContext :: ModelContext, ?applicationContext :: ApplicationContext, ?requestContext :: RequestContext) => controller -> IO ResponseReceived
runAction controller = do
    let ?theAction = controller
    let respond = ?context |> get #requestContext |> get #respond

    let doRunAction = do
            authenticatedModelContext <- prepareRLSIfNeeded ?modelContext

            let ?modelContext = authenticatedModelContext
            beforeAction
            (action controller)
            ErrorController.handleNoResponseReturned controller

    let handleResponseException  (ResponseException response) = respond response

    doRunAction `catches` [ Handler handleResponseException, Handler (\exception -> ErrorController.displayException exception controller "")]

{-# INLINE runActionWithNewContext #-}
runActionWithNewContext :: forall application controller. (Controller controller, ?applicationContext :: ApplicationContext, ?context :: RequestContext, InitControllerContext application, ?application :: application, Typeable application, Typeable controller) => controller -> IO ResponseReceived
runActionWithNewContext controller = do
    let ?modelContext = ApplicationContext.modelContext ?applicationContext
    let ?requestContext = ?context
    controllerContext <- Context.newControllerContext
    let ?context = controllerContext
    Context.putContext ?application
    Context.putContext (Context.ActionType (Typeable.typeOf controller))

    try (initContext @application) >>= \case
        Left exception -> do
            -- Calling `initContext` might fail, so we provide a bit better error messages here
            ErrorController.displayException exception controller " while calling initContext"
        Right context -> do
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
startWebSocketApp :: forall webSocketApp application. (?applicationContext :: ApplicationContext, ?context :: RequestContext, InitControllerContext application, ?application :: application, Typeable application, WebSockets.WSApp webSocketApp) => IO ResponseReceived
startWebSocketApp = do
    let ?modelContext = ApplicationContext.modelContext ?applicationContext
    let ?requestContext = ?context
    let respond = ?context |> get #respond
    let request = ?context |> get #request

    let handleConnection pendingConnection = do
            connection <- WebSockets.acceptRequest pendingConnection

            controllerContext <- Context.newControllerContext
            let ?context = controllerContext

            Context.putContext ?application

            try (initContext @application) >>= \case
                Left (exception :: SomeException) -> putStrLn $ "Unexpected exception in initContext, " <> tshow exception
                Right context -> do
                    WebSockets.startWSApp @webSocketApp connection

    request
        |> WebSockets.websocketsApp WebSockets.defaultConnectionOptions handleConnection
        |> \case
            Just response -> respond response
            Nothing -> respond $ responseLBS HTTP.status400 [(hContentType, "text/plain")] "This endpoint is only available via a WebSocket"


jumpToAction :: forall action. (Controller action, ?context :: ControllerContext, ?modelContext :: ModelContext) => action -> IO ()
jumpToAction theAction = do
    let ?theAction = theAction
    beforeAction @action
    action theAction

{-# INLINE getRequestBody #-}
getRequestBody :: (?context :: ControllerContext) => IO LByteString
getRequestBody =
    ?context
    |> get #requestContext
    |> get #requestBody
    |> \case
        RequestContext.JSONBody { rawPayload } -> pure rawPayload
        _ -> Network.Wai.lazyRequestBody request

-- | Returns the request path, e.g. @/Users@ or @/CreateUser@
getRequestPath :: (?context :: ControllerContext) => ByteString
getRequestPath = Network.Wai.rawPathInfo request
{-# INLINABLE getRequestPath #-}

-- | Returns the request path and the query params, e.g. @/ShowUser?userId=9bd6b37b-2e53-40a4-bb7b-fdba67d6af42@
getRequestPathAndQuery :: (?context :: ControllerContext) => ByteString
getRequestPathAndQuery = Network.Wai.rawPathInfo request <> Network.Wai.rawQueryString request
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
getHeader name = lookup (Data.CaseInsensitive.mk name) (Network.Wai.requestHeaders request)
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

-- | Add headers to current response
-- | Returns a Response with headers
--
-- > addResponseHeaders [("Content-Type", "text/html")] response
--
addResponseHeaders :: [Header] -> Response -> Response
addResponseHeaders headers = Network.Wai.mapResponseHeaders (\hs -> headers <> hs)
{-# INLINABLE addResponseHeaders #-}

-- | Add headers to current response, getting the headers from ControllerContext
-- | Returns a Response with headers
--
-- > addResponseHeadersFromContext response
-- You probabaly want `setHeader`
--
addResponseHeadersFromContext :: (?context :: ControllerContext) => Response -> IO Response
addResponseHeadersFromContext response = do
    maybeHeaders <- Context.maybeFromContext @[Header]
    let headers = fromMaybe [] maybeHeaders
    let responseWithHeaders = addResponseHeaders headers response
    pure responseWithHeaders
{-# INLINABLE addResponseHeadersFromContext #-}

-- | Returns the current HTTP request.
--
-- See https://hackage.haskell.org/package/wai-3.2.2.1/docs/Network-Wai.html#t:Request
request :: (?context :: ControllerContext) => Network.Wai.Request
request = requestContext |> get #request
{-# INLINE request #-}

{-# INLINE getFiles #-}
getFiles :: (?context :: ControllerContext) => [File Data.ByteString.Lazy.ByteString]
getFiles = requestContext
        |> get #requestBody
        |> \case
            RequestContext.FormBody { files } -> files
            _ -> []

requestContext :: (?context :: ControllerContext) => RequestContext
requestContext = get #requestContext ?context
{-# INLINE requestContext #-}

requestBodyJSON :: (?context :: ControllerContext) => Aeson.Value
requestBodyJSON =
    ?context
    |> get #requestContext
    |> get #requestBody
    |> \case
        RequestContext.JSONBody { jsonPayload = Just value } -> value
        _ -> error "Expected JSON body"

{-# INLINE createRequestContext #-}
createRequestContext :: ApplicationContext -> Request -> Respond -> IO RequestContext
createRequestContext ApplicationContext { session, frameworkConfig } request respond = do
    let contentType = lookup hContentType (requestHeaders request)
    requestBody <- case contentType of
        "application/json" -> do
            rawPayload <- Network.Wai.lazyRequestBody request
            let jsonPayload = Aeson.decode rawPayload
            pure RequestContext.JSONBody { jsonPayload, rawPayload }
        _ -> do
            (params, files) <- WaiParse.parseRequestBodyEx (frameworkConfig |> get #parseRequestBodyOptions) WaiParse.lbsBackEnd request
            pure RequestContext.FormBody { .. }

    pure RequestContext.RequestContext { request, respond, requestBody, vault = session, frameworkConfig }

-- Can be thrown from inside the action to abort the current action execution.
-- Does not indicates a runtime error. It's just used for control flow management.
newtype ResponseException = ResponseException Response

instance Show ResponseException where show _ = "ResponseException { .. }"

instance Exception ResponseException

respondAndExit :: (?context::ControllerContext) => Response -> IO ()
respondAndExit response = do
    responseWithHeaders <- addResponseHeadersFromContext response
    Exception.throwIO (ResponseException responseWithHeaders)
{-# INLINE respondAndExit #-}

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
getAppConfig = ?context
        |> getFrameworkConfig
        |> get #appConfig
        |> TypeMap.lookup @configParameter
        |> fromMaybe (error ("Could not find " <> (show (Typeable.typeRep (Typeable.Proxy @configParameter))) <>" in config"))