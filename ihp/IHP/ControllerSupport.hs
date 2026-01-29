{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeFamilies, ConstrainedClassMethods, ScopedTypeVariables, FunctionalDependencies, AllowAmbiguousTypes #-}

module IHP.ControllerSupport
( Action'
, (|>)
, getRequestBody
, getRequestPath
, getRequestPathAndQuery
, getHeader
, request
, requestHeaders
, getFiles
, Controller (..)
, runAction
, ControllerContext
, InitControllerContext (..)
, runActionWithNewContext
, newContextForAction
, respondAndExit
, respondAndExitWithHeaders
, jumpToAction
, requestBodyJSON
, startWebSocketApp
, startWebSocketAppAndFailOnHTTP
, setHeader
, getAppConfig
, Respond
) where

import Prelude
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Control.Exception.Safe (SomeException, fromException, try, catches, Handler(..))
import Data.Typeable (Typeable)
import qualified Data.Text as Text
import IHP.HaskellSupport
import Network.Wai (Request, ResponseReceived, responseLBS, requestHeaders)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai
import IHP.ModelSupport
import Network.Wai.Parse as WaiParse
import qualified Data.ByteString.Lazy
import Wai.Request.Params.Middleware (Respond, RequestBody (..))
import qualified Data.CaseInsensitive
import qualified IHP.ErrorController as ErrorController
import qualified Data.Typeable as Typeable
import IHP.FrameworkConfig.Types (FrameworkConfig (..), ConfigProvider)
import qualified IHP.Controller.Context as Context
import IHP.Controller.Context (ControllerContext(ControllerContext), customFieldsRef)
import IHP.Controller.Response
import Network.HTTP.Types.Header
import qualified Data.Aeson as Aeson
import qualified Network.Wai.Handler.WebSockets as WebSockets
import qualified Network.WebSockets as WebSockets
import qualified IHP.WebSocket as WebSockets
import qualified Data.TMap as TypeMap
import IHP.RequestVault.ModelContext
import IHP.ActionType (setActionType)

type Action' = IO ResponseReceived

class (Show controller, Eq controller) => Controller controller where
    beforeAction :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller, ?respond :: Respond, ?request :: Network.Wai.Request) => IO ()
    beforeAction = pure ()
    {-# INLINABLE beforeAction #-}
    action :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller, ?respond :: Respond, ?request :: Network.Wai.Request) => controller -> IO ()

class InitControllerContext application where
    initContext :: (?modelContext :: ModelContext, ?request :: Request, ?respond :: Respond, ?context :: ControllerContext) => IO ()
    initContext = pure ()
    {-# INLINABLE initContext #-}

instance InitControllerContext () where
    initContext = pure ()

{-# INLINE runAction #-}
runAction :: forall controller. (Controller controller, ?context :: ControllerContext, ?modelContext :: ModelContext, ?respond :: Respond) => controller -> IO ResponseReceived
runAction controller = do
    let ?theAction = controller
    let ?request = ?context.request

    let doRunAction = do
            authenticatedModelContext <- prepareRLSIfNeeded ?modelContext

            let ?modelContext = authenticatedModelContext
            beforeAction
            (action controller)
            ErrorController.handleNoResponseReturned controller

    let handleResponseException (ResponseException response) = ?respond response

    doRunAction `catches` [ Handler handleResponseException, Handler (\exception -> ErrorController.displayException exception controller "")]

{-# INLINE newContextForAction #-}
newContextForAction
    :: forall application controller
     . ( Controller controller
       , ?request :: Request
       , ?respond :: Respond
       , InitControllerContext application
       , ?application :: application
       , Typeable application
       , Typeable controller
       )
    => controller -> IO (Either (IO ResponseReceived) ControllerContext)
newContextForAction controller = do
    let ?modelContext = ?request.modelContext
    controllerContext <- Context.newControllerContext
    let ?context = controllerContext
    Context.putContext ?application

    try (initContext @application) >>= \case
        Left (exception :: SomeException) -> do
            pure $ Left $ case fromException exception of
                Just (ResponseException response) -> ?respond response
                Nothing -> ErrorController.displayException exception controller " while calling initContext"
        Right _ -> pure $ Right ?context

{-# INLINE runActionWithNewContext #-}
runActionWithNewContext :: forall application controller. (Controller controller, ?request :: Request, ?respond :: Respond, InitControllerContext application, ?application :: application, Typeable application, Typeable controller) => controller -> IO ResponseReceived
runActionWithNewContext controller = do
    let request' = setActionType controller ?request
    let ?request = request'
    contextOrResponse <- newContextForAction controller
    case contextOrResponse of
        Left response -> response
        Right context -> do
            let ?modelContext = requestModelContext ?request
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
startWebSocketApp :: forall webSocketApp application. (?request :: Request, ?respond :: Respond, InitControllerContext application, ?application :: application, Typeable application, WebSockets.WSApp webSocketApp) => webSocketApp -> IO ResponseReceived -> Network.Wai.Application
startWebSocketApp initialState onHTTP waiRequest waiRespond = do
    let ?modelContext = requestModelContext ?request
    let ?request = waiRequest
    let ?respond = waiRespond

    let handleConnection pendingConnection = do
            connection <- WebSockets.acceptRequest pendingConnection

            controllerContext <- Context.newControllerContext
            let ?context = controllerContext

            Context.putContext ?application

            try (initContext @application) >>= \case
                Left (exception :: SomeException) -> putStrLn $ "Unexpected exception in initContext, " <> show exception
                Right context -> do
                    WebSockets.startWSApp initialState connection

    let connectionOptions = WebSockets.connectionOptions @webSocketApp

    waiRequest
        |> WebSockets.websocketsApp connectionOptions handleConnection
        |> \case
            Just response -> waiRespond response
            Nothing -> onHTTP
{-# INLINE startWebSocketAppAndFailOnHTTP #-}
startWebSocketAppAndFailOnHTTP :: forall webSocketApp application. (?request :: Request, ?respond :: Respond, InitControllerContext application, ?application :: application, Typeable application, WebSockets.WSApp webSocketApp) => webSocketApp -> Network.Wai.Application
startWebSocketAppAndFailOnHTTP initialState = startWebSocketApp @webSocketApp @application initialState (?respond $ responseLBS HTTP.status400 [(hContentType, "text/plain")] "This endpoint is only available via a WebSocket")


jumpToAction :: forall action. (Controller action, ?context :: ControllerContext, ?modelContext :: ModelContext, ?respond :: Respond, ?request :: Network.Wai.Request) => action -> IO ()
jumpToAction theAction = do
    let ?theAction = theAction
    beforeAction @action
    action theAction

{-# INLINE getRequestBody #-}
getRequestBody :: (?request :: Network.Wai.Request) => IO LBS.ByteString
getRequestBody =
    case ?request.parsedBody of
        JSONBody { rawPayload } -> pure rawPayload
        _ -> Network.Wai.lazyRequestBody ?request

-- | Returns the request path, e.g. @/Users@ or @/CreateUser@
getRequestPath :: (?request :: Network.Wai.Request) => ByteString
getRequestPath = ?request.rawPathInfo
{-# INLINABLE getRequestPath #-}

-- | Returns the request path and the query params, e.g. @/ShowUser?userId=9bd6b37b-2e53-40a4-bb7b-fdba67d6af42@
getRequestPathAndQuery :: (?request :: Network.Wai.Request) => ByteString
getRequestPathAndQuery = ?request.rawPathInfo <> ?request.rawQueryString
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
getHeader :: (?request :: Network.Wai.Request) => ByteString -> Maybe ByteString
getHeader name = lookup (Data.CaseInsensitive.mk name) ?request.requestHeaders
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
request :: (?request :: Network.Wai.Request) => Network.Wai.Request
request = ?request
{-# INLINE request #-}

{-# INLINE getFiles #-}
getFiles :: (?request :: Network.Wai.Request) => [File Data.ByteString.Lazy.ByteString]
getFiles =
    case ?request.parsedBody of
        FormBody { files } -> files
        _ -> []

requestBodyJSON :: (?request :: Network.Wai.Request) => Aeson.Value
requestBodyJSON =
    case ?request.parsedBody of
        JSONBody { jsonPayload = Just value } -> value
        _ -> error "Expected JSON body"

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
