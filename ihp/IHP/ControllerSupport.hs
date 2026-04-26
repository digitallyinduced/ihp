{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeFamilies, ConstrainedClassMethods, ScopedTypeVariables, FunctionalDependencies, AllowAmbiguousTypes, RankNTypes, DefaultSignatures, MultiParamTypeClasses, TypeApplications #-}

module IHP.ControllerSupport
( Action'
, ControllerAction'
, RunControllerAction (..)
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
, Context.ControllerContext
, InitControllerContext (..)
, runActionWithNewContext
, newContextForAction
, respondWith
, respondAndExit
, earlyReturn
, jumpToAction
, requestBodyJSON
, startWebSocketApp
, startWebSocketAppAndFailOnHTTP
, setHeader
, getAppConfig
, Respond
, Request
, rlsContextVaultKey
, setupActionContext
, ResponseReceived
) where

import Prelude
import Data.IORef (IORef, modifyIORef', readIORef)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Control.Exception.Safe (SomeException, fromException, try, throwIO)
import qualified Control.Exception as Exception
import qualified IHP.ErrorController as ErrorController
import Data.Typeable (Typeable)
import IHP.HaskellSupport
import Network.Wai
import qualified Network.HTTP.Types as HTTP
import IHP.ModelSupport
import Network.Wai.Parse as WaiParse
import qualified Data.ByteString.Lazy
import Wai.Request.Params.Middleware (Respond)
import qualified Data.CaseInsensitive
import qualified Data.Typeable as Typeable
import IHP.FrameworkConfig.Types (FrameworkConfig (..), ConfigProvider)
import qualified IHP.Controller.Context as Context
import IHP.Controller.Response
import Network.Wai.Middleware.EarlyReturn (earlyReturnMiddleware)
import Network.HTTP.Types.Header
import qualified Data.Aeson as Aeson
import qualified Network.Wai.Handler.WebSockets as WebSockets
import qualified Network.WebSockets as WebSockets
import qualified Network.Wai.Internal as WaiInternal
import qualified IHP.WebSocket as WebSockets
import qualified Data.TMap as TypeMap
import IHP.RequestVault.ModelContext
import IHP.ActionType (setActionType, actionTypeVaultKey, ActionType(..))
import IHP.RequestVault.Helper (lookupRequestVault)
import qualified IHP.Environment as Environment
import qualified Data.Vault.Lazy as Vault
import qualified Data.Text as Text
import System.IO.Unsafe (unsafePerformIO)

type Action' = IO ResponseReceived

type ControllerAction' controller =
    ( ?context :: Context.ControllerContext
    , ?modelContext :: ModelContext
    , ?theAction :: controller
    , ?respond :: Respond
    , ?request :: Request
    ) =>
    IO ResponseReceived

-- | Runs a controller's 'ControllerAction' value.
--
-- This lets action representations such as 'IHP.Controller.ActionDefinition.ActionDefinition'
-- plug into the normal controller dispatch without requiring each controller instance to
-- repeat the same 'runControllerAction' implementation.
class RunControllerAction controller action where
    runControllerActionDefault :: (?context :: Context.ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller, ?respond :: Respond, ?request :: Request) => action -> IO ResponseReceived

instance RunControllerAction controller (IO ResponseReceived) where
    runControllerActionDefault controllerAction = controllerAction
    {-# INLINABLE runControllerActionDefault #-}

class (Show controller, Eq controller) => Controller controller where
    type ControllerAction controller :: Type
    type ControllerAction controller = IO ResponseReceived

    beforeAction :: (?context :: Context.ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller, ?respond :: Respond, ?request :: Request) => IO ()
    beforeAction = pure ()
    {-# INLINABLE beforeAction #-}

    action :: (?context :: Context.ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller, ?respond :: Respond, ?request :: Request) => controller -> ControllerAction controller

    runControllerAction :: (?context :: Context.ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller, ?respond :: Respond, ?request :: Request) => ControllerAction controller -> IO ResponseReceived
    default runControllerAction :: (RunControllerAction controller (ControllerAction controller), ?context :: Context.ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller, ?respond :: Respond, ?request :: Request) => ControllerAction controller -> IO ResponseReceived
    runControllerAction = runControllerActionDefault @controller
    {-# INLINABLE runControllerAction #-}

class InitControllerContext application where
    initContext :: (?modelContext :: ModelContext, ?request :: Request, ?respond :: Respond, ?context :: Context.ControllerContext) => IO ()
    initContext = pure ()
    {-# INLINABLE initContext #-}

instance InitControllerContext () where
    initContext = pure ()

{-# INLINE runAction #-}
runAction :: forall controller. (Controller controller, ?context :: Context.ControllerContext, ?modelContext :: ModelContext, ?respond :: Respond) => controller -> IO ResponseReceived
runAction controller = do
    let ?theAction = controller
    let ?request = ?context.request

    -- Exceptions are now caught by the error handler middleware
    authenticatedModelContext <- prepareRLSIfNeeded ?modelContext

    let ?modelContext = authenticatedModelContext
    beforeAction
    runControllerAction @controller (action controller)

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
    => controller -> IO Context.ControllerContext
newContextForAction controller = do
    let ?modelContext = ?request.modelContext
    controllerContext <- Context.newControllerContext
    let ?context = controllerContext
    wrapInitContextException (initContext @application)
    pure ?context

-- | Shared request context setup, specialized once per application type.
-- Takes a pre-computed TypeRep to avoid per-controller-type code duplication.
-- NOINLINE ensures GHC compiles one copy shared across all controllers.
--
-- Exceptions from 'initContext' (including 'EarlyReturnException') propagate
-- to the caller, which is expected to catch them.
{-# NOINLINE setupActionContext #-}
setupActionContext
    :: forall application
     . ( InitControllerContext application
       , ?application :: application
       , Typeable application
       )
    => Typeable.TypeRep -> Request -> Respond
    -> IO Context.ControllerContext
setupActionContext controllerTypeRep waiRequest waiRespond = do
    let !request' = waiRequest { vault = Vault.insert actionTypeVaultKey (ActionType controllerTypeRep) waiRequest.vault }
    let ?request = request'
    let ?respond = waiRespond
    let ?modelContext = request'.modelContext
    controllerContext <- Context.newControllerContext
    let ?context = controllerContext
    wrapInitContextException (initContext @application)
    pure ?context

-- | Wraps non-EarlyReturn exceptions from initContext in InitContextException
-- so the error handler middleware can show "while calling initContext".
wrapInitContextException :: IO () -> IO ()
wrapInitContextException action =
    action `Exception.catch` \(e :: SomeException) ->
        case fromException e of
            Just (EarlyReturnException _) -> throwIO e  -- pass through early returns
            Nothing -> throwIO (ErrorController.InitContextException e)

{-# INLINE runActionWithNewContext #-}
runActionWithNewContext :: forall application controller. (Controller controller, ?request :: Request, ?respond :: Respond, InitControllerContext application, ?application :: application, Typeable application, Typeable controller) => controller -> IO ResponseReceived
runActionWithNewContext controller =
    earlyReturnMiddleware (\request respond -> do
        let ?request = setActionType controller request
        let ?respond = respond
        context <- newContextForAction controller
        let ?modelContext = requestModelContext ?request
        let ?context = context
        runAction controller
        ) ?request ?respond

-- | If 'IHP.LoginSupport.Helper.Controller.enableRowLevelSecurityIfLoggedIn' was called, this will copy the
-- the prepared RowLevelSecurityContext from the controller context into the ModelContext.
--
-- If row leve security wasn't enabled, this will just return the current model context.
prepareRLSIfNeeded :: (?request :: Request) => ModelContext -> IO ModelContext
prepareRLSIfNeeded modelContext = do
    rowLevelSecurityContext <- readIORef (lookupRequestVault rlsContextVaultKey ?request)
    case rowLevelSecurityContext of
        Just context -> pure modelContext { rowLevelSecurity = Just context }
        Nothing -> pure modelContext
{-# INLINE prepareRLSIfNeeded #-}

rlsContextVaultKey :: Vault.Key (IORef (Maybe RowLevelSecurityContext))
rlsContextVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE rlsContextVaultKey #-}

{-# INLINE startWebSocketApp #-}
startWebSocketApp :: forall webSocketApp application. (?request :: Request, ?respond :: Respond, InitControllerContext application, ?application :: application, Typeable application, WebSockets.WSApp webSocketApp) => webSocketApp -> IO ResponseReceived -> Application
startWebSocketApp initialState onHTTP waiRequest waiRespond = do
    let ?modelContext = requestModelContext ?request
    let ?request = waiRequest
    let ?respond = waiRespond

    let handleConnection pendingConnection = do
            connection <- WebSockets.acceptRequest pendingConnection

            controllerContext <- Context.newControllerContext
            let ?context = controllerContext

            try (initContext @application) >>= \case
                Left (exception :: SomeException) -> putStrLn $ "Unexpected exception in initContext, " <> show exception
                Right context -> do
                    WebSockets.startWSApp initialState connection

    let connectionOptions = WebSockets.connectionOptions @webSocketApp

    -- On a successful handshake 'websocketsApp' returns a 'ResponseRaw'
    -- wrapping a streaming handler plus a fallback 'Response'. Warp runs the
    -- raw handler and the client correctly receives HTTP 101 Switching
    -- Protocols — but request-logger middlewares (e.g. IHP's Apache access
    -- log in Production) compute the logged status from the fallback's
    -- builder/stream form, and wai-websockets hard-codes that fallback to
    -- 'status500' with a "WebSockets are not supported by your WAI handler"
    -- body. The result is that every successful WebSocket upgrade gets
    -- logged as
    --
    --     GET /DataSyncController HTTP/1.1 500 -
    --
    -- even though nginx / the actual client sees 101.
    --
    -- 'Wai.mapResponseStatus' is explicitly a no-op on 'ResponseRaw' (see
    -- the @mapResponseStatus _ r\@(ResponseRaw _ _) = r@ case in wai), so we
    -- have to pattern-match the raw constructor from 'Network.Wai.Internal'
    -- and rebuild the fallback 'Response' ourselves. We use 'status200'
    -- instead of the semantically correct 'status101' because Warp's
    -- @hasBody@ check (@sc >= 200 && sc /= 204 && sc /= 304@) treats 1xx
    -- as bodyless — causing it to send only the fallback headers and skip
    -- the raw streaming handler entirely, which breaks the WebSocket
    -- handshake. The on-the-wire status remains 101 (sent by the raw
    -- handler); the rewritten fallback status only affects what
    -- request-logger middlewares observe.
    waiRequest
        |> WebSockets.websocketsApp connectionOptions handleConnection
        |> \case
            Just response -> waiRespond (rewriteWebSocketFallbackStatus response)
            Nothing -> onHTTP
{-# INLINE startWebSocketAppAndFailOnHTTP #-}
startWebSocketAppAndFailOnHTTP :: forall webSocketApp application. (?request :: Request, ?respond :: Respond, InitControllerContext application, ?application :: application, Typeable application, WebSockets.WSApp webSocketApp) => webSocketApp -> Application
startWebSocketAppAndFailOnHTTP initialState = startWebSocketApp @webSocketApp @application initialState (?respond $ responseLBS HTTP.status400 [(hContentType, "text/plain")] "This endpoint is only available via a WebSocket")

-- | Rewrite the 'ResponseRaw' fallback produced by 'Network.Wai.Handler.WebSockets.websocketsApp'
-- so the fallback 'Response' reports @200 OK@ instead of the hard-coded @status500@
-- from wai-websockets. We cannot use @status101@ here because Warp's @hasBody@
-- predicate returns @False@ for all 1xx statuses, causing Warp to skip the raw
-- streaming handler and serve the fallback headers directly — which breaks the
-- WebSocket handshake (see #2628). @status200@ is the closest non-alarming status
-- that Warp's @hasBody@ accepts. See the comment in 'startWebSocketApp' for the
-- full rationale.
rewriteWebSocketFallbackStatus :: Response -> Response
rewriteWebSocketFallbackStatus (WaiInternal.ResponseRaw handler fallback) =
    WaiInternal.ResponseRaw handler (mapResponseStatus (const HTTP.status200) fallback)
rewriteWebSocketFallbackStatus other = other


jumpToAction :: forall action. (Controller action, ?context :: Context.ControllerContext, ?modelContext :: ModelContext, ?respond :: Respond, ?request :: Request) => action -> IO ResponseReceived
jumpToAction theAction = do
    let ?theAction = theAction
    beforeAction @action
    runControllerAction @action (action theAction)

getRequestBody :: (?request :: Request) => IO LBS.ByteString
getRequestBody =
    pure ?request.parsedBody.rawPayload

-- | Returns the request path, e.g. @/Users@ or @/CreateUser@
getRequestPath :: (?request :: Request) => ByteString
getRequestPath = ?request.rawPathInfo
{-# INLINABLE getRequestPath #-}

-- | Returns the request path and the query params, e.g. @/ShowUser?userId=9bd6b37b-2e53-40a4-bb7b-fdba67d6af42@
getRequestPathAndQuery :: (?request :: Request) => ByteString
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
getHeader :: (?request :: Request) => ByteString -> Maybe ByteString
getHeader name = lookup (Data.CaseInsensitive.mk name) ?request.requestHeaders
{-# INLINABLE getHeader #-}

-- | Set a header value for a given header name.
--
-- >>> setHeader ("Content-Language", "en")
--
setHeader :: (?request :: Request) => Header -> IO ()
setHeader header = do
    let headersRef = lookupRequestVault responseHeadersVaultKey ?request
    modifyIORef' headersRef (header :)
{-# INLINABLE setHeader #-}

-- | Returns the current HTTP request.
--
-- See https://hackage.haskell.org/package/wai-3.2.2.1/docs/Network-Wai.html#t:Request
request :: (?request :: Request) => Request
request = ?request
{-# INLINE request #-}

{-# INLINE getFiles #-}
getFiles :: (?request :: Request) => [File Data.ByteString.Lazy.ByteString]
getFiles =
    case ?request.parsedBody of
        FormBody { files } -> files
        _ -> []

requestBodyJSON :: (?request :: Request, ?respond :: Respond) => IO Aeson.Value
requestBodyJSON =
    case ?request.parsedBody of
        JSONBody { jsonPayload = Just value } -> pure value
        JSONBody { jsonPayload = Nothing, rawPayload } -> do
            let isDev = ?request.frameworkConfig.environment == Environment.Development
            let errorMessage = "Expected JSON body, but could not decode the request body"
                    <> (if LBS.null rawPayload
                        then ". The request body is empty."
                        else if isDev
                            then ". The raw request body was: " <> truncatePayload rawPayload
                            else ".")
            respondAndExit $ responseLBS HTTP.status400 [(hContentType, "application/json")] $
                Aeson.encode $ Aeson.object [("error", Aeson.String errorMessage)]
            where
                truncatePayload payload =
                    let shown = show payload
                        maxLen = 200
                    in if length shown > maxLen
                        then Text.pack (take maxLen shown) <> "... (truncated)"
                        else Text.pack shown
        FormBody {} ->
            respondAndExit $ responseLBS HTTP.status400 [(hContentType, "application/json")] $
                Aeson.encode $ Aeson.object [("error", Aeson.String "Expected JSON body, but the request has a form content type. Make sure to set 'Content-Type: application/json' in the request header.")]

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
