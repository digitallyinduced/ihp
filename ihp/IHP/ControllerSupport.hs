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
, Context.ControllerContext
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
, Request
, rlsContextVaultKey
, setupActionContext
) where

import Prelude
import Data.IORef (IORef, modifyIORef', readIORef)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Control.Exception.Safe (SomeException, fromException, try, catches, Handler(..))
import Data.Typeable (Typeable)
import IHP.HaskellSupport
import Network.Wai
import qualified Network.HTTP.Types as HTTP
import IHP.ModelSupport
import Network.Wai.Parse as WaiParse
import qualified Data.ByteString.Lazy
import Wai.Request.Params.Middleware (Respond)
import qualified Data.CaseInsensitive
import qualified IHP.ErrorController as ErrorController
import qualified Data.Typeable as Typeable
import IHP.FrameworkConfig.Types (FrameworkConfig (..), ConfigProvider)
import qualified IHP.Controller.Context as Context
import IHP.Controller.Response
import Network.HTTP.Types.Header
import qualified Data.Aeson as Aeson
import qualified Network.Wai.Handler.WebSockets as WebSockets
import qualified Network.WebSockets as WebSockets
import qualified IHP.WebSocket as WebSockets
import qualified Data.TMap as TypeMap
import IHP.RequestVault.ModelContext
import IHP.ActionType (setActionType, actionTypeVaultKey, ActionType(..))
import IHP.RequestVault.Helper (lookupRequestVault)
import qualified Data.Vault.Lazy as Vault
import System.IO.Unsafe (unsafePerformIO)

type Action' = IO ResponseReceived

class (Show controller, Eq controller) => Controller controller where
    beforeAction :: (?context :: Context.ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller, ?respond :: Respond, ?request :: Request) => IO ()
    beforeAction = pure ()
    {-# INLINABLE beforeAction #-}
    action :: (?context :: Context.ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller, ?respond :: Respond, ?request :: Request) => controller -> IO ()

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
    => controller -> IO (Either (IO ResponseReceived) Context.ControllerContext)
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

-- | Shared request context setup, specialized once per application type.
-- Takes a pre-computed TypeRep to avoid per-controller-type code duplication.
-- NOINLINE ensures GHC compiles one copy shared across all controllers.
--
-- Returns @(controllerContext, Nothing)@ on success, or
-- @(controllerContext, Just exception)@ if 'initContext' failed.
-- The context is always returned so callers can use it for error rendering.
{-# NOINLINE setupActionContext #-}
setupActionContext
    :: forall application
     . ( InitControllerContext application
       , ?application :: application
       , Typeable application
       )
    => Typeable.TypeRep -> Request -> Respond
    -> IO (Context.ControllerContext, Maybe SomeException)
setupActionContext controllerTypeRep waiRequest waiRespond = do
    let !request' = waiRequest { vault = Vault.insert actionTypeVaultKey (ActionType controllerTypeRep) waiRequest.vault }
    let ?request = request'
    let ?respond = waiRespond
    let ?modelContext = request'.modelContext
    controllerContext <- Context.newControllerContext
    let ?context = controllerContext
    Context.putContext ?application
    try (initContext @application) >>= \case
        Left exception -> pure (?context, Just exception)
        Right _ -> pure (?context, Nothing)

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
prepareRLSIfNeeded :: (?request :: Request) => ModelContext -> IO ModelContext
prepareRLSIfNeeded modelContext = do
    rowLevelSecurityContext <- readIORef (lookupRequestVault rlsContextVaultKey ?request)
    case rowLevelSecurityContext of
        Just context -> pure modelContext { rowLevelSecurity = Just context }
        Nothing -> pure modelContext

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
startWebSocketAppAndFailOnHTTP :: forall webSocketApp application. (?request :: Request, ?respond :: Respond, InitControllerContext application, ?application :: application, Typeable application, WebSockets.WSApp webSocketApp) => webSocketApp -> Application
startWebSocketAppAndFailOnHTTP initialState = startWebSocketApp @webSocketApp @application initialState (?respond $ responseLBS HTTP.status400 [(hContentType, "text/plain")] "This endpoint is only available via a WebSocket")


jumpToAction :: forall action. (Controller action, ?context :: Context.ControllerContext, ?modelContext :: ModelContext, ?respond :: Respond, ?request :: Request) => action -> IO ()
jumpToAction theAction = do
    let ?theAction = theAction
    beforeAction @action
    action theAction

{-# INLINE getRequestBody #-}
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

requestBodyJSON :: (?request :: Request) => Aeson.Value
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
