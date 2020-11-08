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
) where

import ClassyPrelude
import IHP.HaskellSupport
import Data.String.Conversions (cs)
import Network.Wai (Response, Request, ResponseReceived, responseLBS, requestBody, queryString, requestHeaders)
import qualified Network.Wai
import IHP.ModelSupport
import IHP.ApplicationContext (ApplicationContext (..))
import qualified IHP.ApplicationContext as ApplicationContext
import Network.Wai.Parse as WaiParse
import qualified Data.ByteString.Lazy
import qualified IHP.Controller.RequestContext as RequestContext
import IHP.Controller.RequestContext (RequestContext, Respond)
import qualified Data.CaseInsensitive
import Control.Monad.Reader
import qualified Data.TMap as TypeMap
import qualified Control.Exception as Exception
import qualified IHP.ErrorController as ErrorController
import qualified Data.Typeable as Typeable
import IHP.FrameworkConfig (FrameworkConfig)
import qualified IHP.Controller.Context as Context
import IHP.Controller.Context (ControllerContext)
import IHP.FlashMessages.ControllerFunctions
import Network.HTTP.Types.Header
import qualified Data.Aeson as Aeson

type Action' = IO ResponseReceived

class (Show controller, Eq controller) => Controller controller where
    beforeAction :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller) => IO ()
    beforeAction = pure ()
    action :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller) => controller -> IO ()

class InitControllerContext application where
    initContext :: (?modelContext :: ModelContext, ?requestContext :: RequestContext, ?applicationContext :: ApplicationContext, ?context :: ControllerContext) => IO ()
    initContext = pure ()

{-# INLINE runAction #-}
runAction :: forall controller. (Controller controller, ?context :: ControllerContext, ?modelContext :: ModelContext) => controller -> IO ResponseReceived
runAction controller = do
    let ?theAction = controller
    let respond = ?context |> get #requestContext |> get #respond
    
    let doRunAction = do
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
    initFlashMessages

    try (initContext @application) >>= \case
        Left exception -> do
            -- Calling `initContext` might fail, so we provide a bit better error messages here
            ErrorController.displayException exception controller " while calling initContext"
        Right context -> do
            runAction controller

jumpToAction :: forall action. (Controller action, ?context :: ControllerContext, ?modelContext :: ModelContext) => action -> IO ()
jumpToAction theAction = do
    let ?theAction = theAction
    beforeAction @action
    action theAction

{-# INLINE getRequestBody #-}
getRequestBody :: (?context :: ControllerContext) => IO ByteString
getRequestBody = Network.Wai.getRequestBodyChunk request

-- | Returns the request path, e.g. @/Users@ or @/CreateUser@
getRequestPath :: (?context :: ControllerContext) => ByteString
getRequestPath = Network.Wai.rawPathInfo request
{-# INLINE getRequestPath #-}

-- | Returns the request path and the query params, e.g. @/ShowUser?userId=9bd6b37b-2e53-40a4-bb7b-fdba67d6af42@
getRequestPathAndQuery :: (?context :: ControllerContext) => ByteString
getRequestPathAndQuery = Network.Wai.rawPathInfo request <> Network.Wai.rawQueryString request
{-# INLINE getRequestPathAndQuery #-}

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
{-# INLINE getHeader #-}

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
        RequestContext.JSONBody (Just value) -> value
        _ -> error "Expected JSON body"

{-# INLINE createRequestContext #-}
createRequestContext :: ApplicationContext -> Request -> Respond -> IO RequestContext
createRequestContext ApplicationContext { session, frameworkConfig } request respond = do
    let contentType = lookup hContentType (requestHeaders request)
    requestBody <- case contentType of
        "application/json" -> do
            payload <- Network.Wai.getRequestBodyChunk request
            let value :: Maybe Aeson.Value = (Aeson.decode ((cs payload) :: LByteString))
            pure (RequestContext.JSONBody value)
        _ -> do
            (params, files) <- WaiParse.parseRequestBodyEx WaiParse.defaultParseRequestBodyOptions WaiParse.lbsBackEnd request
            pure RequestContext.FormBody { .. }

    pure RequestContext.RequestContext { request, respond, requestBody, vault = session, frameworkConfig }

-- Can be thrown from inside the action to abort the current action execution.
-- Does not indicates a runtime error. It's just used for control flow management.
data ResponseException = ResponseException Response

instance Show ResponseException where show _ = "ResponseException { .. }"

instance Exception ResponseException


respondAndExit :: Response -> IO ()
respondAndExit response = Exception.throwIO (ResponseException response)
{-# INLINE respondAndExit #-}
