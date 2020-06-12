{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeFamilies, ConstrainedClassMethods, ScopedTypeVariables, FunctionalDependencies, AllowAmbiguousTypes #-}

module IHP.ControllerSupport
( Action'
, (|>)
, getRequestBody
, getRequestUrl
, getHeader
, RequestContext (..)
, request
, requestHeaders
, getFiles
, Controller (..)
, runAction
, createRequestContext
, ControllerContext
, fromControllerContext
, maybeFromControllerContext
, InitControllerContext (..)
, runActionWithNewContext
, emptyControllerContext
, respondAndExit
, ActionType (..)
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

type Action' = IO ResponseReceived

newtype ControllerContext = ControllerContext TypeMap.TMap
newtype ActionType = ActionType Typeable.TypeRep

{-# INLINE fromControllerContext #-}
fromControllerContext :: forall a. (?controllerContext :: ControllerContext, Typeable a) => a
fromControllerContext =
    let
        (ControllerContext context) = ?controllerContext
        notFoundMessage = ("Unable to find value in controller context: " <> show context)
    in
        fromMaybe (error notFoundMessage) (maybeFromControllerContext @a)

{-# INLINE maybeFromControllerContext #-}
maybeFromControllerContext :: forall a. (?controllerContext :: ControllerContext, Typeable a) => Maybe a
maybeFromControllerContext = let (ControllerContext context) = ?controllerContext in TypeMap.lookup @a context

{-# INLINE emptyControllerContext #-}
emptyControllerContext :: ControllerContext
emptyControllerContext = ControllerContext TypeMap.empty

class (Show controller, Eq controller) => Controller controller where
    beforeAction :: (?controllerContext :: ControllerContext, ?modelContext :: ModelContext, ?requestContext :: RequestContext, ?theAction :: controller) => IO ()
    beforeAction = pure ()
    action :: (?controllerContext :: ControllerContext, ?modelContext :: ModelContext, ?requestContext :: RequestContext, ?theAction :: controller) => controller -> IO ()

class InitControllerContext application where
    initContext :: (?modelContext :: ModelContext, ?requestContext :: RequestContext) => TypeMap.TMap -> IO TypeMap.TMap
    initContext context = pure context

{-# INLINE runAction #-}
runAction :: forall controller. (Controller controller, ?requestContext :: RequestContext, ?controllerContext :: ControllerContext, ?modelContext :: ModelContext) => controller -> IO ResponseReceived
runAction controller = do
    let ?theAction = controller
    let respond = ?requestContext |> get #respond
    
    let doRunAction = do
            beforeAction
            (action controller)
            ErrorController.handleNoResponseReturned controller

    let handleResponseException  (ResponseException response) = respond response
        
    doRunAction `catches` [ Handler handleResponseException, Handler (\exception -> ErrorController.displayException exception controller "")]

{-# INLINE runActionWithNewContext #-}
runActionWithNewContext :: forall application controller. (Controller controller, ?applicationContext :: ApplicationContext, ?requestContext :: RequestContext, InitControllerContext application, ?application :: application, Typeable application, Typeable controller) => controller -> IO ResponseReceived
runActionWithNewContext controller = do
    let ?modelContext = ApplicationContext.modelContext ?applicationContext
    let context = TypeMap.empty
            |> TypeMap.insert ?application
            |> TypeMap.insert (ActionType (Typeable.typeOf controller))

    try (initContext @application context) >>= \case
        Left exception -> do
            -- Calling `initContext` might fail, so we provide a bit better error messages here
            ErrorController.displayException exception controller " while calling initContext"
        Right context -> do
            let ?controllerContext = ControllerContext context
            runAction controller

{-# INLINE getRequestBody #-}
getRequestBody :: (?requestContext :: RequestContext) => IO ByteString
getRequestBody = Network.Wai.getRequestBodyChunk request

{-# INLINE getRequestUrl #-}
getRequestUrl :: (?requestContext :: RequestContext) => ByteString
getRequestUrl = Network.Wai.rawPathInfo request

{-# INLINE getHeader #-}
getHeader :: (?requestContext :: RequestContext) => ByteString -> Maybe ByteString
getHeader name = lookup (Data.CaseInsensitive.mk name) (Network.Wai.requestHeaders request)

-- | Returns the current HTTP request.
--
-- See https://hackage.haskell.org/package/wai-3.2.2.1/docs/Network-Wai.html#t:Request
request :: (?requestContext :: RequestContext) => Network.Wai.Request
request = ?requestContext |> get #request
{-# INLINE request #-}

{-# INLINE getFiles #-}
getFiles :: (?requestContext :: RequestContext) => [File Data.ByteString.Lazy.ByteString]
getFiles = ?requestContext |> get #files

{-# INLINE createRequestContext #-}
createRequestContext :: ApplicationContext -> Request -> Respond -> IO RequestContext
createRequestContext ApplicationContext { session } request respond = do
    (params, files) <- WaiParse.parseRequestBodyEx WaiParse.defaultParseRequestBodyOptions WaiParse.lbsBackEnd request
    pure RequestContext.RequestContext { request, respond, params, files, vault = session }



-- Can be thrown from inside the action to abort the current action execution.
-- Does not indicates a runtime error. It's just used for control flow management.
data ResponseException = ResponseException Response

instance Show ResponseException where show _ = "ResponseException { .. }"

instance Exception ResponseException

{-# INLINE respondAndExit #-}
respondAndExit :: Response -> IO ()
respondAndExit response = Exception.throwIO (ResponseException response)