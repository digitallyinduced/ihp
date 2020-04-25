{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeFamilies, ConstrainedClassMethods, ScopedTypeVariables, FunctionalDependencies, AllowAmbiguousTypes #-}

module TurboHaskell.ControllerSupport (Action', cs, (|>), getRequestBody, getRequestUrl, getHeader, RequestContext (..), getRequest, requestHeaders, getFiles, Controller (..), runAction, createRequestContext, ControllerContext, fromControllerContext, maybeFromControllerContext, InitControllerContext (..), runActionWithNewContext, emptyControllerContext, respondAndExit) where
import ClassyPrelude
import TurboHaskell.HaskellSupport
import Data.String.Conversions (cs)
import Network.Wai (Response, Request, ResponseReceived, responseLBS, requestBody, queryString, requestHeaders)
import qualified Network.Wai
import TurboHaskell.ModelSupport
import TurboHaskell.ApplicationContext (ApplicationContext (..))
import qualified TurboHaskell.ApplicationContext as ApplicationContext
import Network.Wai.Parse as WaiParse
import qualified Data.ByteString.Lazy
import TurboHaskell.Controller.RequestContext
import qualified Data.CaseInsensitive
import Control.Monad.Reader
import qualified Data.TMap as TypeMap
import qualified Control.Exception as Exception
import qualified TurboHaskell.ErrorController as ErrorController

type Action' = IO ResponseReceived

newtype ControllerContext = ControllerContext TypeMap.TMap

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
    let handlePatternMatchFailure (e :: Exception.PatternMatchFail) = ErrorController.handlePatternMatchFailure e controller
    let handleGenericException (e :: Exception.SomeException) = ErrorController.handleGenericException e controller
    let (RequestContext _ respond _ _ _) = ?requestContext
    let handleResponseException  (ResponseException response) = respond response
    (((beforeAction >> action controller >> ErrorController.handleNoResponseReturned controller) `Exception.catch` handleResponseException) `Exception.catch` handlePatternMatchFailure) `Exception.catch` handleGenericException

{-# INLINE runActionWithNewContext #-}
runActionWithNewContext :: forall application controller. (Controller controller, ?applicationContext :: ApplicationContext, ?requestContext :: RequestContext, InitControllerContext application) => controller -> IO ResponseReceived
runActionWithNewContext controller = do
    let ?modelContext = ApplicationContext.modelContext ?applicationContext
    context <- initContext @application TypeMap.empty
    let ?controllerContext = ControllerContext context
    runAction controller


{-# INLINE getRequestBody #-}
getRequestBody :: (?requestContext :: RequestContext) => IO ByteString
getRequestBody =
    let (RequestContext request _ _ _ _) = ?requestContext
    in Network.Wai.requestBody request

{-# INLINE getRequestUrl #-}
getRequestUrl :: (?requestContext :: RequestContext) => ByteString
getRequestUrl =
    let (RequestContext request _ _ _ _) = ?requestContext
    in Network.Wai.rawPathInfo request

{-# INLINE getHeader #-}
getHeader :: (?requestContext :: RequestContext) => ByteString -> Maybe ByteString
getHeader name =
    let (RequestContext request _ _ _ _) = ?requestContext
    in lookup (Data.CaseInsensitive.mk name) (Network.Wai.requestHeaders request)

{-# INLINE getRequest #-}
getRequest :: (?requestContext :: RequestContext) => Network.Wai.Request
getRequest =
    let (RequestContext request _ _ _ _) = ?requestContext
    in request

{-# INLINE getFiles #-}
getFiles :: (?requestContext :: RequestContext) => [File Data.ByteString.Lazy.ByteString]
getFiles =
    let (RequestContext _ _ _ files _) = ?requestContext
    in files

{-# INLINE createRequestContext #-}
createRequestContext :: ApplicationContext -> Request -> Respond -> IO RequestContext
createRequestContext ApplicationContext { session } request respond = do
    (params, files) <- WaiParse.parseRequestBodyEx WaiParse.defaultParseRequestBodyOptions WaiParse.lbsBackEnd request
    pure (RequestContext request respond params files session)



-- Can be thrown from inside the action to abort the current action execution.
-- Does not indicates a runtime error. It's just used for control flow management.
data ResponseException = ResponseException Response

instance Show ResponseException where show _ = "ResponseException { .. }"

instance Exception ResponseException

{-# INLINE respondAndExit #-}
respondAndExit :: Response -> IO ()
respondAndExit response = Exception.throw (ResponseException response)