{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances, LambdaCase, ScopedTypeVariables #-}
module IHP.RouterSupport (
CanRoute (..)
, HasPath (..)
, runAction
, runAction'
, get
, post
, startPage
, frontControllerToWAIApp
, withPrefix
, FrontController (..)
, defaultRouter
, parseRoute
, catchAll
, mountFrontController
, urlTo
, parseUUID
, parseId
, parseIntegerId
, remainingText
, parseText
, webSocketApp
, webSocketAppWithCustomPath
, webSocketAppWithHTTPFallback
, onlyAllowMethods
, getMethod
, routeParam
, withImplicits
, ControllerRoute (..)
, findInRouteMaps
, actionPrefixText
, wrapRouterException
) where

import Prelude hiding (take)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Control.Monad (unless)
import Text.Read (readMaybe)
import Unsafe.Coerce
import Control.Exception.Safe (SomeException, catch, throwIO)
import Control.Exception (evaluate)
import qualified IHP.ModelSupport as ModelSupport
import IHP.FrameworkConfig
import Data.UUID
import Network.HTTP.Types.Method
import Network.Wai
import qualified IHP.Router.Trie as Trie
import qualified IHP.Router.Middleware as RouterMiddleware
import IHP.ControllerSupport
import Data.Attoparsec.ByteString.Char8 (string, Parser, parseOnly, take, endOfInput, choice, takeTill, takeByteString)
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import Data.Data
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Typeable as Typeable
import qualified Data.ByteString.Char8 as ByteString
import Data.String.Conversions (ConvertibleStrings (convertString), cs)
import qualified Text.Blaze.Html5 as Html5
import qualified Control.Exception as Exception
import qualified IHP.ErrorController as ErrorController
import qualified Data.Text.Encoding as Text
import IHP.Router.Types
import IHP.Router.UrlGenerator
import qualified Data.HashMap.Strict as HashMap
import IHP.WebSocket (WSApp)
import qualified IHP.WebSocket as WS
import IHP.Controller.Context
import IHP.Controller.Param
import Data.Kind
import qualified Data.TMap as TypeMap
import Network.Wai.Middleware.EarlyReturn (earlyReturnMiddleware)

-- | Binds @?request@ and @?respond@ from WAI arguments, then runs the given action.
--
-- This avoids repeating @let ?request = waiRequest; let ?respond = waiRespond@ at each call site.
{-# INLINE withImplicits #-}
withImplicits :: ((?request :: Request, ?respond :: Respond) => Application) -> Application
withImplicits action waiRequest waiRespond =
    let ?request = waiRequest
        ?respond = waiRespond
    in action waiRequest waiRespond

runAction'
    :: forall application controller
     . ( Controller controller
       , InitControllerContext application
       , ?application :: application
       , Typeable application
       , Typeable controller
       )
     => controller -> Application
runAction' controller waiRequest waiRespond =
    earlyReturnMiddleware (\request respond -> do
        context <- setupActionContext @application (Typeable.typeOf controller) request respond
        let ?context = context
        let ?respond = respond
        let ?request = context.request
        let ?modelContext = ?request.modelContext
        runAction controller
        ) waiRequest waiRespond
{-# INLINE runAction' #-}

-- | Catches exceptions from routing and rethrows them wrapped in
-- 'RouterException' so the error handler middleware can distinguish
-- routing failures from action failures.
wrapRouterException :: IO a -> IO a
wrapRouterException action = action `catch` \(e :: SomeException) -> throwIO (ErrorController.RouterException e)

class FrontController application where
    controllers
        :: (?application :: application, ?request :: Request, ?respond :: Respond)
        => [ControllerRoute application]

    router
        :: (?application :: application, ?request :: Request, ?respond :: Respond)
        => [ControllerRoute application] -> Parser Application
    router = defaultRouter
    {-# INLINABLE router #-}

defaultRouter
    :: (?application :: application, ?request :: Request, ?respond :: Respond, FrontController application)
    => [ControllerRoute application] -> Parser Application
defaultRouter additionalRoutes = do
    let allRoutes = controllers <> additionalRoutes
        path = rawPathInfo ?request
        trie = collectTrie allRoutes
        application = ?application
        legacyParser =
            case findInRouteMaps path allRoutes of
                Just handler -> takeByteString *> pure (handler application)
                Nothing -> do
                    let parsers = concatMap getRouteParsers allRoutes
                    choice (map (<* endOfInput) parsers)

    -- Stage 1: method-aware trie (from the explicit-routes DSL, if any).
    -- Critical for 'mountFrontController': mounted sub-apps with
    -- '[routes|…|]' blocks flow through 'defaultRouter' via the parser
    -- wrapper, so they need the same trie-lookup stage that
    -- 'frontControllerToWAIApp' provides for the top-level app.
    case parseMethod (requestMethod ?request) of
        Right method -> case Trie.lookupTrie trie method (Trie.splitPath path) of
            Trie.Matched handler captures ->
                takeByteString *> pure (handler captures)
            -- On trie 405 we still give legacy routes a chance — a mixed-mode
            -- app may have `GET /foo` declared in the DSL and `POST /foo`
            -- registered via the legacy Attoparsec path. Only if nothing
            -- matches do we fall back to the legacy parser at all.
            _ -> legacyParser
        Left _ -> legacyParser
{-# INLINABLE defaultRouter #-}

-- | Scan 'ControllerRouteMap' entries for a matching path.
-- Returns as soon as a HashMap contains the path. Skips 'ControllerRouteParser'
-- and 'ControllerRouteTrie' entries (those are handled elsewhere).
findInRouteMaps :: ByteString -> [ControllerRoute application] -> Maybe (application -> Application)
findInRouteMaps _ [] = Nothing
findInRouteMaps path (ControllerRouteMap m _ : rest) =
    case HashMap.lookup path m of
        Just handler -> Just handler
        Nothing -> findInRouteMaps path rest
findInRouteMaps path (_ : rest) = findInRouteMaps path rest

-- | Extract fallback Attoparsec parsers from controller routes.
-- 'ControllerRouteTrie' entries contribute no fallback parsers — they
-- are consumed by the trie stage of 'frontControllerToWAIApp'.
getRouteParsers :: ControllerRoute application -> [Parser Application]
getRouteParsers (ControllerRouteMap _ fallback) = [fallback]
getRouteParsers (ControllerRouteParser p) = [p]
getRouteParsers (ControllerRouteTrie _) = []

-- | Merge all 'ControllerRouteTrie' fragments in the route list into a
-- single app-wide 'RouteTrie'. Returns 'Trie.emptyTrie' if no fragments exist.
collectTrie :: [ControllerRoute application] -> Trie.RouteTrie
collectTrie = List.foldl' step Trie.emptyTrie
  where
    step acc (ControllerRouteTrie fragment) = Trie.mergeTrie acc fragment
    step acc _ = acc

-- | Returns the url to a given action.
--
-- Uses the baseUrl configured in @Config/Config.hs@. When no @baseUrl@
-- is configured in development mode, it will automatically detect the
-- correct @baseUrl@ value.
--
-- >>> urlTo UsersAction
-- "http://localhost:8000/Users"
--
-- >>> urlTo ShowUserAction { userId = "a32913dd-ef80-4f3e-9a91-7879e17b2ece" }
-- "http://localhost:8000/ShowUser?userId=a32913dd-ef80-4f3e-9a91-7879e17b2ece"
urlTo :: (?context :: context, ConfigProvider context, HasPath action) => action -> Text
urlTo action = ?context.frameworkConfig.baseUrl <> pathTo action
{-# INLINE urlTo #-}

class HasPath controller => CanRoute controller where
    parseRoute' :: (?request :: Request, ?respond :: Respond) => Parser controller

    -- | Builds a WAI Application parser for this controller.
    --
    -- The default implementation parses the controller action using 'parseRoute''
    -- and applies the given callback. The overlappable 'AutoRoute' instance overrides
    -- this to defer query string parsing and method validation to the Application closure.
    parseRouteWithAction :: (?request :: Request, ?respond :: Respond) => (controller -> Application) -> Parser Application
    parseRouteWithAction toApp = do
        action <- parseRoute'
        pure (toApp action)
    {-# INLINE parseRouteWithAction #-}

    -- | Build a 'ControllerRoute' for this controller.
    --
    -- The default wraps the parser in 'ControllerRouteParser'.
    -- The overlappable 'AutoRoute' instance overrides this to use 'ControllerRouteMap'
    -- for O(1) HashMap dispatch. This is what 'parseRoute' calls.
    toControllerRoute :: forall application.
        ( ?request :: Request
        , ?respond :: Respond
        , Controller controller
        , InitControllerContext application
        , ?application :: application
        , Typeable application
        , Typeable controller
        ) => ControllerRoute application
    toControllerRoute = ControllerRouteParser (parseRouteWithAction @controller (runAction' @application))
    {-# INLINABLE toControllerRoute #-}


-- | Returns the url prefix for a controller. The prefix is based on the
-- module where the controller is defined.
--
-- All controllers defined in the `Web/` directory don't have a prefix at all.
--
-- E.g. controllers in the `Admin/` directory are prefixed with @/admin/@.
actionPrefixText :: forall (controller :: Type). Typeable controller => Text
actionPrefixText
    | "Web." `Text.isPrefixOf` moduleName = "/"
    | "IHP." `Text.isPrefixOf` moduleName = "/"
    | Text.null moduleName = "/"
    | otherwise = "/" <> Text.toLower (getPrefix moduleName) <> "/"
    where
        moduleName :: Text
        moduleName = Text.pack $ Typeable.typeOf (error "unreachable" :: controller)
                |> Typeable.typeRepTyCon
                |> Typeable.tyConModule

        getPrefix :: Text -> Text
        getPrefix t = fst (Text.breakOn "." t)
{-# NOINLINE actionPrefixText #-}


-- | Parses the HTTP Method from the request and returns it.
getMethod :: (?request :: Request, ?respond :: Respond) => Parser StdMethod
getMethod =
    case parseMethod ?request.requestMethod of
        Left error -> fail (ByteString.unpack error)
        Right method -> pure method
{-# INLINABLE getMethod #-}

-- | Routes a given path to an action when requested via GET.
--
-- __Example:__
--
-- > instance FrontController WebApplication where
-- >     controllers = [
-- >             get "/my-custom-page" NewSessionAction
-- >         ]
--
-- The request @GET \/my-custom-page@ is now executing NewSessionAction
--
-- Also see 'post'.
get :: (Controller action
    , InitControllerContext application
    , ?application :: application
    , Typeable application
    , Typeable action
    ) => ByteString -> action -> ControllerRoute application
get path action = ControllerRouteParser $ do
    string path
    pure $ \waiRequest waiRespond ->
        case parseMethod (requestMethod waiRequest) of
            Right GET -> runAction' action waiRequest waiRespond
            Right HEAD -> runAction' action waiRequest waiRespond
            Right method -> Exception.throw UnexpectedMethodException { allowedMethods = [GET, HEAD], method }
            Left err -> error ("Invalid HTTP method: " <> ByteString.unpack err)
{-# INLINABLE get #-}

-- | Routes a given path to an action when requested via POST.
--
-- __Example:__
--
-- > instance FrontController WebApplication where
-- >     controllers = [
-- >             post "/do-something" DoSomethingAction
-- >         ]
--
-- The request @POST \/do-something@ is now executing DoSomethingAction
--
-- Also see 'get'.
post :: (Controller action
    , InitControllerContext application
    , ?application :: application
    , Typeable application
    , Typeable action
    ) => ByteString -> action -> ControllerRoute application
post path action = ControllerRouteParser $ do
    string path
    pure $ \waiRequest waiRespond ->
        case parseMethod (requestMethod waiRequest) of
            Right POST -> runAction' action waiRequest waiRespond
            Right method -> Exception.throw UnexpectedMethodException { allowedMethods = [POST], method }
            Left err -> error ("Invalid HTTP method: " <> ByteString.unpack err)
{-# INLINABLE post #-}

-- | Filter methods when writing a custom routing parser
--
-- __Example:__
--
-- > instance CanRoute ApiController where
-- >    parseRoute' = do
-- >        string "/api/"
-- >        let
-- >            createRecordAction = do
-- >                onlyAllowMethods [POST]
-- >
-- >                table <- parseText
-- >                endOfInput
-- >                pure CreateRecordAction { table }
-- >
-- >            updateRecordAction = do
-- >                onlyAllowMethods [PATCH]
-- >
-- >                table <- parseText
-- >                string "/"
-- >                id <- parseUUID
-- >                pure UpdateRecordAction { table, id }
-- >
-- > createRecordAction <|> updateRecordAction
--
onlyAllowMethods :: (?request :: Request, ?respond :: Respond) => [StdMethod] -> Parser ()
onlyAllowMethods methods = do
    method <- getMethod
    unless (method `elem` methods) (fail ("Invalid method, expected one of: " <> show methods))
{-# INLINABLE onlyAllowMethods #-}

-- | Routes to a given WebSocket app if the path matches the WebSocket app name
--
-- __Example:__
--
-- > instance FrontController WebApplication where
-- >     controllers = [
-- >             webSocketApp @AutoRefreshWSApp
-- >         ]
--
-- The request @\/AutoRefreshWSApp@ will call the AutoRefreshWSApp
--
webSocketApp :: forall webSocketApp application.
    ( WSApp webSocketApp
    , InitControllerContext application
    , ?application :: application
    , Typeable application
    , Typeable webSocketApp
    ) => ControllerRoute application
webSocketApp = webSocketAppWithCustomPath @webSocketApp typeName
    where
        typeName :: ByteString
        typeName = Typeable.typeOf (error "unreachable" :: webSocketApp)
                |> show
                |> ByteString.pack
{-# INLINABLE webSocketApp #-}

webSocketAppWithHTTPFallback :: forall webSocketApp application.
    ( WSApp webSocketApp
    , InitControllerContext application
    , ?application :: application
    , Typeable application
    , Typeable webSocketApp
    , Controller webSocketApp
    ) => ControllerRoute application
webSocketAppWithHTTPFallback = webSocketAppWithCustomPathAndHTTPFallback @webSocketApp @application typeName
    where
        typeName :: ByteString
        typeName = Typeable.typeOf (error "unreachable" :: webSocketApp)
                |> show
                |> ByteString.pack
{-# INLINABLE webSocketAppWithHTTPFallback #-}

-- | Routes to a given WebSocket app if the path matches
--
-- __Example:__
--
-- > instance FrontController WebApplication where
-- >     controllers = [
-- >             webSocketAppWithCustomPath @AutoRefreshWSApp "my-ws-app"
-- >         ]
--
-- The request @\/my-ws-app@ will call the AutoRefreshWSApp
--
webSocketAppWithCustomPath :: forall webSocketApp application.
    ( WSApp webSocketApp
    , InitControllerContext application
    , ?application :: application
    , Typeable application
    , Typeable webSocketApp
    ) => ByteString -> ControllerRoute application
webSocketAppWithCustomPath path = ControllerRouteParser $ do
        Attoparsec.char '/'
        string path
        pure $ withImplicits (startWebSocketAppAndFailOnHTTP @webSocketApp @application (WS.initialState @webSocketApp))
{-# INLINABLE webSocketAppWithCustomPath #-}

webSocketAppWithCustomPathAndHTTPFallback :: forall webSocketApp application.
    ( WSApp webSocketApp
    , InitControllerContext application
    , ?application :: application
    , Typeable application
    , Typeable webSocketApp
    , Controller webSocketApp
    ) => ByteString -> ControllerRoute application
webSocketAppWithCustomPathAndHTTPFallback path = ControllerRouteParser $ do
        Attoparsec.char '/'
        string path
        let action = WS.initialState @webSocketApp
        pure $ withImplicits (startWebSocketApp @webSocketApp @application action (runActionWithNewContext action))
{-# INLINABLE webSocketAppWithCustomPathAndHTTPFallback #-}


-- | Defines the start page for a router (when @\/@ is requested).
startPage :: forall action application. (Controller action, InitControllerContext application, ?application::application, Typeable application, Typeable action) => action -> ControllerRoute application
startPage action = get (Text.encodeUtf8 (actionPrefixText @action)) action
{-# INLINABLE startPage #-}

withPrefix prefix routes = string prefix >> choice (map (\r -> r <* endOfInput) routes)
{-# INLINABLE withPrefix #-}

-- | Build the static portion of a 'FrontController'\'s routing — the
-- union of every 'ControllerRouteTrie' fragment emitted by the
-- @[routes|…|]@ DSL across the app's controllers.
--
-- This is a one-shot, app-construction-time computation. We satisfy
-- 'toControllerRoute'\'s @?request@ / @?respond@ constraints with
-- sentinel values that must not be forced: neither 'collectTrie' nor
-- the trie fragments themselves evaluate those implicits (trie handlers
-- only close over @?application@, which we bind to the real
-- application). The parser thunks stored inside sibling
-- 'ControllerRouteMap' / 'ControllerRouteParser' entries are left
-- unevaluated — 'collectTrie' skips them — and discarded when the list
-- is dropped.
--
-- Deliberately kept as a separate top-level binding from
-- 'frontControllerToWAIApp' so GHC has no opportunity to merge its
-- result with the per-request 'controllers' call inside the request
-- lambda.
collectStaticTrie :: forall app. FrontController app => app -> Trie.RouteTrie
collectStaticTrie application =
    let ?request = startupRequestStub
        ?respond = startupRespondStub
        ?application = application
    in collectTrie (controllers @app)
{-# NOINLINE collectStaticTrie #-}

-- | Real but unused 'Request' and 'Respond' values for
-- 'collectStaticTrie'. We bind them as @?request@ / @?respond@ just so
-- the 'CanRoute.toControllerRoute' methods can be called at startup,
-- but the resulting 'RouteTrie' never actually reads per-request state
-- — DSL-emitted trie handlers close only over @?application@, and the
-- parser thunks inside sibling 'ControllerRouteMap' / 'ControllerRouteParser'
-- entries are never forced by 'collectTrie' (it only inspects
-- 'ControllerRouteTrie' payloads).
--
-- Using 'Network.Wai.defaultRequest' and a trivial respond function
-- (rather than 'error' sentinels) keeps the dictionary fields accessible
-- as real values. Class-method dispatch can evaluate those fields
-- without crashing, which it apparently does somewhere in the AutoRoute
-- dictionary chain — the previous @error \"...\"@ sentinels were being
-- forced there.
startupRequestStub :: Request
startupRequestStub = Network.Wai.defaultRequest
{-# NOINLINE startupRequestStub #-}

startupRespondStub :: Respond
startupRespondStub = \_ -> pure (error "frontControllerToWAIApp: startupRespondStub evaluated")
{-# NOINLINE startupRespondStub #-}

frontControllerToWAIApp :: forall app (autoRefreshApp :: Type). (FrontController app, WSApp autoRefreshApp, Typeable autoRefreshApp, InitControllerContext ()) => Middleware -> app -> Application -> Application
frontControllerToWAIApp middleware application notFoundAction =
    -- Build the static dispatch trie ONCE at Application-construction time.
    -- The resulting RouteTrie closes over ?application (stable for the
    -- app's lifetime) but not over ?request / ?respond. Every request
    -- below reuses the same trie value.
    let !staticTrie = collectStaticTrie application
    in \waiRequest waiRespond -> do
        let ?request = waiRequest
        let ?respond = waiRespond

        let autoRefreshWSParser :: Parser Application
            autoRefreshWSParser =
                let ?application = () in
                let typeName = Typeable.typeOf (error "unreachable" :: autoRefreshApp)
                        |> show |> ByteString.pack
                in do
                    Attoparsec.char '/'
                    string typeName
                    pure $ withImplicits (startWebSocketAppAndFailOnHTTP @autoRefreshApp @() (WS.initialState @autoRefreshApp))

        -- Per-request route list used for the legacy AutoRoute HashMap and
        -- Attoparsec fallback paths. DSL-emitted ControllerRouteTrie entries
        -- are in here too, but we don't consult them — the cached
        -- staticTrie already covers that case.
        let allRoutes = let ?application = application in
                ControllerRouteParser autoRefreshWSParser : controllers @app

        let path = waiRequest.rawPathInfo

        case parseMethod waiRequest.requestMethod of
            Right method -> case Trie.lookupTrie staticTrie method (Trie.splitPath path) of
                Trie.Matched handler captures ->
                    (middleware (handler captures)) waiRequest waiRespond
                -- On trie 405 we still try the legacy path before finalising
                -- the response. In mixed-mode apps, a DSL `GET /foo` plus a
                -- legacy `POST /foo` would otherwise reject the POST with 405
                -- even though the legacy route exists. The trie's allowed
                -- method list becomes the final 405 payload only if legacy
                -- dispatch also can't find a handler.
                Trie.MethodNotAllowed allowed ->
                    legacyDispatchOr405 waiRequest waiRespond allRoutes path allowed
                Trie.NotMatched -> legacyDispatch waiRequest waiRespond allRoutes path
            Left _nonStandardMethod -> legacyDispatch waiRequest waiRespond allRoutes path
  where
    legacyDispatch waiRequest waiRespond allRoutes path =
        -- Stage 2: legacy AutoRoute HashMap fast path.
        case findInRouteMaps path allRoutes of
            Just handler -> (middleware (handler application)) waiRequest waiRespond
            Nothing -> do
                -- Stage 3: Attoparsec fallback for custom/dynamic route parsers.
                let customParsers = concatMap getRouteParsers allRoutes

                routedAction :: Either String Application <-
                    (do
                        res <- evaluate $ parseOnly (choice (map (<* endOfInput) customParsers)) path
                        case res of
                            Left s -> pure $ Left s
                            Right action -> pure $ Right action
                        )
                    |> wrapRouterException
                case routedAction of
                    Left _ -> notFoundAction waiRequest waiRespond
                    Right action -> (middleware action) waiRequest waiRespond

    -- Variant of 'legacyDispatch' used when the trie reports
    -- 'MethodNotAllowed': legacy routes still get a chance to handle
    -- the request (mixed-mode apps commonly split methods across DSL
    -- and legacy routes for the same path). Only if legacy finds no
    -- match do we commit to the trie's 405.
    legacyDispatchOr405 waiRequest waiRespond allRoutes path trieAllowed =
        case findInRouteMaps path allRoutes of
            Just handler -> (middleware (handler application)) waiRequest waiRespond
            Nothing -> do
                let customParsers = concatMap getRouteParsers allRoutes
                routedAction :: Either String Application <-
                    (do
                        res <- evaluate $ parseOnly (choice (map (<* endOfInput) customParsers)) path
                        case res of
                            Left s -> pure $ Left s
                            Right action -> pure $ Right action
                        )
                    |> wrapRouterException
                case routedAction of
                    Right action -> (middleware action) waiRequest waiRespond
                    Left _ ->
                        waiRespond (RouterMiddleware.methodNotAllowedResponse trieAllowed)
{-# INLINABLE frontControllerToWAIApp #-}

mountFrontController :: forall frontController application. (?request :: Request, ?respond :: Respond, FrontController frontController) => frontController -> ControllerRoute application
mountFrontController application = ControllerRouteParser (let ?application = application in router [])
{-# INLINABLE mountFrontController #-}

-- | Create a route entry for a controller.
--
-- Automatically uses the HashMap fast path when 'AutoRoute' is available
-- (via the overlappable 'CanRoute' instance), or falls back to Attoparsec
-- for controllers with custom 'CanRoute' instances.
--
-- No user code changes needed — @parseRoute \@PostsController@ picks the
-- optimal strategy at compile time.
parseRoute :: forall controller application.
    ( ?request :: Request
    , ?respond :: Respond
    , CanRoute controller
    , Controller controller
    , InitControllerContext application
    , ?application :: application
    , Typeable application
    , Typeable controller
    ) => ControllerRoute application
parseRoute = toControllerRoute @controller @application
{-# INLINABLE parseRoute #-}


catchAll :: forall action application. (Controller action, InitControllerContext application, Typeable action, ?application :: application, Typeable application, Data action) => action -> ControllerRoute application
catchAll action = ControllerRouteParser $ do
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
{-# INLINABLE parseUUID #-}

-- | Parses an UUID, afterwards wraps it in an Id
parseId :: ((ModelSupport.PrimaryKey table) ~ UUID) => Parser (ModelSupport.Id' table)
parseId = ModelSupport.Id <$> parseUUID
{-# INLINABLE parseId #-}

-- | Returns all the remaining text until the end of the input
remainingText :: Parser Text
remainingText = Text.decodeUtf8 <$> takeByteString
{-# INLINABLE remainingText #-}

-- | Parses until the next @/@
parseText :: Parser Text
parseText = Text.decodeUtf8 <$> takeTill ('/' ==)
{-# INLINABLE parseText #-}

parseIntegerId :: (Data idType) => ByteString -> Maybe idType
parseIntegerId queryVal = let
    rawValue :: Maybe Integer = readMaybe (cs queryVal :: String)
    in
       rawValue >>= Just . unsafeCoerce

-- | Parses and returns an integer
-- parseRational :: (Integral a) => Parser a
-- parseRational = Attoparsec.decimal

-- | Parses a route query parameter
--
-- __Example:__
--
-- > let showPost = do
-- >     string "/post"
-- >     let postId = routeParam "postId"
-- >     pure ShowPostAction { .. }
-- Will parse the `postId` query in `/post?postId=09b545dd-9744-4ef8-87b8-8d227f4faa1e`
--
routeParam :: (?request :: Request, ?respond :: Respond, ParamReader paramType) => ByteString -> paramType
routeParam paramName =
    let customFields = TypeMap.insert ?request TypeMap.empty
    in
        let ?context = FrozenControllerContext { customFields }
        in param paramName

