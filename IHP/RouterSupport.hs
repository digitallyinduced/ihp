{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances, LambdaCase #-}
module IHP.RouterSupport (
CanRoute (..)
, HasPath (..)
, AutoRoute (..)
, runAction
, get
, post
, startPage
, frontControllerToWAIApp
, withPrefix
, FrontController (..)
, parseRoute
, catchAll
, mountFrontController
, createAction
, updateAction
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
) where

import qualified Prelude
import ClassyPrelude hiding (index, delete, take)
import qualified IHP.ModelSupport as ModelSupport
import IHP.FrameworkConfig
import IHP.ApplicationContext hiding (frameworkConfig)
import Data.UUID
import Network.HTTP.Types.Method
import GHC.Records
import IHP.Controller.RequestContext
import Network.Wai
import IHP.ControllerSupport
import Data.Attoparsec.ByteString.Char8 (string, Parser, parseOnly, take, endOfInput, choice, takeTill, takeByteString)
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import GHC.TypeLits
import Data.Data
import qualified Control.Monad.State.Strict as State
import qualified Data.Text as Text
import Network.HTTP.Types.URI
import qualified Data.List as List
import Unsafe.Coerce
import IHP.HaskellSupport hiding (get)
import qualified Data.Typeable as Typeable
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Char as Char
import Control.Monad.Fail
import Data.String.Conversions (ConvertibleStrings (convertString), cs)
import qualified Text.Blaze.Html5 as Html5
import qualified IHP.ErrorController as ErrorController
import qualified Control.Exception as Exception
import qualified Data.List.Split as List
import qualified Network.URI.Encode as URI
import qualified Data.Text.Encoding as Text
import Data.Dynamic
import IHP.Router.Types
import IHP.WebSocket (WSApp)
import qualified IHP.WebSocket as WS
import GHC.TypeLits as T
import IHP.Controller.Context
import IHP.Controller.Param
import qualified Data.TMap as TMap
import qualified IHP.ApplicationContext as ApplicationContext

applyContextSetter :: (TMap.TMap -> TMap.TMap) -> ControllerContext -> IO ControllerContext
applyContextSetter setter ctx@ControllerContext { customFieldsRef } = do
    modifyIORef customFieldsRef (applySetter setter)
    pure $ ctx { customFieldsRef }
    where
        fromSetter :: (TMap.TMap -> TMap.TMap) -> TMap.TMap
        fromSetter f = f TMap.empty

        applySetter :: (TMap.TMap -> TMap.TMap) -> TMap.TMap -> TMap.TMap
        applySetter f map = TMap.union (fromSetter f) map

runAction'
    :: forall application controller
     . ( Controller controller
       , ?applicationContext :: ApplicationContext
       , ?context :: RequestContext
       , InitControllerContext application
       , ?application :: application
       , Typeable application
       , Typeable controller
       )
     => controller -> (TMap.TMap -> TMap.TMap) -> IO ResponseReceived
runAction' controller contextSetter = do
    let ?modelContext = ApplicationContext.modelContext ?applicationContext
    let ?requestContext = ?context
    contextOrErrorResponse <- newContextForAction controller
    case contextOrErrorResponse of
        Left res -> res
        Right context -> do
            context' <- applyContextSetter contextSetter context
            runActionWithContext context' controller

type RouteParseResult = IO (TMap.TMap -> TMap.TMap, (TMap.TMap -> TMap.TMap) -> IO ResponseReceived)
type RouteParser = Parser (RouteParseResult)

toRouteParser :: Parser (IO ResponseReceived) -> RouteParser
toRouteParser parser = do
    controller <- parser
    pure $ pure (\t -> t, \_ -> controller)

toRouteParser' :: Parser ((TMap.TMap -> TMap.TMap) -> IO ResponseReceived) -> RouteParser
toRouteParser' parser = do
    controller <- parser
    pure $ pure (\t -> t, controller)

toRouteParseResult :: IO ResponseReceived -> RouteParseResult
toRouteParseResult ioResponseReceived = pure (\t -> t, \_ -> ioResponseReceived)

class FrontController application where
    controllers
        :: forall controller
         . (?applicationContext :: ApplicationContext, ?application :: application, ?context :: RequestContext)
        => [RouteParser]

    router
        :: (?applicationContext :: ApplicationContext, ?application :: application, ?context :: RequestContext)
        => [RouteParser] -> RouteParser
    router = defaultRouter

    defaultRouter
        :: (?applicationContext :: ApplicationContext, ?application :: application, ?context :: RequestContext)
        => [RouteParser] -> RouteParser
    defaultRouter additionalControllers = do
        let allControllers = controllers <> additionalControllers
        ioResponseReceived <- choice $ map (\r -> r <* endOfInput) allControllers
        pure ioResponseReceived

class HasPath controller where
    -- | Returns the path to a given action
    --
    -- >>> pathTo UsersAction
    -- "/Users"
    --
    -- >>> pathTo ShowUserAction { userId = "a32913dd-ef80-4f3e-9a91-7879e17b2ece" }
    -- "/ShowUser?userId=a32913dd-ef80-4f3e-9a91-7879e17b2ece"
    pathTo :: controller -> Text

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
urlTo action = (fromConfig baseUrl) <> pathTo action
{-# INLINE urlTo #-}

class HasPath controller => CanRoute controller where
    parseRoute' :: (?context :: RequestContext) => Parser controller


-- | Each of these is tried when trying to parse an argument to a controller constructor (i.e. in IHP, an action).
-- The type @d@ is an the type of the argument, and all we know about this type that its conforms to @Data@.
-- We cannot cast @d@ to some arbitrary type, since adding additional constraints to @d@ (such as Read)
-- will break the @fromConstrM@ function which actually constructs the action.
--
-- The approach taken here is to make use of the type equality operator @:~:@
-- to check and see if @d@ happens to be a certain type. If it is,
-- by matching on Just Refl, we are able to use @d@ as the type we matched it to.
--
-- Please consult your doctor before engaging in Haskell type programming.
parseFuncs :: forall d idType. (Data d, Data idType) => (ByteString -> Maybe idType) -> [Maybe ByteString -> Either TypedAutoRouteError d]
parseFuncs parseIdType = [
            -- Try and parse @Int@ or @Maybe Int@
            \case
                Just queryValue -> case eqT :: Maybe (d :~: Int) of
                    Just Refl -> readMay (cs queryValue :: String)
                        |> \case
                            Just int -> Right int
                            Nothing -> Left BadType { field = "", value = Just queryValue, expectedType = "Int" }
                    Nothing -> case eqT :: Maybe (d :~: Maybe Int) of
                        Just Refl -> Right $ readMay (cs queryValue :: String)
                        Nothing -> Left NotMatched
                Nothing -> case eqT :: Maybe (d :~: Maybe Int) of
                    Just Refl -> Right Nothing
                    Nothing -> Left NotMatched,

            \case
                Just queryValue -> case eqT :: Maybe (d :~: Integer) of
                    Just Refl -> readMay (cs queryValue :: String)
                        |> \case
                            Just int -> Right int
                            Nothing -> Left BadType { field = "", value = Just queryValue, expectedType = "Integer" }
                    Nothing -> case eqT :: Maybe (d :~: Maybe Integer) of
                        Just Refl -> Right $ readMay (cs queryValue :: String)
                        Nothing -> Left NotMatched
                Nothing -> case eqT :: Maybe (d :~: Maybe Integer) of
                    Just Refl -> Right Nothing
                    Nothing -> Left NotMatched,

            -- Try and parse @Text@ or @Maybe Text@
            \case
                Just queryValue -> case eqT :: Maybe (d :~: Text) of
                    Just Refl -> Right $ cs queryValue
                    Nothing -> case eqT :: Maybe (d :~: Maybe Text) of
                        Just Refl -> Right $ Just $ cs queryValue
                        Nothing -> Left NotMatched
                Nothing -> case eqT :: Maybe (d :~: Maybe Text) of
                    Just Refl -> Right Nothing
                    Nothing -> Left NotMatched,
            \case
                Just queryValue -> case parseIdType queryValue of
                    Just idValue -> case eqT :: Maybe (d :~: idType) of
                        Just Refl -> Right idValue
                        Nothing -> Left NotMatched
                    Nothing -> Left NotMatched
                Nothing -> Left NotMatched,

            -- Try and parse @[Text]@. If value is not present then default to empty list.
            \queryValue -> case eqT :: Maybe (d :~: [Text]) of
                Just Refl -> case queryValue of
                    Just queryValue -> Right $ Text.splitOn "," (cs queryValue)
                    Nothing -> Right []
                Nothing -> Left NotMatched,

            -- Try and parse @[Int]@. If value is not present then default to empty list.
            \queryValue -> case eqT :: Maybe (d :~: [Int]) of
                Just Refl -> case queryValue of
                    Just queryValue -> Text.splitOn "," (cs queryValue)
                        |> map readMay
                        |> catMaybes
                        |> Right
                    Nothing -> Right []
                Nothing -> Left NotMatched,

            \queryValue -> case eqT :: Maybe (d :~: [Integer]) of
                Just Refl -> case queryValue of
                    Just queryValue -> Text.splitOn "," (cs queryValue)
                        |> map readMay
                        |> catMaybes
                        |> Right
                    Nothing -> Right []
                Nothing -> Left NotMatched,

            -- Try and parse a raw [UUID]
            \queryValue -> case eqT :: Maybe (d :~: [UUID]) of
                Just Refl -> case queryValue of
                    Just queryValue -> queryValue
                        |> cs
                        |> Text.splitOn ","
                        |> map (fromASCIIBytes . cs)
                        |> catMaybes
                        |> Right
                    Nothing -> Right []
                Nothing -> Left NotMatched,

            -- Try and parse a raw UUID
            \queryValue -> case eqT :: Maybe (d :~: UUID) of
                Just Refl -> case queryValue of
                    Just queryValue -> queryValue
                        |> fromASCIIBytes
                        |> \case
                            Just uuid -> uuid |> Right
                            Nothing -> Left BadType { field = "", value = Just queryValue, expectedType = "UUID" }
                    Nothing -> Left NotMatched
                Nothing -> Left NotMatched,

            -- This has to be last parser in the list
            --
            -- Try and parse a UUID wrapped with a Id. In IHP types these are wrapped in a newtype @Id@ such as @Id User@.
            -- Since @Id@ is a newtype wrapping a UUID, it has the same data representation in GHC.
            -- Therefore, we're able to safely cast it to its @Id@ type with @unsafeCoerce@.
            --
            -- We cannot use 'eqT' here for checking the types, as it's wrapped inside the @Id@ type. We expect
            -- that if it looks like a UUID, we can just treat it like an @Id@ type. For that to not overshadow other
            -- parsers, we need to have this last.
            \queryValue -> case queryValue of
                Just queryValue -> queryValue
                    |> fromASCIIBytes
                    |> \case
                        Just uuid -> uuid |> unsafeCoerce |> Right
                        Nothing -> Left BadType { field = "", value = Just queryValue, expectedType = "UUID" }
                Nothing -> Left NotMatched
            ]

-- | As we fold over a constructor, we want the values parsed from the query string
-- to be in the same order as they are in the constructor.
-- This function uses the field labels from the constructor to sort the values from
-- the query string. As a consequence, constructors with basic record syntax will not work with auto types.
--
-- @data MyController = MyAction Text Int@
--
-- does not work. Instead use,
--
-- @data MyController = MyAction { textArg :: Text, intArg :: Int }@
querySortedByFields :: Query -> Constr -> Query
querySortedByFields query constructor = constrFields constructor
        |> map cs
        |> map (\field -> (field, join $ List.lookup field query))

-- | Given a constructor and a parsed query string, attempt to construct a value of the constructor's type.
-- For example, given the controller
--
-- @data MyController = MyAction { textArg :: Text, intArg :: Int }@
--
-- this function will receive a representation of the @MyAction@ constructor as well as some query string
-- @[("textArg", "some text"), ("intArg", "123")]@.
--
-- By iterating through the query and attempting to match the type of each constructor argument
-- with some transformation of the query string, we attempt to call @MyAction@.
applyConstr :: (Data controller, Data idType) => (ByteString -> Maybe idType) -> Constr -> Query -> Either TypedAutoRouteError controller
applyConstr parseIdType constructor query = let

    -- | Given some query item (key, optional value), try to parse into the current expected type
    -- by iterating through the available parse functions.
    attemptToParseArg :: forall d. (Data d) => (ByteString, Maybe ByteString) -> [Maybe ByteString -> Either TypedAutoRouteError d] -> State.StateT Query (Either TypedAutoRouteError) d
    attemptToParseArg queryParam@(queryName, queryValue) [] = State.lift (Left NoConstructorMatched
                { field = queryName
                , value = queryValue
                , expectedType = (dataTypeOf (undefined :: d)) |> dataTypeName |> cs
                })
    attemptToParseArg queryParam@(k, v) (parseFunc:restFuncs) = case parseFunc v of
            Right result -> pure result
            -- BadType will be returned if, for example, a text is passed to a query parameter typed as int.
            Left badType@BadType{} -> State.lift (Left badType { field = k })
            -- otherwise, safe to assume the match just failed, so recurse on the rest of the functions and try to find one that matches.
            Left _ -> attemptToParseArg queryParam restFuncs

    -- | Attempt to parse the current expected type, and return its value.
    -- For the example @MyController@ this is called twice by @fromConstrM@.
    -- Once, it is called for @textArg@ where @d :: Text@. Then it is called
    -- for @intArg@ with @d ::: Int@. With both of these values parsed from the query string,
    -- the controller action is able to be created.
    nextField :: forall d. (Data d) => State.StateT Query (Either TypedAutoRouteError) d
    nextField = do
            queryParams <- State.get
            case queryParams of
                [] -> State.lift (Left TooFewArguments)
                (p@(key, value):rest) -> do
                    State.put rest
                    attemptToParseArg p (parseFuncs parseIdType)


   in case State.runStateT (fromConstrM nextField constructor) (querySortedByFields query constructor) of
        Right (x, []) -> pure x
        Right (_) -> Left TooFewArguments
        Left e -> Left e  -- runtime type error


class Data controller => AutoRoute controller where
    autoRouteWithIdType :: (?context :: RequestContext, Data idType) => (ByteString -> Maybe idType) -> Parser controller
    autoRouteWithIdType parseIdFunc =
        let
            allConstructors :: [Constr]
            allConstructors = dataTypeConstrs (dataTypeOf (Prelude.undefined :: controller))

            query :: Query
            query = queryString (getField @"request" ?context)

            paramValues :: [ByteString]
            paramValues = catMaybes $ map snd query

            parseAction :: Constr -> Parser controller
            parseAction constr = let
                    prefix :: ByteString
                    prefix = ByteString.pack (actionPrefix @controller)

                    actionName = ByteString.pack (showConstr constr)

                    actionPath :: ByteString
                    actionPath = stripActionSuffix actionName

                    allowedMethods = allowedMethodsForAction @controller actionName

                    checkRequestMethod action = do
                            method <- getMethod
                            unless (allowedMethods |> includes method) (Exception.throw UnexpectedMethodException { allowedMethods, method })
                            pure action

                    action = case applyConstr parseIdFunc constr query of
                        Right parsedAction -> pure parsedAction
                        Left e -> Exception.throw e

                in do
                    parsedAction <- string prefix >> (string actionPath <* endOfInput) *> action
                    checkRequestMethod parsedAction

        in choice (map parseAction allConstructors)

    autoRoute :: (?context :: RequestContext) => Parser controller
    autoRoute = autoRouteWithIdType (\_ -> Nothing :: Maybe Integer)

    -- | Specifies the allowed HTTP methods for a given action
    --
    -- The default implementation does a smart guess based on the
    -- usual naming conventions for controllers.
    --
    -- __Example (for default implementation):__
    --
    -- >>> allowedMethodsForAction @ProjectsController "DeleteProjectAction"
    -- [DELETE]
    --
    -- >>> allowedMethodsForAction @ProjectsController "UpdateProjectAction"
    -- [POST, PATCH]
    --
    -- >>> allowedMethodsForAction @ProjectsController "CreateProjectAction"
    -- [POST]
    --
    -- >>> allowedMethodsForAction @ProjectsController "ShowProjectAction"
    -- [GET, HEAD]
    --
    -- >>> allowedMethodsForAction @ProjectsController "HelloAction"
    -- [GET, POST, HEAD]
    --
    allowedMethodsForAction :: ByteString -> [StdMethod]
    allowedMethodsForAction actionName =
            case actionName of
                a | "Delete" `ByteString.isPrefixOf` a -> [DELETE]
                a | "Update" `ByteString.isPrefixOf` a -> [POST, PATCH]
                a | "Create" `ByteString.isPrefixOf` a -> [POST]
                a | "Show"   `ByteString.isPrefixOf` a -> [GET, HEAD]
                _ -> [GET, POST, HEAD]
    {-# INLINE allowedMethodsForAction #-}

-- | Returns the url prefix for a controller. The prefix is based on the
-- module where the controller is defined.
--
-- All controllers defined in the `Web/` directory don't have a prefix at all.
--
-- E.g. controllers in the `Admin/` directory are prefixed with @/admin/@.
actionPrefix :: forall controller. Typeable controller => String
actionPrefix =
        case moduleName of
            ('W':'e':'b':'.':_) -> "/"
            ('I':'H':'P':'.':_) -> "/"
            ("") -> "/"
            moduleName -> "/" <> let (prefix:_) = List.splitWhen (== '.') moduleName in map Char.toLower prefix <> "/"
    where
        moduleName :: String
        moduleName = Typeable.typeOf (error "unreachable" :: controller)
                |> Typeable.typeRepTyCon
                |> Typeable.tyConModule
{-# INLINE actionPrefix #-}

-- | Strips the "Action" at the end of action names
--
-- >>> stripActionSuffix "ShowUserAction"
-- "ShowUser"
--
-- >>> stripActionSuffix "UsersAction"
-- "UsersAction"
--
-- >>> stripActionSuffix "User"
-- "User"
stripActionSuffix actionName = fromMaybe actionName (stripSuffix "Action" actionName)
{-# INLINE stripActionSuffix #-}

-- | Returns the create action for a given controller.
-- Example: `createAction @UsersController == Just CreateUserAction`
createAction :: forall controller. AutoRoute controller => Maybe controller
createAction = fmap fromConstr createConstructor
    where
        createConstructor :: Maybe Constr
        createConstructor = find isCreateConstructor allConstructors

        allConstructors :: [Constr]
        allConstructors = dataTypeConstrs (dataTypeOf (Prelude.undefined :: controller))

        isCreateConstructor :: Constr -> Bool
        isCreateConstructor constructor = "Create" `isPrefixOf` showConstr constructor && ClassyPrelude.null (constrFields constructor)
{-# INLINE createAction #-}

-- | Returns the update action when given a controller and id.
-- Example: `updateAction @UsersController == Just (\id -> UpdateUserAction id)`
updateAction :: forall controller id. AutoRoute controller => Maybe (id -> controller)
updateAction =
        case updateConstructor of
            Just constructor -> Just $ \id -> buildInstance constructor id
            Nothing -> Nothing
    where
        updateConstructor :: Maybe Constr
        updateConstructor = find isUpdateConstructor allConstructors

        buildInstance :: Constr -> id -> controller
        buildInstance constructor id = State.evalState ((fromConstrM (do
                i <- State.get

                State.modify (+1)
                pure (unsafeCoerce id)
            )) constructor) 0

        allConstructors :: [Constr]
        allConstructors = dataTypeConstrs (dataTypeOf (Prelude.undefined :: controller))

        isUpdateConstructor :: Constr -> Bool
        isUpdateConstructor constructor = "Update" `isPrefixOf` (showConstr constructor) && (length (constrFields constructor) == 1)
{-# INLINE updateAction #-}

instance {-# OVERLAPPABLE #-} (AutoRoute controller, Controller controller) => CanRoute controller where
    parseRoute' = autoRoute
    {-# INLINABLE parseRoute' #-}

-- | Instances of the @QueryParam@ type class can be represented in URLs as query parameters.
-- Currently this is only Int, Text, and both wrapped in List and Maybe.
-- IDs also are representable in a URL, but we are unable to match on polymorphic types using reflection,
-- so we fall back to the default "show" for these.
class Data a => QueryParam a where
    showQueryParam :: a -> String

instance QueryParam Text where
    showQueryParam text = Text.unpack text

instance QueryParam Int where
    showQueryParam = show

instance QueryParam Integer where
    showQueryParam = show

instance QueryParam UUID where
    showQueryParam = show

instance QueryParam a => QueryParam (Maybe a) where
    showQueryParam (Just val) = showQueryParam val
    showQueryParam Nothing = ""

instance QueryParam a => QueryParam [a] where
    showQueryParam = List.intercalate "," . map showQueryParam

instance {-# OVERLAPPABLE #-} (Show controller, AutoRoute controller) => HasPath controller where
    {-# INLINABLE pathTo #-}
    pathTo !action = Text.pack (appPrefix <> actionName <> arguments)
        where
            appPrefix :: String
            !appPrefix = actionPrefix @controller

            actionName :: String
            !actionName = (stripActionSuffix $! showConstr constructor)

            constructor = toConstr action

            stripQuotes ('"':rest) = List.init rest
            stripQuotes otherwise = otherwise

            -- | The @gmapQ@ function allows us to iterate over each term in a constructor function and
            -- build a list of results from performing some function on each term.
            -- Here we send each term through @constrShow@, giving us our preferred representation for
            -- use in URLs.
            showTerms :: controller -> [Maybe String]
            showTerms = gmapQ (constrShow typeShows)

            -- | @constrShow@ tries to convert each value @d@ into a String representation.
            -- If one passes, return it immediately, otherwise try all the defined @typeShow@ functions.
            constrShow :: Data d => [(d -> Maybe String)] -> d -> Maybe String
            constrShow [] _ = Nothing
            constrShow (f:fs) d = case f d of
                Just str -> Just str
                Nothing -> constrShow fs d

            -- | Try and match some value to all of the types we can represent in a URL.
            -- Only type not contained in here is the "Id" type, since we cannot match
            -- on polymorphic types.
            typeShows :: forall d. Data d => [(d -> Maybe String)]
            typeShows = [
                \val -> (eqT :: Maybe (d :~: Text))
                    >>= \Refl -> Just (showQueryParam val),
                \val -> (eqT :: Maybe (d :~: [Text]))
                    >>= \Refl -> Just (showQueryParam (val :: [Text])),
                \val -> (eqT :: Maybe (d :~: Maybe Text))
                    >>= \Refl -> Just (showQueryParam val),
                \val -> (eqT :: Maybe (d :~: Int))
                    >>= \Refl -> Just (showQueryParam val),
                \val -> (eqT :: Maybe (d :~: [Int]))
                    >>= \Refl -> Just (showQueryParam val),
                \val -> (eqT :: Maybe (d :~: Maybe Int))
                    >>= \Refl -> Just (showQueryParam val),
                \val -> (eqT :: Maybe (d :~: Integer))
                    >>= \Refl -> Just (showQueryParam val),
                \val -> (eqT :: Maybe (d :~: [Integer]))
                    >>= \Refl -> Just (showQueryParam val),
                \val -> (eqT :: Maybe (d :~: Maybe Integer))
                    >>= \Refl -> Just (showQueryParam val),
                \val -> (eqT :: Maybe (d :~: UUID))
                    >>= \Refl -> Just (showQueryParam val),
                \val -> (eqT :: Maybe (d :~: Maybe UUID))
                    >>= \Refl -> Just (showQueryParam val),
                \val -> (eqT :: Maybe (d :~: [UUID]))
                    >>= \Refl -> Just (showQueryParam val)
                ]

            arguments :: String
            !arguments = show action -- `SomeRecord { a = b, c = d }`
                    |> List.filter (/= ' ')
                    |> List.break (== '{')
                    |> snd
                    |> List.drop 1
                    |> List.break (== '}')
                    |> fst -- `a=b,c=d`
                    |> List.splitWhen (== ',') -- ["a=b", "c=d"]
                    |> map (\s -> let (key, value) = List.break (== '=') s in (key, List.drop 1 value))
                    |> map (\(k ,v) -> (k, stripQuotes v)) -- "value" -> value
                    |> filter (\(k, v) -> (not . List.null) k && (not . List.null) v)
                    -- At this point we have a list of keys and values as represented by @show@.
                    -- For Lists and Maybe types, we want to represent these in a different way,
                    -- so we construct another list of values using type reflection and the QueryParam type class.
                    |> \(kvs :: [(String, String)]) -> zip (showTerms action) kvs
                    -- If an Id type was present in the action, it will be returned as Nothing by @showTerms@
                    -- as we are not able to match on the type using reflection.
                    -- In this case we default back to the @show@ representation.
                    |> map (\(v1, (k, v2)) -> (k, fromMaybe v2 v1))
                    |> map (\(k, v) -> if isEmpty v
                        then ""
                        else  k <> "=" <> URI.encode v)
                    |> List.filter (not . isEmpty)
                    |> List.intercalate "&"
                    |> (\q -> if List.null q then q else '?':q)

-- | Parses the HTTP Method from the request and returns it.
getMethod :: (?context :: RequestContext) => Parser StdMethod
getMethod =
        ?context
        |> IHP.Controller.RequestContext.request
        |> requestMethod
        |> parseMethod
        |> \case
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
    , ?applicationContext :: ApplicationContext
    , ?context :: RequestContext
    , Typeable application
    , Typeable action
    ) => ByteString -> action -> RouteParser
get path action = toRouteParser' do
    method <- getMethod
    case method of
        GET -> do
            string path
            pure (runAction' action)
        _   -> fail "Invalid method, expected GET"
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
    , ?applicationContext :: ApplicationContext
    , ?context :: RequestContext
    , Typeable application
    , Typeable action
    ) => ByteString -> action -> RouteParser
post path action = toRouteParser' do
    method <- getMethod
    case method of
        POST -> do
            string path
            pure (runAction' action)
        _   -> fail "Invalid method, expected POST"
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
onlyAllowMethods :: (?context :: RequestContext) => [StdMethod] -> Parser ()
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
webSocketApp :: forall webSocketApp application controller.
    ( WSApp webSocketApp
    , InitControllerContext application
    , ?application :: application
    , ?applicationContext :: ApplicationContext
    , ?context :: RequestContext
    , Typeable application
    , Typeable webSocketApp
    ) => RouteParser
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
    , ?applicationContext :: ApplicationContext
    , ?context :: RequestContext
    , Typeable application
    , Typeable webSocketApp
    , Controller webSocketApp
    ) => Parser (IO ResponseReceived)
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
webSocketAppWithCustomPath :: forall webSocketApp application controller.
    ( WSApp webSocketApp
    , InitControllerContext application
    , ?application :: application
    , ?applicationContext :: ApplicationContext
    , ?context :: RequestContext
    , Typeable application
    , Typeable webSocketApp
    ) => ByteString -> RouteParser
webSocketAppWithCustomPath path = toRouteParser $ do
        Attoparsec.char '/'
        string path
        pure (startWebSocketAppAndFailOnHTTP @webSocketApp)
{-# INLINABLE webSocketAppWithCustomPath #-}

webSocketAppWithCustomPathAndHTTPFallback :: forall webSocketApp application.
    ( WSApp webSocketApp
    , InitControllerContext application
    , ?application :: application
    , ?applicationContext :: ApplicationContext
    , ?context :: RequestContext
    , Typeable application
    , Typeable webSocketApp
    , Controller webSocketApp
    ) => ByteString -> Parser (IO ResponseReceived)
webSocketAppWithCustomPathAndHTTPFallback path = do
        Attoparsec.char '/'
        string path
        pure (startWebSocketApp @webSocketApp (runActionWithNewContext (WS.initialState @webSocketApp)))
{-# INLINABLE webSocketAppWithCustomPathAndHTTPFallback #-}


-- | Defines the start page for a router (when @\/@ is requested).
startPage :: forall action application controller. (Controller action, InitControllerContext application, ?application::application, ?applicationContext::ApplicationContext, ?context::RequestContext, Typeable application, Typeable action) => action -> RouteParser
startPage action = get (ByteString.pack (actionPrefix @action)) action
{-# INLINABLE startPage #-}

withPrefix prefix routes = string prefix >> choice (map (\r -> r <* endOfInput) routes)
{-# INLINABLE withPrefix #-}

runApp :: (?applicationContext :: ApplicationContext, ?context :: RequestContext) => RouteParser -> IO ResponseReceived -> IO ResponseReceived
runApp routes notFoundAction = do
    let path = ?context
                |> getField @"request"
                |> rawPathInfo
        handleException :: SomeException -> IO (Either String (IO ResponseReceived))
        handleException exception = pure $ Right $ ErrorController.handleRouterException exception

    routedAction :: Either String (IO ResponseReceived) <-
        (do
            res <- evaluate $ parseOnly (routes <* endOfInput) path
            case res of
                Left s -> pure $ Left s
                Right io -> do
                    (tmapSetter, controllerFn) <- io
                    pure $ Right $ controllerFn $ tmapSetter
            )
            -- pure (undefined::IO ResponseReceived)))
        `Exception.catch` handleException
    case routedAction of
        Left message -> notFoundAction
        Right action -> action
{-# INLINABLE runApp #-}

frontControllerToWAIApp :: forall app. (?applicationContext :: ApplicationContext, ?context :: RequestContext, FrontController app) => app -> [RouteParser] -> IO ResponseReceived -> IO ResponseReceived
frontControllerToWAIApp application additionalControllers notFoundAction = runApp defaultRouter notFoundAction
    where
        defaultRouter :: RouteParser = (let ?application = application in router additionalControllers)
{-# INLINABLE frontControllerToWAIApp #-}

mountFrontController :: forall frontController. (?applicationContext :: ApplicationContext, ?context :: RequestContext, FrontController frontController) => frontController -> RouteParser
mountFrontController application = let ?application = application in router []
{-# INLINABLE mountFrontController #-}

parseRoute :: forall controller application. (?applicationContext :: ApplicationContext, ?context :: RequestContext, Controller controller, CanRoute controller, InitControllerContext application, ?application :: application, Typeable application, Data controller) => RouteParser
parseRoute = toRouteParser' $ do
    action <- parseRoute' @controller
    pure $ runAction' @application action
{-# INLINABLE parseRoute #-}

parseUUIDOrTextId ::  ByteString -> Maybe Dynamic
parseUUIDOrTextId queryVal = queryVal
    |> fromASCIIBytes
    |> \case
        Just uuid -> uuid |> toDyn |> Just
        Nothing -> Nothing

parseRouteWithId
    :: forall controller application.
        (?applicationContext :: ApplicationContext,
            ?context :: RequestContext,
            Controller controller,
            CanRoute controller,
            InitControllerContext application,
            ?application :: application,
            Typeable application,
            Data controller)
        => RouteParser
parseRouteWithId = toRouteParser' do
    action <- parseRoute' @controller
    pure (runAction' @application action)

catchAll :: forall action application. (?applicationContext :: ApplicationContext, ?context :: RequestContext, Controller action, InitControllerContext application, Typeable action, ?application :: application, Typeable application, Data action) => action -> RouteParser
catchAll action = toRouteParser' do
    string (ByteString.pack (actionPrefix @action))
    _ <- takeByteString
    pure (runAction' @application action)
{-# INLINABLE catchAll #-}

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
    rawValue :: Maybe Integer = readMay (cs queryVal :: String)
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
routeParam :: (?context::RequestContext, ParamReader paramType) => ByteString -> paramType
routeParam paramName =
    let requestContext = ?context
    in
        let ?context = FrozenControllerContext { requestContext = requestContext, customFields = mempty }
        in param paramName

-- | Display a better error when the user missed to pass an argument to an action.
--
-- E.g. when you forgot to pass a projectId to the ShowProjectAction:
--
-- > <a href={ShowProjectAction}>Show project</a>
--
-- The correct code would be this:
--
-- > <a href={ShowProjectAction projectId}>Show project</a>
--
-- See https://forum.ihpapp.com/ShowThread?threadId=ad73d6a5-2481-4e2f-af46-9bf8849f998b
-- See https://github.com/digitallyinduced/ihp/issues/840
instance ((T.TypeError (T.Text "Looks like you forgot to pass a " :<>: (T.ShowType argument) :<>: T.Text " to this " :<>: (T.ShowType controller))), Data argument, Data controller) => AutoRoute (argument -> controller) where
