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
, parseTextArgument
, parseIntArgument
, parseUUIDArgument
, urlTo
, parseUUID
, parseId
, remainingText
, parseText
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
import Data.String.Conversions (cs)
import IHP.ControllerSupport
import Data.Attoparsec.ByteString.Char8 (string, Parser, (<?>), parseOnly, take, endOfInput, choice, takeTill, takeByteString)
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import GHC.TypeLits
import Data.Data
import qualified Data.UUID as UUID
import Data.Maybe (fromJust)
import qualified Text.Inflections as Inflections
import qualified Data.Either as Either
import qualified Control.Monad.State.Strict as State
import qualified Data.Text as Text
import Network.HTTP.Types.URI
import Data.List ((!!))
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

class FrontController application where
    controllers :: (?applicationContext :: ApplicationContext, ?application :: application, ?context :: RequestContext) => [Parser (IO ResponseReceived)]

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
    parseRoute' :: (?applicationContext :: ApplicationContext, ?context :: RequestContext) => Parser controller


class Data controller => AutoRoute controller where
    {-# INLINE autoRoute #-}
    autoRoute :: (?applicationContext :: ApplicationContext, ?context :: RequestContext) => Parser controller
    autoRoute  =
        let
            allConstructors :: [Constr]
            allConstructors = dataTypeConstrs (dataTypeOf (Prelude.undefined :: controller))

            parseCustomAction :: Constr -> Parser controller
            parseCustomAction constructor = string prefix >> (string actionPath <* endOfInput >> checkRequestMethod action)
                where
                    prefix :: ByteString
                    prefix = actionPrefix @controller

                    action :: controller
                    action = actionInstance constructor

                    fields :: [String]
                    fields = constrFields constructor

                    query :: Query
                    query = queryString (getField @"request" ?context)

                    actionInstance :: Constr -> controller
                    actionInstance constructor = State.evalState ((fromConstrM (do
                            i <- State.get
                            let field :: ByteString = cs (fields !! i)
                            let value :: ByteString = fromMaybe (error "AutoRoute: Param empty") $ fromMaybe (error "AutoRoute: Param missing") (lookup field query)
                            let id = parseArgument @controller field value

                            State.modify (+1)
                            pure id
                        )) constructor) 0

                    actionName = showConstr constructor

                    actionPath :: ByteString
                    actionPath = cs $! stripActionSuffix actionName

                    allowedMethods = allowedMethodsForAction @controller actionName

                    checkRequestMethod action = do
                            method <- getMethod
                            unless (allowedMethods |> includes method) (error ("Invalid method, expected one of: " <> show allowedMethods))
                            pure action
        in choice (map parseCustomAction allConstructors)

    parseArgument :: forall d. Data d => ByteString -> ByteString -> d
    parseArgument = parseUUIDArgument
    {-# INLINE parseArgument #-}

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
    allowedMethodsForAction :: String -> [StdMethod]
    allowedMethodsForAction actionName =
            case actionName of
                a | "Delete" `isPrefixOf` a -> [DELETE]
                a | "Update" `isPrefixOf` a -> [POST, PATCH]
                a | "Create" `isPrefixOf` a -> [POST]
                a | "Show"   `isPrefixOf` a -> [GET, HEAD]
                _ -> [GET, POST, HEAD]
    {-# INLINE allowedMethodsForAction #-}

-- | When the arguments for your AutoRoute based actions are not UUIDs or IDs
-- you can override the 'parseArgument' function of your 'AutoRoute' instance
-- with 'parseTextArgument' to receive them as a @Text@
--
-- __Example:__
--
-- >
-- > data HelloWorldController = HelloAction { name :: Text }
-- >     deriving (Eq, Show, Data)
-- >
-- > instance AutoRoute HelloWorldController where
-- >     parseArgument = parseTextArgument
parseTextArgument :: forall d. Data d => ByteString -> ByteString -> d
parseTextArgument field value = unsafeCoerce ((cs value) :: Text)
{-# INLINE parseTextArgument #-}

-- | When the arguments for your AutoRoute based actions are Integers instead
-- of UUIDs, you can override the 'parseArgument' function of your 'AutoRoute' instance
-- with 'parseIntArgument' to receive them as a @Int@
--
-- __Example:__
--
-- >
-- > data HelloWorldController = HelloAction { page :: Int }
-- >     deriving (Eq, Show, Data)
-- >
-- > instance AutoRoute HelloWorldController where
-- >     parseArgument = parseIntArgument
parseIntArgument :: forall d. Data d => ByteString -> ByteString -> d
parseIntArgument field value =
    value
    |> Attoparsec.parseOnly (Attoparsec.decimal <* Attoparsec.endOfInput)
    |> \case
        Right value -> unsafeCoerce value
        Left _ -> error "AutoRoute: Failed parsing Int"
{-# INLINE parseIntArgument #-}

-- | The default implementation for 'parseArgument' in 'AutoRoute'.
parseUUIDArgument :: forall d. Data d => ByteString -> ByteString -> d
parseUUIDArgument field value =
    value
    |> fromASCIIBytes
    |> fromMaybe (error "AutoRoute: Failed parsing argument as UUID. You most likely are trying to use AutoRoute with a non-UUID attribute. See https://ihp.digitallyinduced.com/Guide/routing.html#parameter-types for details on how to configure this.")
    |> unsafeCoerce
{-# INLINE parseUUIDArgument #-}

-- | Returns the url prefix for a controller. The prefix is based on the
-- module where the controller is defined.
-- 
-- All controllers defined in the `Web/` directory don't have a prefix at all.
--
-- E.g. controllers in the `Admin/` directory are prefixed with @/admin/@.
actionPrefix :: forall controller. Typeable controller => ByteString
actionPrefix =
        case appModule of
            "Web" -> "/"
            "IHP" -> "/"
            "" -> "/"
            appName -> "/" <> ByteString.map Char.toLower appName <> "/"
    where
        appModule :: ByteString
        appModule = fromMaybe "" (headMay moduleParts)

        moduleParts :: [ByteString]
        moduleParts = ByteString.split '.' moduleName

        moduleName :: ByteString
        moduleName = Typeable.typeOf (error "unreachable" :: controller)
                |> Typeable.typeRepTyCon
                |> Typeable.tyConModule
                |> cs
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
    {-# INLINE parseRoute' #-}
    parseRoute' = autoRoute

instance {-# OVERLAPPABLE #-} (Show controller, AutoRoute controller) => HasPath controller where
    {-# INLINE pathTo #-}
    pathTo !action = appPrefix <> actionName <> cs arguments
        where
            appPrefix :: Text 
            !appPrefix = cs (actionPrefix @controller)

            actionName :: Text
            !actionName = cs (stripActionSuffix $! showConstr constructor) 

            constructor = toConstr action
            arguments :: ByteString
            !arguments  = tshow action -- `SomeRecord { a = b, c = d }`
                    |> Text.breakOn "{"
                    |> snd
                    |> Text.drop 1
                    |> Text.breakOn "}"
                    |> fst -- ` a = b, c = d`
                    |> Text.splitOn "," -- [" a = b", " c = d "]
                    |> map (\s -> let (key, value) = Text.breakOn "=" s in (Text.strip key, Text.strip (Text.drop 1 value)))
                    |> map (\(k ,v) -> (k, Text.dropAround (== '"') v)) -- "value" -> value
                    |> map (\(k, v) -> (cs k, cs v))
                    |> filter (\(k, v) -> (not . ClassyPrelude.null) k && (not . ClassyPrelude.null) v)
                    |> (\q -> if ClassyPrelude.null q then mempty else renderSimpleQuery True q)


-- | Parses the HTTP Method from the request and returns it.
getMethod :: (?context :: RequestContext) => Parser StdMethod
getMethod = 
        ?context
        |> IHP.Controller.RequestContext.request
        |> requestMethod
        |> parseMethod
        |> \case
            Left error -> fail (cs error)
            Right method -> pure method
{-# INLINE getMethod #-}

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
    ) => ByteString -> action -> Parser (IO ResponseReceived)
get path action = do
    method <- getMethod
    case method of 
        GET -> do
            string path
            pure (runActionWithNewContext action)
        _   -> fail "Invalid method, expected GET"
{-# INLINE get #-}

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
    ) => ByteString -> action -> Parser (IO ResponseReceived)
post path action = do
    method <- getMethod
    case method of 
        POST -> do
            string path
            pure (runActionWithNewContext action)
        _   -> fail "Invalid method, expected POST"
{-# INLINE post #-}

-- | Defines the start page for a router (when @\/@ is requested).
startPage :: (Controller action, InitControllerContext application, ?application::application, ?applicationContext::ApplicationContext, ?context::RequestContext, Typeable application, Typeable action) => action -> Parser (IO ResponseReceived)
startPage action = get "/" action
{-# INLINE startPage #-}

withPrefix prefix routes = string prefix >> choice (map (\r -> r <* endOfInput) routes)
{-# INLINE withPrefix #-}

runApp :: (?applicationContext :: ApplicationContext, ?context :: RequestContext) => Parser (IO ResponseReceived) -> IO ResponseReceived -> IO ResponseReceived
runApp routes notFoundAction = do
    let path = ?context
                |> getField @"request"
                |> rawPathInfo
        handleException exception = pure $ Right (ErrorController.handleRouterException exception)

    routedAction <- (evaluate $ parseOnly (routes <* endOfInput) path) `Exception.catch` handleException
    case routedAction of
        Left message -> notFoundAction
        Right action -> action
{-# INLINE runApp #-}

frontControllerToWAIApp :: forall app parent config controllerContext. (?applicationContext :: ApplicationContext, ?context :: RequestContext, FrontController app) => app -> IO ResponseReceived -> IO ResponseReceived
frontControllerToWAIApp application notFoundAction = runApp (choice (map (\r -> r <* endOfInput) (let ?application = application in controllers))) notFoundAction
{-# INLINE frontControllerToWAIApp #-}

mountFrontController :: forall frontController application. (?applicationContext :: ApplicationContext, ?context :: RequestContext, FrontController frontController) => frontController -> Parser (IO ResponseReceived)
mountFrontController application = let ?application = application in choice (map (\r -> r <* endOfInput) controllers)
{-# INLINE mountFrontController #-}

parseRoute :: forall controller application. (?applicationContext :: ApplicationContext, ?context :: RequestContext, Controller controller, CanRoute controller, InitControllerContext application, ?application :: application, Typeable application, Data controller) => Parser (IO ResponseReceived)
parseRoute = parseRoute' @controller >>= pure . runActionWithNewContext @application
{-# INLINE parseRoute #-}

catchAll :: forall action application. (?applicationContext :: ApplicationContext, ?context :: RequestContext, Controller action, InitControllerContext application, Typeable action, ?application :: application, Typeable application, Data action) => action -> Parser (IO ResponseReceived)
catchAll action = do
    string (actionPrefix @action)
    _ <- takeByteString
    pure (runActionWithNewContext @application action)
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
{-# INLINE parseUUID #-}

-- | Parses an UUID, afterwards wraps it in an Id
parseId :: ((ModelSupport.PrimaryKey (ModelSupport.GetTableName record)) ~ UUID) => Parser (ModelSupport.Id record)
parseId = ModelSupport.Id <$> parseUUID
{-# INLINE parseId #-}

-- | Returns all the remaining text until the end of the input
remainingText :: Parser Text
remainingText = cs <$> takeByteString
{-# INLINE remainingText #-}

-- | Parses until the next @/@
parseText :: Parser Text
parseText = cs <$> takeTill ('/' ==)
{-# INLINE parseText #-}
