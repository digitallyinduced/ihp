{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances #-}
module TurboHaskell.RouterSupport (
    CanRoute (..)
    , HasPath (..)
    , AutoRoute (..)
    , runAction
    , get
    , post
    , frontControllerToWAIApp
    , withPrefix
    , ModelControllerMap
    , FrontController (..)
    , parseRoute 
    , catchAll
    , mountFrontController
    , createAction
    , updateAction
    , parseTextArgument
) where

import ClassyPrelude hiding (index, delete, take)
import qualified TurboHaskell.ModelSupport as ModelSupport
import TurboHaskell.ApplicationContext
import Data.UUID
import Network.HTTP.Types.Method
import GHC.Records
import TurboHaskell.Controller.RequestContext
import Network.Wai
import Data.String.Conversions (cs)
import TurboHaskell.ControllerSupport
import Data.Attoparsec.Char8 (string, Parser, (<?>), parseOnly, take, endOfInput, choice, takeTill, takeByteString)
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
import TurboHaskell.HaskellSupport hiding (get)
import qualified Data.Typeable as Typeable
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Char as Char

class FrontController application where
    controllers :: (?applicationContext :: ApplicationContext, ?application :: Proxy application, ?requestContext :: RequestContext) => [Parser (IO ResponseReceived)]

class HasPath controller where
    pathTo :: controller -> Text    

class HasPath controller => CanRoute controller where
    parseRoute' :: (?applicationContext :: ApplicationContext, ?requestContext :: RequestContext) => Parser controller


-- | Maps models to their restful controllers
-- E.g. ModelControllerMap ControllerContext User = UsersController
type family ModelControllerMap controllerContext model

class Data controller => AutoRoute controller where
    {-# INLINE autoRoute #-}
    autoRoute :: (?applicationContext :: ApplicationContext, ?requestContext :: RequestContext) => Parser controller
    autoRoute  =
        let
            allConstructors :: [Constr]
            allConstructors = dataTypeConstrs (dataTypeOf (ClassyPrelude.undefined :: controller))

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
                    query = queryString (getField @"request" ?requestContext)

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
                            if method `elem` allowedMethods
                                then pure action
                                else error ("Invalid method, expected one of: " <> show allowedMethods)
        in choice (map parseCustomAction allConstructors)

    parseArgument :: forall d. Data d => ByteString -> ByteString -> d
    parseArgument field value =
        value
        |> fromASCIIBytes
        |> fromMaybe (error "AutoRoute: Failed parsing UUID")
        |> unsafeCoerce
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
    -- >>> allowedMethodsForAction @ProjectsController "HelloAction"
    -- [GET, POST]
    --
    allowedMethodsForAction :: String -> [StdMethod]
    allowedMethodsForAction actionName =
            case actionName of
                a | "Delete" `isPrefixOf` a -> [DELETE]
                a | "Update" `isPrefixOf` a -> [POST, PATCH]
                a | "Create" `isPrefixOf` a -> [POST]
                _ -> [GET, POST]
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

-- | Returns the create action for a given controller.
-- Example: `createAction @UsersController == Just CreateUserAction`
createAction :: forall controller. AutoRoute controller => Maybe controller
createAction = fmap fromConstr createConstructor
    where
        createConstructor :: Maybe Constr
        createConstructor = find isCreateConstructor allConstructors

        allConstructors :: [Constr]
        allConstructors = dataTypeConstrs (dataTypeOf (ClassyPrelude.undefined :: controller))

        isCreateConstructor :: Constr -> Bool
        isCreateConstructor constructor = "Create" `isPrefixOf` showConstr constructor && ClassyPrelude.null (constrFields constructor)


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
        allConstructors = dataTypeConstrs (dataTypeOf (ClassyPrelude.undefined :: controller))

        isUpdateConstructor :: Constr -> Bool
        isUpdateConstructor constructor = "Update" `isPrefixOf` (showConstr constructor) && (length (constrFields constructor) == 1)

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

{-# INLINE getMethod #-}
getMethod :: (?requestContext :: RequestContext) => Parser StdMethod
getMethod = 
    let methodOrError = parseMethod (requestMethod (TurboHaskell.Controller.RequestContext.request ?requestContext))
    in
        case methodOrError of
            Left error -> fail (cs error)
            Right method -> pure method

withMethod :: (?requestContext :: RequestContext) => StdMethod -> RequestContext
withMethod requestMethod = ?requestContext { request = newRequest }
    where
        newRequest = (TurboHaskell.Controller.RequestContext.request ?requestContext) { requestMethod = renderStdMethod requestMethod }

{-# INLINE post #-}
post action = do
    method <- getMethod
    case method of 
        POST -> pure action
        _   -> fail "Invalid method, expected POST"

{-# INLINE get #-}
get action = do
    method <- getMethod
    case method of 
        GET -> pure action
        _   -> fail "Invalid method, expected GET"

{-# INLINE onGetOrPost #-}
onGetOrPost getResult postResult = do
    method <- getMethod
    case method of
        GET  -> pure getResult
        POST -> pure postResult
        _    -> fail "Invalid method, expected GET or POST"

{-# INLINE onGetOrPostOrDelete #-}
onGetOrPostOrDelete getResult postResult deleteResult = do
    method <- getMethod
    case method of
        GET    -> pure getResult
        POST   -> pure postResult
        DELETE -> pure deleteResult
        _      -> fail "Invalid method, expected GET, POST or DELETE"



{-# INLINE withPrefix #-}
withPrefix prefix routes = string prefix >> choice (map (\r -> r <* endOfInput) routes)

{-# INLINE runApp #-}
runApp :: (?applicationContext :: ApplicationContext, ?requestContext :: RequestContext) => Parser (IO ResponseReceived) -> IO ResponseReceived -> IO ResponseReceived
runApp routes notFoundAction =
    let
        path = ?requestContext
                |> getField @"request"
                |> rawPathInfo
    in case parseOnly (routes <* endOfInput) path of
            Left message -> notFoundAction
            Right action -> action

{-# INLINE frontControllerToWAIApp #-}
frontControllerToWAIApp :: forall app parent config controllerContext. (Eq app, ?applicationContext :: ApplicationContext, ?requestContext :: RequestContext, FrontController app) => IO ResponseReceived -> IO ResponseReceived
frontControllerToWAIApp notFoundAction = runApp (choice (map (\r -> r <* endOfInput) (let ?application = Proxy @app in controllers))) notFoundAction

{-# INLINE mountFrontController #-}
mountFrontController :: forall frontController. (?applicationContext :: ApplicationContext, ?requestContext :: RequestContext, FrontController frontController) => Parser (IO ResponseReceived)
mountFrontController = choice (map (\r -> r <* endOfInput) (let ?application = Proxy @frontController in controllers))

{-# INLINE parseRoute #-}
parseRoute :: forall controller application. (?applicationContext :: ApplicationContext, ?requestContext :: RequestContext, Controller controller, CanRoute controller, InitControllerContext application, ?application :: Proxy application) => Parser (IO ResponseReceived)
parseRoute = parseRoute' @controller >>= pure . runActionWithNewContext @application

{-# INLINE catchAll #-}
catchAll :: forall action application. (?applicationContext :: ApplicationContext, ?requestContext :: RequestContext, Controller action, InitControllerContext application, Typeable action, ?application :: Proxy application) => action -> Parser (IO ResponseReceived)
catchAll action = do
    string (actionPrefix @action)
    _ <- takeByteString
    pure (runActionWithNewContext @application action)
