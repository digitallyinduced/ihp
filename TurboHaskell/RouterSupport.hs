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
    , FrontControllerPrefix (..)
    , ControllerApplicationMap
    , parseRoute 
    , catchAll
    , mountFrontController
    , createAction
    , updateAction
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
import Data.Attoparsec.Char8 (string, Parser, (<?>), parseOnly, take, endOfInput, choice, takeTill)
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

class FrontControllerPrefix application where
    prefix :: ByteString
    prefix = "/"

class FrontController application where
    controllers :: (?applicationContext :: ApplicationContext, ?requestContext :: RequestContext) => [Parser (IO ResponseReceived)]

class (FrontControllerPrefix (ControllerApplicationMap controller)) => HasPath controller where
    pathTo :: controller -> Text    

class HasPath controller => CanRoute controller where
    parseRoute' :: (?applicationContext :: ApplicationContext, ?requestContext :: RequestContext) => Parser controller


-- Maps models to their restful controllers
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
            parseCustomAction constructor = (string actionPath <* endOfInput >> checkRequestMethod action)
                where
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
                            let value :: ByteString = fromMaybe (error "AutoRoute: Param empty") $ fromMaybe (error "AutoRoute: Param missign") (lookup field query)
                            let id :: UUID = fromMaybe (error "AutoRoute: Failed parsing UUID") (fromASCIIBytes value)

                            State.modify (+1)
                            pure (unsafeCoerce id)
                        )) constructor) 0

                    actionName = showConstr constructor

                    actionPath :: ByteString
                    actionPath = cs $! stripActionSuffix actionName

                    allowedMethods :: [StdMethod]
                    allowedMethods =
                            case actionName of
                                a | ("Delete" `isPrefixOf` a) -> [DELETE]
                                a | ("Update" `isPrefixOf` a) -> [POST, PATCH]
                                a | ("Create" `isPrefixOf` a) -> [POST]
                                _ -> [GET, POST]

                    checkRequestMethod action = do
                            method <- getMethod
                            if method `elem` allowedMethods
                                then pure action
                                else error ("Invalid method, expected one of: " <> show allowedMethods)
        in choice (map parseCustomAction allConstructors)
            

stripActionSuffix actionName = fromMaybe actionName (stripSuffix "Action" actionName)

-- | Returns the create action for a given controller.
-- Example: `createAction @UsersController == Just CreateUserAction`
createAction :: forall controller. AutoRoute controller => Maybe controller
createAction = maybe Nothing (Just . fromConstr) createConstructor
    where
        createConstructor :: Maybe Constr
        createConstructor = find isCreateConstructor allConstructors

        allConstructors :: [Constr]
        allConstructors = dataTypeConstrs (dataTypeOf (ClassyPrelude.undefined :: controller))

        isCreateConstructor :: Constr -> Bool
        isCreateConstructor constructor = "Create" `isPrefixOf` (showConstr constructor) && ClassyPrelude.null (constrFields constructor)


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

instance {-# OVERLAPPABLE #-} (AutoRoute controller, Controller controller, FrontControllerPrefix (ControllerApplicationMap controller)) => CanRoute controller where
    {-# INLINE parseRoute' #-}
    parseRoute' = autoRoute

instance {-# OVERLAPPABLE #-} (Show controller, AutoRoute controller, FrontControllerPrefix (ControllerApplicationMap controller)) => HasPath controller where
    {-# INLINE pathTo #-}
    pathTo !action = appPrefix <> actionName <> cs arguments
        where
            appPrefix :: Text 
            !appPrefix = cs (prefix @(ControllerApplicationMap controller))

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
                    |> map (\(k, v) -> (cs k, cs v))
                    |> filter (\(k, v) -> (not . ClassyPrelude.null) k && (not . ClassyPrelude.null) v)
                    |> (\q -> (if ClassyPrelude.null q then mempty else renderSimpleQuery True q))


{-# INLINE getMethod #-}
getMethod :: (?requestContext :: RequestContext) => Parser StdMethod
getMethod = 
    let methodOrError = parseMethod (requestMethod (TurboHaskell.Controller.RequestContext.request ?requestContext))
    in
        case methodOrError of
            Left error -> fail (cs error)
            Right method -> pure method

withMethod :: (?requestContext :: RequestContext) => StdMethod -> RequestContext
withMethod requestMethod = (?requestContext) { request = newRequest }
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
    (case method of
                    GET  -> pure getResult
                    POST -> pure postResult
                    _    -> fail "Invalid method, expected GET or POST"
                )

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
runApp :: (?applicationContext :: ApplicationContext, ?requestContext :: RequestContext) => Parser (IO ResponseReceived) -> IO ResponseReceived
runApp routes = let path = (rawPathInfo (getField @"request" ?requestContext)) in case parseOnly (routes <* endOfInput) path of
            Left message -> error ("Failed to route `" <> cs path <> "`: " <> message)
            Right action -> action

{-# INLINE frontControllerToWAIApp #-}
frontControllerToWAIApp :: forall app parent config controllerContext. (Eq app, ?applicationContext :: ApplicationContext, ?requestContext :: RequestContext, FrontController app, FrontControllerPrefix app) => IO ResponseReceived
frontControllerToWAIApp = runApp (withPrefix (prefix @app) (controllers @app))

{-# INLINE mountFrontController #-}
mountFrontController :: forall frontController. (?applicationContext :: ApplicationContext, ?requestContext :: RequestContext, FrontController frontController, FrontControllerPrefix frontController) => Parser (IO ResponseReceived)
mountFrontController = withPrefix (prefix @frontController) (controllers @frontController)

{-# INLINE parseRoute #-}
parseRoute :: forall controller parent. (?applicationContext :: ApplicationContext, ?requestContext :: RequestContext, Controller controller, CanRoute controller, InitControllerContext (ControllerApplicationMap controller)) => Parser (IO ResponseReceived)
parseRoute = parseRoute' @controller >>= pure . runActionWithNewContext

{-# INLINE catchAll #-}
catchAll :: (?applicationContext :: ApplicationContext, ?requestContext :: RequestContext, Controller action, InitControllerContext (ControllerApplicationMap action)) => action -> Parser (IO ResponseReceived)
catchAll action = pure (runActionWithNewContext action)

-- | Parses a text until the next `/`
parseText :: Parser Text
parseText = takeTill ((==) '/') >>= return . cs

-- | Parses an UUID-based Id (e.g. user id, project id)
parseId = parseUUID >>= pure . ModelSupport.Id

-- | Parses a UUID. Use `parseId` if you need an `Id model`.
parseUUID :: forall m. Parser UUID
parseUUID = do
    uuid <- take 36
    case fromASCIIBytes uuid of
        Just uuid -> pure uuid
        Nothing -> fail "parsing uuid failed"