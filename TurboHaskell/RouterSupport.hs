{-# LANGUAGE FunctionalDependencies, AllowAmbiguousTypes, UndecidableInstances, TypeOperators, TypeFamilyDependencies #-}
module TurboHaskell.RouterSupport (
    (:>)(..)
    , CanRoute (..)
    , HasPath (..)
    , RestfulController (..)
    , runAction
    , get
    , post
    , frontControllerToWAIApp
    , RestfulControllerId
    , withPrefix
    , parseUUID
    , parsePathArgument
    , isIndexAction
    , isShowAction
    , isCreateAction
    , isUpdateAction
    , isDeleteAction
    , isEditAction
    , modelId
    , Child
    , Parent
    , PathArgument (..)
    , ModelControllerMap
    , FrontController (..)
    , FrontControllerPrefix (..)
    , ControllerApplicationMap
    , parseRoute 
    , catchAll
    , mountFrontController
    , strippedControllerName
) where

import ClassyPrelude hiding (index, delete, take)
import qualified TurboHaskell.ModelSupport as ModelSupport
import TurboHaskell.ApplicationContext
import Data.UUID
import           Network.HTTP.Types.Method
import GHC.Records
import TurboHaskell.Controller.RequestContext
import Network.Wai
import Control.Lens hiding (index, (:>), Context)
import Data.Generics.Product hiding (getField)
import Data.String.Conversions (cs)
import Data.Proxy
import TurboHaskell.ControllerSupport
import Data.Attoparsec.Char8 (string, Parser, (<?>), parseOnly, take, endOfInput, choice, takeTill)
import Data.Typeable (typeRep)
import GHC.Generics
import GHC.TypeLits
import Data.Data
import TurboHaskell.NameSupport
import qualified Data.UUID as UUID
import Data.Default

import Data.Generics.Product hiding (getField)
import Data.Maybe (fromJust)
import qualified Control.Newtype.Generics as Newtype
import qualified Text.Inflections as Inflections
import qualified Data.Either as Either
import qualified Text.Countable as Countable

type family Parent controller where
    Parent (parent :> child) = parent
    Parent parent = parent

type family Child controller where
    Child (parent :> child) = child
    Child child = child

data (parent :> child) = parent :> child deriving (Generic, Eq, Data, Show)

class FrontControllerPrefix application where
    prefix :: ByteString
    prefix = "/"

class FrontController application where
    controllers :: (?applicationContext :: ApplicationContext, ?requestContext :: RequestContext) => [Parser (IO ResponseReceived)]

class (FrontControllerPrefix (ControllerApplicationMap controller)) => HasPath controller where
    pathTo :: controller -> Text    

class HasPath controller => CanRoute controller parent | controller -> parent where
    parseRoute' :: (?applicationContext :: ApplicationContext, ?requestContext :: RequestContext) => Parser controller

{-# INLINE parseUUID #-}
parseUUID :: Parser UUID
parseUUID = do
        uuid <- take 36
        case fromASCIIBytes uuid of 
            Just theUUID -> return $! theUUID
            Nothing -> fail "not uuid"

type family RestfulControllerId controller where
    RestfulControllerId (parent :> child) = RestfulControllerId child
    RestfulControllerId controller = HeadOrDefault (RestfulControllerId' (Rep controller)) ()

type family RestfulControllerId' (x :: * -> *) where
    RestfulControllerId' (D1 _d d) = RestfulControllerId' d
    RestfulControllerId' (C1 _c c) = RestfulControllerId' c
    RestfulControllerId' (a :+: b) = Concat (RestfulControllerId' a) (RestfulControllerId' b)
    -- RestfulControllerId' (x :+: (C1 _c U1)) = RestfulControllerId' x
    --RestfulControllerId' ((C1 _c c) :+: b) = RestfulControllerId' c
    -- RestfulControllerId' (a :*: b) = 
    RestfulControllerId' (S1 _s (K1 _i b)) = '[b]
    RestfulControllerId' U1 = '[]
    -- (C1 _c (S1 _s (K1 _i b)))

type family HeadOrDefault x d where
    HeadOrDefault (x ': xs) d = x
    HeadOrDefault otherwise d = d

type family Concat a b where
    Concat a '[] = a
    Concat a (x ': xs) = Concat (x ': a) xs


-- Maps models to their restful controllers
-- E.g. ModelControllerMap ControllerContext User = UsersController
type family ModelControllerMap controllerContext model

{-# INLINE getConstructorByName #-}
getConstructorByName :: forall theType. Data theType => String -> Maybe Constr
getConstructorByName name = readConstr (dataTypeOf (ClassyPrelude.undefined :: theType)) name

singularize "BrainWaves" = "BrainWave"
singularize word = Countable.singularize word

{-# INLINE constructorWithId #-}
constructorWithId :: forall controller. (RestfulController controller, Data (Child controller), Data (RestfulControllerId controller)) => Text -> Maybe (RestfulControllerId controller -> Child controller)
constructorWithId name =
    case getConstructorByName @(Child controller) (cs (name <> (singularize $ cs (strippedControllerName @controller)) <> "Action")) of
        Just constructor -> Just (\id -> fromJust $ fromConstrM (cast id :: forall d. Data d => Maybe d) constructor)
        Nothing -> Nothing


{-# INLINE strippedControllerName #-}
strippedControllerName :: forall controller. Typeable (Child controller) => Text
strippedControllerName = fromMaybe controllerName (stripSuffix "Controller" controllerName)
    where
        controllerName = tshow (typeRep (Proxy :: Proxy (Child controller)))

class (Typeable controller, Generic controller, Data controller, Data (Child controller), Data (RestfulControllerId controller)) => RestfulController controller where
    {-# INLINE basePath #-}
    basePath :: ByteString
    basePath =
        let controllerName = tshow $ typeRep (Proxy :: Proxy (Child controller))
        in controllerNameToPathName controllerName
    {-# INLINE indexAction #-}
    indexAction :: Maybe (Child controller)
    indexAction = fromConstr <$> getConstructorByName @(Child controller) (cs (strippedControllerName @controller <> "Action"))
    {-# INLINE newAction #-}
    newAction :: Maybe (Child controller)
    newAction = fromConstr <$> getConstructorByName @(Child controller) (cs ("New" <> (singularize $ cs (strippedControllerName @controller)) <> "Action"))
    {-# INLINE createAction #-}
    createAction :: Maybe (Child controller)
    createAction = fromConstr <$> getConstructorByName @(Child controller) (cs ("Create" <> (singularize $ cs (strippedControllerName @controller)) <> "Action"))
    {-# INLINE showAction #-}
    showAction :: Maybe (RestfulControllerId controller -> Child controller)
    showAction = constructorWithId @controller "Show"
    {-# INLINE editAction #-}
    editAction :: Maybe (RestfulControllerId controller -> Child controller)
    editAction = constructorWithId @controller "Edit"
    {-# INLINE updateAction #-}
    updateAction :: Maybe (RestfulControllerId controller -> Child controller)
    updateAction = constructorWithId @controller "Update"
    {-# INLINE deleteAction #-}
    deleteAction :: Maybe (RestfulControllerId controller -> Child controller)
    deleteAction = constructorWithId @controller "Delete"
    {-# INLINE customActions #-}
    customActions :: (?applicationContext :: ApplicationContext, ?requestContext :: RequestContext, HasTypes (Child controller) (RestfulControllerId controller)) => Maybe (Child controller) -> Parser controller
    customActions idContainer =
        let
            id :: Maybe (RestfulControllerId controller)
            id = case idContainer of
                Just id -> modelId @controller id
                Nothing -> Nothing
            allConstructors = dataTypeConstrs (dataTypeOf (ClassyPrelude.undefined :: Child controller))
            customConstructors = filter (not . isRestConstructor) allConstructors
            isRestConstructor constructor = (cs (showConstr constructor)) `elem` restConstructorNames
            controllerName = strippedControllerName @controller
            restConstructorNames =
                    [ controllerName <> "Action"
                    , "New" <> singularControllerName <> "Action"
                    , "Create" <> singularControllerName <> "Action"
                    , "Show" <> singularControllerName <> "Action"
                    , "Edit" <> singularControllerName <> "Action"
                    , "Update" <> singularControllerName <> "Action"
                    , "Delete" <> singularControllerName <> "Action"
                    ]
                        where
                            singularControllerName = singularize controllerName
            parseCustomAction action' = (string actionPath <* endOfInput >> onGetOrPost action action)
                where
                    action = initiateAction action' id
                    initiateAction constructor id = 
                        case id of
                            Just id -> fromMaybe (error $ "Could not find constructor " <> show constructor) $ fromConstrM (cast id :: forall d. Data d => Maybe d) constructor
                            Nothing -> fromConstr constructor
                    actionName = showConstr action'
                    withoutActionSuffix = fromMaybe actionName (stripSuffix "Action" actionName)
                    modelName = cs $ singularize $ cs controllerName
                    withoutModelPrefix = fromMaybe withoutActionSuffix (stripPrefix modelName withoutActionSuffix)
                    actionPath = controllerNameToPathName (cs withoutModelPrefix)
        in choice (map parseCustomAction customConstructors)
            

-- controllerNameToPathName "XController" = "X"
{-# INLINE controllerNameToPathName #-}
controllerNameToPathName :: Text -> ByteString
controllerNameToPathName controllerName = cs (Either.fromRight baseName (Inflections.toDashed baseName))
    where
        baseName = (fromMaybe controllerName (stripSuffix "Controller" controllerName))

class PathArgument a where
    parsePathArgument :: Parser a

instance PathArgument () where
    {-# INLINE parsePathArgument #-}
    parsePathArgument = string "current" >> return ()

instance PathArgument (ModelSupport.Id' (model :: Symbol)) where
    {-# INLINE parsePathArgument #-}
    parsePathArgument = parseUUID >>= return . Newtype.pack

instance PathArgument Text where
    {-# INLINE parsePathArgument #-}
    parsePathArgument = takeTill ((==) '/') >>= return . cs


instance {-# OVERLAPPABLE #-} forall id controller parent child. (Eq controller, Generic controller, Show id, Show controller, PathArgument id, RestfulController controller, RestfulControllerId controller ~ id, Controller controller, parent ~ (), Child controller ~ controller, HasTypes controller id, Default id, FrontControllerPrefix (ControllerApplicationMap controller)) => CanRoute controller parent where
    --pathTo action | action == indexAction = "/Members"
    --pathTo action | action == newAction = pathTo (indexAction @controller) <> "/new"
    --pathTo action | action == createAction = pathTo (indexAction @controller)
    --pathTo action = error "TODO"
        --let id = unsafeHead (toListOf (types @id) action)
        --in pathTo (showAction @controller id) <> "/" <> tshow id <> (if editAction id == action then "/edit" else "")
    {-# INLINE parseRoute' #-}
    parseRoute' =
        let
            indexAction' = fromMaybe (error "parseRoute': Failed to locate index action") (indexAction @controller)
            newAction' = fromMaybe (error "parseRoute': Failed to locate new action") (newAction @controller)
            createAction' = fromMaybe (error "parseRoute': Failed to locate create action") (createAction @controller)
            showAction' :: RestfulControllerId controller -> Child controller
            showAction' memberId = fromMaybe (error "parseRoute': Failed to locate show action") (showAction @controller) memberId
            updateAction' :: RestfulControllerId controller -> Child controller
            updateAction' memberId = fromJust (updateAction @controller) $ memberId
            deleteAction' :: RestfulControllerId controller -> Child controller
            deleteAction' memberId = fromJust (deleteAction @controller) $ memberId
            editAction' :: RestfulControllerId controller -> Child controller
            editAction' memberId = fromJust (editAction @controller) $ memberId
        in (string (basePath @controller)) >> (
            string "/" >> ((string "new" <* endOfInput >> get (newAction'))
                <|> (do
                    memberId <- parsePathArgument
                    let edit = string "edit" >> get (editAction' memberId)
                    let custom = customActions (Just (showAction' memberId))
                    (string "/" >> (custom <|> edit))
                        <|> (onGetOrPostOrDelete (showAction' memberId) (updateAction' memberId) (deleteAction' memberId))
                ))
            )
            <|> (string "/" >> customActions Nothing)
            <|> onGetOrPost (indexAction') (createAction')


instance {-# OVERLAPPABLE #-} forall id controller parent child. (Eq controller, Eq child, Generic controller, Show id, PathArgument id, RestfulController controller, RestfulControllerId controller ~ id, parent ~ Parent controller, controller ~ (parent :> Child controller), child ~ Child controller, HasPath parent, HasTypes child id, Child child ~ child, Show child, Show controller, Default id, FrontControllerPrefix (ControllerApplicationMap parent), FrontControllerPrefix (ControllerApplicationMap controller)) => HasPath (parent :> child) where
    {-# INLINE pathTo #-}
    pathTo (parent :> child) = pathTo parent <> "/" <> genericPathTo @controller child




instance {-# OVERLAPPABLE #-} forall id controller parent child. (Eq controller, Generic controller, Show id, Show controller, PathArgument id, RestfulController controller, RestfulControllerId controller ~ id, Child controller ~ controller, HasTypes controller id, Default id, FrontControllerPrefix (ControllerApplicationMap controller)) => HasPath controller where
    {-# INLINE pathTo #-}
    pathTo action = (cs (prefix @(ControllerApplicationMap controller))) <> genericPathTo @controller action

{-# INLINE genericPathTo #-}
genericPathTo :: forall controller action id parent frontController. (Eq action, Generic controller, Show id, Show controller, PathArgument id, RestfulController controller, RestfulControllerId controller ~ id, HasTypes action id, RestfulController controller, Child controller ~ action, Default id) => action -> Text
genericPathTo action = genericPathTo' action
    where
        indexBasePath = (maybe (cs (basePath @controller)) (\indexAction -> genericPathTo @controller indexAction) (indexAction @controller))
        genericPathTo' action
            | (isIndexAction @controller action) || (isCreateAction @controller action)
                = cs (basePath @controller)
            | isNewAction @controller action
                = indexBasePath <> "/new"
            | isEditAction @controller action
                = let id = unsafeHead (toListOf (types @id) action)
                in
                    (maybe (indexBasePath <> "/edit") (\showAction -> genericPathTo @controller (showAction id)) (showAction @controller)) <> "/edit"
                    
            | (isShowAction @controller action) || (isDeleteAction @controller action) || (isUpdateAction @controller action)
                = let
                    id = headMay (toListOf (types @id) action)
                in
                    indexBasePath <> "/" <> maybe "current" tshow id
            | otherwise =
                let
                    id = headMay (toListOf (types @id) action)
                    actionName = showConstr (toConstr action)
                    withoutActionSuffix = fromMaybe actionName (stripSuffix "Action" actionName)
                    modelName = cs $ singularize $ cs (strippedControllerName @controller)
                    withoutModelPrefix = fromMaybe withoutActionSuffix (stripPrefix modelName withoutActionSuffix)
                in
                    indexBasePath <> "/" <> maybe "" (\id -> tshow id <> "/") id <> (cs $ controllerNameToPathName (cs withoutModelPrefix))

{-# INLINE isIndexAction #-}
isIndexAction :: forall controller. (RestfulController controller, Eq (Child controller)) => Child controller -> Bool
isIndexAction action = (isJust (indexAction @controller) && action == fromJust (indexAction @controller))

{-# INLINE isCreateAction #-}
isCreateAction :: forall controller. (RestfulController controller, Eq (Child controller)) => Child controller -> Bool
isCreateAction action = (isJust (createAction @controller) && action == fromJust (createAction @controller))

{-# INLINE isNewAction #-}
isNewAction :: forall controller. (RestfulController controller, Eq (Child controller)) => Child controller -> Bool
isNewAction action = (isJust (newAction @controller) && action == fromJust (newAction @controller))

{-# INLINE isEditAction #-}
isEditAction :: forall controller. (RestfulController controller, Eq (Child controller), Default (RestfulControllerId controller)) => Child controller -> Bool
isEditAction action = (isJust (editAction @controller) && toConstr action == toConstr (fromJust (editAction @controller) $ def))

{-# INLINE isShowAction #-}
isShowAction :: forall controller. (RestfulController controller, Eq (Child controller), Default (RestfulControllerId controller)) => Child controller -> Bool
isShowAction action = (isJust (showAction @controller) && toConstr action == toConstr (fromJust (showAction @controller) $ def))

{-# INLINE isDeleteAction #-}
isDeleteAction :: forall controller. (RestfulController controller, Eq (Child controller), Default (RestfulControllerId controller)) => Child controller -> Bool
isDeleteAction action = (isJust (deleteAction @controller) && toConstr action == toConstr (fromJust (deleteAction @controller) $ def))

{-# INLINE isUpdateAction #-}
isUpdateAction :: forall controller. (RestfulController controller, Eq (Child controller), Default (RestfulControllerId controller)) => Child controller -> Bool
isUpdateAction action = (isJust (updateAction @controller) && toConstr action == toConstr (fromJust (updateAction @controller) $ def))

{-# INLINE modelId #-}
modelId :: forall controller. (RestfulController controller, HasTypes (Child controller) (RestfulControllerId controller)) => Child controller -> Maybe (RestfulControllerId controller)
modelId action = headMay (toListOf (types @(RestfulControllerId controller)) action)

instance {-# OVERLAPPABLE #-} forall id controller parent child parentParent. (Eq controller, Eq child, Generic controller, Show id, PathArgument id, RestfulController controller, RestfulControllerId controller ~ id, Controller controller, parent ~ Parent controller, controller ~ (parent :> Child controller), child ~ Child controller, HasPath parent, HasTypes child id, Child child ~ child, Show child, Show controller, CanRoute parent parentParent, Default id, FrontControllerPrefix (ControllerApplicationMap parent), FrontControllerPrefix (ControllerApplicationMap (parent :> child))) => CanRoute (parent :> child) parent where
    --pathTo action | action == indexAction = "/Members"
    --pathTo action | action == newAction = pathTo (indexAction @controller) <> "/new"
    --pathTo action | action == createAction = pathTo (indexAction @controller)
    --pathTo action = error "TODO"
        --let id = unsafeHead (toListOf (types @id) action)
        --in pathTo (showAction @controller id) <> "/" <> tshow id <> (if editAction id == action then "/edit" else "")
    parseRoute' = do
        -- We temporary change the request method to GET while parsing the parent route
        -- This is equivalent to the following transformation:
        -- `UpdateProjectAction { .. } :> UpdateTaskAction { .. }` => `ShowProjectAction { .. } :> UpdateTaskAction { .. }`
        let requestContextWithGetMethod = withMethod GET
        parent <- let ?requestContext = requestContextWithGetMethod in parseRoute' @parent
        string "/"
        let
            indexAction' = parent :> (fromJust $ indexAction @controller)
            newAction' = parent :> (fromJust $ newAction @controller)
            createAction' = parent :> (fromJust $ createAction @controller)
            showActionWithoutParent memberId = (fromJust (showAction @controller) $ memberId)
            showAction' memberId = parent :> showActionWithoutParent memberId
            updateAction' memberId = parent :> (fromJust (updateAction @controller) $ memberId )
            deleteAction' memberId = parent :> (fromJust (deleteAction @controller) $ memberId )
            editAction' memberId = parent :> (fromJust (editAction @controller) $ memberId )
        (string (basePath @controller)) >> (
            string "/" >> (string "new" <* endOfInput >> get (newAction'))
                <|> (do
                    memberId <- parsePathArgument
                    let custom = (customActions (Just (showActionWithoutParent memberId)) >>= return )
                    let edit = (string "edit" >> get (editAction' memberId))
                    (string "/" >> (custom <|> edit))
                        <|> (onGetOrPostOrDelete (showAction' memberId) (updateAction' memberId) (deleteAction' memberId))
                )
            )
            <|> (string "/" >> customActions Nothing)
            <|> onGetOrPost indexAction' createAction'


{-# INLINE getMethod #-}
getMethod :: (?requestContext :: RequestContext) => Parser StdMethod
getMethod = 
    let methodOrError = parseMethod (requestMethod (TurboHaskell.Controller.RequestContext.request ?requestContext))
    in
        case methodOrError of
            Left error -> fail (cs error)
            Right method -> return method

withMethod :: (?requestContext :: RequestContext) => StdMethod -> RequestContext
withMethod requestMethod = (?requestContext) { request = newRequest }
    where
        newRequest = (TurboHaskell.Controller.RequestContext.request ?requestContext) { requestMethod = renderStdMethod requestMethod }

{-# INLINE post #-}
post action = do
    method <- getMethod
    case method of 
        POST -> return action
        _   -> fail "Invalid method, expected POST"

{-# INLINE get #-}
get action = do
    method <- getMethod
    case method of 
        GET -> return action
        _   -> fail "Invalid method, expected GET"

{-# INLINE onGetOrPost #-}
onGetOrPost getResult postResult = do
    method <- getMethod
    (case method of
                    GET  -> return getResult
                    POST -> return postResult
                    _    -> fail "Invalid method, expected GET or POST"
                )

{-# INLINE onGetOrPostOrDelete #-}
onGetOrPostOrDelete getResult postResult deleteResult = do
    method <- getMethod
    case method of
        GET    -> return getResult
        POST   -> return postResult
        DELETE -> return deleteResult
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
parseRoute :: forall controller parent. (?applicationContext :: ApplicationContext, ?requestContext :: RequestContext, Controller controller, CanRoute controller parent, InitControllerContext (ControllerApplicationMap controller)) => Parser (IO ResponseReceived)
parseRoute = parseRoute' @controller >>= return . runActionWithNewContext

{-# INLINE catchAll #-}
catchAll :: (?applicationContext :: ApplicationContext, ?requestContext :: RequestContext, Controller action, InitControllerContext (ControllerApplicationMap action)) => action -> Parser (IO ResponseReceived)
catchAll action = return (runActionWithNewContext action)