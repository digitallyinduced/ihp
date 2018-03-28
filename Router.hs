{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses      #-}

module Foundation.Router
    ( match
    , get
    , post
    , delete
    , prefix
    , Router (..)
    , arg
    , action, urlGenerators, UrlGenerator (..), UrlGeneratorPath (..)
    , resource
    , index
    , new
    , create
    , baseUrl
    , toRoutes
    , destroy
    , update
    , show
    , child
    ) where

import           ClassyPrelude                 hiding (index, delete, show)
import           Data.ByteString.Char8         (split)
import           Data.String.Conversions       (cs)
import           Foundation.ApplicationContext
import qualified Foundation.ControllerSupport  as ControllerSupport
import           Network.HTTP.Types.Method     (Method, methodGet, methodPost, methodDelete)
import           Network.Wai                   (Application, Request, rawPathInfo, requestMethod)
import           Text.Read                     (read)
import Data.UUID (UUID)
import qualified Data.UUID

data Router = MatchMethod Method Matchable
    | Prefix Text Matchable
    | Capture (UUID -> Matchable) -- | Capture (Int -> ApplicationContext -> Application)
    | Action (ApplicationContext -> Application)

data UrlGenerator = UrlGenerator { path :: [UrlGeneratorPath] } deriving (Show)
data UrlGeneratorPath = Constant Text | Variable Text deriving (Show)

class Match a where
    match' :: ByteString -> Request -> a -> Maybe (ApplicationContext -> Application)
    urlGenerators :: a -> UrlGenerator -> [UrlGenerator]

data Matchable = forall a . Match a => Matchable a
toMatchable :: Match a => a -> Matchable
toMatchable = Matchable

get :: Match a => a -> Router
get matchable = MatchMethod methodGet (toMatchable matchable)

post :: Match a => a -> Router
post matchable = MatchMethod methodPost (toMatchable matchable)

delete :: Match a => a -> Router
delete matchable = MatchMethod methodDelete (toMatchable matchable)

prefix :: Match a => Text -> a -> Router
prefix url matchable = Prefix url (toMatchable matchable)

arg :: Match a => (UUID -> a) -> Router
arg matchable = Capture (\value -> toMatchable (matchable value))

match :: Request -> Router -> Maybe (ApplicationContext -> Application)
match request router = match' (rawPathInfo request) request router

action :: ControllerSupport.Action -> Router
action = Action . ControllerSupport.withContext

instance Match Router where
    match' requestUrl request router =
        case router of
            Action action ->
                case requestUrl of
                    "" -> Just action
                    _  -> Nothing
            MatchMethod method next ->
                if requestMethod request == method
                    then match' requestUrl request next
                    else Nothing
            Prefix prefix routes ->
                case stripPrefix (cs prefix) requestUrl of
                    Just remainder -> match' remainder request routes
                    Nothing        -> Nothing
            Capture action ->
                let
                    token = headMay $ split '/' requestUrl
                in
                    case token of
                        Just token ->
                            let
                                parsed = read $ cs token
                                remainingUrl = fromMaybe mempty (stripPrefix (cs token) requestUrl)
                            in
                                match' remainingUrl request (action parsed)
                        Nothing -> Nothing

    urlGenerators router urlGenerator =
        case router of
            Action _                -> [urlGenerator]
            MatchMethod method next -> urlGenerators next urlGenerator
            Prefix prefix routes -> map (\urlGenerator -> (urlGenerator { path = (Constant prefix):(path urlGenerator) })) (urlGenerators routes urlGenerator)
            Capture next -> map (\urlGenerator -> (urlGenerator { path = (Variable "x"):(path urlGenerator) } )) $ urlGenerators (next Data.UUID.nil) urlGenerator




instance Match a => Match [a] where
    match' requestUrl request router = headMay $ mapMaybe (match' requestUrl request) router
    urlGenerators routes urlGenerator = join $ map (\route -> urlGenerators route urlGenerator) routes

instance Match Matchable where
    match' requestUrl request (Matchable matchable) = match' requestUrl request matchable
    urlGenerators (Matchable matchable) urlGenerator = urlGenerators matchable urlGenerator

data Resource baseUrlType indexActionType newActionType createActionType destroyActionType updateActionType showActionType childType = Resource { baseUrl :: baseUrlType, index :: indexActionType, new :: newActionType, create :: createActionType, destroy :: destroyActionType, update :: updateActionType, show :: showActionType, child :: childType }

resource = Resource { baseUrl = (), index = (), new = (), create = (), destroy = (), update = (), show = (), child = () }

toRoutes :: (ValueOrUnit a Router, ValueOrUnit b Router, ValueOrUnit c Router, ValueOrUnit d (UUID -> Router), ValueOrUnit e (UUID -> Router), ValueOrUnit f (UUID -> Router), ValueOrUnit g (UUID -> Router)) => Resource Text a b c d e f g -> Router
toRoutes resource =
    let
        newAction :: Maybe Router
        newAction = toMaybeValue $ new resource
        createAction :: Maybe Router
        createAction = toMaybeValue $ create resource
        destroyAction :: Maybe (UUID -> Router)
        destroyAction = toMaybeValue $ destroy resource
        indexAction :: Maybe Router
        indexAction = toMaybeValue $ index resource
        updateAction :: Maybe (UUID -> Router)
        updateAction = toMaybeValue $ update resource
        showAction :: Maybe (UUID -> Router)
        showAction = toMaybeValue $ show resource
        child :: Maybe (UUID -> Router)
        child = toMaybeValue $ let Resource {child} = resource in child
    in prefix (baseUrl resource) (catMaybes [
            Just (prefix "/" (catMaybes [
                    case newAction of
                        Just action -> Just (prefix "new" $ get action)
                        Nothing     -> Nothing
                    ,
                    Just (arg $ \id -> (catMaybes [
                        case destroyAction of
                            Just action -> Just (delete (action id))
                            Nothing -> Nothing,
                        case updateAction  of
                            Just action -> Just (post (action id))
                            Nothing -> Nothing,
                        case showAction of
                            Just action -> Just (get (action id))
                            Nothing -> Nothing,
                        case child of
                            Just router -> Just (router id)
                            Nothing -> Nothing
                    ]))
                ])
            ),
            case createAction of
                Just action -> Just (post action)
                Nothing -> Nothing
            ,
            case indexAction of
                Just action -> Just (get action)
                Nothing -> Nothing
        ])


class ValueOrUnit a b where
    toMaybeValue :: a -> Maybe b

instance ValueOrUnit () Router where
    toMaybeValue :: () -> Maybe Router
    toMaybeValue _ = Nothing

instance ValueOrUnit Router Router where
    toMaybeValue router = Just router

instance ValueOrUnit () (UUID -> Router) where
    toMaybeValue _ = Nothing

instance ValueOrUnit (UUID -> Router) (UUID -> Router) where
    toMaybeValue curriedRouter = Just curriedRouter
