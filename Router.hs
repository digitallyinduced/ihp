{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, ScopedTypeVariables, AllowAmbiguousTypes #-}

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
    , edit
    , resource'
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
import Foundation.ModelSupport (NewTypeWrappedUUID, wrap, unwrap)

data Router = MatchMethod Method Matchable
    | Prefix ByteString Matchable
    | Capture (UUID -> Matchable) -- | Capture (Int -> ApplicationContext -> Application)
    | Action (ApplicationContext -> Application)

data UrlGenerator = UrlGenerator { path :: [UrlGeneratorPath] } deriving (Show)
data UrlGeneratorPath = Constant ByteString | Variable ByteString deriving (Show)

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

prefix :: Match a => ByteString -> a -> Router
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
                case stripPrefix prefix requestUrl of
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

data Resource idType = Resource {
        baseUrl :: ByteString,
        index :: Maybe Router,
        new :: Maybe Router,
        create :: Maybe Router,
        destroy ::  Maybe (idType -> Router),
        update :: Maybe (idType -> Router),
        show :: Maybe (idType -> Router),
        edit :: Maybe (idType -> Router),
        child :: Maybe (idType -> Router)
    }

resource = Resource { baseUrl = "", index = Nothing, new = Nothing, create = Nothing, destroy = Nothing, update = Nothing, show = Nothing, edit = Nothing, child = Nothing }

resource' :: Resource NoUUID
resource' = Resource { baseUrl = "", index = Nothing, new = Nothing, create = Nothing, destroy = Nothing, update = Nothing, show = Nothing, edit = Nothing, child = Nothing }

newtype NoUUID = NoUUID ()
instance NewTypeWrappedUUID NoUUID where
    wrap = error ""
    unwrap = error ""

toRoutes :: NewTypeWrappedUUID idType => Resource idType -> Router
toRoutes resource =
    prefix (baseUrl resource) (catMaybes [
            Just (prefix "/" (catMaybes [
                    case new resource of
                        Just action -> Just (prefix "new" $ get action)
                        Nothing     -> Nothing
                    ,
                    Just (arg $ \(id :: UUID) -> (catMaybes [
                        case let Resource{destroy} = resource in destroy of
                            Just action -> Just (delete (action $ (wrap id)))
                            Nothing -> Nothing,
                        case let Resource{update} = resource in update of
                            Just action -> Just (post (action $ wrap id))
                            Nothing -> Nothing,
                        case let Resource{show} = resource in show of
                            Just action -> Just (get (action $ wrap id))
                            Nothing -> Nothing,
                        case let Resource{edit} = resource in edit of
                            Just action -> Just (prefix "/edit" $ get (action $ wrap id))
                            Nothing     -> Nothing,
                        case let Resource{child} = resource in child of
                            Just router -> Just (router $ wrap id)
                            Nothing -> Nothing
                    ]))
                ])
            ),
            case let Resource{create} = resource in create of
                Just action -> Just (post action)
                Nothing -> Nothing
            ,
            case let Resource{index} = resource in index of
                Just action -> Just (get action)
                Nothing -> Nothing
        ])
