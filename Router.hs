{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, ScopedTypeVariables, FunctionalDependencies #-}

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
    , justAction
    , AppRouter
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

import qualified Controller.Context
import qualified Foundation.ModelSupport
import qualified Foundation.Controller.RequestContext

data Router = MatchMethod !Method !Matchable
    | Prefix !ByteString !Matchable
    | Capture !(UUID -> Matchable) -- | Capture (Int -> ApplicationContext -> Application)
    | Action !ControllerSupport.Action'

data UrlGenerator = UrlGenerator { path :: [UrlGeneratorPath] } deriving (Show)
data UrlGeneratorPath = Constant ByteString | Variable ByteString deriving (Show)

class Match a where
    match' :: ByteString -> Request -> a -> Maybe ControllerSupport.Action'
    urlGenerators :: a -> UrlGenerator -> [UrlGenerator]

data Matchable = forall a . Match a => Matchable a
toMatchable :: Match a => a -> Matchable
toMatchable = Matchable

{-# INLINE get #-}
get :: Match a => a -> Router
get matchable = MatchMethod methodGet (toMatchable matchable)

{-# INLINE post #-}
post :: Match a => a -> Router
post matchable = MatchMethod methodPost (toMatchable matchable)

{-# INLINE delete #-}
delete :: Match a => a -> Router
delete matchable = MatchMethod methodDelete (toMatchable matchable)

{-# INLINE prefix #-}
prefix :: Match a => ByteString -> a -> Router
prefix url matchable = Prefix url (toMatchable matchable)

{-# INLINE arg #-}
arg :: Match a => (UUID -> a) -> Router
arg matchable = Capture (\value -> toMatchable (matchable value))

{-# INLINE match #-}
match :: Request -> Router -> Maybe ControllerSupport.Action'
match request router = match' (rawPathInfo request) request router

{-# INLINE action #-}
action :: (?controllerContext::Controller.Context.ControllerContext, ?modelContext::Foundation.ModelSupport.ModelContext, ?requestContext::Foundation.Controller.RequestContext.RequestContext)  => ControllerSupport.Action -> Router
action = Action

{-# INLINE justAction #-}
justAction :: (?controllerContext::Controller.Context.ControllerContext, ?modelContext::Foundation.ModelSupport.ModelContext, ?requestContext::Foundation.Controller.RequestContext.RequestContext)  => ControllerSupport.Action -> Maybe Router
justAction = Just . action

type AppRouter = (?controllerContext::Controller.Context.ControllerContext, ?modelContext::Foundation.ModelSupport.ModelContext, ?requestContext::Foundation.Controller.RequestContext.RequestContext) => Router

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

data Resource idType newType createType indexType showType editType updateType destroyType = Resource  {
        baseUrl :: !ByteString,
        index :: indexType,
        new :: newType,
        create :: createType,
        destroy :: idType -> destroyType,
        update :: idType -> updateType,
        show :: showType,
        edit :: editType,
        child :: Maybe (idType -> Router)
    }

type EmptyResource idType = Resource idType (Maybe Router) (Maybe Router) (Maybe Router) (Maybe (idType -> Router)) (Maybe (idType -> Router)) () ()

{-# INLINE resource #-}
resource :: EmptyResource idType
resource = Resource { baseUrl = "", index = Nothing, new = Nothing, create = Nothing, destroy = const (), update = const (), show = Nothing, edit = Nothing, child = Nothing }

{-# INLINE resource' #-}
resource' :: EmptyResource NoUUID
resource' = Resource { baseUrl = "", index = Nothing, new = Nothing, create = Nothing, destroy = const (), update = const (), show = Nothing, edit = Nothing, child = Nothing }

newtype NoUUID = NoUUID ()
instance NewTypeWrappedUUID NoUUID where
    wrap = error ""
    unwrap = error ""

{-# INLINE toRoutes #-}
toRoutes :: forall idType newType createType indexType showType editType updateType destroyType. (NewTypeWrappedUUID idType, ToMaybeRouter newType, ToMaybeRouter createType, ToMaybeRouter indexType, ToMaybeRouterWithId showType idType, ToMaybeRouterWithId editType idType, ToMaybeRouter destroyType, ToMaybeRouter updateType) => Resource idType newType createType indexType showType editType updateType destroyType -> Router
toRoutes resource =
    prefix (baseUrl resource) (catMaybes [
            Just (prefix "/" (catMaybes [
                    case toMaybeRouter (new resource) of
                        Just action -> Just (prefix "new" $ get action)
                        Nothing     -> Nothing
                    ,
                    Just (arg $ \(id :: UUID) -> (catMaybes [
                        case let Resource{destroy} = resource in toMaybeRouter (destroy (wrap id)) of
                            Just action -> Just (delete action)
                            Nothing -> Nothing,
                        case let Resource{update} = resource in toMaybeRouter (update (wrap id)) of
                            Just action -> Just (post action)
                            Nothing -> Nothing,
                        case let Resource{show} = resource in (toMaybeRouterWithId :: showType -> Maybe (idType -> Router)) show of
                            Just action -> Just (get (action $ wrap id))
                            Nothing -> Nothing,
                        case let Resource{edit} = resource in (toMaybeRouterWithId :: editType -> Maybe (idType -> Router)) edit of
                            Just action -> Just (prefix "/edit" $ get (action $ wrap id))
                            Nothing     -> Nothing,
                        case let Resource{child} = resource in child of
                            Just router -> Just (router $ wrap id)
                            Nothing -> Nothing
                    ]))
                ])
            ),
            case let Resource{create} = resource in toMaybeRouter create of
                Just action -> Just (post action)
                Nothing -> Nothing
            ,
            case let Resource{index} = resource in toMaybeRouter index of
                Just action -> Just (get action)
                Nothing -> Nothing
        ])

class ToMaybeRouter a where toMaybeRouter :: a -> Maybe Router
instance ToMaybeRouter (Maybe Router) where
    {-# INLINE toMaybeRouter #-}
    toMaybeRouter a = a
instance ToMaybeRouter (()) where
    {-# INLINE toMaybeRouter #-}
    toMaybeRouter _ = Nothing
instance ToMaybeRouter Router where
    {-# INLINE toMaybeRouter #-}
    toMaybeRouter = Just
instance ToMaybeRouter ControllerSupport.Action' where
    {-# INLINE toMaybeRouter #-}
    toMaybeRouter = Just . Action

class ToMaybeRouterWithId value idType | value -> idType where
    toMaybeRouterWithId :: value -> Maybe (idType -> Router)
instance ToMaybeRouterWithId (idType -> Router) idType where
    {-# INLINE toMaybeRouterWithId #-}
    toMaybeRouterWithId = Just
instance ToMaybeRouterWithId ((idType -> ControllerSupport.Action')) idType where
    {-# INLINE toMaybeRouterWithId #-}
    toMaybeRouterWithId action = Just $ \idType -> Action (action idType)

instance {-# OVERLAPS #-} ToMaybeRouterWithId (Maybe (idType -> Router)) idType where
    {-# INLINE toMaybeRouterWithId #-}
    toMaybeRouterWithId value = value
