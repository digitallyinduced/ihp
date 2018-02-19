{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}

module Foundation.Router
    ( match
    , get
    , post
    , prefix
    , Router
    , arg
    , action
    ) where

import           ClassyPrelude                 hiding (index)
import           Data.String.Conversions       (cs)
import           Foundation.ApplicationContext
import qualified Foundation.ControllerSupport  as ControllerSupport
import           Network.HTTP.Types.Method     (Method, methodGet, methodPost)
import           Network.Wai                   (Application, Request, rawPathInfo, requestMethod)
import           Text.Read                     (read)
import Data.ByteString.Char8 (split)

data Router = MatchMethod Method Matchable
    | Prefix Text Matchable
    | Capture (Int -> Matchable) -- | Capture (Int -> ApplicationContext -> Application)
    | Action (ApplicationContext -> Application)

class Match a where
    match' :: ByteString -> Request -> a -> Maybe (ApplicationContext -> Application)

data Matchable = forall a . Match a => Matchable a
toMatchable :: Match a => a -> Matchable
toMatchable = Matchable

get :: Match a => a -> Router
get matchable = MatchMethod methodGet (toMatchable matchable)

post :: Match a => a -> Router
post matchable = MatchMethod methodPost (toMatchable matchable)

prefix :: Match a => Text -> a -> Router
prefix url matchable = Prefix url (toMatchable matchable)

arg :: Match a => (Int -> a) -> Router
arg matchable = Capture (\value -> toMatchable (matchable value))

match :: Request -> Router -> Maybe (ApplicationContext -> Application)
match request router = match' (rawPathInfo request) request router

action :: ControllerSupport.Action -> Router
action = Action . ControllerSupport.withContext

instance Match Router where
    match' requestUrl request router =
        case router of
            Action action -> Just action
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
                    token = unsafeHead $ split '/' requestUrl
                    parsed = read $ cs token
                    remainingUrl = fromMaybe mempty (stripPrefix (cs $ token) requestUrl)
                in
                    match' remainingUrl request (action parsed)
                -- Just $ action $ read $ cs requestUrl

instance Match a => Match [a] where
    match' requestUrl request router = headMay $ mapMaybe (match' requestUrl request) router

instance Match Matchable where
    match' requestUrl request (Matchable matchable) = match' requestUrl request matchable