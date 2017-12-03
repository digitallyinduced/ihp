module Foundation.Router
    ( match
    , get
    , post
    , prefix
    , Router
    , arg
    ) where

import ClassyPrelude
import qualified Foundation.ControllerSupport as ControllerSupport
import Data.String.Conversions (cs)
import Foundation.ApplicationContext
import Network.HTTP.Types.Method (Method, methodGet, methodPost)
import Network.Wai
       (Application, Request, rawPathInfo, requestMethod)
import Text.Read (read)

data Router
    = Match Method
            Text
            (ApplicationContext -> Application)
    | Prefix Text
             [Router]
    | Capture (Int -> ApplicationContext -> Application)

get :: Text -> ControllerSupport.Action -> Router
get url action = Match methodGet url (ControllerSupport.withContext action)

post :: Text -> ControllerSupport.Action -> Router
post url action = Match methodPost url (ControllerSupport.withContext action)

prefix :: Text -> [Router] -> Router
prefix = Prefix

arg :: (Int -> ControllerSupport.Action) -> Router
arg action = Capture (\captured -> ControllerSupport.withContext $ action captured)

applyPrefix prefix (Match method url action) = Match method (prefix <> url) action
applyPrefix prefix (Prefix prefix' routes) = Prefix (prefix <> prefix') routes

match :: Request -> Router -> Maybe (ApplicationContext -> Application)
match request router = match' (rawPathInfo request) request router

match' :: ByteString -> Request -> Router -> Maybe (ApplicationContext -> Application)
match' requestUrl request router =
    case router of
        Match method url action ->
            if requestMethod request == method && requestUrl == cs url
                then Just action
                else Nothing
        Prefix prefix routes ->
            case stripPrefix (cs prefix) requestUrl of
                Just remainder -> headMay $ mapMaybe (match' remainder request) routes
                Nothing -> Nothing
        Capture action -> Just $ action $ read $ cs requestUrl
