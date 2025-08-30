module IHP.Controller.Response
( respondAndExit
, addResponseHeaders
, addResponseHeadersFromContext
, ResponseException (..)
)
where

import ClassyPrelude
import Network.HTTP.Types.Header
import qualified IHP.Controller.Context as Context
import qualified Network.Wai
import Network.Wai (Response)
import qualified Control.Exception as Exception

respondAndExit :: (?context :: Context.ControllerContext) => Response -> IO ()
respondAndExit response = do
    responseWithHeaders <- addResponseHeadersFromContext response
    Exception.throwIO (ResponseException responseWithHeaders)
{-# INLINE respondAndExit #-}

-- | Add headers to current response
-- | Returns a Response with headers
--
-- > addResponseHeaders [("Content-Type", "text/html")] response
--
addResponseHeaders :: [Header] -> Response -> Response
addResponseHeaders headers = Network.Wai.mapResponseHeaders (\hs -> headers <> hs)
{-# INLINABLE addResponseHeaders #-}

-- | Add headers to current response, getting the headers from ControllerContext
-- | Returns a Response with headers
--
-- > addResponseHeadersFromContext response
-- You probabaly want `setHeader`
--
addResponseHeadersFromContext :: (?context :: Context.ControllerContext) => Response -> IO Response
addResponseHeadersFromContext response = do
    maybeHeaders <- Context.maybeFromContext @[Header]
    let headers = fromMaybe [] maybeHeaders
    let responseWithHeaders = addResponseHeaders headers response
    pure responseWithHeaders
{-# INLINABLE addResponseHeadersFromContext #-}

-- Can be thrown from inside the action to abort the current action execution.
-- Does not indicates a runtime error. It's just used for control flow management.
newtype ResponseException = ResponseException Response

instance Show ResponseException where show _ = "ResponseException { .. }"

instance Exception ResponseException