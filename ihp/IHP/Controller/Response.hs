module IHP.Controller.Response
( respondWith
, addResponseHeaders
, addResponseHeadersFromContext
-- Re-exported from Network.Wai.Middleware.EarlyReturn
, earlyReturn
)
where

import ClassyPrelude
import Network.HTTP.Types.Header
import qualified IHP.Controller.Context as Context
import IHP.Controller.Context (ControllerContext(..))
import IHP.Controller.RequestContext (RequestContext(..))
import qualified Network.Wai
import Network.Wai (Response, ResponseReceived)
import Network.Wai.Middleware.EarlyReturn (earlyReturn)

-- | Sends a response to the client. Used by render functions.
--
-- This is the normal way to respond - it calls the WAI respond callback directly
-- and returns the ResponseReceived.
respondWith :: (?context :: Context.ControllerContext) => Response -> IO ResponseReceived
respondWith response = do
    responseWithHeaders <- addResponseHeadersFromContext response
    ?context.requestContext.respond responseWithHeaders
{-# INLINE respondWith #-}

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