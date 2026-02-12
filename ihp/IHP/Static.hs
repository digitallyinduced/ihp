module IHP.Static (staticRouteShortcut) where

import Network.Wai
import qualified Data.ByteString.Char8 as ByteString

-- | Routes requests with a @/static/@ prefix directly to the static file server,
-- bypassing the full middleware stack (session, CORS, auto-refresh, etc.).
-- All other requests go through the normal middleware pipeline.
--
-- This improves performance for static assets since they only need to be
-- read from disk.
staticRouteShortcut :: Application -> Application -> Application
staticRouteShortcut staticApp fallbackApp request respond =
    case request.pathInfo of
        ("static":rest) ->
            let strippedRequest = request
                    { pathInfo = rest
                    , rawPathInfo = ByteString.drop (ByteString.length "/static") request.rawPathInfo
                    }
            in staticApp strippedRequest respond
        _ -> fallbackApp request respond
