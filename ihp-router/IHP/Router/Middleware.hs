{-|
Module: IHP.Router.Middleware
Description: WAI Middleware that dispatches via a 'RouteTrie'

'routeTrieMiddleware' wraps a pre-built 'RouteTrie' as a standard WAI
'Middleware'. On a match it invokes the trie-selected handler with the
captured path segments; on a method mismatch it returns @405 Method Not
Allowed@ with an @Allow@ header; on no match at all it delegates to the
fallback 'Application' passed in by middleware composition.
-}
module IHP.Router.Middleware
    ( routeTrieMiddleware
    , methodNotAllowedResponse
    ) where

import Prelude
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.List as List
import Network.HTTP.Types (methodNotAllowed405, hContentType, parseMethod)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Method (StdMethod, renderStdMethod)
import Network.Wai (Middleware, Response, responseLBS, requestMethod, rawPathInfo)
import IHP.Router.Trie (RouteTrie, LookupResult (..), lookupTrie, splitPath)

-- | Wrap a 'RouteTrie' as a WAI 'Middleware'.
--
-- __Semantics:__
--
-- * If the trie matches, the matched handler (a @'Captures' -> 'Application'@) runs with the captured path segments and the incoming request.
-- * If the path matches but the HTTP method does not, a @405 Method Not Allowed@ response is returned with an @Allow@ header listing the supported methods.
-- * If neither the path nor a splat matches, the request is handed to the @fallback@ application — which is the standard WAI behaviour when a middleware declines a request.
--
-- The middleware is pure with respect to the trie: construction happens at
-- application startup, lookup on every request.
routeTrieMiddleware :: RouteTrie -> Middleware
routeTrieMiddleware trie fallback req respond =
    case parseMethod (requestMethod req) of
        Left _notStandard -> fallback req respond
        Right method -> case lookupTrie trie method (splitPath (rawPathInfo req)) of
            Matched handler captures -> handler captures req respond
            MethodNotAllowed allowed -> respond (methodNotAllowedResponse allowed)
            NotMatched -> fallback req respond
{-# INLINE routeTrieMiddleware #-}

-- | Build a @405 Method Not Allowed@ response with a properly-formatted
-- @Allow@ header (comma-separated, per RFC 7231 §7.4.1).
methodNotAllowedResponse :: [StdMethod] -> Response
methodNotAllowedResponse allowed =
    responseLBS methodNotAllowed405
        [ (hContentType, "text/plain; charset=utf-8")
        , (hAllow, allowHeader)
        ]
        "405 Method Not Allowed"
  where
    allowHeader = ByteString.Char8.intercalate ", " (List.map renderStdMethod allowed)
    hAllow :: HeaderName
    hAllow = "Allow"
