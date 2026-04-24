{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module: IHP.Router.DSL.Runtime
Description: Runtime helpers used by code emitted from the @routes@ quasi-quoter

The TH splice in "IHP.Router.DSL.TH" keeps its emitted expressions small
by delegating the non-trivial work to the helpers in this module.
Everything here is plain Haskell — importable and testable without
running any splice.
-}
module IHP.Router.DSL.Runtime
    ( buildRouteTrie
    , captureSpec
    , dispatch
    , mkHandler
    , mkHandlerQ
    -- * Query-string helpers
    , queryParamRequired
    , queryParamOptional
    , queryParamList
    , renderQueryString
    ) where

import Prelude
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text.Encoding
import Network.HTTP.Types (status404, Query)
import Network.HTTP.Types.Method (StdMethod)
import Network.HTTP.Types.URI (urlEncode)
import Network.Wai (Application, Request, queryString, responseLBS)

import IHP.Router.Trie
import IHP.Router.Capture (UrlCapture, parseCapture)

-- | Build a 'RouteTrie' from a compile-time-known list of
-- @(methods, pattern, handler)@ triples. Each entry can register one
-- handler under several HTTP methods at once — the TH splice uses this
-- so a @GET@ route auto-registers @HEAD@ via the same 'WaiHandler' value
-- (no duplicate lambda in the emitted Core).
buildRouteTrie :: [([StdMethod], [PatternSegment], WaiHandler)] -> RouteTrie
buildRouteTrie = foldr step emptyTrie
  where
    step (ms, p, h) t = insertRouteMethods p ms h t
{-# INLINE buildRouteTrie #-}

-- | Build a 'CaptureSpec' from a capture name. Type-level validation
-- is done by the handler, not the trie — see 'dispatch'.
captureSpec :: Text -> CaptureSpec
captureSpec = CaptureSpec
{-# INLINE captureSpec #-}

-- | Dispatch a positional capture list to a controller action, falling
-- back to @404 Not Found@ when construction fails (typically because a
-- capture bytestring couldn't be decoded as the expected type).
--
-- The TH splice emits a call to 'dispatch' per route handler; the second
-- argument is a 'Maybe' expression that tries 'parseCapture' on each
-- positional bytestring and assembles the action constructor.
dispatch :: (controller -> Application) -> Maybe controller -> Application
dispatch _   Nothing   _req respond = respond (responseLBS status404 [] "Not Found")
dispatch run (Just c)  req  respond = run c req respond
{-# INLINE dispatch #-}

-- | Wrap a per-route capture-and-action builder into a 'WaiHandler'.
--
-- The TH splice emits one call to 'mkHandler' (or 'mkHandlerQ' for routes
-- that read query parameters) per route. Pulling the WAI shell out of
-- the splice and into a runtime helper saves ~3 KB of Core per route on
-- the user-facing module: the per-route emission shrinks from a 3-arg
-- @\\captures req respond -> let q = queryString req in case captures of …@
-- shape down to a single 'mkHandler' application plus a small lambda.
--
-- The builder receives the captures unchanged and returns @'Just' action@
-- on a successful decode or 'Nothing' if any capture rejected. The
-- runtime then dispatches the action via the user-supplied runner
-- (typically @runAction'@ in IHP, or any @Controller -> Application@
-- function in a generic WAI app).
mkHandler
    :: (controller -> Application)            -- ^ runner (e.g. @runAction'@)
    -> (Captures -> Maybe controller)         -- ^ per-route capture decoder
    -> WaiHandler
mkHandler runner build captures req respond =
    dispatch runner (build captures) req respond
{-# NOINLINE mkHandler #-}
-- ^ NOINLINE is deliberate: with INLINE, GHC would inline the body at
-- every per-route call site (one per route in the user's Routes.hs),
-- regenerating the @\\captures req respond -> dispatch …@ shell we just
-- factored out. NOINLINE keeps a single shared body in Runtime, the
-- per-route Core is just one application of @mkHandler@. The dispatch
-- helper itself is INLINE and folds into mkHandler's body once.
-- Runtime cost is one extra indirect call per request — negligible
-- against actual handler work, and well below the latency we shaved
-- by avoiding AutoRoute's per-request 'Data' reflection.

-- | Variant of 'mkHandler' that also threads the query string through.
-- Used by the TH splice for routes that declare any @?name@ query
-- parameters; routes without query params use the plainer 'mkHandler'.
--
-- Splitting these two avoids paying for the @queryString req@ pull on
-- routes that don't read query params (the vast majority of static
-- routes), and keeps the simple-case Core small.
mkHandlerQ
    :: (controller -> Application)
    -> (Captures -> Query -> Maybe controller)
    -> WaiHandler
mkHandlerQ runner build captures req respond =
    dispatch runner (build captures (queryString req)) req respond
{-# NOINLINE mkHandlerQ #-}

---------------------------------------------------------------------------
-- Query-string helpers
---------------------------------------------------------------------------

-- | Look up a required query parameter and decode it via 'parseCapture'.
--
-- Returns 'Nothing' if the key is absent, the value is missing (@?k@
-- without @=v@), or 'parseCapture' rejects the raw bytes. A 'Nothing'
-- flows up through the splice's 'Maybe'-do block and triggers a 404,
-- matching the behaviour of an unmatched path capture.
queryParamRequired
    :: forall a. UrlCapture a
    => ByteString -> Query -> Maybe a
queryParamRequired name query = case lookup name query of
    Just (Just raw) -> parseCapture raw
    _               -> Nothing
{-# INLINE queryParamRequired #-}

-- | Look up an optional query parameter. Absent or malformed values
-- decode to 'Nothing'; a present-and-parseable value decodes to
-- @'Just' v@.
--
-- Always succeeds at the outer level — the splice wraps the result in
-- @'Just' . Just@ for required contexts, so no 404 is triggered by an
-- absent optional parameter.
queryParamOptional
    :: forall a. UrlCapture a
    => ByteString -> Query -> Maybe a
queryParamOptional name query = case lookup name query of
    Just (Just raw) -> parseCapture raw
    _               -> Nothing
{-# INLINE queryParamOptional #-}

-- | Collect all values of a repeated query parameter (e.g. @?tag=a&tag=b@
-- yields @[\"a\",\"b\"]@). Values that fail to decode are silently
-- dropped — preserves AutoRoute's behaviour of tolerating bad list
-- entries rather than 400-ing the whole request.
queryParamList
    :: forall a. UrlCapture a
    => ByteString -> Query -> [a]
queryParamList name query =
    mapMaybe (parseCapture @a) [ raw | (k, Just raw) <- query, k == name ]
{-# INLINE queryParamList #-}

-- | Render a list of @(name, 'Just' value)@ pairs as a @?k1=v1&k2=v2@
-- suffix (URL-encoded). Empty list yields the empty string. Pairs with
-- 'Nothing' values are dropped — 'pathTo' passes 'Nothing' for absent
-- 'Maybe' fields and for empty list fields, so they correctly disappear
-- from the URL.
--
-- The splice's generated 'pathTo' clauses call this once per
-- constructor with the list of query-param tuples for that route.
renderQueryString :: [(ByteString, Maybe Text)] -> Text
renderQueryString pairs =
    let kept = [(k, v) | (k, Just v) <- pairs]
     in if null kept
            then ""
            else Text.Encoding.decodeUtf8
                    $ LBS.toStrict $ Builder.toLazyByteString
                    $ buildQueryString kept
  where
    buildQueryString = go True
      where
        go _ []                 = mempty
        go first ((k, v) : rest) =
            (if first then "?" else "&")
                <> Builder.byteString (urlEncode True k)
                <> "="
                <> Builder.byteString (urlEncode True (Text.Encoding.encodeUtf8 v))
                <> go False rest
