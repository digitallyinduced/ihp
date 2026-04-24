{-|
Module: IHP.Router.Trie
Description: Path-indexed trie for fast URL dispatch

A 'RouteTrie' is the runtime data structure used by IHP's router to dispatch
incoming requests to handlers. Paths are split on @/@ and walked segment-by-segment.
At each node, literal matches are tried first, then a typed capture, then a
splat fall-through. Handlers at a leaf are indexed by HTTP method so the same
path can dispatch to different actions for GET / POST / PATCH / etc.

The trie is built once at application startup and shared across all
requests; lookup is a single linear walk with no backtracking.
-}
module IHP.Router.Trie
    ( RouteTrie (..)
    , CaptureSpec (..)
    , PatternSegment (..)
    , HandlersByMethod
    , WaiHandler
    , Captures
    , LookupResult (..)
    , emptyTrie
    , insertRoute
    , lookupTrie
    , mergeTrie
    , splitPath
    ) where

import Prelude
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Encoding.Error as Text.Encoding.Error
import Data.Dynamic (Dynamic, toDyn)
import Network.HTTP.Types.Method (StdMethod)
import Network.Wai (Application)

-- | Captures collected while walking the trie, keyed by the capture name
-- declared in the route. Values are 'Dynamic'-wrapped; the handler
-- downcasts them using the type associated with the capture.
type Captures = HashMap Text Dynamic

-- | A WAI 'Application' that also receives the captures collected during
-- trie traversal.
type WaiHandler = Captures -> Application

-- | Per-method handlers at a trie leaf. Missing entries correspond to
-- @405 Method Not Allowed@. Backed by 'Map' (not 'HashMap') because
-- 'StdMethod' doesn't have a 'Hashable' instance and the per-node
-- cardinality is tiny (≤7 methods).
type HandlersByMethod = Map StdMethod WaiHandler

-- | Specification of a single typed capture at a trie node.
data CaptureSpec = CaptureSpec
    { captureName  :: !Text
    , captureParse :: !(ByteString -> Maybe Dynamic)
    }

-- | Path-indexed trie node.
--
-- Lookup priority at each node is: literal segment > typed capture > splat.
-- Splat captures consume the remaining path and land on the bundled
-- 'HandlersByMethod'.
data RouteTrie = RouteTrie
    { static   :: !(HashMap ByteString RouteTrie)
    , dynamic  :: !(Maybe (CaptureSpec, RouteTrie))
    , splat    :: !(Maybe (Text, HandlersByMethod))
    , handlers :: !HandlersByMethod
    }

-- | A single segment of a route pattern during trie construction.
data PatternSegment
    = LiteralSeg !ByteString
    | CaptureSeg !CaptureSpec
    | SplatSeg !Text

-- | Outcome of a trie lookup.
data LookupResult
    = Matched !WaiHandler !Captures
    | MethodNotAllowed ![StdMethod]
    | NotMatched

-- | An empty trie — matches nothing, has no handlers.
emptyTrie :: RouteTrie
emptyTrie = RouteTrie HashMap.empty Nothing Nothing Map.empty

-- | Split a path like @"/posts/123/edit"@ into @["posts","123","edit"]@.
-- Leading, trailing, and empty segments are dropped so @"/"@ and @""@ both
-- yield @[]@.
splitPath :: ByteString -> [ByteString]
splitPath = filter (not . ByteString.null) . ByteString.Char8.split '/'
{-# INLINE splitPath #-}

-- | Insert a route into the trie. Given a list of pattern segments, a
-- method, and a handler, this either extends the trie with new nodes or
-- merges into existing ones.
--
-- Inserting the same @(method, pattern)@ twice silently overrides.
insertRoute
    :: [PatternSegment]
    -> StdMethod
    -> WaiHandler
    -> RouteTrie
    -> RouteTrie
insertRoute [] method handler trie =
    trie { handlers = Map.insert method handler (handlers trie) }
insertRoute (seg : rest) method handler trie = case seg of
    LiteralSeg lit ->
        let child = HashMap.lookupDefault emptyTrie lit (static trie)
            child' = insertRoute rest method handler child
         in trie { static = HashMap.insert lit child' (static trie) }
    CaptureSeg spec ->
        let child = case dynamic trie of
                Just (_, existing) -> existing
                Nothing            -> emptyTrie
            child' = insertRoute rest method handler child
         in trie { dynamic = Just (spec, child') }
    SplatSeg name ->
        let existing = case splat trie of
                Just (_, h) -> h
                Nothing     -> Map.empty
         in trie { splat = Just (name, Map.insert method handler existing) }

-- | Deep-merge two tries. Used at application startup to combine
-- fragments coming from separate controllers into one app-wide trie.
--
-- Right-biased: where both sides define a handler for the same method
-- at the same leaf, @b@ wins.
mergeTrie :: RouteTrie -> RouteTrie -> RouteTrie
mergeTrie a b = RouteTrie
    { static = HashMap.unionWith mergeTrie (static a) (static b)
    , dynamic = case (dynamic a, dynamic b) of
        (Nothing, x) -> x
        (x, Nothing) -> x
        (Just (_, aChild), Just (bSpec, bChild)) -> Just (bSpec, mergeTrie aChild bChild)
    , splat = case (splat a, splat b) of
        (Nothing, x) -> x
        (x, Nothing) -> x
        (Just (_, aH), Just (bName, bH)) -> Just (bName, Map.union bH aH)
    , handlers = Map.union (handlers b) (handlers a)
    }

-- | Look up a method+path pair in the trie.
--
-- Walks path segments with the priority order literal > typed capture > splat.
-- Returns 'Matched' with collected captures on success; if the path matches
-- a leaf but the method isn't registered, 'MethodNotAllowed' is returned
-- with the list of methods that do match. 'NotMatched' is returned when
-- no path walk reaches a leaf or splat.
lookupTrie :: RouteTrie -> StdMethod -> [ByteString] -> LookupResult
lookupTrie trie method = go trie HashMap.empty
  where
    go node captures [] = case Map.lookup method (handlers node) of
        Just h  -> Matched h captures
        Nothing
            | Map.null (handlers node) -> trySplatEmpty node captures
            | otherwise -> MethodNotAllowed (Map.keys (handlers node))

    go node captures (seg : rest) =
        case HashMap.lookup seg (static node) of
            Just child -> case go child captures rest of
                NotMatched -> tryCapture node captures seg rest
                other -> other
            Nothing -> tryCapture node captures seg rest

    tryCapture node captures seg rest = case dynamic node of
        Just (spec, child) -> case captureParse spec seg of
            Just value ->
                let captures' = HashMap.insert (captureName spec) value captures
                 in case go child captures' rest of
                        NotMatched -> trySplat node captures (seg : rest)
                        other -> other
            Nothing -> trySplat node captures (seg : rest)
        Nothing -> trySplat node captures (seg : rest)

    trySplat node captures segs = case splat node of
        Just (name, byMethod) ->
            let raw = ByteString.intercalate "/" segs
                captures' = HashMap.insert name (toDyn (decodeLenient raw)) captures
             in case Map.lookup method byMethod of
                    Just h  -> Matched h captures'
                    Nothing
                        | Map.null byMethod -> NotMatched
                        | otherwise -> MethodNotAllowed (Map.keys byMethod)
        Nothing -> NotMatched

    trySplatEmpty node captures = case splat node of
        Just (name, byMethod) ->
            let captures' = HashMap.insert name (toDyn ("" :: Text)) captures
             in case Map.lookup method byMethod of
                    Just h  -> Matched h captures'
                    Nothing
                        | Map.null byMethod -> NotMatched
                        | otherwise -> MethodNotAllowed (Map.keys byMethod)
        Nothing -> NotMatched

    decodeLenient :: ByteString -> Text
    decodeLenient = Text.Encoding.decodeUtf8With Text.Encoding.Error.lenientDecode
