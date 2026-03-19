-- | Byte-keyed trie for O(key_length) exact path lookup.
--
-- Used by IHP's routing system to match request paths to controller actions
-- in a single traversal. Prefix mismatches are detected in 1-2 byte lookups.
module IHP.Router.Trie
    ( RouteTrie
    , emptyTrie
    , lookupTrie
    , insertTrie
    , fromListTrie
    ) where

import Prelude
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)

-- | A trie mapping 'ByteString' keys to values.
--
-- Each node stores an optional value (present if a key ends here)
-- and a map from the next byte to the child trie.
data RouteTrie a = RouteTrie !(Maybe a) !(IntMap (RouteTrie a))

-- | The empty trie.
emptyTrie :: RouteTrie a
emptyTrie = RouteTrie Nothing IntMap.empty
{-# INLINE emptyTrie #-}

-- | Look up a key in the trie. O(key_length).
--
-- Returns 'Nothing' as soon as a byte has no matching child,
-- so prefix mismatches fail fast.
lookupTrie :: ByteString -> RouteTrie a -> Maybe a
lookupTrie !bs !trie = go 0 trie
    where
        !len = ByteString.length bs
        go !i (RouteTrie val children)
            | i == len = val
            | otherwise =
                let !w = fromIntegral (ByteString.index bs i)
                in case IntMap.lookup w children of
                    Nothing -> Nothing
                    Just !child -> go (i + 1) child
{-# INLINE lookupTrie #-}

-- | Insert a key-value pair into the trie.
insertTrie :: ByteString -> a -> RouteTrie a -> RouteTrie a
insertTrie !bs !v = go 0
    where
        !len = ByteString.length bs
        go !i (RouteTrie val children)
            | i == len = RouteTrie (Just v) children
            | otherwise =
                let !w = fromIntegral (ByteString.index bs i)
                    child = fromMaybe emptyTrie (IntMap.lookup w children)
                in RouteTrie val (IntMap.insert w (go (i + 1) child) children)

-- | Build a trie from a list of key-value pairs.
fromListTrie :: [(ByteString, a)] -> RouteTrie a
fromListTrie = foldl' (\t (k, v) -> insertTrie k v t) emptyTrie
