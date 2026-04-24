{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module: IHP.Router.DSL.Runtime
Description: Runtime helpers used by code emitted from the @routes@ quasi-quoter

The TH splice in "IHP.Router.DSL.TH" keeps its emitted expressions small by
delegating the non-trivial work to the helpers in this module. Everything
here is plain Haskell — importable and testable without running any splice.
-}
module IHP.Router.DSL.Runtime
    ( buildRouteTrie
    , captureSpec
    , requireCapture
    , runTrieHandler
    ) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Data.Dynamic (Dynamic, toDyn, fromDynamic)
import Data.Typeable (Typeable)
import Network.HTTP.Types.Method (StdMethod)
import Network.Wai (Application)

import IHP.Router.Trie
import IHP.Router.Capture (UrlCapture, parseCapture)

-- | Build a 'RouteTrie' from a compile-time-known list of
-- @(method, pattern, handler)@ triples. The TH splice produces one call
-- to this function per @[routes|...|]@ block.
buildRouteTrie :: [(StdMethod, [PatternSegment], WaiHandler)] -> RouteTrie
buildRouteTrie = foldr step emptyTrie
  where
    step (m, p, h) t = insertRoute p m h t
{-# INLINE buildRouteTrie #-}

-- | Build a 'CaptureSpec' for a type with a 'UrlCapture' instance.
--
-- The TH splice emits one call to @captureSpec \@FieldType "captureName"@
-- per dynamic segment in a route pattern.
captureSpec :: forall a. (UrlCapture a) => Text -> CaptureSpec
captureSpec name = CaptureSpec
    { captureName = name
    , captureParse = \bs -> toDyn <$> parseCapture @a bs
    }
{-# INLINE captureSpec #-}

-- | Look up a capture by name, downcast to the expected type, or bail with
-- a clear error message.
--
-- Capture types are declared at TH time and enforced by the trie's
-- parsing step, so the 'Nothing' branches of this helper only fire for
-- genuine bugs in the generated code.
requireCapture :: forall a. (Typeable a) => Text -> Captures -> a
requireCapture name caps = case HashMap.lookup name caps of
    Nothing ->
        error ("routes: capture '" <> Text.unpack name <> "' was not collected during dispatch")
    Just dyn -> case fromDynamic @a dyn of
        Just v  -> v
        Nothing ->
            error ("routes: capture '" <> Text.unpack name
                <> "' had an unexpected Haskell type (generated code bug)")

-- | Wrap a thunk @(-> Application)@ — that is, a controller action whose
-- captures have already been extracted — as a 'WaiHandler' suitable for
-- insertion into the trie.
--
-- The generated code uses this when the controller action is a nullary
-- constructor (no path captures).
runTrieHandler :: (Captures -> Application) -> WaiHandler
runTrieHandler = id
{-# INLINE runTrieHandler #-}
