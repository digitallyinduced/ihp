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
    ) where

import Prelude
import Data.Text (Text)
import Network.HTTP.Types (status404)
import Network.HTTP.Types.Method (StdMethod)
import Network.Wai (Application, responseLBS)

import IHP.Router.Trie

-- | Build a 'RouteTrie' from a compile-time-known list of
-- @(method, pattern, handler)@ triples. The TH splice produces one call
-- to this function per @[routes|...|]@ block.
buildRouteTrie :: [(StdMethod, [PatternSegment], WaiHandler)] -> RouteTrie
buildRouteTrie = foldr step emptyTrie
  where
    step (m, p, h) t = insertRoute p m h t
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
