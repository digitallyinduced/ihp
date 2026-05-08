{-# LANGUAGE AllowAmbiguousTypes, OverloadedStrings, BangPatterns, ScopedTypeVariables, TypeFamilies, DeriveDataTypeable #-}
module Main where

import Prelude
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Data
import Network.Wai (defaultRequest, Request(..), responseLBS)
import Network.HTTP.Types.Method (StdMethod(..))
import Network.HTTP.Types (status200)
import IHP.RouterSupport hiding (get)
import IHP.ControllerSupport
import qualified IHP.Router.Trie as Trie
import IHP.Router.Trie (RouteTrie, PatternSegment(..), LookupResult(..))
import qualified IHP.Router.Capture as Capture
import Test.Tasty.Bench

-- ============================================================================
-- Test fixture: 5 controllers × 15 actions = 75 static routes.
--
-- We benchmark two dispatch paths for the SAME 75 URLs:
--
--   (a) the legacy AutoRoute HashMap ('findInRouteMaps'), which keys on
--       full paths like "/main/Ctrl3Golf" — one HashMap per controller,
--       scanned in order.
--   (b) the new 'RouteTrie' ('Trie.lookupTrie'), which walks segments and
--       is method-aware at the leaf — one trie for the whole app.
--
-- Action names follow the NATO phonetic alphabet so the 15 actions per
-- controller are easy to read.
-- ============================================================================

data BenchApp = BenchApp deriving (Eq, Show, Data)

data BenchController1
    = Ctrl1AlphaAction | Ctrl1BravoAction | Ctrl1CharlieAction | Ctrl1DeltaAction | Ctrl1EchoAction
    | Ctrl1FoxtrotAction | Ctrl1GolfAction | Ctrl1HotelAction | Ctrl1IndiaAction | Ctrl1JulietAction
    | Ctrl1KiloAction | Ctrl1LimaAction | Ctrl1MikeAction | Ctrl1NovemberAction | Ctrl1OscarAction
    deriving (Eq, Show, Data)

data BenchController2
    = Ctrl2AlphaAction | Ctrl2BravoAction | Ctrl2CharlieAction | Ctrl2DeltaAction | Ctrl2EchoAction
    | Ctrl2FoxtrotAction | Ctrl2GolfAction | Ctrl2HotelAction | Ctrl2IndiaAction | Ctrl2JulietAction
    | Ctrl2KiloAction | Ctrl2LimaAction | Ctrl2MikeAction | Ctrl2NovemberAction | Ctrl2OscarAction
    deriving (Eq, Show, Data)

data BenchController3
    = Ctrl3AlphaAction | Ctrl3BravoAction | Ctrl3CharlieAction | Ctrl3DeltaAction | Ctrl3EchoAction
    | Ctrl3FoxtrotAction | Ctrl3GolfAction | Ctrl3HotelAction | Ctrl3IndiaAction | Ctrl3JulietAction
    | Ctrl3KiloAction | Ctrl3LimaAction | Ctrl3MikeAction | Ctrl3NovemberAction | Ctrl3OscarAction
    deriving (Eq, Show, Data)

data BenchController4
    = Ctrl4AlphaAction | Ctrl4BravoAction | Ctrl4CharlieAction | Ctrl4DeltaAction | Ctrl4EchoAction
    | Ctrl4FoxtrotAction | Ctrl4GolfAction | Ctrl4HotelAction | Ctrl4IndiaAction | Ctrl4JulietAction
    | Ctrl4KiloAction | Ctrl4LimaAction | Ctrl4MikeAction | Ctrl4NovemberAction | Ctrl4OscarAction
    deriving (Eq, Show, Data)

data BenchController5
    = Ctrl5AlphaAction | Ctrl5BravoAction | Ctrl5CharlieAction | Ctrl5DeltaAction | Ctrl5EchoAction
    | Ctrl5FoxtrotAction | Ctrl5GolfAction | Ctrl5HotelAction | Ctrl5IndiaAction | Ctrl5JulietAction
    | Ctrl5KiloAction | Ctrl5LimaAction | Ctrl5MikeAction | Ctrl5NovemberAction | Ctrl5OscarAction
    deriving (Eq, Show, Data)

instance AutoRoute BenchController1
instance AutoRoute BenchController2
instance AutoRoute BenchController3
instance AutoRoute BenchController4
instance AutoRoute BenchController5

instance Controller BenchController1 where action _ = error "not called"
instance Controller BenchController2 where action _ = error "not called"
instance Controller BenchController3 where action _ = error "not called"
instance Controller BenchController4 where action _ = error "not called"
instance Controller BenchController5 where action _ = error "not called"

instance InitControllerContext BenchApp where
    initContext = pure ()

instance FrontController BenchApp where
    controllers =
        [ parseRoute @BenchController1
        , parseRoute @BenchController2
        , parseRoute @BenchController3
        , parseRoute @BenchController4
        , parseRoute @BenchController5
        ]

data SingleControllerApp = SingleControllerApp deriving (Eq, Show, Data)

instance InitControllerContext SingleControllerApp where
    initContext = pure ()

instance FrontController SingleControllerApp where
    controllers = [ parseRoute @BenchController1 ]

mockRequest :: ByteString -> Request
mockRequest path = defaultRequest
    { rawPathInfo = path
    , requestMethod = "GET"
    , queryString = []
    }

-- ---------------------------------------------------------------------------
-- (a) AutoRoute dispatch
-- ---------------------------------------------------------------------------

benchAutoRoute :: ByteString -> ()
benchAutoRoute !path =
    let ?request = mockRequest path
        ?respond = error "not used"
        ?application = BenchApp
    in let allRoutes = controllers @BenchApp
    in case findInRouteMaps path allRoutes of
        Just _ -> ()
        Nothing -> ()

benchAutoRouteSingle :: ByteString -> ()
benchAutoRouteSingle !path =
    let ?request = mockRequest path
        ?respond = error "not used"
        ?application = SingleControllerApp
    in let allRoutes = controllers @SingleControllerApp
    in case findInRouteMaps path allRoutes of
        Just _ -> ()
        Nothing -> ()

-- ---------------------------------------------------------------------------
-- (b) RouteTrie dispatch with the SAME 75 URLs
-- ---------------------------------------------------------------------------

-- | No-op handler. We measure lookup cost, not handler body execution.
stubHandler :: Trie.WaiHandler
stubHandler _captures _req respond = respond (responseLBS status200 [] "")

-- | Constructor names for all 75 AutoRoute-generated paths, with the
-- "Action" suffix stripped. Matches the URLs AutoRoute produces
-- (@/main/Ctrl1Alpha@, @/main/Ctrl1Bravo@, ...).
allActionNames :: [ByteString]
allActionNames = map toPath $ concat
    [ map show ctrl1Constructors
    , map show ctrl2Constructors
    , map show ctrl3Constructors
    , map show ctrl4Constructors
    , map show ctrl5Constructors
    ]
  where
    toPath :: String -> ByteString
    toPath = BS8.pack . stripSuffix "Action"

    stripSuffix :: String -> String -> String
    stripSuffix suf s =
        let n = length s - length suf
         in if drop n s == suf then take n s else s

    ctrl1Constructors =
        [ Ctrl1AlphaAction, Ctrl1BravoAction, Ctrl1CharlieAction, Ctrl1DeltaAction, Ctrl1EchoAction
        , Ctrl1FoxtrotAction, Ctrl1GolfAction, Ctrl1HotelAction, Ctrl1IndiaAction, Ctrl1JulietAction
        , Ctrl1KiloAction, Ctrl1LimaAction, Ctrl1MikeAction, Ctrl1NovemberAction, Ctrl1OscarAction
        ]
    ctrl2Constructors =
        [ Ctrl2AlphaAction, Ctrl2BravoAction, Ctrl2CharlieAction, Ctrl2DeltaAction, Ctrl2EchoAction
        , Ctrl2FoxtrotAction, Ctrl2GolfAction, Ctrl2HotelAction, Ctrl2IndiaAction, Ctrl2JulietAction
        , Ctrl2KiloAction, Ctrl2LimaAction, Ctrl2MikeAction, Ctrl2NovemberAction, Ctrl2OscarAction
        ]
    ctrl3Constructors =
        [ Ctrl3AlphaAction, Ctrl3BravoAction, Ctrl3CharlieAction, Ctrl3DeltaAction, Ctrl3EchoAction
        , Ctrl3FoxtrotAction, Ctrl3GolfAction, Ctrl3HotelAction, Ctrl3IndiaAction, Ctrl3JulietAction
        , Ctrl3KiloAction, Ctrl3LimaAction, Ctrl3MikeAction, Ctrl3NovemberAction, Ctrl3OscarAction
        ]
    ctrl4Constructors =
        [ Ctrl4AlphaAction, Ctrl4BravoAction, Ctrl4CharlieAction, Ctrl4DeltaAction, Ctrl4EchoAction
        , Ctrl4FoxtrotAction, Ctrl4GolfAction, Ctrl4HotelAction, Ctrl4IndiaAction, Ctrl4JulietAction
        , Ctrl4KiloAction, Ctrl4LimaAction, Ctrl4MikeAction, Ctrl4NovemberAction, Ctrl4OscarAction
        ]
    ctrl5Constructors =
        [ Ctrl5AlphaAction, Ctrl5BravoAction, Ctrl5CharlieAction, Ctrl5DeltaAction, Ctrl5EchoAction
        , Ctrl5FoxtrotAction, Ctrl5GolfAction, Ctrl5HotelAction, Ctrl5IndiaAction, Ctrl5JulietAction
        , Ctrl5KiloAction, Ctrl5LimaAction, Ctrl5MikeAction, Ctrl5NovemberAction, Ctrl5OscarAction
        ]

ctrl1ActionNames :: [ByteString]
ctrl1ActionNames = take 15 allActionNames

staticTrieAll :: RouteTrie
staticTrieAll = foldr install Trie.emptyTrie allActionNames
  where
    install name = Trie.insertRoute [LiteralSeg "main", LiteralSeg name] GET stubHandler

staticTrieSingle :: RouteTrie
staticTrieSingle = foldr install Trie.emptyTrie ctrl1ActionNames
  where
    install name = Trie.insertRoute [LiteralSeg "main", LiteralSeg name] GET stubHandler

benchTrie :: RouteTrie -> ByteString -> ()
benchTrie !trie !path =
    case Trie.lookupTrie trie GET (Trie.splitPath path) of
        Matched _ _        -> ()
        MethodNotAllowed _ -> ()
        NotMatched         -> ()

-- ============================================================================
-- Capture-route benchmarks
--
-- Four parameterised routes:
--   GET /users/{id}
--   GET /users/{id}/edit
--   GET /posts/{postId}
--   GET /posts/{postId}/comments/{commentId}
--
-- 'benchTrieCaptures' is lookup only — shows the cost of collecting
-- captures as the trie walk produces them. 'benchTrieCapturesDecoded'
-- adds a single parseCapture @Int on the first capture to approximate
-- the handler work.
--
-- A head-to-head comparison with the previous Dynamic-based API is
-- included below the main function in a comment block.
-- ============================================================================

captureIdSpec :: Trie.CaptureSpec
captureIdSpec = Trie.CaptureSpec { Trie.captureName = "id" }

capturePostIdSpec :: Trie.CaptureSpec
capturePostIdSpec = Trie.CaptureSpec { Trie.captureName = "postId" }

captureCommentIdSpec :: Trie.CaptureSpec
captureCommentIdSpec = Trie.CaptureSpec { Trie.captureName = "commentId" }

captureTrie :: RouteTrie
captureTrie = foldr install Trie.emptyTrie routes
  where
    routes =
        [ [LiteralSeg "users", CaptureSeg captureIdSpec]
        , [LiteralSeg "users", CaptureSeg captureIdSpec, LiteralSeg "edit"]
        , [LiteralSeg "posts", CaptureSeg capturePostIdSpec]
        , [LiteralSeg "posts", CaptureSeg capturePostIdSpec
          , LiteralSeg "comments", CaptureSeg captureCommentIdSpec]
        ]
    install pat = Trie.insertRoute pat GET stubHandler

benchTrieCaptures :: ByteString -> ()
benchTrieCaptures !path =
    case Trie.lookupTrie captureTrie GET (Trie.splitPath path) of
        Matched _ caps     -> caps `seq` ()
        MethodNotAllowed _ -> ()
        NotMatched         -> ()

benchTrieCapturesDecoded :: ByteString -> Maybe Int
benchTrieCapturesDecoded !path =
    case Trie.lookupTrie captureTrie GET (Trie.splitPath path) of
        Matched _ (firstBs : _) -> Capture.parseCapture firstBs
        _                       -> Nothing

-- ============================================================================
-- main
-- ============================================================================

main :: IO ()
main = defaultMain
    [ bgroup "single controller (15 routes)"
        [ bgroup "AutoRoute (findInRouteMaps)"
            [ bench "first action"        $ whnf benchAutoRouteSingle "/main/Ctrl1Alpha"
            , bench "last action"         $ whnf benchAutoRouteSingle "/main/Ctrl1Oscar"
            , bench "miss (wrong name)"   $ whnf benchAutoRouteSingle "/main/NoSuchAction"
            , bench "miss (wrong prefix)" $ whnf benchAutoRouteSingle "/other/Ctrl1Alpha"
            ]
        , bgroup "RouteTrie (lookupTrie)"
            [ bench "first action"        $ whnf (benchTrie staticTrieSingle) "/main/Ctrl1Alpha"
            , bench "last action"         $ whnf (benchTrie staticTrieSingle) "/main/Ctrl1Oscar"
            , bench "miss (wrong name)"   $ whnf (benchTrie staticTrieSingle) "/main/NoSuchAction"
            , bench "miss (wrong prefix)" $ whnf (benchTrie staticTrieSingle) "/other/Ctrl1Alpha"
            ]
        ]
    , bgroup "5 controllers x 15 actions (75 routes)"
        [ bgroup "AutoRoute (findInRouteMaps)"
            [ bench "match ctrl 1, first"    $ whnf benchAutoRoute "/main/Ctrl1Alpha"
            , bench "match ctrl 1, last"     $ whnf benchAutoRoute "/main/Ctrl1Oscar"
            , bench "match ctrl 3, middle"   $ whnf benchAutoRoute "/main/Ctrl3Golf"
            , bench "match ctrl 5, last"     $ whnf benchAutoRoute "/main/Ctrl5Oscar"
            , bench "miss (no match)"        $ whnf benchAutoRoute "/main/NoSuchAction"
            , bench "miss (wrong prefix)"    $ whnf benchAutoRoute "/other/Ctrl1Alpha"
            ]
        , bgroup "RouteTrie (lookupTrie)"
            [ bench "match ctrl 1, first"    $ whnf (benchTrie staticTrieAll) "/main/Ctrl1Alpha"
            , bench "match ctrl 1, last"     $ whnf (benchTrie staticTrieAll) "/main/Ctrl1Oscar"
            , bench "match ctrl 3, middle"   $ whnf (benchTrie staticTrieAll) "/main/Ctrl3Golf"
            , bench "match ctrl 5, last"     $ whnf (benchTrie staticTrieAll) "/main/Ctrl5Oscar"
            , bench "miss (no match)"        $ whnf (benchTrie staticTrieAll) "/main/NoSuchAction"
            , bench "miss (wrong prefix)"    $ whnf (benchTrie staticTrieAll) "/other/Ctrl1Alpha"
            ]
        ]
    , bgroup "capture routes (RouteTrie)"
        [ bgroup "lookup only"
            [ bench "/users/42"               $ whnf benchTrieCaptures "/users/42"
            , bench "/users/42/edit"          $ whnf benchTrieCaptures "/users/42/edit"
            , bench "/posts/42"               $ whnf benchTrieCaptures "/posts/42"
            , bench "/posts/42/comments/7"    $ whnf benchTrieCaptures "/posts/42/comments/7"
            ]
        , bgroup "lookup + decode first capture as Int"
            [ bench "/users/42"               $ whnf benchTrieCapturesDecoded "/users/42"
            , bench "/users/42/edit"          $ whnf benchTrieCapturesDecoded "/users/42/edit"
            , bench "/posts/42"               $ whnf benchTrieCapturesDecoded "/posts/42"
            , bench "/posts/42/comments/7"    $ whnf benchTrieCapturesDecoded "/posts/42/comments/7"
            ]
        ]
    ]

-- ============================================================================
-- Dynamic-vs-positional head-to-head (reference results)
--
-- Same fixture, run on HEAD~1 (commit 8f2dc385, Dynamic-based) and HEAD
-- (commit 339bbfcd, positional [ByteString]) in ghci interpreted mode:
--
-- Test                                  Dynamic     Positional   Speedup   Alloc cut
-- /users/42              (lookup)        2.97 μs     1.67 μs      1.78×    6.4 → 2.4 KB  (-62%)
-- /users/42/edit         (lookup)        3.42 μs     2.18 μs      1.57×    7.0 → 3.0 KB  (-57%)
-- /posts/42              (lookup)        2.96 μs     1.71 μs      1.73×    6.4 → 2.4 KB  (-62%)
-- /posts/42/comments/7   (lookup)        5.11 μs     2.82 μs      1.81×    12  → 3.9 KB  (-67%)
-- /users/42              (+ decode)      3.26 μs     2.50 μs      1.30×    6.8 → 5.9 KB  (-13%)
-- /users/42/edit         (+ decode)      3.70 μs     2.85 μs      1.30×    7.4 → 6.5 KB  (-12%)
-- /posts/42              (+ decode)      3.57 μs     2.44 μs      1.46×    7.5 → 5.9 KB  (-21%)
-- /posts/42/comments/7   (+ decode)      5.88 μs     3.48 μs      1.69×    13  → 7.4 KB  (-43%)
--
-- The trie walk itself is where the real win is (≈1.7× faster, ≈60% less
-- allocation) — dropping the toDyn/HashMap-insert per capture. For the
-- full lookup+decode path the win shrinks to ≈1.3–1.7× because the
-- decoder runs in both cases; but even there, the positional version
-- avoids the fromDynamic TypeRep check and the HashMap.lookup.
-- ============================================================================
