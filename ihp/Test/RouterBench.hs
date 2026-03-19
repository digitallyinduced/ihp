{-# LANGUAGE AllowAmbiguousTypes, OverloadedStrings, BangPatterns, ScopedTypeVariables, TypeFamilies, DeriveDataTypeable #-}
module Main where

import Prelude hiding (take)
import Data.ByteString (ByteString)
import Data.Data
import Network.Wai (defaultRequest, Request(..))
import IHP.RouterSupport hiding (get)
import IHP.ControllerSupport
import Test.Tasty.Bench

-- Application type for benchmarking
data BenchApp = BenchApp deriving (Eq, Show, Data)

-- 5 controllers with 15 actions each = 75 total routes
-- All in module Main, so prefix is "/main/"
-- Action names follow pattern: <ControllerPrefix><Letter>Action

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

-- AutoRoute instances (all defaults)
instance AutoRoute BenchController1
instance AutoRoute BenchController2
instance AutoRoute BenchController3
instance AutoRoute BenchController4
instance AutoRoute BenchController5

-- Controller instances (action never called during routing benchmark)
instance Controller BenchController1 where action _ = error "not called"
instance Controller BenchController2 where action _ = error "not called"
instance Controller BenchController3 where action _ = error "not called"
instance Controller BenchController4 where action _ = error "not called"
instance Controller BenchController5 where action _ = error "not called"

instance InitControllerContext BenchApp where
    initContext = pure ()

instance FrontController BenchApp where
    controllers =
        [ parseAutoRoute @BenchController1
        , parseAutoRoute @BenchController2
        , parseAutoRoute @BenchController3
        , parseAutoRoute @BenchController4
        , parseAutoRoute @BenchController5
        ]

-- Mock request for benchmarking
mockRequest :: ByteString -> Request
mockRequest path = defaultRequest
    { rawPathInfo = path
    , requestMethod = "GET"
    , queryString = []
    }

-- Benchmark direct dispatch (like frontControllerToWAIApp does: findInRouteMaps)
benchDirectDispatch :: ByteString -> ()
benchDirectDispatch !path =
    let ?request = mockRequest path
        ?respond = error "not used"
        ?application = BenchApp
    in let allRoutes = controllers @BenchApp
    in case findInRouteMaps path allRoutes of
        Just _ -> ()
        Nothing -> ()

-- Benchmark single controller direct dispatch
data SingleControllerApp = SingleControllerApp deriving (Eq, Show, Data)

instance InitControllerContext SingleControllerApp where
    initContext = pure ()

instance FrontController SingleControllerApp where
    controllers = [ parseAutoRoute @BenchController1 ]

benchSingleDirect :: ByteString -> ()
benchSingleDirect !path =
    let ?request = mockRequest path
        ?respond = error "not used"
        ?application = SingleControllerApp
    in let allRoutes = controllers @SingleControllerApp
    in case findInRouteMaps path allRoutes of
        Just _ -> ()
        Nothing -> ()

main :: IO ()
main = defaultMain
    [ bgroup "single controller (15 actions)"
        [ bench "first action" $ whnf benchSingleDirect "/main/Ctrl1Alpha"
        , bench "last action"  $ whnf benchSingleDirect "/main/Ctrl1Oscar"
        , bench "miss (wrong name)" $ whnf benchSingleDirect "/main/NoSuchAction"
        , bench "miss (wrong prefix)" $ whnf benchSingleDirect "/other/Ctrl1Alpha"
        ]
    , bgroup "5 controllers x 15 actions (75 routes)"
        [ bench "match controller 1, first action" $ whnf benchDirectDispatch "/main/Ctrl1Alpha"
        , bench "match controller 1, last action"  $ whnf benchDirectDispatch "/main/Ctrl1Oscar"
        , bench "match controller 3, middle"       $ whnf benchDirectDispatch "/main/Ctrl3Golf"
        , bench "match controller 5, last action"  $ whnf benchDirectDispatch "/main/Ctrl5Oscar"
        , bench "miss (no match)"                  $ whnf benchDirectDispatch "/main/NoSuchAction"
        , bench "miss (wrong prefix)"              $ whnf benchDirectDispatch "/other/Ctrl1Alpha"
        ]
    ]
