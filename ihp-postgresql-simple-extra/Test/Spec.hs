module Main where

import CorePrelude
import Test.Hspec

import qualified Test.Postgres.Point
import qualified Test.Postgres.Polygon
import qualified Test.Postgres.Interval
import qualified Test.Postgres.TSVector

main :: IO ()
main = hspec do
    Test.Postgres.Point.tests
    Test.Postgres.Polygon.tests
    Test.Postgres.Interval.tests
    Test.Postgres.TSVector.tests