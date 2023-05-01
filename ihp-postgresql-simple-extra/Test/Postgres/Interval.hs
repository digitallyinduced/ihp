{-|
Module: Test.Postgres.Interval
Copyright: (c) digitally induced GmbH, 2023
-}
module Test.Postgres.Interval where

import Test.Hspec
import IHP.Postgres.Interval
import IHP.Postgres.TimeParser
import Database.PostgreSQL.Simple.ToField
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec

tests = do
    describe "Interval" do
        describe "Parser" do
            it "Should Parse" do
                unpackInterval (PGInterval "25 years 6 mons 4 days 114:00:00.123") `shouldBe`  (PGTimeInterval 25 6 4 410400.123)
            it "Should Parse" do
               unpackInterval (PGInterval "25 years 01:00:00") `shouldBe`  (PGTimeInterval 25 0 0 3600)
            it "Should Parse" do
                unpackInterval (PGInterval "11 years 10 mons 683 days") `shouldBe` (PGTimeInterval 11 10 683 0)
