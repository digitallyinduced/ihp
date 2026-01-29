{-|
Module: Test.Postgres.Point
Copyright: (c) digitally induced GmbH, 2021
-}
module Test.Postgres.Point where

import Data.Either
import Test.Hspec
import IHP.Postgres.Point
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec

tests = do
    let raw = "(100,200)"
    let parsed = Point { x = 100.0, y = 200.0 }

    describe "Point" do
        describe "Parser" do
            it "Should Parse" do
                Attoparsec.parseOnly parsePoint raw `shouldBe` Right parsed

        describe "Serializer" do
            it "Should Serialize to Text" do
                pointToText parsed `shouldBe` "(100.0,200.0)"
