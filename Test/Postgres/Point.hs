{-|
Module: Test.Postgres.Point
Copyright: (c) digitally induced GmbH, 2021
-}
module Test.Postgres.Point where

import Test.Hspec
import Test.Postgres.Support
import IHP.Prelude
import IHP.Postgres.Point
import Database.PostgreSQL.Simple.ToField
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec

tests = do
    let raw = "(100,200)"
    let parsed = Point { x = 100.0, y = 200.0 }
    let serialized = Many [ Plain "point(", Plain "100.0", Plain ",", Plain "200.0",Plain ")" ]

    describe "Point" do
        describe "Parser" do
            it "Should Parse" do
                Attoparsec.parseOnly parsePoint raw `shouldBe` Right parsed

        describe "Serializer" do
            it "Should Serialize" do
                serializePoint parsed `shouldBe` serialized
