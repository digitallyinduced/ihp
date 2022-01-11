{-|
Module: Test.Postgres.Polygon
Copyright: (c) digitally induced GmbH, 2021
-}
module Test.Postgres.Polygon where

import Test.Hspec
import Test.Postgres.Support
import IHP.Prelude
import IHP.Postgres.Point
import IHP.Postgres.Polygon
import Database.PostgreSQL.Simple.ToField
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec

tests = do
    let rawPoint1 = "(100,200)"
    let parsedPoint1 = Point { x = 100, y = 200 }
    let rawPoint2 = "(300,400)"
    let parsedPoint2 = Point { x = 300, y = 400 }
    let raw = "(" ++ rawPoint1 ++ "," ++ rawPoint2 ++ ")"
    let parsed = Polygon { points = [ parsedPoint1, parsedPoint2 ] }
    let serialized = Many
            [ Plain "polygon'"
            , Many
                [ Plain "("
                , Plain "100.0"
                , Plain ","
                , Plain "200.0"
                , Plain ")"
                ]
            , Plain ","
            , Many
                [ Plain "("
                , Plain "300.0"
                , Plain ","
                , Plain "400.0"
                , Plain ")"
                ]
            , Plain "'"
            ]

    describe "Polygon" do
        describe "Parser" do
            it "Should Parse" do
                Attoparsec.parseOnly parsePolygon raw `shouldBe` Right parsed

        describe "Serializer" do
            it "Should Serialize" do
                serializePolygon parsed `shouldBe` serialized
