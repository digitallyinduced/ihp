{-|
Module: Test.Postgres.TSVector
Copyright: (c) digitally induced GmbH, 2021
-}
module Test.Postgres.TSVector where

import Test.Hspec
import Test.Postgres.Support
import IHP.Prelude
import IHP.Postgres.TSVector
import Database.PostgreSQL.Simple.ToField
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec

tests = do
    let raw = "'dummi':9 'industri':16 'ipsum':4,6 'lorem':3,5 'print':13 'simpli':8 'text':10 'typeset':15"
    let parsed = TSVector
            [ Lexeme { token = "dummi",    ranking = [ LexemeRanking { position = 9,  weight = 'D'} ] }
            , Lexeme { token = "industri", ranking = [ LexemeRanking { position = 16, weight = 'D' } ] }
            , Lexeme { token = "ipsum",    ranking = [ LexemeRanking { position = 4,  weight = 'D' }, LexemeRanking { position = 6, weight = 'D'} ] }
            , Lexeme { token = "lorem",    ranking = [ LexemeRanking { position = 3,  weight = 'D' }, LexemeRanking { position = 5, weight = 'D'} ] }
            , Lexeme { token = "print",    ranking = [ LexemeRanking { position = 13, weight = 'D' } ] }
            , Lexeme { token = "simpli",   ranking = [ LexemeRanking { position = 8,  weight = 'D' } ] }
            , Lexeme { token = "text",     ranking = [ LexemeRanking { position = 10, weight = 'D' } ] }
            , Lexeme { token = "typeset",  ranking = [ LexemeRanking { position = 15, weight = 'D' } ] }
            ]
    let serialized = Many
            [ Many [ Plain "dummi",    Plain ":",  Many [ Many [ Plain "9",  Plain "D" ] ] ]
            , Many [ Plain "industri", Plain ":",  Many [ Many [ Plain "16", Plain "D" ] ] ]
            , Many [ Plain "ipsum",    Plain ":",  Many [ Many [ Plain "4",  Plain "D" ], Plain ",", Many [ Plain "6", Plain "D" ] ] ]
            , Many [ Plain "lorem",    Plain ":",  Many [ Many [ Plain "3",  Plain "D" ], Plain ",", Many [ Plain "5", Plain "D" ] ] ]
            , Many [ Plain "print",    Plain ":",  Many [ Many [ Plain "13", Plain "D" ] ] ]
            , Many [ Plain "simpli",   Plain ":",  Many [ Many [ Plain "8",  Plain "D" ] ] ]
            , Many [ Plain "text",     Plain ":",  Many [ Many [ Plain "10", Plain "D" ] ] ]
            , Many [ Plain "typeset",  Plain ":",  Many [ Many [ Plain "15", Plain "D" ] ] ]
            ]

    describe "TSVector" do
        describe "Parser" do
            it "Should Parse" do
                Attoparsec.parseOnly parseTSVector raw `shouldBe` Right parsed

        describe "Serializer" do
            it "Should Serialize" do
                serializeTSVector parsed `shouldBe` serialized
