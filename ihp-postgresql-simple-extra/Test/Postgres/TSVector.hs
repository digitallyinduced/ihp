{-|
Module: Test.Postgres.TSVector
Copyright: (c) digitally induced GmbH, 2021
-}
module Test.Postgres.TSVector where

import Prelude
import Test.Hspec
import Test.Postgres.Support ()
import IHP.Postgres.TSVector

tests = do
    let raw = "'dummi':9 'industri':16 'ipsum':4,6 'lorem':3,5 'print':13 'simpli':8 'text':10 'typeset':15"
    let Just parsed = fromLexemeList
            [ ("dummi",    [(9,  WeightD)])
            , ("industri", [(16, WeightD)])
            , ("ipsum",    [(4,  WeightD), (6, WeightD)])
            , ("lorem",    [(3,  WeightD), (5, WeightD)])
            , ("print",    [(13, WeightD)])
            , ("simpli",   [(8,  WeightD)])
            , ("text",     [(10, WeightD)])
            , ("typeset",  [(15, WeightD)])
            ]

    describe "TSVector" do
        describe "Tsvector type" do
            it "should construct from lexeme list" do
                toLexemeList parsed `shouldSatisfy` (not . null)
