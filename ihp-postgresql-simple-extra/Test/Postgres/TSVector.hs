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
            [ ("dummi",    [(9,  DWeight)])
            , ("industri", [(16, DWeight)])
            , ("ipsum",    [(4,  DWeight), (6, DWeight)])
            , ("lorem",    [(3,  DWeight), (5, DWeight)])
            , ("print",    [(13, DWeight)])
            , ("simpli",   [(8,  DWeight)])
            , ("text",     [(10, DWeight)])
            , ("typeset",  [(15, DWeight)])
            ]

    describe "TSVector" do
        describe "Tsvector type" do
            it "should construct from lexeme list" do
                toLexemeList parsed `shouldSatisfy` (not . null)
