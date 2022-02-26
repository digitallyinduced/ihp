{-|
Module: Test.GraphQL.CompilerSpec
Copyright: (c) digitally induced GmbH, 2022
-}
module Test.GraphQL.CompilerSpec where

import Test.Hspec
import IHP.Prelude
import qualified IHP.GraphQL.Compiler as Compiler
import IHP.GraphQL.Types
import qualified Data.Attoparsec.Text as Attoparsec
import Test.GraphQL.ParserSpec (parseGQL)
import qualified Database.PostgreSQL.Simple.ToField as PG
import Test.Postgres.Support

tests = do
    describe "The GraphQL Compiler" do
        it "should compile a trivial selection" do
            compileGQL "{ user }"  `shouldBe` (
                "SELECT json_agg(_root.data) FROM ((SELECT json_build_object(?, json_agg(?.*)) AS data FROM (SELECT * FROM ?) AS ?)) AS _root",
                    [PG.Escape "user", PG.EscapeIdentifier "_user", PG.EscapeIdentifier "user", PG.EscapeIdentifier "_user"]
                )

compileGQL gql = Compiler.compileDocument (parseGQL gql)