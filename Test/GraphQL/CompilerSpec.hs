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
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import Test.Postgres.Support
import qualified Data.Text as Text
import qualified Data.ByteString.Builder

tests = do
    describe "The GraphQL Compiler" do
        it "should compile a trivial selection" do
            compileGQL "{ users { id email } }"  `shouldBe` "SELECT json_agg(_root.data) FROM ((SELECT json_build_object('users', json_agg(_users.*)) AS data FROM (SELECT id, email FROM users) AS _users)) AS _root"

        it "should compile a trivial selection with an alias" do
            compileGQL "{ users { id userEmail: email } }"  `shouldBe` [trimming|
                SELECT json_agg(_root.data) FROM ((SELECT json_build_object('users', json_agg(_users.*)) AS data FROM (SELECT id, email AS "userEmail" FROM users) AS _users)) AS _root
            |]
        
        it "should compile a selection set accessing multiple tables" do
            compileGQL "{ users { id } tasks { id title } }"  `shouldBe` [trimming|
                 SELECT json_agg(_root.data) FROM ((SELECT json_build_object('users', json_agg(_users.*)) AS data FROM (SELECT id FROM users) AS _users) UNION ALL (SELECT json_build_object('tasks', json_agg(_tasks.*)) AS data FROM (SELECT id, title FROM tasks) AS _tasks)) AS _root
            |]

compileGQL gql = gql
        |> parseGQL
        |> Compiler.compileDocument
        |> substituteParams

substituteParams :: (PG.Query, [PG.Action]) -> Text
substituteParams (PG.Query query, params) = 
        query
        |> cs
        |> Text.splitOn "?"
        |> \q -> zip q (params <> (repeat (PG.Plain "")))
        |> foldl' foldQuery ""
    where
        foldQuery acc (query, action) = acc <> query <> actionToText action

        actionToText (PG.Plain plain) = cs (Data.ByteString.Builder.toLazyByteString plain)
        actionToText (PG.Escape escape) = "'" <> cs escape <> "'"
        actionToText (PG.EscapeIdentifier escape) | cs escape == Text.toLower (cs escape) = cs escape
        actionToText (PG.EscapeIdentifier escape) = "\"" <> cs escape <> "\""