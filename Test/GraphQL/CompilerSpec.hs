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
import Test.GraphQL.ParserSpec (parseGQL, parseValue)
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import Test.Postgres.Support
import qualified Data.Text as Text
import qualified Data.ByteString.Builder

tests = do
    describe "The GraphQL Compiler" do
        it "should compile a trivial selection" do
            compileGQL "{ users { id email } }" [] `shouldBe` "SELECT json_agg(_root.data) FROM ((SELECT json_build_object('users', json_agg(_users.*)) AS data FROM (SELECT id, email FROM users) AS _users)) AS _root"

        it "should compile a trivial selection with an alias" do
            compileGQL "{ users { id userEmail: email } }" [] `shouldBe` [trimming|
                SELECT json_agg(_root.data) FROM ((SELECT json_build_object('users', json_agg(_users.*)) AS data FROM (SELECT id, email AS "userEmail" FROM users) AS _users)) AS _root
            |]
        
        it "should compile a selection set accessing multiple tables" do
            compileGQL "{ users { id } tasks { id title } }" [] `shouldBe` [trimming|
                 SELECT json_agg(_root.data) FROM ((SELECT json_build_object('users', json_agg(_users.*)) AS data FROM (SELECT id FROM users) AS _users) UNION ALL (SELECT json_build_object('tasks', json_agg(_tasks.*)) AS data FROM (SELECT id, title FROM tasks) AS _tasks)) AS _root
            |]
        it "should compile a named query" do
            compileGQL "query GetUsers { users { id } tasks { id title } }" [] `shouldBe` [trimming|
                 SELECT json_agg(_root.data) FROM ((SELECT json_build_object('users', json_agg(_users.*)) AS data FROM (SELECT id FROM users) AS _users) UNION ALL (SELECT json_build_object('tasks', json_agg(_tasks.*)) AS data FROM (SELECT id, title FROM tasks) AS _tasks)) AS _root
            |]
        it "should compile a 'user(id: $id)' selection" do
            compileGQL "{ user(id: \"dde8fd2c-4941-4262-a8e0-cc4cd40bacba\") { id } }" [] `shouldBe` [trimming|
                 SELECT json_agg(_root.data) FROM ((SELECT json_build_object('user', json_agg(_user.*)) AS data FROM (SELECT id FROM users WHERE id = 'dde8fd2c-4941-4262-a8e0-cc4cd40bacba') AS _user)) AS _root
            |]
        it "should compile a create mutation" do
            let mutation = [trimming|
                mutation CreateProject($$project: Project) {
                    createProject(project: $$project) {
                        id title
                    }
                }
            |]
            let arguments = [
                    Argument
                        { argumentName = "project"
                        , argumentValue = parseValue [trimming|
                            { title: "Hello World"
                            , userId: "dc984c2f-d91c-4143-9091-400ad2333f83"
                            }
                        |] }
                    ]
            compileGQL mutation arguments `shouldBe` [trimming|
                 INSERT INTO projects (user_id, title) VALUES ('dc984c2f-d91c-4143-9091-400ad2333f83', 'Hello World') RETURNING json_build_object('id', projects.id, 'title', projects.title)
            |]
        it "should compile a delete mutation" do
            let mutation = [trimming|
                mutation DeleteProject($$projectId: ProjectId) {
                    deleteProject(id: $$projectId) {
                        id title
                    }
                }
            |]
            let arguments = [
                    Argument
                        { argumentName = "projectId"
                        , argumentValue = parseValue [trimming|
                            "dc984c2f-d91c-4143-9091-400ad2333f83"
                        |] }
                    ]
            compileGQL mutation arguments `shouldBe` [trimming|
                 DELETE FROM projects WHERE id = 'dc984c2f-d91c-4143-9091-400ad2333f83' RETURNING json_build_object('id', projects.id, 'title', projects.title)
            |]
        it "should compile a update mutation" do
            let mutation = [trimming|
                mutation UpdateProject($$projectId: ProjectId, $$patch: ProjectPatch) {
                    updateProject(id: $$projectId, set: $$patch) {
                        id title
                    }
                }
            |]
            let arguments = [
                    Argument
                        { argumentName = "patch"
                        , argumentValue = parseValue [trimming|
                            { title: "Hello World"
                            , userId: "dc984c2f-d91c-4143-9091-400ad2333f83"
                            }
                        |] }
                    , Argument
                        { argumentName = "projectId"
                        , argumentValue = parseValue [trimming|"df1f54d5-ced6-4f65-8aea-fcd5ea6b9df1"|] }
                    ]
            compileGQL mutation arguments `shouldBe` [trimming|
                 UPDATE projects SET user_id = 'dc984c2f-d91c-4143-9091-400ad2333f83', title = 'Hello World' WHERE id = 'df1f54d5-ced6-4f65-8aea-fcd5ea6b9df1' RETURNING json_build_object('id', projects.id, 'title', projects.title)
            |]

compileGQL gql arguments = gql
        |> parseGQL
        |> Compiler.compileDocument arguments
        |> map substituteParams
        |> intercalate "\n"

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


