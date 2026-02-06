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
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import Hasql.Statement (Statement(..))
import qualified Hasql.DynamicStatements.Statement as DynStatement
import qualified Hasql.Decoders as Decoders
import qualified Data.Text as Text

tests = do
    describe "The GraphQL Compiler" do
        it "should compile a trivial selection" do
            compileGQL "{ users { id email } }" [] `shouldBe` "SELECT to_json(_root.data) FROM ((SELECT json_build_object($1, json_agg(\"_users\".*)) AS data FROM (SELECT \"users\".\"id\", \"users\".\"email\" FROM \"users\") AS \"_users\")) AS _root"

        it "should compile a trivial selection with an alias" do
            compileGQL "{ users { id userEmail: email } }" [] `shouldBe` [trimming|
                SELECT to_json(_root.data) FROM ((SELECT json_build_object($1, json_agg("_users".*)) AS data FROM (SELECT "users"."id", "users"."email" AS "userEmail" FROM "users") AS "_users")) AS _root
            |]
        it "should compile a trivial selection with a fragment spread" do
            let query = [trimming|
                query {
                    users {
                        id
                        ...userFragment
                    }
                }

                fragment userFragment { email }
            |]
            compileGQL query [] `shouldBe` [trimming|
                SELECT to_json(_root.data) FROM ((SELECT json_build_object($1, json_agg("_users".*)) AS data FROM (SELECT "users"."id", "users"."email" FROM "users") AS "_users")) AS _root
            |]

        it "should compile a selection set accessing multiple tables" do
            compileGQL "{ users { id } tasks { id title } }" [] `shouldBe` [trimming|
                SELECT to_json(_root.data) FROM ((SELECT json_build_object($1, json_agg("_users".*)) AS data FROM (SELECT "users"."id" FROM "users") AS "_users") UNION ALL (SELECT json_build_object($2, json_agg("_tasks".*)) AS data FROM (SELECT "tasks"."id", "tasks"."title" FROM "tasks") AS "_tasks")) AS _root
            |]
        it "should compile a named query" do
            compileGQL "query GetUsers { users { id } tasks { id title } }" [] `shouldBe` [trimming|
                SELECT to_json(_root.data) FROM ((SELECT json_build_object($1, json_agg("_users".*)) AS data FROM (SELECT "users"."id" FROM "users") AS "_users") UNION ALL (SELECT json_build_object($2, json_agg("_tasks".*)) AS data FROM (SELECT "tasks"."id", "tasks"."title" FROM "tasks") AS "_tasks")) AS _root
            |]
        it "should compile a 'user(id: $id)' selection" do
            compileGQL "{ user(id: \"dde8fd2c-4941-4262-a8e0-cc4cd40bacba\") { id } }" [] `shouldBe` [trimming|
                SELECT to_json(_root.data) FROM ((SELECT json_build_object($1, json_agg("_user".*)) AS data FROM (SELECT "users"."id" FROM "users" WHERE id = $2) AS "_user")) AS _root
            |]
        it "should compile a single selection with a one-to-many relationship" do
            compileGQL "{ user(id: \"40f1dbb4-403c-46fd-8062-fcf5362f2154\") { id email tasks { id title } } }" [] `shouldBe` [trimming|
                SELECT to_json(_root.data) FROM ((SELECT json_build_object($1, json_agg("_user".*)) AS data FROM (SELECT "users"."id", "users"."email", "tasks" FROM "users" LEFT JOIN LATERAL (SELECT ARRAY(SELECT to_json(_sub) FROM (SELECT "tasks"."id", "tasks"."title" FROM "tasks" WHERE "tasks"."user_id" = "users"."id") AS _sub) AS "tasks") "tasks" ON true WHERE id = $2) AS "_user")) AS _root
            |]
        it "should compile a multi selection with a one-to-many relationship" do
            compileGQL "{ users { id email tasks { id title } } }" [] `shouldBe` [trimming|
                SELECT to_json(_root.data) FROM ((SELECT json_build_object($1, json_agg("_users".*)) AS data FROM (SELECT "users"."id", "users"."email", "tasks" FROM "users" LEFT JOIN LATERAL (SELECT ARRAY(SELECT to_json(_sub) FROM (SELECT "tasks"."id", "tasks"."title" FROM "tasks" WHERE "tasks"."user_id" = "users"."id") AS _sub) AS "tasks") "tasks" ON true) AS "_users")) AS _root
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
                 INSERT INTO "projects" ("user_id", "title") VALUES ($1, $2) RETURNING json_build_object($3, "projects"."id", $4, "projects"."title")
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
                 DELETE FROM "projects" WHERE id = $1 RETURNING json_build_object($2, "projects"."id", $3, "projects"."title")
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
                 UPDATE "projects" SET "user_id" = $1, "title" = $2 WHERE id = $3 RETURNING json_build_object($4, "projects"."id", $5, "projects"."title")
            |]

-- | Convert a Snippet to its SQL text representation for testing purposes.
snippetToSql :: Snippet -> ByteString
snippetToSql snippet = case DynStatement.dynamicallyParameterized snippet Decoders.noResult False of
    Statement sql _ _ _ -> sql

compileGQL gql arguments = gql
        |> parseGQL
        |> Compiler.compileDocument (Variables arguments)
        |> map (cs . snippetToSql)
        |> intercalate "\n"
