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
import Test.IDE.SchemaDesigner.ParserSpec (parseSqlStatements)
import qualified IHP.GraphQL.SchemaCompiler as SchemaCompiler

tests = do
    describe "The GraphQL Compiler" do
        it "should compile a trivial selection" do
            compileGQL "{ users { id email } }" [] `shouldBe` "SELECT json_build_object('users', json_agg(_users)) FROM (SELECT users.id, users.email FROM users) AS _users"

        it "should compile a trivial selection with an alias" do
            compileGQL "{ users { id userEmail: email } }" [] `shouldBe` [trimming|
                SELECT json_build_object('users', json_agg(_users)) FROM (SELECT users.id, users.email AS "userEmail" FROM users) AS _users
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
                SELECT json_build_object('users', json_agg(_users)) FROM (SELECT users.id, users.email FROM users) AS _users
            |]
        
        it "should compile a selection set accessing multiple tables" do
            compileGQL "{ users { id } tasks { id title } }" [] `shouldBe` [trimming|
                SELECT json_build_object('users', json_agg(_users), 'tasks', json_agg(_tasks)) FROM (SELECT users.id FROM users) AS _users, (SELECT tasks.id, tasks.title FROM tasks) AS _tasks
            |]
        it "should compile a named query" do
            compileGQL "query GetUsers { users { id } tasks { id title } }" [] `shouldBe` [trimming|
                SELECT json_build_object('users', json_agg(_users), 'tasks', json_agg(_tasks)) FROM (SELECT users.id FROM users) AS _users, (SELECT tasks.id, tasks.title FROM tasks) AS _tasks
            |]
        it "should compile a 'user(id: $id)' selection" do
            compileGQL "{ user(id: \"dde8fd2c-4941-4262-a8e0-cc4cd40bacba\") { id } }" [] `shouldBe` [trimming|
                SELECT json_build_object('user', _user) FROM (SELECT users.id FROM users WHERE id = 'dde8fd2c-4941-4262-a8e0-cc4cd40bacba') AS _user
            |]
        it "should compile a single selection with a one-to-many relationship" do
            compileGQL "{ user(id: \"40f1dbb4-403c-46fd-8062-fcf5362f2154\") { id email tasks { id title } } }" [] `shouldBe` [trimming|
                SELECT json_build_object('user', _user) FROM (SELECT users.id, users.email, tasks FROM users LEFT JOIN LATERAL (SELECT ARRAY(SELECT to_json(_sub) FROM (SELECT tasks.id, tasks.title FROM tasks WHERE tasks.user_id = users.id) AS _sub) AS tasks) tasks ON true WHERE id = '40f1dbb4-403c-46fd-8062-fcf5362f2154') AS _user
            |]
        it "should compile a multi selection with a one-to-many relationship" do
            compileGQL "{ users { id email tasks { id title } } }" [] `shouldBe` [trimming|
                SELECT json_build_object('users', json_agg(_users)) FROM (SELECT users.id, users.email, tasks FROM users LEFT JOIN LATERAL (SELECT ARRAY(SELECT to_json(_sub) FROM (SELECT tasks.id, tasks.title FROM tasks WHERE tasks.user_id = users.id) AS _sub) AS tasks) tasks ON true) AS _users
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

        it "should compile a introspection query" do
            let schema = [trimming|
                CREATE TABLE users (
                    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                    email TEXT NOT NULL,
                    password_hash TEXT NOT NULL,
                    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
                    failed_login_attempts INT DEFAULT 0 NOT NULL,
                    access_token TEXT DEFAULT NULL,
                    confirmation_token TEXT DEFAULT NULL,
                    is_confirmed BOOLEAN DEFAULT false NOT NULL
                );
                CREATE TABLE tasks (
                    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                    title TEXT NOT NULL,
                    body TEXT NOT NULL,
                    user_id UUID DEFAULT ihp_user_id() NOT NULL
                );
                CREATE INDEX tasks_user_id_index ON tasks (user_id);
                CREATE POLICY "Users can manage their tasks" ON tasks USING (user_id = ihp_user_id()) WITH CHECK (user_id = ihp_user_id());
                ALTER TABLE tasks ENABLE ROW LEVEL SECURITY;
                CREATE POLICY all ON users USING (true) WITH CHECK (true);
                ALTER TABLE users ENABLE ROW LEVEL SECURITY;
                ALTER TABLE tasks ADD CONSTRAINT tasks_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
            |]
            let introspectionQuery = [trimming|
                query IntrospectionQuery {
                    __schema {
                        queryType { name }
                        mutationType { name }
                        subscriptionType { name }
                        types {
                            ...FullType
                        }
                    }
                }

                fragment FullType {
                  kind
                  name
                  description

                  fields(includeDeprecated: true) {
                    name
                    description
                    args {
                      ...InputValue
                    }
                    type {
                      ...TypeRef
                    }
                    isDeprecated
                    deprecationReason
                  }
                  inputFields {
                    ...InputValue
                  }
                  interfaces {
                    ...TypeRef
                  }
                  enumValues(includeDeprecated: true) {
                    name
                    description
                    isDeprecated
                    deprecationReason
                  }
                  possibleTypes {
                    ...TypeRef
                  }
                }

                fragment InputValue on __InputValue {
                  name
                  description
                  type { ...TypeRef }
                  defaultValue


                }

                fragment TypeRef on __Type {
                  kind
                  name
                  ofType {
                    kind
                    name
                    ofType {
                      kind
                      name
                      ofType {
                        kind
                        name
                        ofType {
                          kind
                          name
                          ofType {
                            kind
                            name
                            ofType {
                              kind
                              name
                              ofType {
                                kind
                                name
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
            |]
            compileGQL introspectionQuery [] `shouldBe` [trimming|
                 SELECT json_build_object('__schema', json_build_object('queryType', json_build_object('name', 'Query'), 'mutationType', json_build_object('name', 'Mutation'), 'subscriptionType', json_build_object('name', 'Subscription')))
            |]

compileGQL gql arguments = gql
        |> parseGQL
        |> Compiler.compileDocument (Variables arguments)
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


