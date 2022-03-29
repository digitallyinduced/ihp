{-|
Module: Test.GraphQL.SchemaCompilerSpec
Copyright: (c) digitally induced GmbH, 2022
-}
module Test.GraphQL.SchemaCompilerSpec where

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

import qualified IHP.GraphQL.ToText as GraphQL
import qualified IHP.GraphQL.SchemaCompiler as GraphQL

import Test.IDE.SchemaDesigner.ParserSpec (parseSqlStatements)

tests = do
    describe "IHP.GraphQL.SchemaCompiler" do
        it "should compile a basic SQL Schema to a GraphQL Schema" do
            let sqlSchema = parseSqlStatements [trimming|
                -- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
                CREATE TABLE users (
                    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                    email TEXT NOT NULL,
                    password_hash TEXT NOT NULL,
                    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
                    failed_login_attempts INT DEFAULT 0 NOT NULL
                );
                CREATE TABLE tasks (
                    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
                    title TEXT NOT NULL,
                    body TEXT NOT NULL,
                    user_id UUID NOT NULL
                );
                CREATE POLICY "Allow access" ON tasks USING (true) WITH CHECK (true);
                ALTER TABLE tasks ENABLE ROW LEVEL SECURITY;
                CREATE INDEX tasks_user_id_index ON tasks (user_id);
                ALTER TABLE tasks ADD CONSTRAINT tasks_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
            |]
            let graphQLSchema = [trimming|
                schema {
                    query: Query
                    mutation: Mutation
                }
                type Query {
                    "Returns all records from the `users` table"
                    users: [User!]!
                    "Returns a single record from the `users` table"
                    user(id: UUID!): User!
                    "Returns all records from the `tasks` table"
                    tasks: [Task!]!
                    "Returns a single record from the `tasks` table"
                    task(id: UUID!): Task!
                }
                type Mutation {
                    createUser(user: NewUser!): User!
                    updateUser(id: ID!, patch: UserPatch!): User!
                    deleteUser(id: ID!): User!
                    createTask(task: NewTask!): Task!
                    updateTask(id: ID!, patch: TaskPatch!): Task!
                    deleteTask(id: ID!): Task!
                }
                scalar UUID
                scalar Timestamp
                type User {
                    id: ID!
                    email: String!
                    passwordHash: String!
                    lockedAt: Timestamp
                    failedLoginAttempts: Int!
                    tasks: [Task!]!
                }
                type Task {
                    id: ID!
                    title: String!
                    body: String!
                    userId: UUID!
                }
                input NewUser {
                    id: ID
                    email: String!
                    passwordHash: String!
                    lockedAt: Timestamp
                    failedLoginAttempts: Int
                }
                input NewTask {
                    id: ID
                    title: String!
                    body: String!
                    userId: UUID!
                }
                input UserPatch {
                    id: ID
                    email: String
                    passwordHash: String
                    lockedAt: Timestamp
                    failedLoginAttempts: Int
                }
                input TaskPatch {
                    id: ID
                    title: String
                    body: String
                    userId: UUID
                }
            |]

            GraphQL.toText (GraphQL.sqlSchemaToGraphQLSchema sqlSchema) `shouldBe` graphQLSchema
