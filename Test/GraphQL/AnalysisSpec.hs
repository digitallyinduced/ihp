{-|
Module: Test.GraphQL.AnalysisSpec
Copyright: (c) digitally induced GmbH, 2022
-}
module Test.GraphQL.AnalysisSpec where

import Test.Hspec
import IHP.Prelude
import IHP.GraphQL.Types
import Test.GraphQL.ParserSpec (parseGQL, parseValue)
import IHP.GraphQL.Analysis
import qualified Data.Set as Set
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap

tests = do
    describe "IHP.GraphQL.Analysis" do
        describe "tablesUsedInDocument" do 
            it "should return the tables used in a trivial query" do
                let document = parseGQL "{ users { id email } }"
                (tablesUsedInDocument document) `shouldBe` (Set.fromList ["users"])
            
            it "should return the tables used in a complex query" do
                let document = parseGQL "{ users { id email tasks { id title } } projects { id name } }"
                (tablesUsedInDocument document) `shouldBe` (Set.fromList ["users", "tasks", "projects"])
            
            it "should return the tables used in a create mutation" do
                let document = parseGQL "mutation { createUser(user: $user) { id } }"
                (tablesUsedInDocument document) `shouldBe` (Set.fromList ["users"])
            
            it "should return the tables used in a delete mutation" do
                let document = parseGQL "mutation { deleteUser(id: $userId) { id } }"
                (tablesUsedInDocument document) `shouldBe` (Set.fromList ["users"])
            
            it "should return the tables used in a update mutation" do
                let document = parseGQL "mutation { updateUser(id: $userId, patch: $patch) { id } }"
                (tablesUsedInDocument document) `shouldBe` (Set.fromList ["users"])

        describe "recordIds" do
            it "should return the ids for all database records returned in a GraphQL query" do
                let document = parseGQL "{ users { id email tasks { id title } } projects { id name } }"
                let (Just result) = Aeson.decode $ cs [trimming|
                    {
                        "users": [
                            {
                                "id": "8475f021-6d75-4df2-a5aa-3921323c1123",
                                "email": "marc@example.com",
                                "tasks": [
                                    {
                                        "id": "093bf50a-45e2-46bb-9eb5-e0131a3e710b",
                                        "title": "Build live queries for GraphQL"
                                    },
                                    {
                                        "id": "b30c70ba-821a-4e04-95ed-e0dae046c361",
                                        "title": "Finish recordIds function"
                                    }
                                ]
                            },
                            {
                                "id": "12fbe9c9-bad6-46e4-b29c-06ab1061610a",
                                "email": "second@example.com",
                                "tasks": [
                                    {
                                        "id": "374bdb8a-61ba-49ef-97c9-160383d82780",
                                        "title": "Build live queries for GraphQL"
                                    },
                                    {
                                        "id": "ac22357e-a06f-4f6b-bad0-a4037158ca4f",
                                        "title": "Finish recordIds function"
                                    }
                                ]
                            }
                        ],
                        "projects": [
                            { "id": "c8ffab51-4ea6-412e-8f3e-58c62be76cf0", "name": "A" },
                            { "id": "98230bf9-2351-48a7-91cf-fca85a33355f", "name": "B" }
                        ]
                    }
                |]
                (recordIds document result) `shouldBe` (HashMap.fromList
                        [ ("users", Set.fromList ["8475f021-6d75-4df2-a5aa-3921323c1123", "12fbe9c9-bad6-46e4-b29c-06ab1061610a"])
                        , ("tasks", Set.fromList ["093bf50a-45e2-46bb-9eb5-e0131a3e710b", "b30c70ba-821a-4e04-95ed-e0dae046c361", "374bdb8a-61ba-49ef-97c9-160383d82780", "ac22357e-a06f-4f6b-bad0-a4037158ca4f"])
                        , ("projects", Set.fromList ["c8ffab51-4ea6-412e-8f3e-58c62be76cf0", "98230bf9-2351-48a7-91cf-fca85a33355f"])
                    ])
        describe "nodePathsForTable" do
            it "should return the paths to table nodes" do
                let document = parseGQL "{ users { id email tasks { id title users { id } } } projects { id name } }"
                (nodePathsForTable "users" document) `shouldBe`
                        [ (Path ["users"])
                        , (Path ["users", "tasks", "users"])
                        ]
            it "should return the paths even for aliased fields" do
                let document = parseGQL "{ appUsers: users { id email myTasks: tasks { id title taskUsers: users { id } } } }"
                (nodePathsForTable "users" document) `shouldBe`
                        [ (Path ["appUsers"])
                        , (Path ["appUsers", "myTasks", "taskUsers"])
                        ]
            
            it "should return nothing if the table is not found" do
                let document = parseGQL "{ appUsers: users { id email myTasks: tasks { id title taskUsers: users { id } } } }"
                (nodePathsForTable "projects" document) `shouldBe` []
        describe "applyFunctionAtNode" do
            it "should apply a function on a single node" do
                let (Just json) = Aeson.decode $ cs [trimming|{
                        "a": [
                            {"field": "a0c2ea7b-1e68-4ffd-aa43-3a1f4f7e2cd5"}
                        ]
                    }|]
                let (Just output) = Aeson.decode $ cs [trimming|{
                        "a": null
                    }|]

                let function (Aeson.Array _) = Aeson.Null

                (applyFunctionAtNode function (Path ["a"]) json) `shouldBe` output
            
            it "should apply a function on nested nodes" do
                let (Just json) = Aeson.decode $ cs [trimming|{
                        "a": [
                            {"b": [
                                {"nested": true}
                            ]},
                            {"b": [
                                {"nested": true}
                            ]}
                        ]
                    }|]
                let (Just output) = Aeson.decode $ cs [trimming|{
                        "a": [
                            {"b": null},
                            {"b": null}
                        ]
                    }|]

                let function (Aeson.Array _) = Aeson.Null

                (applyFunctionAtNode function (Path ["a", "b"]) json) `shouldBe` output

        describe "splitDocumentIntoResolvableUnits" do
            it "should split into postgres and introspection resolvers" do
                let document = parseGQL "{ users { id email tasks { id title } } projects { id name } __schema { queryType { name } } }"
                let postgresDocument = parseGQL "{ users { id email tasks { id title } } projects { id name } }"
                let introspectionDocument = parseGQL "{ __schema { queryType { name } } }"

                (splitDocumentIntoResolvableUnits document) `shouldBe` [(PostgresResolver, postgresDocument), (IntrospectionResolver, introspectionDocument)]

            it "should keep fragments in both output graphs" do
                let document = parseGQL "{ users { id email tasks { id title } } projects { id name } __schema { queryType { name } } } fragment A { x }"
                let postgresDocument = parseGQL "{ users { id email tasks { id title } } projects { id name } } fragment A { x }"
                let introspectionDocument = parseGQL "{ __schema { queryType { name } } } fragment A { x }"

                (splitDocumentIntoResolvableUnits document) `shouldBe` [(PostgresResolver, postgresDocument), (IntrospectionResolver, introspectionDocument)]
