{-|
Module: Test.GraphQL.AnalysisSpec
Copyright: (c) digitally induced GmbH, 2022
-}
module Test.GraphQL.AnalysisSpec where

import Test.Hspec
import IHP.Prelude
import qualified IHP.GraphQL.Compiler as Compiler
import IHP.GraphQL.Types
import Test.GraphQL.ParserSpec (parseGQL, parseValue)
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
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
