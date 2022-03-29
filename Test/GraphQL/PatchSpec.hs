{-|
Module: Test.GraphQL.PatchSpec
Copyright: (c) digitally induced GmbH, 2022
-}
module Test.GraphQL.PatchSpec where

import Test.Hspec
import IHP.Prelude
import IHP.GraphQL.Types
import Test.GraphQL.ParserSpec (parseGQL, parseValue)
import IHP.GraphQL.Analysis
import qualified Data.Aeson as Aeson
import qualified IHP.GraphQL.Patch as GraphQL

tests = do
    describe "IHP.GraphQL.Patch" do
        describe "insertRecord" do 
            it "should insert a record for a simple graphql result" do
                let document = parseGQL "{ users { id email } }"
                let (Just result) = Aeson.decode $ cs [trimming|{
                        "users": [
                            {"id": "a0c2ea7b-1e68-4ffd-aa43-3a1f4f7e2cd5", "email": "a@example.com"},
                            {"id": "860b49f1-2fcb-45e7-a39a-eabe98c86676", "email": "b@example.com"}
                            
                        ]
                    }|]
                let (Just new) = Aeson.decode $ cs [trimming|{"id": "83dbad75-e8f0-44dc-b432-9d0a7ad48b06", "email": "c@example.com"}|]
                let (Just output) = Aeson.decode $ cs [trimming|{
                        "users": [
                            {"id": "a0c2ea7b-1e68-4ffd-aa43-3a1f4f7e2cd5", "email": "a@example.com"},
                            {"id": "860b49f1-2fcb-45e7-a39a-eabe98c86676", "email": "b@example.com"},
                            {"id": "83dbad75-e8f0-44dc-b432-9d0a7ad48b06", "email": "c@example.com"}
                        ]
                    }|]
                (GraphQL.insertRecord "users" "860b49f1-2fcb-45e7-a39a-eabe98c86676" new document result) `shouldBe` output

        describe "updateRecord" do
            it "should update a record for a simple graphql result" do
                let document = parseGQL "{ users { id email } }"
                let (Just result) = Aeson.decode $ cs [trimming|{
                        "users": [
                            {"id": "a0c2ea7b-1e68-4ffd-aa43-3a1f4f7e2cd5", "email": "a@example.com"},
                            {"id": "860b49f1-2fcb-45e7-a39a-eabe98c86676", "email": "b@example.com"},
                            {"id": "83dbad75-e8f0-44dc-b432-9d0a7ad48b06", "email": "c@example.com"}
                        ]
                    }|]
                let (Just patch) = Aeson.decode $ cs [trimming|{"email": "b+changed@example.com"}|]
                let (Just output) = Aeson.decode $ cs [trimming|{
                        "users": [
                            {"id": "a0c2ea7b-1e68-4ffd-aa43-3a1f4f7e2cd5", "email": "a@example.com"},
                            {"id": "860b49f1-2fcb-45e7-a39a-eabe98c86676", "email": "b+changed@example.com"},
                            {"id": "83dbad75-e8f0-44dc-b432-9d0a7ad48b06", "email": "c@example.com"}
                        ]
                    }|]
                (GraphQL.updateRecord "users" "860b49f1-2fcb-45e7-a39a-eabe98c86676" patch document result) `shouldBe` output

        describe "deleteRecord" do 
            it "should delete a record for a simple graphql result" do
                let document = parseGQL "{ users { id email } }"
                let (Just result) = Aeson.decode $ cs [trimming|{
                        "users": [
                            {"id": "a0c2ea7b-1e68-4ffd-aa43-3a1f4f7e2cd5", "email": "a@example.com"},
                            {"id": "860b49f1-2fcb-45e7-a39a-eabe98c86676", "email": "b@example.com"},
                            {"id": "83dbad75-e8f0-44dc-b432-9d0a7ad48b06", "email": "c@example.com"}
                        ]
                    }|]
                let (Just output) = Aeson.decode $ cs [trimming|{
                        "users": [
                            {"id": "a0c2ea7b-1e68-4ffd-aa43-3a1f4f7e2cd5", "email": "a@example.com"},
                            {"id": "83dbad75-e8f0-44dc-b432-9d0a7ad48b06", "email": "c@example.com"}
                        ]
                    }|]
                (GraphQL.deleteRecord "users" "860b49f1-2fcb-45e7-a39a-eabe98c86676" document result) `shouldBe` output
            
            it "should delete a record in nested results with an alias" do
                let document = parseGQL "{ users { id email projects { id projectUsers: users { id email } } } }"
                let (Just result) = Aeson.decode $ cs [trimming|{
                        "users": [
                            {
                                "id": "a0c2ea7b-1e68-4ffd-aa43-3a1f4f7e2cd5",
                                "email": "a@example.com",
                                "projects": [
                                    {
                                        "id": "ec5c4514-e596-4506-80b4-f99def6d4b4a",
                                        "projectUsers": [
                                            {"id": "a0c2ea7b-1e68-4ffd-aa43-3a1f4f7e2cd5", "email": "a@example.com"},
                                            {"id": "860b49f1-2fcb-45e7-a39a-eabe98c86676", "email": "b@example.com"},
                                            {"id": "83dbad75-e8f0-44dc-b432-9d0a7ad48b06", "email": "c@example.com"}
                                        ]
                                    }
                                ]
                            },
                            {
                                "id": "860b49f1-2fcb-45e7-a39a-eabe98c86676",
                                "email": "b@example.com",
                                "projects": [
                                    {
                                        "id": "f9effec8-6a66-4b11-bd20-6649d87f3f16",
                                        "projectUsers": [
                                            {"id": "a0c2ea7b-1e68-4ffd-aa43-3a1f4f7e2cd5", "email": "a@example.com"},
                                            {"id": "83dbad75-e8f0-44dc-b432-9d0a7ad48b06", "email": "c@example.com"}
                                        ]
                                    }
                                ]
                            },
                            {
                                "id": "83dbad75-e8f0-44dc-b432-9d0a7ad48b06",
                                "email": "c@example.com",
                                "projects": [
                                    {
                                        "id": "f9effec8-6a66-4b11-bd20-6649d87f3f16",
                                        "projectUsers": [
                                            {"id": "83dbad75-e8f0-44dc-b432-9d0a7ad48b06", "email": "c@example.com"}
                                        ]
                                    }
                                ]
                            }
                        ]
                    }|]
                let (Just output) = Aeson.decode $ cs [trimming|{
                        "users": [
                            {
                                "id": "a0c2ea7b-1e68-4ffd-aa43-3a1f4f7e2cd5",
                                "email": "a@example.com",
                                "projects": [
                                    {
                                        "id": "ec5c4514-e596-4506-80b4-f99def6d4b4a",
                                        "projectUsers": [
                                            {"id": "a0c2ea7b-1e68-4ffd-aa43-3a1f4f7e2cd5", "email": "a@example.com"},
                                            {"id": "83dbad75-e8f0-44dc-b432-9d0a7ad48b06", "email": "c@example.com"}
                                        ]
                                    }
                                ]
                            },
                            {
                                "id": "83dbad75-e8f0-44dc-b432-9d0a7ad48b06",
                                "email": "c@example.com",
                                "projects": [
                                    {
                                        "id": "f9effec8-6a66-4b11-bd20-6649d87f3f16",
                                        "projectUsers": [
                                            {"id": "83dbad75-e8f0-44dc-b432-9d0a7ad48b06", "email": "c@example.com"}
                                        ]
                                    }
                                ]
                            }
                        ]
                    }|]
                (GraphQL.deleteRecord "users" "860b49f1-2fcb-45e7-a39a-eabe98c86676" document result) `shouldBe` output