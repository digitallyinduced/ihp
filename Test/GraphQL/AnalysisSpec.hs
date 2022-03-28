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
            
            it "should return the tables used in a single query" do
                let document = parseGQL "{ project(id: $projectId) { id } }"
                (tablesUsedInDocument document) `shouldBe` (Set.fromList ["projects"])

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
            it "should deal with single element nodes" do
                let document = parseGQL "{ channel(id: $channelId) { id messages { id body userId createdAt updatedAt __typename }  __typename } }"
                let (Just result) = Aeson.decode $ cs [trimming|
                    {"channel" : {"id":"5cfac699-ec97-4d4b-b4bf-cae5ea5d5c94","messages":[{"id":"a91d7623-3a42-4c31-bd70-bc2d8a532e12","body":"test","userId":"c9dbe606-611e-48bd-8690-31d89e49d5c5","createdAt":"2022-03-28T18:55:57.906206+02:00","updatedAt":"2022-03-28T18:55:57.906206+02:00","__typename":"Message"},{"id":"a604abf3-3b78-4b1e-bbbe-efc9c10bfdf7","body":"asd","userId":"c9dbe606-611e-48bd-8690-31d89e49d5c5","createdAt":"2022-03-28T18:55:58.898494+02:00","updatedAt":"2022-03-28T18:55:58.898494+02:00","__typename":"Message"},{"id":"cf0c93fe-e6a2-413f-b502-319109253d49","body":"asd","userId":"c9dbe606-611e-48bd-8690-31d89e49d5c5","createdAt":"2022-03-28T18:56:02.743004+02:00","updatedAt":"2022-03-28T18:56:02.743004+02:00","__typename":"Message"},{"id":"a7cb3351-265d-460a-99e7-badf75060117","body":"test","userId":"c9dbe606-611e-48bd-8690-31d89e49d5c5","createdAt":"2022-03-28T19:01:07.88219+02:00","updatedAt":"2022-03-28T19:01:07.88219+02:00","__typename":"Message"},{"id":"6bf81565-515e-40c0-bdc0-583c755caeca","body":"asd","userId":"c9dbe606-611e-48bd-8690-31d89e49d5c5","createdAt":"2022-03-28T19:03:03.128652+02:00","updatedAt":"2022-03-28T19:03:03.128652+02:00","__typename":"Message"}],"__typename":"Channel"}}
                |]
                (recordIds document result) `shouldBe` (HashMap.fromList
                        [ ("channels", Set.fromList ["5cfac699-ec97-4d4b-b4bf-cae5ea5d5c94"])
                        , ("messages", Set.fromList ["6bf81565-515e-40c0-bdc0-583c755caeca","a604abf3-3b78-4b1e-bbbe-efc9c10bfdf7","a7cb3351-265d-460a-99e7-badf75060117","a91d7623-3a42-4c31-bd70-bc2d8a532e12","cf0c93fe-e6a2-413f-b502-319109253d49"])
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

        describe "extractRecordById" do
            it "should return a record" do
                let (Just json) = Aeson.decode $ cs [trimming|
                    {"channel" : {"id":"8458071e-8efb-4bcd-97c2-f6a981d457e4","messages":[{"id":"df513586-11c0-4603-9c1a-c888eaceda80","body":"test","userId":"f81a9c61-9399-4b85-bf4e-54717ba2b6cb","createdAt":"2022-03-28T15:45:20.523007+00:00","updatedAt":"2022-03-28T15:45:20.523007+00:00","__typename":"Message"},{"id":"acce162b-d23f-4bb6-a541-e8779128e36a","body":"test","userId":"f81a9c61-9399-4b85-bf4e-54717ba2b6cb","createdAt":"2022-03-28T15:45:52.976865+00:00","updatedAt":"2022-03-28T15:45:52.976865+00:00","__typename":"Message"},{"id":"f78cc087-d1c4-4196-8a38-f57b7ba6f2f4","body":"test","userId":"f81a9c61-9399-4b85-bf4e-54717ba2b6cb","createdAt":"2022-03-28T15:45:58.351805+00:00","updatedAt":"2022-03-28T15:45:58.351805+00:00","__typename":"Message"},{"id":"fbc5e0cd-292f-4434-a3c8-c9a8754e1753","body":"asd","userId":"f81a9c61-9399-4b85-bf4e-54717ba2b6cb","createdAt":"2022-03-28T15:48:50.678426+00:00","updatedAt":"2022-03-28T15:48:50.678426+00:00","__typename":"Message"},{"id":"3493ff1e-5010-404b-8904-9c4fabc55437","body":"asd","userId":"f81a9c61-9399-4b85-bf4e-54717ba2b6cb","createdAt":"2022-03-28T15:48:54.213279+00:00","updatedAt":"2022-03-28T15:48:54.213279+00:00","__typename":"Message"},{"id":"71a4a8b2-de68-45df-a2ab-171780fa346d","body":"test","userId":"f81a9c61-9399-4b85-bf4e-54717ba2b6cb","createdAt":"2022-03-28T15:49:01.516882+00:00","updatedAt":"2022-03-28T15:49:01.516882+00:00","__typename":"Message"},{"id":"aa6ea658-8291-47c0-9b88-3fa1a5c1bd69","body":"test","userId":"f81a9c61-9399-4b85-bf4e-54717ba2b6cb","createdAt":"2022-03-28T15:49:10.845321+00:00","updatedAt":"2022-03-28T15:49:10.845321+00:00","__typename":"Message"},{"id":"35726c56-3d2b-4a31-8b22-2fa04a37726b","body":"asd","userId":"f81a9c61-9399-4b85-bf4e-54717ba2b6cb","createdAt":"2022-03-28T15:56:43.300526+00:00","updatedAt":"2022-03-28T15:56:43.300526+00:00","__typename":"Message"},{"id":"5b94b9f5-db80-4b51-b945-ffbbbb7f13fe","body":"asd","userId":"f81a9c61-9399-4b85-bf4e-54717ba2b6cb","createdAt":"2022-03-28T15:57:09.370495+00:00","updatedAt":"2022-03-28T15:57:09.370495+00:00","__typename":"Message"},{"id":"ede386be-526e-497c-8b3f-7d7cd62ef65e","body":"test","userId":"f81a9c61-9399-4b85-bf4e-54717ba2b6cb","createdAt":"2022-03-28T15:58:51.179022+00:00","updatedAt":"2022-03-28T15:58:51.179022+00:00","__typename":"Message"}],"__typename":"Channel"}}
                |]
                let (Just result) = Aeson.decode $ cs [trimming|
                    {"id":"df513586-11c0-4603-9c1a-c888eaceda80","body":"test","userId":"f81a9c61-9399-4b85-bf4e-54717ba2b6cb","createdAt":"2022-03-28T15:45:20.523007+00:00","updatedAt":"2022-03-28T15:45:20.523007+00:00","__typename":"Message"}
                |]

                extractRecordById "df513586-11c0-4603-9c1a-c888eaceda80" json `shouldBe` (Just result)

