{-|
Module: Test.GraphQL.ParserSpec
Copyright: (c) digitally induced GmbH, 2022
-}
module Test.GraphQL.ParserSpec where

import Test.Hspec
import IHP.Prelude
import qualified IHP.GraphQL.Parser as Parser
import IHP.GraphQL.Types
import qualified Data.Attoparsec.Text as Attoparsec
import Data.HashMap.Strict as HashMap

tests = do
    describe "The GraphQL Parser" do
        it "should parse a trivial selection" do
            parseGQL "{ user }"  `shouldBe` Document
                    { definitions =
                        [ ExecutableDefinition
                            { operation = OperationDefinition
                                { operationType = Query
                                , name = Nothing
                                , variableDefinitions = []
                                , selectionSet = [
                                    Field { alias = Nothing, name = "user", arguments = [], directives = [], selectionSet = [] }
                                ] }
                            }
                        ]
                    }

        it "should parse nested selections" do
            parseGQL "{ user { tasks { id name createdAt } } }"  `shouldBe` Document
                    { definitions =
                        [ ExecutableDefinition
                            { operation = OperationDefinition
                                { operationType = Query
                                , name = Nothing
                                , variableDefinitions = []
                                , selectionSet = [
                                    (field "user") { selectionSet = [
                                        (field "tasks") { selectionSet =
                                        [ field "id"
                                        , field "name"
                                        , field "createdAt"
                                        ] }
                                    ] }
                                ] }
                            }
                        ]
                    }

        it "should parse a trivial selection with an alias" do
            parseGQL "{ user { userId: id } }"  `shouldBe` Document
                    { definitions =
                        [ ExecutableDefinition
                            { operation = OperationDefinition
                                { operationType = Query
                                , name = Nothing
                                , variableDefinitions = []
                                , selectionSet = [
                                    Field { alias = Nothing, name = "user", arguments = [], directives = [], selectionSet = [
                                        (field "id") { alias = "userId" }
                                    ] }
                                ] }
                            }
                        ]
                    }

        it "should parse a multi selection with an alias" do
            parseGQL "{ users { id } tasks { id } }"  `shouldBe` Document
                    { definitions =
                        [ ExecutableDefinition
                            { operation = OperationDefinition
                                { operationType = Query
                                , name = Nothing
                                , variableDefinitions = []
                                , selectionSet = [
                                    (field "users") { selectionSet = [ field "id" ] },
                                    (field "tasks") { selectionSet = [ field "id" ] }
                                ] }
                            }
                        ]
                    }

        it "should parse a mutation" do
            let query = [trimming|
                mutation CreateProject($$project: Project) {
                    createProject(project: $$project) {
                        id title
                    }
                }
            |]
            parseGQL query  `shouldBe` Document
                    { definitions =
                        [ ExecutableDefinition
                            { operation = OperationDefinition
                                { operationType = Mutation
                                , name = "CreateProject"
                                , variableDefinitions = [VariableDefinition { variableName = "project", variableType = NamedType "Project" }]
                                , selectionSet = [
                                    (field "createProject")
                                        { arguments = [Argument { argumentName = "project", argumentValue = Variable "project" }]
                                        , selectionSet = [ field "id", field "title" ]
                                        }
                                ] }
                            }
                        ]
                    }

        it "should parse a unnamed mutation" do
            let query = [trimming|
                mutation ($$project: Project) {
                    createProject(project: $$project) {
                        id title
                    }
                }
            |]
            parseGQL query  `shouldBe` Document
                    { definitions =
                        [ ExecutableDefinition
                            { operation = OperationDefinition
                                { operationType = Mutation
                                , name = Nothing
                                , variableDefinitions = [VariableDefinition { variableName = "project", variableType = NamedType "Project" }]
                                , selectionSet = [
                                    (field "createProject")
                                        { arguments = [Argument { argumentName = "project", argumentValue = Variable "project" }]
                                        , selectionSet = [ field "id", field "title" ]
                                        }
                                ] }
                            }
                        ]
                    }
        it "should parse a mutation starting with lots of whitespace" do
            let query = cs [plain|
                mutation {
                    createTask(task: {
                        title: "Hello World",
                        body: "hello world",
                        userId: "40f1dbb4-403c-46fd-8062-fcf5362f2154"
                    }) {
                        id
                    }
                }
            |]
            parseGQL query  `shouldBe` Document
                    { definitions =
                        [ ExecutableDefinition
                            { operation = OperationDefinition
                                { operationType = Mutation
                                , name = Nothing
                                , variableDefinitions = []
                                , selectionSet = [
                                    (field "createTask")
                                        { arguments = [
                                            Argument
                                            { argumentName = "task"
                                            , argumentValue = ObjectValue (HashMap.fromList
                                                [ ("body", StringValue "hello world")
                                                , ("userId", StringValue "40f1dbb4-403c-46fd-8062-fcf5362f2154")
                                                , ("title", StringValue "Hello World")
                                                ]
                                            ) }
                                        ]
                                        , selectionSet = [ field "id" ]
                                        }
                                ] }
                            }
                        ]
                    }

        it "should parse a fragment" do
            let query = cs [plain|
                fragment user {
                    id email
                }
            |]
            parseGQL query  `shouldBe` Document
                    { definitions =
                        [ FragmentDefinition (Fragment { name = "user", selectionSet = [ field "id", field "email" ] })
                        ]
                    }

        it "should parse a fragment spread" do
            let query = cs [plain|
                query { users { id ...userFragment } }
                fragment userFragment { email }
            |]
            parseGQL query  `shouldBe` Document
                    { definitions =
                        [ ExecutableDefinition
                            { operation = OperationDefinition
                                { operationType = Query
                                , name = Nothing
                                , selectionSet = [ (field "users") { selectionSet = [field "id", FragmentSpread {fragmentName = "userFragment"} ] } ]
                                , variableDefinitions = []
                                }
                            }
                        , FragmentDefinition (Fragment
                                { name = "userFragment"
                                , selectionSet = [field "email"]
                                }
                            )
                        ]
                    }
        it "should parse a mutation with a non null type argument" do
            let query = cs [plain|
                mutation createUser ($user: NewUser!) {
                    createUser (user: $user) {
                        id
                        email
                        passwordHash
                        lockedAt
                        failedLoginAttempts
                        tasks {
                            id
                            title
                            body
                            userId
                        }
                    }
                }
            |]
            parseGQL query  `shouldBe` Document
                { definitions =
                    [ ExecutableDefinition { operation = OperationDefinition
                        { operationType = Mutation
                        , name = "createUser"
                        , selectionSet = [
                            (field "createUser")
                                { arguments = [Argument { argumentName = "user", argumentValue = Variable "user" } ]
                                , selectionSet = [field "id", field "email", field "passwordHash", field "lockedAt", field "failedLoginAttempts", (field "tasks") { selectionSet = [field "id", field "title", field "body", field "userId"] }]
                                }
                        ]
                        , variableDefinitions = [VariableDefinition {variableName = "user", variableType = NonNullType (NamedType "NewUser")}]}}]}

        describe "parseName" do
            it "should accept letters" do
                runParser Parser.parseName "id" `shouldBe` "id"

field name = Field { alias = Nothing, name, arguments = [], directives = [], selectionSet = [] }

parseGQL :: Text -> Document
parseGQL gql = runParser Parser.parseDocument gql

parseValue :: Text -> Value
parseValue expression = runParser Parser.parseValue expression

runParser parser text =
    case Attoparsec.parseOnly parser text of
            Left parserError -> error (cs $ tshow parserError)
            Right statements -> statements

