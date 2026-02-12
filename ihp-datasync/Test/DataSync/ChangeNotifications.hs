module Test.DataSync.ChangeNotifications where

import Test.Hspec
import IHP.Prelude
import Data.Aeson
import IHP.DataSync.ChangeNotifications (Change(..))
import IHP.DataSync.ControllerImpl (changesToValue)
import IHP.DataSync.DynamicQueryCompiler (Renamer(..))
import IHP.DataSync.DynamicQuery (ConditionExpression(..), ConditionOperator(..), FunctionCall(..), conditionColumns)
import qualified Data.Set as Set
import qualified Prelude

tests = do
    describe "IHP.DataSync.ChangeNotifications" do
        describe "FromJSON Change" do
            it "parses a regular Change with 'new' key" do
                let json = "{\"col\":\"body\",\"new\":\"Hello\"}"
                let expected = Change { col = "body", new = String "Hello" }
                eitherDecode json `shouldBe` Right expected

            it "parses an AppendChange with 'append' key" do
                let json = "{\"col\":\"body\",\"append\":\" World\"}"
                let expected = AppendChange { col = "body", append = " World" }
                eitherDecode json `shouldBe` Right expected

        describe "ToJSON Change" do
            it "serializes a regular Change" do
                let change = Change { col = "body", new = String "Hello" }
                let result = toJSON change
                result `shouldBe` object ["col" .= ("body" :: Text), "new" .= ("Hello" :: Text)]

            it "serializes an AppendChange" do
                let change = AppendChange { col = "body", append = " World" }
                let result = toJSON change
                result `shouldBe` object ["col" .= ("body" :: Text), "append" .= (" World" :: Text)]

        describe "ToJSON/FromJSON round-trip" do
            it "round-trips a regular Change" do
                let change = Change { col = "title", new = String "New Title" }
                (eitherDecode (encode change)) `shouldBe` Right change

            it "round-trips an AppendChange" do
                let change = AppendChange { col = "body", append = " appended text" }
                (eitherDecode (encode change)) `shouldBe` Right change

        describe "changesToValue" do
            let identityRenamer = Renamer { fieldToColumn = Prelude.id, columnToField = Prelude.id }

            it "splits mixed changes into changeSet and appendSet" do
                let changes =
                        [ Change { col = "title", new = String "New Title" }
                        , AppendChange { col = "body", append = " more text" }
                        ]
                let (changeSet, appendSet) = changesToValue identityRenamer changes
                changeSet `shouldBe` Just (object ["title" .= ("New Title" :: Text)])
                appendSet `shouldBe` Just (object ["body" .= (" more text" :: Text)])

            it "returns empty appendSet when all changes are regular" do
                let changes =
                        [ Change { col = "title", new = String "New Title" }
                        , Change { col = "body", new = String "Full body" }
                        ]
                let (changeSet, appendSet) = changesToValue identityRenamer changes
                changeSet `shouldBe` Just (object ["title" .= ("New Title" :: Text), "body" .= ("Full body" :: Text)])
                appendSet `shouldBe` Nothing

            it "returns empty changeSet when all changes are appends" do
                let changes =
                        [ AppendChange { col = "body", append = " suffix" }
                        ]
                let (changeSet, appendSet) = changesToValue identityRenamer changes
                changeSet `shouldBe` Nothing
                appendSet `shouldBe` Just (object ["body" .= (" suffix" :: Text)])

            it "applies renamer to column names" do
                let renamer = Renamer { fieldToColumn = Prelude.id, columnToField = \col -> case col of
                        "user_name" -> "userName"
                        other -> other
                    }
                let changes =
                        [ Change { col = "user_name", new = String "Alice" }
                        , AppendChange { col = "user_name", append = " Smith" }
                        ]
                let (changeSet, appendSet) = changesToValue renamer changes
                changeSet `shouldBe` Just (object ["userName" .= ("Alice" :: Text)])
                appendSet `shouldBe` Just (object ["userName" .= (" Smith" :: Text)])

        describe "conditionColumns" do
            it "returns a single column for a simple WHERE" do
                let condition = InfixOperatorExpression
                        { left = ColumnExpression "conversationId"
                        , op = OpEqual
                        , right = LiteralExpression (String "00000000-0000-0000-0000-000000000000")
                        }
                conditionColumns condition `shouldBe` Set.fromList ["conversationId"]

            it "returns multiple columns for a compound WHERE with AND" do
                let condition = InfixOperatorExpression
                        { left = InfixOperatorExpression
                            { left = ColumnExpression "conversationId"
                            , op = OpEqual
                            , right = LiteralExpression (String "00000000-0000-0000-0000-000000000000")
                            }
                        , op = OpAnd
                        , right = InfixOperatorExpression
                            { left = ColumnExpression "status"
                            , op = OpEqual
                            , right = LiteralExpression (String "active")
                            }
                        }
                conditionColumns condition `shouldBe` Set.fromList ["conversationId", "status"]

            it "extracts columns from nested AND/OR" do
                let condition = InfixOperatorExpression
                        { left = InfixOperatorExpression
                            { left = ColumnExpression "a"
                            , op = OpEqual
                            , right = LiteralExpression (Number 1)
                            }
                        , op = OpOr
                        , right = InfixOperatorExpression
                            { left = ColumnExpression "b"
                            , op = OpAnd
                            , right = ColumnExpression "c"
                            }
                        }
                conditionColumns condition `shouldBe` Set.fromList ["a", "b", "c"]

            it "returns empty set for CallExpression" do
                let condition = CallExpression (ToTSQuery "hello")
                conditionColumns condition `shouldBe` Set.empty

            it "returns empty set for ListExpression" do
                let condition = ListExpression [Number 1, Number 2]
                conditionColumns condition `shouldBe` Set.empty

            it "returns empty set for LiteralExpression" do
                let condition = LiteralExpression (String "hello")
                conditionColumns condition `shouldBe` Set.empty
