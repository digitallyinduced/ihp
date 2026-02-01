module Test.DataSync.ChangeNotifications where

import Test.Hspec
import IHP.Prelude
import Data.Aeson
import qualified Data.Aeson.Key as Aeson
import IHP.DataSync.ChangeNotifications (Change(..), ChangeNotification(..))
import IHP.DataSync.ControllerImpl (changesToValue)
import IHP.DataSync.DynamicQueryCompiler (Renamer(..))
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
