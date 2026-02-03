{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-|
Module: Test.AutoRefreshSpec

Unit tests for Auto Refresh helpers.
-}
module Test.AutoRefreshSpec where

import qualified Data.Aeson                    as Aeson
import qualified Data.UUID                     as UUID
import           IHP.AutoRefresh
import           IHP.AutoRefresh.Types
import           IHP.AutoRefresh.View
import           IHP.Controller.Context
import           IHP.Prelude
import qualified Network.Wai                   as Wai
import           Test.Hspec
import           Text.Blaze.Html.Renderer.Text (renderHtml)

renderMeta :: (?context :: ControllerContext) => Text
renderMeta = cs (renderHtml autoRefreshMeta)

withFreshContext :: (ControllerContext -> IO a) -> IO a
withFreshContext block = do
    let ?request = Wai.defaultRequest
    context <- newControllerContext
    block context

freezeContext :: ControllerContext -> IO ControllerContext
freezeContext = freeze

tests :: Spec
tests = do
    describe "AutoRefresh change set" do
        it "stores row json and allows field access" do
            let userId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a001"
            let row = Aeson.object ["id" Aeson..= userId, "user_id" Aeson..= userId, "name" Aeson..= ("Riley" :: Text)]
            let payload = AutoRefreshRowChangePayload { payloadOperation = AutoRefreshUpdate, payloadRowId = Aeson.toJSON userId, payloadOldRow = Nothing, payloadNewRow = Nothing, payloadLargePayloadId = Nothing }
            let changeSet = insertRowChange "users" payload row mempty
            let [change] = changesForTable "users" changeSet
            change.table `shouldBe` "users"
            rowField @"userId" change `shouldBe` Just userId
            rowFieldByColumnName "user_id" row `shouldBe` Just userId
            rowsForTable "users" changeSet `shouldBe` [row]

        it "prefers new row data and exposes old/new fields" do
            let userId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a005"
            let oldRow = Aeson.object ["id" Aeson..= userId, "name" Aeson..= ("Old" :: Text)]
            let newRow = Aeson.object ["id" Aeson..= userId, "name" Aeson..= ("New" :: Text)]
            let payload = AutoRefreshRowChangePayload
                    { payloadOperation = AutoRefreshUpdate
                    , payloadRowId = Aeson.toJSON userId
                    , payloadOldRow = Just oldRow
                    , payloadNewRow = Just newRow
                    , payloadLargePayloadId = Nothing
                    }
            let changeSet = insertRowChangeFromPayload "users" payload mempty
            let [change] = changesForTable "users" changeSet
            rowField @"name" change `shouldBe` Just ("New" :: Text)
            rowFieldNew @"name" change `shouldBe` Just ("New" :: Text)
            rowFieldOld @"name" change `shouldBe` Just ("Old" :: Text)

        it "uses old row data when only delete payload is available" do
            let userId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a006"
            let oldRow = Aeson.object ["id" Aeson..= userId, "name" Aeson..= ("Deleted" :: Text)]
            let payload = AutoRefreshRowChangePayload
                    { payloadOperation = AutoRefreshDelete
                    , payloadRowId = Aeson.toJSON userId
                    , payloadOldRow = Just oldRow
                    , payloadNewRow = Nothing
                    , payloadLargePayloadId = Nothing
                    }
            let changeSet = insertRowChangeFromPayload "users" payload mempty
            let [change] = changesForTable "users" changeSet
            rowField @"name" change `shouldBe` Just ("Deleted" :: Text)
            rowFieldNew @"name" change `shouldBe` (Nothing :: Maybe Text)
            rowFieldOld @"name" change `shouldBe` Just ("Deleted" :: Text)

        it "routes changes to the matching table slot" do
            let userId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a002"
            let projectId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a003"
            let userRow = Aeson.object ["id" Aeson..= userId, "user_id" Aeson..= userId]
            let projectRow = Aeson.object ["id" Aeson..= projectId, "user_id" Aeson..= userId]
            let userPayload = AutoRefreshRowChangePayload { payloadOperation = AutoRefreshInsert, payloadRowId = Aeson.toJSON userId, payloadOldRow = Nothing, payloadNewRow = Nothing, payloadLargePayloadId = Nothing }
            let projectPayload = AutoRefreshRowChangePayload { payloadOperation = AutoRefreshUpdate, payloadRowId = Aeson.toJSON projectId, payloadOldRow = Nothing, payloadNewRow = Nothing, payloadLargePayloadId = Nothing }
            let changeSet =
                    mempty
                        |> insertRowChange "projects" projectPayload projectRow
                        |> insertRowChange "users" userPayload userRow
            length (changesForTable "projects" changeSet) `shouldBe` 1
            length (changesForTable "users" changeSet) `shouldBe` 1

        it "detects table changes" do
            let row = Aeson.object ["id" Aeson..= (1 :: Int)]
            let payload = AutoRefreshRowChangePayload { payloadOperation = AutoRefreshInsert, payloadRowId = Aeson.toJSON (1 :: Int), payloadOldRow = Nothing, payloadNewRow = Nothing, payloadLargePayloadId = Nothing }
            let changeSet = insertRowChange "users" payload row mempty
            anyChangeOnTable "users" changeSet `shouldBe` True
            anyChangeOnTable "projects" changeSet `shouldBe` False

        it "checks fields across all tables without table filtering" do
            let userId :: UUID = "d3f0e0f8-6a4a-4b0a-9ac2-7c29f9c0a004"
            let userRow = Aeson.object ["id" Aeson..= userId, "user_id" Aeson..= userId]
            let projectRow = Aeson.object ["id" Aeson..= ("p-1" :: Text), "user_id" Aeson..= userId]
            let userPayload = AutoRefreshRowChangePayload { payloadOperation = AutoRefreshInsert, payloadRowId = Aeson.toJSON userId, payloadOldRow = Nothing, payloadNewRow = Nothing, payloadLargePayloadId = Nothing }
            let projectPayload = AutoRefreshRowChangePayload { payloadOperation = AutoRefreshUpdate, payloadRowId = Aeson.toJSON ("p-1" :: Text), payloadOldRow = Nothing, payloadNewRow = Nothing, payloadLargePayloadId = Nothing }
            let changeSet =
                    mempty
                        |> insertRowChange "users" userPayload userRow
                        |> insertRowChange "projects" projectPayload projectRow
            anyChangeWithField @"userId" userId changeSet `shouldBe` True
