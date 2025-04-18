{-|
Module: Test.PGListenerSpec
Copyright: (c) digitally induced GmbH, 2022
-}
module Test.PGListenerSpec where

import Test.Hspec
import IHP.Prelude
import IHP.HaskellSupport
import IHP.ModelSupport
import qualified IHP.PGListener as PGListener
import Data.HashMap.Strict as HashMap

tests = do
    describe "IHP.PGListener" do
        let modelContext = notConnectedModelContext undefined

        describe "subscribe" do
            it "should add a subscriber" do
                PGListener.withPGListener modelContext \pgListener -> do
                    subscriptionsCount <- length . concat . HashMap.elems <$> readIORef pgListener.subscriptions
                    subscriptionsCount `shouldBe` 0

                    let didInsertRecordCallback notification = pure ()

                    pgListener |> PGListener.subscribe "did_insert_record" didInsertRecordCallback

                    subscriptionsCount <- length . concat . HashMap.elems <$> readIORef pgListener.subscriptions
                    subscriptionsCount `shouldBe` 1

        describe "unsubscribe" do
            it "remove the subscription" do
                PGListener.withPGListener modelContext \pgListener -> do
                    subscription <- pgListener |> PGListener.subscribe "did_insert_record" (const (pure ()))
                    pgListener |> PGListener.unsubscribe subscription

                    subscriptions <- readIORef pgListener.subscriptions
                    subscriptionsCount <- length . concat . HashMap.elems <$> readIORef pgListener.subscriptions

                    subscriptionsCount `shouldBe` 0
