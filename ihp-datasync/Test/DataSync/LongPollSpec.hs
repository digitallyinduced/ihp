{-# LANGUAGE ViewPatterns #-}
module DataSync.LongPollSpec where

import Test.Hspec
import IHP.Prelude
import IHP.DataSync.LongPoll
import DataSync.DataSyncIntegrationSpec (withDB, withHasqlPool, setupTestSchema, insertTestData, withDataSyncEnvironment, encodeDataSyncQuery)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Data.Foldable (toList)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID as UUID

testConfig :: LongPollConfig
testConfig = LongPollConfig { receiveTimeout = 2_000_000, idleTimeout = 30 }

-- | Answers every client message with the message prefixed by @echo:@
echoController :: IO ByteString -> (LByteString -> IO ()) -> IO ()
echoController receive send = forever do
    message <- receive
    send (cs ("echo:" <> message))

-- | Receives until the batch holds at least the given number of messages.
-- A receive returns as soon as one message is pending, so wait a little
-- before asking again for the rest.
awaitMessages :: LongPollConnection -> Int -> Int -> IO LongPollBatch
awaitMessages connection acknowledged count = go (200 :: Int)
    where
        go attemptsLeft = do
            batch <- awaitServerMessages testConfig connection acknowledged
            if length batch.messages >= count || batch.closed || attemptsLeft == 0
                then pure batch
                else do
                    threadDelay 10_000
                    go (attemptsLeft - 1)

registryIsEmpty :: LongPollRegistry -> IO Bool
registryIsEmpty registry = HashMap.null <$> readTVarIO registry

tests :: Spec
tests = describe "IHP.DataSync.LongPoll" do
    it "passes client messages to the controller in order" do
        registry <- newLongPollRegistry
        connection <- openLongPollConnection registry testConfig Nothing echoController
        pushClientMessages connection ["a", "b"]
        batch <- awaitMessages connection 0 2
        batch `shouldBe` LongPollBatch { messages = ["echo:a", "echo:b"], lastSequence = 2, closed = False }
        closeLongPollConnection connection

    it "delivers a message again until the client acknowledges it" do
        registry <- newLongPollRegistry
        connection <- openLongPollConnection registry testConfig Nothing echoController
        pushClientMessages connection ["a"]
        first <- awaitMessages connection 0 1
        first.messages `shouldBe` ["echo:a"]
        repeated <- awaitMessages connection 0 1
        repeated.messages `shouldBe` ["echo:a"]

        pushClientMessages connection ["b"]
        next <- awaitMessages connection first.lastSequence 1
        next `shouldBe` LongPollBatch { messages = ["echo:b"], lastSequence = 2, closed = False }
        closeLongPollConnection connection

    it "returns an empty batch when no message arrives in time" do
        registry <- newLongPollRegistry
        let config = testConfig { receiveTimeout = 50_000 }
        connection <- openLongPollConnection registry config Nothing echoController
        batch <- awaitServerMessages config connection 0
        batch `shouldBe` LongPollBatch { messages = [], lastSequence = 0, closed = False }
        closeLongPollConnection connection

    it "closes the connection when the controller stops" do
        registry <- newLongPollRegistry
        connection <- openLongPollConnection registry testConfig Nothing (\_ _ -> pure ())
        batch <- awaitServerMessages testConfig connection 0
        batch.closed `shouldBe` True
        registryIsEmpty registry `shouldReturn` True

    it "closes the connection when the client closes it" do
        registry <- newLongPollRegistry
        connection <- openLongPollConnection registry testConfig Nothing echoController
        closeLongPollConnection connection
        registryIsEmpty registry `shouldReturn` True
        batch <- awaitServerMessages testConfig connection 0
        batch.closed `shouldBe` True

    it "closes connections without requests after the idle timeout" do
        registry <- newLongPollRegistry
        let config = testConfig { idleTimeout = 0.2 }
        connection <- openLongPollConnection registry config Nothing echoController
        threadDelay 600_000
        registryIsEmpty registry `shouldReturn` True
        readTVarIO connection.closed `shouldReturn` True

    it "finds a connection only for the user who opened it" do
        registry <- newLongPollRegistry
        connection <- openLongPollConnection registry testConfig (Just "alice") echoController
        let lookupAs user = fmap (.connectionId) <$> lookupLongPollConnection registry connection.connectionId user
        lookupAs (Just "alice") `shouldReturn` Just connection.connectionId
        lookupAs (Just "bob") `shouldReturn` Nothing
        lookupAs Nothing `shouldReturn` Nothing
        closeLongPollConnection connection

    it "embeds the encoded messages in the batch JSON" do
        let batch = LongPollBatch { messages = ["{\"tag\":\"DidDelete\"}", "{\"tag\":\"DidInsert\"}"], lastSequence = 3, closed = False }
        encodeLongPollBatch batch `shouldBe` "{\"lastSequence\":3,\"closed\":false,\"messages\":[{\"tag\":\"DidDelete\"},{\"tag\":\"DidInsert\"}]}"

    it "answers a DataSyncQuery over long polling" do
        withDB \connStr -> do
            withHasqlPool connStr \pool -> do
                setupTestSchema pool
                (userId, messageId) <- insertTestData pool

                withDataSyncEnvironment connStr userId \runController -> do
                    registry <- newLongPollRegistry
                    connection <- openLongPollConnection registry testConfig (Just (UUID.toText userId)) \receive send ->
                        runController receive (send . Aeson.encode)
                    pushClientMessages connection [encodeDataSyncQuery "messages" 1 Nothing]
                    batch <- awaitMessages connection 0 1
                    closeLongPollConnection connection

                    case map Aeson.decode batch.messages of
                        [Just (Aeson.Object response)] -> do
                            KeyMap.lookup "tag" response `shouldBe` Just (Aeson.String "DataSyncResult")
                            KeyMap.lookup "requestId" response `shouldBe` Just (Aeson.Number 1)
                            case KeyMap.lookup "result" response of
                                Just (Aeson.Array (toList -> [Aeson.Object row])) -> do
                                    KeyMap.lookup "id" row `shouldBe` Just (Aeson.String (UUID.toText messageId))
                                    KeyMap.lookup "body" row `shouldBe` Just (Aeson.String "Hello")
                                other -> expectationFailure (cs ("Expected one row, got " <> tshow other))
                        other -> expectationFailure (cs ("Expected one DataSyncResult, got " <> tshow other))
