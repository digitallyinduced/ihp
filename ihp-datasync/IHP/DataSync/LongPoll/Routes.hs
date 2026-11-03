module IHP.DataSync.LongPoll.Routes where

import IHP.RouterPrelude
import IHP.DataSync.LongPoll.Types

-- | All long polling requests are POST requests, so no proxy or browser cache answers them.
instance CanRoute DataSyncLongPollController where
    parseRoute' = do
        string "/DataSyncLongPoll"

        let
            openAction = do
                endOfInput
                onlyAllowMethods [POST]
                pure OpenLongPollAction

            connectionAction = do
                string "/"
                connectionId <- parseUUID
                string "/"
                let action name constructor = do
                        string name
                        endOfInput
                        onlyAllowMethods [POST]
                        pure (constructor connectionId)
                action "send" SendLongPollAction
                    <|> action "receive" ReceiveLongPollAction
                    <|> action "close" CloseLongPollAction

        openAction <|> connectionAction

instance HasPath DataSyncLongPollController where
    pathTo OpenLongPollAction = "/DataSyncLongPoll"
    pathTo SendLongPollAction { connectionId } = "/DataSyncLongPoll/" <> tshow connectionId <> "/send"
    pathTo ReceiveLongPollAction { connectionId } = "/DataSyncLongPoll/" <> tshow connectionId <> "/receive"
    pathTo CloseLongPollAction { connectionId } = "/DataSyncLongPoll/" <> tshow connectionId <> "/close"
