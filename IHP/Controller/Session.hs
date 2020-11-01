module IHP.Controller.Session
( setSession
, getSession
, getSessionAndClear
, getSessionInt
, getSessionUUID
) where

import IHP.Prelude
import IHP.Controller.RequestContext
import qualified Data.Text.Read as Read
import qualified Data.UUID as UUID
import qualified Network.Wai as Wai
import qualified Data.Vault.Lazy as Vault


setSession :: (?context :: RequestContext) => Text -> Text -> IO ()
setSession name value = sessionInsert (cs name) (cs value)
    where
        (RequestContext request _ _ _ session _) = ?context
        Just (_, sessionInsert) = Vault.lookup session (Wai.vault request)


getSession :: (?context :: RequestContext) => Text -> IO (Maybe Text)
getSession name = do
        value <- (sessionLookup (cs name))
        let textValue = fmap cs value
        pure $! if textValue == Just "" then Nothing else textValue
    where
        (RequestContext request _ _ _ session _) = ?context
        Just (sessionLookup, _) = Vault.lookup session (Wai.vault request)


getSessionAndClear :: (?context :: RequestContext) => Text -> IO (Maybe Text)
getSessionAndClear name = do
    value <- getSession name
    when (isJust value) (setSession name "")
    pure value

getSessionInt :: (?context :: RequestContext) => Text -> IO (Maybe Int)
getSessionInt name = do
    value <- getSession name
    pure $! case fmap (Read.decimal . cs) value of
            Just (Right value) -> Just $ fst value
            _                  -> Nothing

getSessionUUID :: (?context :: RequestContext) => Text -> IO (Maybe UUID)
getSessionUUID name = do
    value <- getSession name
    pure $! case fmap UUID.fromText value of
            Just (Just value) -> Just value
            _                 -> Nothing
