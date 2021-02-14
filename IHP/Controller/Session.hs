module IHP.Controller.Session
( setSession
, getSession
, getSessionAndClear
, getSessionInt
, getSessionUUID
) where

import IHP.Prelude
import IHP.Controller.RequestContext
import IHP.Controller.Context
import qualified Data.Text.Read as Read
import qualified Data.UUID as UUID
import qualified Network.Wai as Wai
import qualified Data.Vault.Lazy as Vault


setSession :: (?context :: ControllerContext) => Text -> Text -> IO ()
setSession name value = case vaultLookup of
    Just (_, sessionInsert) -> sessionInsert (cs name) (cs value)
    Nothing -> pure ()
    where
        RequestContext { request, vault } = get #requestContext ?context
        vaultLookup = Vault.lookup vault (Wai.vault request)


getSession :: (?context :: ControllerContext) => Text -> IO (Maybe Text)
getSession name = case vaultLookup of
    Just (sessionLookup, _) -> do
        value <- (sessionLookup (cs name))
        let textValue = fmap cs value
        pure $! if textValue == Just "" then Nothing else textValue
    Nothing -> pure Nothing
    where
        RequestContext { request, vault } = get #requestContext ?context
        vaultLookup = Vault.lookup vault (Wai.vault request)


getSessionAndClear :: (?context :: ControllerContext) => Text -> IO (Maybe Text)
getSessionAndClear name = do
    value <- getSession name
    when (isJust value) (setSession name "")
    pure value

getSessionInt :: (?context :: ControllerContext) => Text -> IO (Maybe Int)
getSessionInt name = do
    value <- getSession name
    pure $! case fmap (Read.decimal . cs) value of
            Just (Right value) -> Just $ fst value
            _                  -> Nothing

getSessionUUID :: (?context :: ControllerContext) => Text -> IO (Maybe UUID)
getSessionUUID name = do
    value <- getSession name
    pure $! case fmap UUID.fromText value of
            Just (Just value) -> Just value
            _                 -> Nothing
