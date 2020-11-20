{-# LANGUAGE TemplateHaskell #-}
{-|
Module: IHP.Telemetry
Description: Reports IHP Version + OS to digitally induced
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Telemetry where

import IHP.Prelude
import qualified IHP.Version as Version
import qualified System.Info as System
import qualified Network.Wreq as Wreq
import qualified System.Environment as Env
import qualified Control.Exception as Exception
import qualified Crypto.Hash.SHA512 as SHA512
import qualified System.Directory as Directory
import qualified Data.ByteString.Base16 as Base16

data TelemetryInfo = TelemetryInfo
    { ihpVersion :: !Text
    , os :: !Text
    , arch :: !Text
    , projectId :: !Text
    } deriving (Eq, Show)

-- | Reports telemetry info to the IHP Telemetry server
--
-- This can be disabled by setting the env var IHP_TELEMETRY_DISABLED=1
reportTelemetry :: IO ()
reportTelemetry = do
    isDisabled <- maybe False (\value -> value == "1") <$> Env.lookupEnv "IHP_TELEMETRY_DISABLED"
    unless isDisabled do
        payload <- toPayload <$> getTelemetryInfo
        result <- Exception.try (Wreq.post "https://ihp-telemetry.digitallyinduced.com/CreateEvent" payload)
        case result of
            Left (e :: SomeException) -> putStrLn ("Telemetry failed: " <> show e)
            Right _ -> putStrLn "IHP Telemetry is activated. This can be disabled by setting a env variable to IHP_TELEMETRY_DISABLED=1"

getTelemetryInfo :: IO TelemetryInfo
getTelemetryInfo = do
    projectId <- getProjectId
    pure TelemetryInfo { ihpVersion = Version.ihpCommit, os = cs System.os, arch = cs System.arch, projectId }

-- | The project id is a an anonymous identifier to keep track of distinct projects.
--
-- The project id is a hash of the current working directory. We use sha512 to make sure no one
-- is able to get back the original path from the hash.
getProjectId :: IO Text
getProjectId = do
    cwd <- Directory.getCurrentDirectory
    cwd
        |> cs
        |> SHA512.hash
        |> Base16.encode
        |> cs
        |> pure

-- | Transforms a telemetry info into a payload to be used with the telemetry request
toPayload :: TelemetryInfo -> [(ByteString, ByteString)]
toPayload TelemetryInfo { .. } = [("ihpVersion", cs ihpVersion), ("os", cs os), ("arch", cs arch), ("projectId", cs projectId)]
