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
import qualified Control.Exception.Safe as Exception
import qualified System.Timeout as Timeout
import qualified Crypto.Hash.SHA512 as SHA512
import qualified System.Directory.OsPath as Directory
import qualified Data.ByteString.Base16 as Base16
import System.OsPath (decodeUtf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Log.FastLogger (FastLogger, toLogStr)
import qualified IHP.EnvVar as EnvVar

data TelemetryInfo = TelemetryInfo
    { ihpVersion :: !Text
    , os :: !Text
    , arch :: !Text
    , projectId :: !Text
    } deriving (Eq, Show)

-- | Reports telemetry info to the IHP Telemetry server
--
-- This can be disabled by setting the env var IHP_TELEMETRY_DISABLED=1
--
-- A failing report is logged and otherwise ignored, so telemetry never stops the dev server.
reportTelemetry :: (?context :: context, HasField "logger" context FastLogger) => IO ()
reportTelemetry = do
    isDisabled <- EnvVar.envOrDefault "IHP_TELEMETRY_DISABLED" False
    unless isDisabled (reportTelemetryTo telemetryUrl)

telemetryUrl :: String
telemetryUrl = "https://ihp-telemetry.digitallyinduced.com/CreateEvent"

-- | Sends the telemetry report to the given URL. Never throws a synchronous exception:
-- network errors (e.g. a proxy refusing the request), timeouts and errors while collecting
-- the telemetry info are logged as a single @Telemetry failed: ...@ line.
reportTelemetryTo :: (?context :: context, HasField "logger" context FastLogger) => String -> IO ()
reportTelemetryTo url = do
    let logger = ?context.logger
    result <- Exception.tryAny do
        payload <- toPayload <$> getTelemetryInfo
        logger (toLogStr (tshow payload))
        Timeout.timeout telemetryTimeout (Wreq.post url payload)
    case result of
        Left exception -> logger (toLogStr ("Telemetry failed: " <> singleLine (tshow exception)))
        Right Nothing -> logger (toLogStr ("Telemetry failed: request timed out" :: Text))
        Right (Just _) -> logger (toLogStr ("IHP Telemetry is activated. This can be disabled by setting env variable IHP_TELEMETRY_DISABLED=1" :: Text))
    where
        -- The 'Show' instance of http-client's exceptions spans many lines
        singleLine text = T.unwords (T.words text)

-- | 5 seconds, in microseconds
telemetryTimeout :: Int
telemetryTimeout = 5000000

getTelemetryInfo :: IO TelemetryInfo
getTelemetryInfo = do
    projectId <- getProjectId
    iswin <- isWindows `catch` \(_ :: IOException) -> pure False
    let opsys
         | System.os == "linux" && iswin = "linux (WSL)"
         | otherwise = System.os
    pure TelemetryInfo { ihpVersion = Version.ihpVersion, os = cs opsys, arch = cs System.arch, projectId }

-- this seems to be the generally accepted way of detecting running under the Windows Subsystem for Linux
isWindows :: IO Bool
isWindows = do
    p <- TIO.readFile "/proc/version"
    pure $ T.isInfixOf "microsoft" (T.toLower p)    -- WSL1 is Microsoft, WSL2 is microsoft

-- | The project id is a an anonymous identifier to keep track of distinct projects.
--
-- The project id is a hash of the current working directory. We use sha512 to make sure no one
-- is able to get back the original path from the hash.
getProjectId :: IO Text
getProjectId = do
    cwdOsPath <- Directory.getCurrentDirectory
    cwd <- decodeUtf cwdOsPath
    cwd
        |> cs
        |> SHA512.hash
        |> Base16.encode
        |> cs
        |> pure

-- | Transforms a telemetry info into a payload to be used with the telemetry request
toPayload :: TelemetryInfo -> [(ByteString, ByteString)]
toPayload TelemetryInfo { .. } = [("ihpVersion", cs ihpVersion), ("os", cs os), ("arch", cs arch), ("projectId", cs projectId)]
