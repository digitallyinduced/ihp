module IHP.Log.Logging (
  debug,
  info
) where

import IHP.Prelude hiding (log, debug)
import IHP.Log.Types
import IHP.FrameworkConfig

log :: (?context :: context, ConfigProvider context) => LogLevel -> Text -> IO ()
log level text = do
  let config = getFrameworkConfig ?context
  let logger = get #logger config
  let formatter = get #formatter logger
  formatter level text
    |> writeLog level logger

debug :: (?context :: context, ConfigProvider context) => Text -> IO ()
debug = log Debug

info :: (?context :: context, ConfigProvider context) => Text -> IO ()
info = log Info

writeLog :: LogLevel -> Logger -> Text -> IO ()
writeLog level logger text = do
  when (level >= get #level logger) (text |> get #write logger)