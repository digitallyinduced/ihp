module IHP.Log.Logging (
  debug,
  info
) where

import IHP.HaskellSupport hiding (debug)

import qualified Prelude
import CorePrelude hiding (putStr, putStrLn, print, error, show, log, debug)
import Control.Monad (when)
import Data.Text as Text
import IHP.Log.Types

log :: (?context :: context, LoggingProvider context) => LogLevel -> Text -> IO ()
log level text = do
  let logger = getLogger ?context
  let formatter = get #formatter logger
  formatter level text
    |> writeLog level logger

debug :: (?context :: context, LoggingProvider context) => Text -> IO ()
debug = log Debug

info :: (?context :: context, LoggingProvider context) => Text -> IO ()
info = log Info

writeLog :: LogLevel -> Logger -> Text -> IO ()
writeLog level logger text = do
  when (level >= get #level logger) (text |> get #write logger)