{-|
Module: IHP.Log.Logging
Description:  Functions to write logs at all log levels.

Import this module qualified!

-}
module IHP.Log (
  debug,
  info,
  warn,
  error,
  fatal,
  unknown,
  makeRequestLogger,
  defaultRequestLogger
) where

import IHP.HaskellSupport hiding (debug)

import qualified Prelude
import CorePrelude hiding (putStr, putStrLn, print, error, show, log, debug)
import Control.Monad (when)
import Data.Text as Text
import IHP.Log.Types
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (mkRequestLogger, RequestLoggerSettings, destination)
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import Data.Default.Class (Default (def))
import Data.String.Conversions (cs)
import System.IO.Unsafe (unsafePerformIO)

-- | Format a log and send it to the logger.
log :: (?context :: context, LoggingProvider context) => LogLevel -> Text -> IO ()
log level text = do
  let logger = getLogger ?context
  let formatter = get #formatter logger
  timestamp <- get #timeCache logger
  formatter timestamp level text
    |> writeLog level logger

debug :: (?context :: context, LoggingProvider context) => Text -> IO ()
debug = log Debug

info :: (?context :: context, LoggingProvider context) => Text -> IO ()
info = log Info

warn :: (?context :: context, LoggingProvider context) => Text -> IO ()
warn = log Warn

error :: (?context :: context, LoggingProvider context) => Text -> IO ()
error = log Error

fatal :: (?context :: context, LoggingProvider context) => Text -> IO ()
fatal = log Fatal

unknown :: (?context :: context, LoggingProvider context) => Text -> IO ()
unknown = log Unknown

-- | Write a log if the given log level is greater than or equal to the logger's log level.
writeLog :: LogLevel -> Logger -> Text -> IO ()
writeLog level logger text = do
  when (level >= get #level logger) (text |> get #write logger)

-- | Wraps 'RequestLogger' from wai-extra to log to an IHP logger.
makeRequestLogger :: RequestLoggerSettings -> Logger -> Middleware
makeRequestLogger settings logger = unsafePerformIO $
  mkRequestLogger settings {
    destination = RequestLogger.Callback (\logStr ->
      let ?context = logger
        in logStr |> fromLogStr |> cs |> info)
  }

-- | Create a request logger with default settings wrapped in an IHP logger.
defaultRequestLogger :: Logger -> Middleware
defaultRequestLogger = makeRequestLogger def