{-|
Module: IHP.Log.Logging
Description:  Functions to write logs at all log levels.

Import this module qualified!

-}
module IHP.Log.Logging (
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

warn :: (?context :: context, LoggingProvider context) => Text -> IO ()
warn = log Warn

error :: (?context :: context, LoggingProvider context) => Text -> IO ()
error = log Error

fatal :: (?context :: context, LoggingProvider context) => Text -> IO ()
fatal = log Fatal

unknown :: (?context :: context, LoggingProvider context) => Text -> IO ()
unknown = log Unknown

writeLog :: LogLevel -> Logger -> Text -> IO ()
writeLog level logger text = do
  when (level >= get #level logger) (text |> get #write logger)

makeRequestLogger :: RequestLoggerSettings -> Logger -> Middleware
makeRequestLogger settings logger = unsafePerformIO $
  mkRequestLogger settings {
    destination = RequestLogger.Callback (\logStr ->
      let ?context = logger
        in logStr |> fromLogStr |> cs |> info)
  }

defaultRequestLogger :: Logger -> Middleware
defaultRequestLogger = makeRequestLogger def