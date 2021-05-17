{-|
Module: IHP.Log
Description:  Functions to write logs at all log levels.

Import this module qualified! All code examples
assume you have imported the module as follows:

> import qualified IHP.Log as Log

-}
module IHP.Log
( module IHP.Log.Types
, debug
, info
, warn
, error
, fatal
, unknown
, makeRequestLogger
, defaultRequestLogger
) where

import IHP.HaskellSupport hiding (debug)

import qualified Prelude
import CorePrelude hiding (putStr, putStrLn, print, error, show, log, debug)
import Control.Monad (when)
import IHP.Log.Types
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (mkRequestLogger, RequestLoggerSettings, destination)
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import Data.Default (Default (def))
import Data.String.Conversions (cs)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Log.FastLogger as FastLogger

-- | Format a log and send it to the logger.
-- Internal use only -- application code should call the
-- function corresponding to the desired log level.
log :: (?context :: context, LoggingProvider context, FastLogger.ToLogStr string) => LogLevel -> string -> IO ()
log level text = do
    let logger = getLogger ?context
    writeLog level logger text

-- | Log a debug level message.
--
-- > action CreateUserAction { .. } = do
-- >     Log.debug "entered CreateUserAction"
-- >     ...
debug :: (?context :: context, LoggingProvider context, FastLogger.ToLogStr string) => string -> IO ()
debug = log Debug

-- | Log an info level message.
--
-- > action UsersAction = do
-- >     users <- query @User |> fetch
-- >     Log.info $ show (lengh users) <> " users fetched."
-- >     ...
info :: (?context :: context, LoggingProvider context, FastLogger.ToLogStr string) => string -> IO ()
info = log Info

-- | Log a warning level message.
--
-- > action UsersAction = do
-- >     users <- query @User |> fetch
-- >     whenEmpty users $ Log.warn "No users found. Something might be wrong!"
-- >     ...
warn :: (?context :: context, LoggingProvider context, FastLogger.ToLogStr string) => string -> IO ()
warn = log Warn

-- |Log a warning level message.
--
-- @
--    action CreatePostAction = do
--        let post = newRecord @Post
--        post
--            |> buildPost
--            |> ifValid \case
--                Left post -> do
--                    Log.error "Invalid post."
--                    render NewView { .. }
--                Right post -> do
--                    post <- post |> createRecord
--                    setSuccessMessage "Post created"
--                    redirectTo PostsAction
-- @
error :: (?context :: context, LoggingProvider context, FastLogger.ToLogStr string) => string -> IO ()
error = log Error

-- | Log a fatal level message.
-- Note this does not exit the program for you -- it only logs to the "Fatal" log level.
--
-- > Log.fatal "Unrecoverable application error!"
fatal :: (?context :: context, LoggingProvider context, FastLogger.ToLogStr string) => string -> IO ()
fatal = log Fatal

-- | Log an "unknown" level message.
-- This is the highest log level and will always be output by the logger.
--
-- > Log.unknown "This will be sent to the logger no matter what!"
unknown :: (?context :: context, LoggingProvider context, FastLogger.ToLogStr string) => string -> IO ()
unknown = log Unknown

-- | Write a log if the given log level is greater than or equal to the logger's log level.
writeLog :: (FastLogger.ToLogStr string) => LogLevel -> Logger -> string -> IO ()
writeLog level logger text = do
    let write = get #write logger
    let formatter = get #formatter logger
    when (level >= get #level logger) do
        write (\time -> formatter time level (toLogStr text))

-- | Wraps 'RequestLogger' from wai-extra to log to an IHP logger.
-- See 'Network.Wai.Middleware.RequestLogger'.
makeRequestLogger :: RequestLoggerSettings -> Logger -> IO Middleware
makeRequestLogger settings logger = 
    mkRequestLogger settings {
        destination = RequestLogger.Callback (\logStr ->
            let ?context = logger in
                logStr |> fromLogStr |> info
            )
        }

-- | Create a request logger with default settings wrapped in an IHP logger.
-- See 'Network.Wai.Middleware.RequestLogger'.
defaultRequestLogger :: Logger -> IO Middleware
defaultRequestLogger = makeRequestLogger def
