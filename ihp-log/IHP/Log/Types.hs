{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoFieldSelectors #-}
{-|
Module: IHP.Log.Types
Description:  Types for the IHP logging system
-}

module IHP.Log.Types
( LogStr
, BufSize
, TimeFormat
, LogType'(..)
, LogType
, FileLogSpec(..)
, TimedFileLogSpec(..)
, toLogStr
, fromLogStr
, defaultBufSize
, simpleTimeFormat
, simpleTimeFormat'
, Logger(..)
, LogLevel(..)
, LoggingProvider(..)
, LogFormatter
, FormattedTime
, newLogger
, defaultLogger
, withLogger
, withDefaultLogger
, defaultFormatter
, withLevelFormatter
, withTimeFormatter
, withTimeAndLevelFormatter
) where

import Prelude
import Data.ByteString (ByteString)
import Data.Text as Text
import qualified Control.Exception as Exception
import System.Log.FastLogger (
    LogStr,
    LogType'(..),
    LogType,
    BufSize,
    FileLogSpec(..),
    TimedFileLogSpec(..),
    TimeFormat,
    toLogStr,
    fromLogStr,
    defaultBufSize,
    newTimeCache,
    simpleTimeFormat,
    simpleTimeFormat',
    newTimedFastLogger,
    ToLogStr (..)
    )

import qualified System.Log.FastLogger as FastLogger (FormattedTime)
import GHC.Records


-- some functions brought over from IHP.Prelude
-- can't import due to circular dependency with IHP.ModelSupport which relies on this module

tshow :: Show a => a -> Text
tshow value = Text.pack (Prelude.show value)

show :: Show a => a -> Text
show = tshow

-- | Interal logger type that encapsulates information needed to perform
-- logging operations. Users can also access this though the 'LoggingProvider'
-- class in controller and model actions to perform logic based on the set log level.
--
-- The 'log' closure bakes in level checking, formatting, and time caching,
-- so callers just provide a level and message. Cleanup is handled externally
-- via bracket (see 'withLogger', 'withDefaultLogger').
data Logger = Logger {
    log   :: !(LogLevel -> LogStr -> IO ()),
    level :: !LogLevel
}

data LogLevel
    -- | For general messages to help with debugging during development.
    -- Default log level in development.
    -- Also the log level used for SQL queries.
    -- See 'IHP.Log.debug' for example usage.
    = Debug
    -- | For info messages that help montior application usage.
    -- Default log level for production.
    -- See 'IHP.Log.info' for example usage.
    | Info
    -- | For warning messages when something might be wrong.
    -- See 'IHP.Log.warn' for example usage.
    | Warn
    -- | For application errors that can be recovered from.
    -- See 'IHP.Log.error' for example usage.
    | Error
    -- | For application errors that are fatal
    -- See 'IHP.Log.fatal' for example usage.
    | Fatal
    -- | For miscallenaous log messages. Highest log level - will always be logged
    -- See 'IHP.Log.unknown' for example usage.
    | Unknown
    deriving (Enum, Eq, Ord, Show)

instance ToLogStr LogLevel where
    toLogStr Debug = "DEBUG"
    toLogStr Info = "INFO"
    toLogStr Warn = "WARN"
    toLogStr Error = "ERROR"
    toLogStr Fatal = "FATAL"
    toLogStr Unknown = "UNKNOWN"

-- | The timestamp in the formatted defined by the logger's timeFormat string.
type FormattedTime = ByteString

-- | Called every time a message is sent to the logger.
-- Since this is just a function type, it's trivial to define custom formatters:
--
-- @
--     withTimeAndLevelFormatterUpcaseAndHappy :: LogFormatter
--     withTimeAndLevelFormatterUpcaseAndHappy time level msg =
--        "[" <> toUpper (show level) <> "]"
--          <> "[" <> time <> "] "
--          <> toUpper msg <> " :) \n"
-- @
type LogFormatter = FormattedTime -> LogLevel -> LogStr -> LogStr

-- | Used to get the logger for a given environment.
-- | Call in any instance of 'LoggingProvider' get the the environment's current logger.
-- Useful in controller and model actions, which both have logging contexts.
type LoggingProvider context = HasField "logger" context Logger

instance HasField "logger" Logger Logger where
    getField logger = logger

-- | Create a new 'FastLogger' and wrap it in an IHP 'Logger'.
--
-- Returns the logger and an IO action to clean up resources (flush and close).
-- Use 'withLogger' for bracket-style resource management.
--
-- > (logger, cleanup) <- newLogger Debug defaultFormatter (LogStdout defaultBufSize) simpleTimeFormat'
newLogger :: LogLevel -> LogFormatter -> LogType -> TimeFormat -> IO (Logger, IO ())
newLogger level formatter destination timeFormat = do
    timeCache <- newTimeCache timeFormat
    (write, cleanup) <- newTimedFastLogger timeCache destination
    let log logLevel msg =
            if logLevel >= level
            then write (\time -> formatter time logLevel msg)
            else pure ()
    pure (Logger { log, level }, cleanup)

-- | Create a default logger that logs to stdout with no special formatting.
--
-- Returns the logger and an IO action to clean up resources.
--
-- > (logger, cleanup) <- defaultLogger
defaultLogger :: IO (Logger, IO ())
defaultLogger = newLogger Debug defaultFormatter (LogStdout defaultBufSize) simpleTimeFormat'

-- | Bracket-style logger construction. Ensures cleanup runs even on exception.
--
-- > withLogger Debug defaultFormatter (LogStdout defaultBufSize) simpleTimeFormat' \logger -> do
-- >     -- use logger
withLogger :: LogLevel -> LogFormatter -> LogType -> TimeFormat -> (Logger -> IO a) -> IO a
withLogger level formatter destination timeFormat action =
    Exception.bracket (newLogger level formatter destination timeFormat) snd (action . fst)

-- | Bracket-style default logger. Logs to stdout at Debug level.
--
-- > withDefaultLogger \logger -> do
-- >     -- use logger
withDefaultLogger :: (Logger -> IO a) -> IO a
withDefaultLogger action =
    Exception.bracket defaultLogger snd (action . fst)

-- | Formats the log as-is with a newline added.
defaultFormatter :: LogFormatter
defaultFormatter _ _ msg = msg <> "\n"

-- | Prepends the timestamp to the log message and adds a new line.
withTimeFormatter :: LogFormatter
withTimeFormatter  time _ msg = "[" <> toLogStr time <> "] " <> msg <> "\n"

-- | Prepends the log level to the log message and adds a new line.
withLevelFormatter :: LogFormatter
withLevelFormatter time level msg = "[" <> (toLogStr level) <> "] " <> msg <> "\n"

-- | Prepends the log level and timestamp to the log message and adds a new line.
withTimeAndLevelFormatter :: LogFormatter
withTimeAndLevelFormatter time level msg = "[" <> (toLogStr level) <> "] [" <> toLogStr time <> "] " <> msg <> "\n"
