{-|
Module: IHP.Log.Types
Description:  Types for the IHP logging system
-}
{-# LANGUAGE GADTs #-}

module IHP.Log.Types (
  module System.Log.FastLogger,
  Logger(..),
  LogLevel(..),
  LogDestination(..),
  LoggingProvider(..),
  LoggerSettings(..),
  LogFormatter,
  newLogger,
  defaultLogger,
  defaultDestination,
  defaultFormatter,
  withLevelFormatter,
  withTimeFormatter,
  withTimeAndLevelFormatter,
) where

import IHP.HaskellSupport
import qualified Prelude
import CorePrelude hiding (putStr, putStrLn, print, error, show)
import Data.Text as Text
import Data.String.Conversions (cs)
import Data.Default.Class (Default (def))
import System.Log.FastLogger (
  LogStr,
  LogType'(..),
  BufSize,
  FileLogSpec(..),
  TimedFileLogSpec(..),
  TimeFormat,
  FormattedTime,
  newFastLogger,
  toLogStr,
  fromLogStr,
  defaultBufSize,
  newTimeCache,
  simpleTimeFormat'
  )


-- some functions brought over from IHP.Prelude
-- can't import due to circular dependency with IHP.ModelSupport which relies on this module

tshow :: Show a => a -> Text
tshow value = Text.pack (Prelude.show value)

show :: Show a => a -> Text
show = tshow

data Logger = Logger {
  write     :: Text -> IO (),
  level     :: LogLevel,
  formatter :: LogFormatter,
  timeCache :: IO FormattedTime,
  cleanup   :: IO ()
}

data LogLevel =
  Debug
  | Info
  | Warn
  | Error
  | Fatal
  | Unknown
  deriving (Enum, Eq, Ord, Show)

type LogFormatter = FormattedTime -> LogLevel -> Text -> Text

-- | Where logged messages will be delivered to. Types correspond with those in fast-logger.
data LogDestination where
  None            :: LogDestination
  -- | Log messages to standard output.
  Stdout          :: BufSize -> LogDestination
  -- | Log messages to standard error.
  Stderr          :: BufSize -> LogDestination
  -- | Log messages to a file which is never rotated.
  FileNoRotate    :: FilePath -> BufSize -> LogDestination
  -- | Log messages to a file rotated automatically based on the critera in 'FileLogSpec'.
  File            :: FileLogSpec -> BufSize -> LogDestination
  -- | Log messages to a file rotated on a timed basis as defined in 'TimedFileLogSpec'.
  FileTimedRotate :: TimedFileLogSpec -> BufSize -> LogDestination
  -- | Send logged messages to a callback. Flush action called after every log.
  Callback        :: (LogStr -> IO ()) -> IO () -> LogDestination

data LoggerSettings = LoggerSettings {
  level       :: LogLevel,
  formatter   :: LogFormatter,
  destination :: LogDestination,
  timeFormat  :: TimeFormat
}

instance Default LoggerSettings where
  def = LoggerSettings {
    level = Debug,
    formatter = defaultFormatter,
    destination = defaultDestination,
    timeFormat = simpleTimeFormat'
  }

-- Logger default destination is to standard out.
defaultDestination :: LogDestination
defaultDestination = Stdout defaultBufSize

class LoggingProvider a where
  getLogger :: a -> Logger

instance LoggingProvider Logger where
  getLogger = id

-- | Create a new 'FastLogger' and wrap it in an IHP 'Logger'.
newLogger :: LoggerSettings -> IO Logger
newLogger LoggerSettings { .. } = do
  timeCache <- newTimeCache timeFormat
  (write', cleanup) <- makeFastLogger destination
  let write = write' . toLogStr
  pure Logger { .. }
  where
    makeFastLogger destination = newFastLogger $
      case destination of
        None                     -> LogNone
        (Stdout buf)             -> LogStdout buf
        Stderr buf               -> LogStderr buf
        FileNoRotate path buf    -> LogFileNoRotate path buf
        File spec buf            -> LogFile spec buf
        FileTimedRotate spec buf -> LogFileTimedRotate spec buf
        Callback callback flush  -> LogCallback callback flush

-- Formats logs as-is to stdout.
defaultLogger :: IO Logger
defaultLogger = newLogger def

-- | Formats the log as-is with a newline added.
defaultFormatter :: LogFormatter
defaultFormatter _ _ msg = msg <> "\n"

-- | Prepends the timestamp to the log message and adds a new line.
withTimeFormatter :: LogFormatter
withTimeFormatter  time _ msg = "[" <> cs time <> "] " <> msg <> "\n"

-- | Prepends the log level to the log message and adds a new line.
withLevelFormatter :: LogFormatter
withLevelFormatter time level msg = "[" <> toUpper (show level) <> "] " <> msg <> "\n"

-- | Prepends the log level and timestamp to the log message and adds a new line.
withTimeAndLevelFormatter :: LogFormatter
withTimeAndLevelFormatter time level msg = "[" <> toUpper (show level) <> "] [" <> cs time <> "] " <> msg <> "\n"