{-|
Module: IHP.Log.Types
Description:  Types for the IHP logging system
-}

module IHP.Log.Types (
  LogStr,
  BufSize,
  FileLogSpec(..),
  TimedFileLogSpec(..),
  TimeFormat,
  toLogStr,
  fromLogStr,
  defaultBufSize,
  simpleTimeFormat,
  simpleTimeFormat',
  Logger(..),
  LogLevel(..),
  LogDestination(..),
  LoggingProvider(..),
  LoggerSettings(..),
  LogFormatter,
  FormattedTime,
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
import Data.Default (Default (def))
import System.Log.FastLogger (
  LogStr,
  LogType'(..),
  BufSize,
  FileLogSpec(..),
  TimedFileLogSpec(..),
  TimeFormat,
  newFastLogger,
  toLogStr,
  fromLogStr,
  defaultBufSize,
  newTimeCache,
  simpleTimeFormat,
  simpleTimeFormat'
  )

import qualified System.Log.FastLogger as FastLogger (FormattedTime)


-- some functions brought over from IHP.Prelude
-- can't import due to circular dependency with IHP.ModelSupport which relies on this module

tshow :: Show a => a -> Text
tshow value = Text.pack (Prelude.show value)

show :: Show a => a -> Text
show = tshow

-- | Interal logger type that encapsulates information needed to perform
-- logging operations. Users can also access this though the 'LoggingProvider'
-- class in controller and model actions to perform logic based on the set log level.
data Logger = Logger {
  write     :: Text -> IO (),
  level     :: LogLevel,
  formatter :: LogFormatter,
  timeCache :: IO FastLogger.FormattedTime,
  cleanup   :: IO ()
}

data LogLevel
  -- | For general messages to help with debugging during development.
  -- Default log level in development.
  -- See 'IHP.Log.debug' for example usage.
  = Debug
  -- | For info messages that help montior application usage.
  -- Also the log level used for SQL queries.
  -- Disabled by default in production.
  -- See 'IHP.Log.info' for example usage.
  | Info
  -- | For warning messages when something might be wrong.
  -- Default log level for production.
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

-- | The timestamp in the formatted defined by the logger's timeFormat string.
type FormattedTime = Text

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
type LogFormatter = FormattedTime -> LogLevel -> Text -> Text

-- | Where logged messages will be delivered to. Types correspond with those in fast-logger.
data LogDestination
  = None
  -- | Log messages to standard output.
  | Stdout BufSize
  -- | Log messages to standard error.
  | Stderr BufSize
  -- | Log messages to a file which is never rotated.
  --
  -- @
  -- newLogger def {
  --    destination = FileNoRotate "Log/production.log" defaultBufSize
  --    }
  -- @
  | FileNoRotate FilePath BufSize
  -- | Log messages to a file rotated automatically based on the criteria in 'FileLogSpec'.
  --
  -- @
  --    newLogger def {
  --      destination = File (FileLogSpec "Log/production.log" (4 * 1024 * 1024)) defaultBufSize
  --      }
  -- @
  | File FileLogSpec BufSize
  -- | Log messages to a file rotated on a timed basis as defined in 'TimedFileLogSpec'.
  --
  -- @
  --   let
  --       filePath = "Log/production.log"
  --       formatString = "%FT%H%M%S"
  --       timeCompare = (==) on C8.takeWhile (/=T))
  --       compressFile fp = void . forkIO $
  --           callProcess "tar" [ "--remove-files", "-caf", fp <> ".gz", fp ]
  --   in
  --     newLogger def {
  --        destination = FileTimedRotate
  --           (TimedFileLogSpec filePath formatString timeCompare compressFile)
  --        defaultBufSize
  --        }
  -- @
  | FileTimedRotate TimedFileLogSpec BufSize
  -- | Send logged messages to a callback. Flush action called after every log.
  | Callback (LogStr -> IO ()) (IO ())

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

-- | Logger default destination is to standard out.
defaultDestination :: LogDestination
defaultDestination = Stdout defaultBufSize

-- | Used to get the logger for a given environment.
class LoggingProvider a where
  -- | Call in any instance of 'LoggingProvider' get the the environment's current logger.
  -- Useful in controller and model actions, which both have logging contexts.
  getLogger :: a -> Logger

instance {-# OVERLAPS #-} LoggingProvider Logger where
  getLogger = id

-- | Create a new 'FastLogger' and wrap it in an IHP 'Logger'.
-- Use with the default logger settings and record update syntax for nice configuration:
--
-- > newLogger def { level = Error }
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

-- | Formats logs as-is to stdout.
defaultLogger :: IO Logger
defaultLogger = newLogger def

-- | Formats the log as-is with a newline added.
defaultFormatter :: LogFormatter
defaultFormatter _ _ msg = msg <> "\n"

-- | Prepends the timestamp to the log message and adds a new line.
withTimeFormatter :: LogFormatter
withTimeFormatter  time _ msg = "[" <> time <> "] " <> msg <> "\n"

-- | Prepends the log level to the log message and adds a new line.
withLevelFormatter :: LogFormatter
withLevelFormatter time level msg = "[" <> toUpper (show level) <> "] " <> msg <> "\n"

-- | Prepends the log level and timestamp to the log message and adds a new line.
withTimeAndLevelFormatter :: LogFormatter
withTimeAndLevelFormatter time level msg = "[" <> toUpper (show level) <> "] [" <> time <> "] " <> msg <> "\n"