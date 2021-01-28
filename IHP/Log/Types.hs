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
  LogFormatter,
  newLogger,
  defaultLogger,
  defaultDestination,
  defaultFormatter,
  withLevelFormatter,
) where

import IHP.HaskellSupport
import qualified Prelude
import CorePrelude hiding (putStr, putStrLn, print, error, show)
import Data.Text as Text
import System.Log.FastLogger


-- some functions brought over from IHP.Prelude
-- can't import due to circular dependency with IHP.ModelSupport which relies on this module

tshow :: Show a => a -> Text
tshow value = Text.pack (Prelude.show value)

show :: Show a => a -> Text
show = tshow

data Logger = Logger {
  write :: Text -> IO (),
  level :: LogLevel,
  formatter :: LogFormatter,
  cleanup :: IO ()
}


data LogLevel =
  Debug
  | Info
  | Warn
  | Error
  | Fatal
  | Unknown
  deriving (Enum, Eq, Ord, Show)

type LogFormatter = LogLevel -> Text -> Text

data LogDestination where
  None            :: LogDestination
  Stdout          :: BufSize -> LogDestination
  Stderr          :: BufSize -> LogDestination
  FileNoRotate    :: FilePath -> BufSize -> LogDestination
  -- File            :: FilePath -> BufSize -> LogDestination
  -- FileTimedRotate :: FilePath -> BufSize -> LogDestination
  Callback        :: (LogStr -> IO ()) -> IO () -> LogDestination

defaultDestination :: LogDestination
defaultDestination = Stdout defaultBufSize

class LoggingProvider a where
  getLogger :: a -> Logger

instance LoggingProvider Logger where
  getLogger = id

newLogger :: LogLevel -> LogFormatter -> LogDestination -> IO Logger
newLogger level formatter destination = do
  (write', cleanup) <- makeFastLogger destination
  let write = write' . toLogStr
  pure Logger { .. }
  where
    makeFastLogger destination = newFastLogger $
      case destination of
        None -> LogNone
        (Stdout buf) -> LogStdout buf
        Stderr buf -> LogStderr buf
        FileNoRotate path buf -> LogFileNoRotate path buf
        Callback callback flush -> LogCallback callback flush

defaultLogger :: IO Logger
defaultLogger = newLogger Debug defaultFormatter defaultDestination

-- | Formats the log as-is with a newline added.
defaultFormatter :: LogFormatter
defaultFormatter _ msg = msg <> "\n"

-- | Prepends the log level to the log message and adds a new line.
withLevelFormatter :: LogFormatter
withLevelFormatter level msg = "[" <> toUpper (show level) <> "] " <> msg <> "\n"
