{-|
Module: IHP.Log.Types
Description:  Types for the IHP logging system
-}
module IHP.Log.Types (
  module System.Log.FastLogger,
  Logger(..),
  LogLevel(..),
  LoggingProvider(..),
  LogFormatter,
  newLogger,
  defaultLogger
) where

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

class LoggingProvider a where
  getLogger :: a -> Logger

instance LoggingProvider Logger where
  getLogger = id

newLogger :: LogLevel -> LogFormatter -> IO Logger
newLogger level formatter = do
  (write', cleanup) <- newFastLogger (LogStdout defaultBufSize)
  let write = \text -> write' $ toLogStr text
  pure Logger { .. }

defaultLogger :: IO Logger
defaultLogger = newLogger Debug defaultFormatter

-- | Formats the log as-is with a newline added.
defaultFormatter :: LogFormatter
defaultFormatter _ msg = msg <> "\n"

-- | Prepends the log level to the log message and adds a new line.
withLevelFormatter :: LogFormatter
withLevelFormatter level msg = "[" <> toUpper (show level) <> "] " <> msg <> "\n"
