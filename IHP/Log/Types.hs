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

newLogger :: LogLevel -> IO Logger
newLogger level = do
  (write', cleanup) <- newFastLogger (LogStdout defaultBufSize)
  let write = \text -> write' $ toLogStr text
  let formatter = withLevelFormatter
  pure Logger { .. }

defaultLogger :: IO Logger
defaultLogger = newLogger Debug

defaultFormatter :: LogFormatter
defaultFormatter _ msg = msg <> "\n"

withLevelFormatter :: LogFormatter
withLevelFormatter level msg = "[" <> show level <> "] " <> msg <> "\n"
