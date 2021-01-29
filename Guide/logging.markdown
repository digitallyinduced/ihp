# Logging

```toc

```

## Introduction

IHP applications and the framework itself can log output using the `IHP.Log` module.

**Note:** since the logging system is multi-threaded for optimal performance, it is not guaranteed that messages will be printed in order.
If you need to know exact ordering it's recommended you rely on the timestamp.

### Log levels
IHP logging uses log levels to determine which messages should be printed.
This way, you can log messages to help in development without flooding production logs.

The available log levels are `debug`, `info`, `warn`, `error`, `fatal`, and `unknown`.
Log messages will only be output if their log level is greater than or equal to the logger's configured log level.

### Sending messages

In any controller or model code, you can log a message at a given log level by simply calling
`Log.debug`, `Log.info`, or any of the other available log levels.

Example:
```haskell
action TopPerformancesAction {collection} = do
    Log.debug "starting TopPerformancesAction"
    let n = paramOrDefault 5 "numPerformances"
    band <- fetchBand collection
    topPerformances <- fetchTopPerformances collection n
    Log.debug $ show (length topPerformances) <> " top performances received."
    whenEmpty topPerformances $ Log.warn "No performances found! Something might be wrong"
    render TopPerformancesView {..}
```

Make sure you have the `IHP.Log` module imported qualified as `Log`:

```haskell
import qualified IHP.Log as Log
```

### Configuration

Configure the IHP logger in `Config/Config.hs`. First, make sure you have imported the `IHP.Log` modules:

```haskell
import qualified IHP.Log as Log
import IHP.Log.Types
```

Using the `newLogger` function, create a logger with the desired options. For example, here is a logger that formats
logs with a timestamp at the `Debug` log level:

```haskell
logger <- liftIO $ newLogger def {
  level = Debug,
  formatter = withTimeFormatter
  }
option logger
```

The available configuration options can be found in the `LoggerSettings` record.

```haskell
data LoggerSettings = LoggerSettings {
  level       :: LogLevel,
  formatter   :: LogFormatter,
  destination :: LogDestination,
  timeFormat  :: TimeFormat
}
```

#### Configuring log level

Set `level` to one of the available constructors for the `LogLevel` type:

```haskell
data LogLevel =
  Debug
  | Info
  | Warn
  | Error
  | Fatal
  | Unknown
```

#### Configuring log format

IHP ships with four available log formats.

- `defaultFormatter` simply prints the log message with a newline.
  - `Server started`
- `withTimeFormatter` prepends a timestamp.
  - `[28-Jan-2021 10:07:58] Server started`
- `withLevelFormatter` prepends the message's log level
  - `[INFO] Server started`
- `withTimeAndLevelFormatter` prepends both a timestamp and log level.
  - `[INFO] [28-Jan-2021 10:07:58] Server started`

You can also define you own formatter:

```haskell
type LogFormatter = FormattedTime -> LogLevel -> Text -> Text
```

#### Configuring log destination

By default, messages are logged to standard out.
IHP includes all the destinations included in `fast-logger`,
see the [`fast-logger` docs](https://hackage.haskell.org/package/fast-logger-3.0.2/docs/System-Log-FastLogger.html#t:LogType-39-) for instructions on configuring the options for each:

```haskell
-- | Where logged messages will be delivered to. Types correspond with those in fast-logger.
data LogDestination =
  None LogDestination
  -- | Log messages to standard output.
  Stdout BufSize
  -- | Log messages to standard error.
  Stderr BufSize
  -- | Log messages to a file which is never rotated.
  FileNoRotate FilePath BufSize
  -- | Log messages to a file rotated automatically based on the criteria in 'FileLogSpec'.
  File FileLogSpec BufSize
  -- | Log messages to a file rotated on a timed basis as defined in 'TimedFileLogSpec'.
  FileTimedRotate TimedFileLogSpec BufSize
  -- | Send logged messages to a callback. Flush action called after every log.
  Callback (LogStr -> IO ()) IO ()
```

#### Configuring timestamp format

`timeFormat` expects a time format string as defined [here](https://man7.org/linux/man-pages/man3/strptime.3.html).



