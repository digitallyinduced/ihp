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
In development, the default log level is debug. In production, the default log level is warn.
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
data LogLevel
  = Debug
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

You can also define your own formatter. Since a LogFormatter is just a type alias:

```haskell
type LogFormatter = FormattedTime -> LogLevel -> Text -> Text
```

you can define a formatter as a simple function:

```haskell
-- | For when debugging is getting you down
withTimeAndLevelFormatterUpcaseAndHappy :: LogFormatter
withTimeAndLevelFormatterUpcaseAndHappy time level msg =
    "[" <> toUpper (show level) <> "]"
    <> "[" <> time <> "] "
    <> toUpper msg <> " :) \n"
```

Which logs a message like:

    [INFO] [28-Jan-2021 10:07:58] SERVER STARTED :)

#### Configuring log destination

By default, messages are logged to standard out.
IHP includes all the destinations included in `fast-logger` wrapped in a custom API.

```haskell
data LogDestination
    = None
    -- | Log messages to standard output.
    | Stdout BufSize
    -- | Log messages to standard error.
    | Stderr BufSize
    -- | Log message to a file. Rotate the log file with the behavior given by 'RotateSettings'.
    | File FilePath RotateSettings BufSize
    -- | Send logged messages to a callback. Flush action called after every log.
    | Callback (LogStr -> IO ()) (IO ())
```

##### Logging to a file

When logging to a file, it is common to rotate the file logged to in order to prevent
the log file from getting too big. IHP allows for this in three ways, through the `RotateSettings` record.

- `NoRotate` never rotates the file, meaning the log file can become arbitrarily large.
  Use with caution. The following example will log all messages to a file at `Log/production.log`.

```haskell
newLogger def {
    destination = File "Log/production.log" NoRotate defaultBufSize
}
```

- `SizeRotate` rotates the file after reaching a specified size (in bytes).
  The following example will log all messages to a file at `Log/production.log`,
  and rotate the file once it reaches 4 megabytes in size. It will
  keep 7 log files before overwriting the first file.

```haskell
newLogger def {
    destination = File "Log/production.log" (SizeRotate (Bytes (4 * 1024 * 1024)) 7) defaultBufSize
}
```

- `TimedRotate` rotates the file based on a time format string and a function which compares two times formatted by said format string. It also passes the rotated log's file path to a function, which can be used to compress old logs as in this example which rotates once per day:

```haskell
let
    filePath = "Log/production.log"
    formatString = "%FT%H%M%S"
    timeCompare = (==) on C8.takeWhile (/=T))
    compressFile fp = void . forkIO $
        callProcess "tar" [ "--remove-files", "-caf", fp <> ".gz", fp ]
in
  newLogger def {
     destination = File
       filePath
       (TimedRotate formatString timeCompare compressFile)
       defaultBufSize
     }
```

#### Configuring timestamp format

`timeFormat` expects a time format string as defined [here](https://man7.org/linux/man-pages/man3/strptime.3.html).

Example:

```haskell
newLogger def {
    timeFormat = "%A, %Y-%m-%d %H:%M:%S"
}
```

Would log a timestamp as:

> Sunday, 2020-1-31 22:10:21



