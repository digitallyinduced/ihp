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

The available log levels are [`debug`](https://ihp.digitallyinduced.com/api-docs/IHP-Log.html#v:debug), [`info`](https://ihp.digitallyinduced.com/api-docs/IHP-Log.html#v:info), [`warn`](https://ihp.digitallyinduced.com/api-docs/IHP-Log.html#v:warn), [`error`](https://ihp.digitallyinduced.com/api-docs/IHP-Log.html#v:error), [`fatal`](https://ihp.digitallyinduced.com/api-docs/IHP-Log.html#v:fatal), and [`unknown`](https://ihp.digitallyinduced.com/api-docs/IHP-Log.html#v:unknown).
In development, the default log level is debug. In production, the default log level is info.
Log messages will only be output if their log level is greater than or equal to the logger's configured log level.

### Sending messages

In any controller or model code, you can log a message at a given log level by simply calling
[`Log.debug`](https://ihp.digitallyinduced.com/api-docs/IHP-Log.html#v:debug), [`Log.info`](https://ihp.digitallyinduced.com/api-docs/IHP-Log.html#v:info), or any of the other available log levels.

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

Use the [`configureLogger`](https://ihp.digitallyinduced.com/api-docs/IHP-FrameworkConfig.html#v:configureLogger) helper to set up a logger with custom options. For example, here is a logger that formats
logs with a timestamp at the `Debug` log level:

```haskell
config :: ConfigBuilder
config = do
    configureLogger Debug withTimeFormatter (LogStdout defaultBufSize) simpleTimeFormat'
```

The `configureLogger` function takes four arguments:

- **level** - The minimum log level (e.g. `Debug`, `Info`, `Warn`, `Error`)
- **formatter** - How to format log messages (see below)
- **destination** - Where to send logs (a `LogType'` value)
- **timeFormat** - The time format string for timestamps

You can also construct a logger directly using [`newLogger`](https://ihp.digitallyinduced.com/api-docs/IHP-Log-Types.html#v:newLogger):

```haskell
(logger, cleanup) <- liftIO $ newLogger Debug withTimeFormatter (LogStdout defaultBufSize) simpleTimeFormat'
option logger
```

#### Configuring log level

Set the level to one of the available constructors for the [`LogLevel`](https://ihp.digitallyinduced.com/api-docs/IHP-Log-Types.html#t:LogLevel) type:

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

- [`defaultFormatter`](https://ihp.digitallyinduced.com/api-docs/IHP-Log-Types.html#v:defaultFormatter) simply prints the log message with a newline.
  - `Server started`
- [`withTimeFormatter`](https://ihp.digitallyinduced.com/api-docs/IHP-Log-Types.html#v:withTimeFormatter) prepends a timestamp.
  - `[28-Jan-2021 10:07:58] Server started`
- [`withLevelFormatter`](https://ihp.digitallyinduced.com/api-docs/IHP-Log-Types.html#v:withLevelFormatter) prepends the message's log level
  - `[INFO] Server started`
- [`withTimeAndLevelFormatter`](https://ihp.digitallyinduced.com/api-docs/IHP-Log-Types.html#v:withTimeAndLevelFormatter) prepends both a timestamp and log level.
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
IHP uses the `LogType'` constructors from the `fast-logger` package to configure destinations:

- `LogStdout BufSize` - Log to standard output
- `LogStderr BufSize` - Log to standard error
- `LogFileNoRotate FilePath BufSize` - Log to a file without rotation
- `LogFile FileLogSpec BufSize` - Log to a file with size-based rotation
- `LogFileTimedRotate TimedFileLogSpec BufSize` - Log to a file with time-based rotation
- `LogCallback (LogStr -> IO ()) (IO ())` - Log to a custom callback

##### Logging to a file

When logging to a file, you can configure rotation to prevent log files from getting too large.

**No rotation** - the log file can grow without limit. Use with caution:

```haskell
config :: ConfigBuilder
config = do
    configureLogger Debug defaultFormatter (LogFileNoRotate "Log/production.log" defaultBufSize) simpleTimeFormat'
```

**Size-based rotation** using `FileLogSpec` - rotates the file after reaching a specified size.
The following example rotates at 4 megabytes, keeping 7 backup files:

```haskell
config :: ConfigBuilder
config = do
    let fileSpec = FileLogSpec "Log/production.log" (4 * 1024 * 1024) 7
    configureLogger Debug defaultFormatter (LogFile fileSpec defaultBufSize) simpleTimeFormat'
```

**Time-based rotation** using `TimedFileLogSpec` - rotates based on a time format string:

```haskell
config :: ConfigBuilder
config = do
    let timedSpec = TimedFileLogSpec
            "Log/production.log"
            "%FT%H%M%S"
            (\oldPath -> void . forkIO $ callProcess "tar" ["--remove-files", "-caf", oldPath <> ".gz", oldPath])
    configureLogger Debug defaultFormatter (LogFileTimedRotate timedSpec defaultBufSize) simpleTimeFormat'
```

#### Configuring timestamp format

The time format argument expects a time format string as defined [here](https://man7.org/linux/man-pages/man3/strptime.3.html).

Example:

```haskell
config :: ConfigBuilder
config = do
    configureLogger Debug defaultFormatter (LogStdout defaultBufSize) "%A, %Y-%m-%d %H:%M:%S"
```

Would log a timestamp as:

> Sunday, 2020-1-31 22:10:21

### Decorating the Logs with the User ID

You can override the default logger and have it decorated with additional information. A typical use case is adding the current user's ID or name to the log messages.


```haskell
-- Web/FrontController.hs

-- Add imports
import IHP.Log.Types as Log
import IHP.Controller.Context

instance InitControllerContext WebApplication where
    initContext = do
        initAuthentication @User
        -- ... your other initContext code

        putContext userIdLogger

userIdLogger :: (?context :: ControllerContext) => Logger
userIdLogger =
    baseLogger { Log.log = \lvl msg -> baseLogger.log lvl (prependUserId msg) }
    where
        baseLogger = ?context.frameworkConfig.logger

prependUserId :: (?context :: ControllerContext) => LogStr -> LogStr
prependUserId string =
    toLogStr $ userInfo <> show string
    where
        userInfo =
            case currentUserOrNothing of
                Just currentUser -> "Authenticated user ID: " <> show currentUser.id <> " "
                Nothing -> "Anonymous user: "
```

From your controller you can now add a log message

```haskell
    action PostsAction = do
        Log.debug ("This log message should have user info" :: Text)
        -- Rest of the action code.
```

In your log output, you will see the user info prepended to the log message.

```
[30-Mar-2024 18:28:29] Authenticated user ID: 5f32a9e3-da09-48d8-9712-34c935a72c7a "This log message should have user info"
```
