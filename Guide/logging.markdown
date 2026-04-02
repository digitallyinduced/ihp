# Logging

```toc

```

## Introduction

IHP uses [fast-logger](https://hackage.haskell.org/package/fast-logger) for logging. A `FastLogger` (which is just `LogStr -> IO ()`) is available via `?context.logger` in controllers and `?modelContext.logger` in model code.

### Sending messages

In any controller or model code, you can log a message by calling the logger directly:

```haskell
action TopPerformancesAction {collection} = do
    ?context.logger (toLogStr ("starting TopPerformancesAction" :: Text))
    let n = paramOrDefault 5 "numPerformances"
    band <- fetchBand collection
    topPerformances <- fetchTopPerformances collection n
    ?context.logger (toLogStr (show (length topPerformances) <> " top performances received." :: Text))
    render TopPerformancesView {..}
```

Make sure you have `System.Log.FastLogger` imported:

```haskell
import System.Log.FastLogger (toLogStr)
```

### How it works

IHP creates a `FastLogger` at startup using `withFastLogger` from the fast-logger package. This logger writes to stdout by default. It is stored in `FrameworkConfig.logger` and `ModelContext.logger`, and is accessible via implicit parameters in controllers and models.

The `FastLogger` type is just a function:

```haskell
type FastLogger = LogStr -> IO ()
```

You convert text to `LogStr` using `toLogStr` and append a newline with `<> "\n"`.

### Query timing

In development mode (when the `DEBUG` environment variable is set), IHP automatically logs query timing information. This is controlled by the `debugMode` field on `ModelContext`, which is set based on the `DEBUG` environment variable.

### Suppressing query logging

Use `withoutQueryLogging` to suppress query logs for a specific block:

```haskell
users <- withoutQueryLogging (sqlQuery "SELECT * FROM users" ())
```
