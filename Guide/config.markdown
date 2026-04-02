# Config

At a certain point in the lifetime of your IHP app you will want to add your own config parameters, e.g. for managing secrets, API keys or external services. This guide explains the best practises for doing that.

```toc
```

## Custom Configuration

### Dealing with Secrets

Sometimes you want to have a custom configuration flag inside your application.

The recommended way is to declare a custom `newtype` in `Config/Config.hs` like this:

```haskell
-- Config.hs

import IHP.EnvVar

newtype StripePublicKey = StripePublicKey Text
```

We want our new config parameter to be filled from a `STRIPE_PUBLIC_KEY` env variable. Therefore we add this to our Config.hs:

```haskell
module Config where

import IHP.EnvVar

newtype StripePublicKey = StripePublicKey Text

config :: ConfigBuilder
config = do
    -- ...
    stripePublicKey <- StripePublicKey <$> env @Text "STRIPE_PUBLIC_KEY"
    option stripePublicKey
```

Now the app reads the `STRIPE_PUBLIC_KEY` env variable at startup and makes it available to the app.

Before we proceed we should add a default value for this in dev mode. Create a file `.env` and add the following env variables:

```bash
# Add this at the end of the file
export STRIPE_PUBLIC_KEY="pk_test_..."
```

The `.env` is not committed to the repo as it's part of the default `.gitignore` for IHP projects. The `.envrc` has a snippet to load environment variables from `.env` into your shell.

If you are ok to commit your secrets to git repo, you can also put the env vars directly into the `.envrc` file.

#### Using Custom Config Parameters

You can now access the `StripePublicKey` parameter by calling `getAppConfig @Config.StripePublicKey`:

```haskell
action MyAction = do
    let (StripePublicKey stripePublicKey) = getAppConfig @Config.StripePublicKey

    putStrLn ("Stripe public key: " <> stripePublicKey)
```

If you want to fetch it in a helper function, we need to define the `?context`:

```haskell
getStripePublicKey :: (?context :: ControllerContext) => StripePublicKey
getStripePublicKey = getAppConfig @Config.StripePublicKey
```

## Environment Variables

### Reading Environment Variables

Inside `Config/Config.hs` you can use `env` to read environment variables.

```haskell
module Config where

config :: ConfigBuilder
config = do
    someString <- env @Text "SOME_STRING"
```

The `env` function will raise an error if the env var is not defined.

The `env` function can also deal with other common types:

```haskell
module Config where

config :: ConfigBuilder
config = do
    maxRetryCount <- env @Int "MAX_RETRY_COUNT"
    byteString <- env @ByteString "SOME_BYTESTRING"
```

### Default Values

Use `envOrDefault` to provide a default value for an env var:

```haskell
module Config where

config :: ConfigBuilder
config = do
    redisPort <- envOrDefault @Int 6379 "REDIS_PORT"
```


### Optional Env Variables

When an env variable is optional and has no good default value, use `envOrNothing`. It will return `Nothing` if the env variable is not set:

```haskell
module Config where

config :: ConfigBuilder
config = do
    redisUrl :: Maybe Text <- envOrNothing "REDIS_URL"
```

### Custom Parser

When you're dealing with a custom enum type it can be useful to write a custom env parser by implementing an `EnvVarReader`:

```haskell
module Config where

config :: ConfigBuilder
config = do
    ipAddrSource :: IPAddrSource <- envOrDefault "IP_ADDR_SOURCE" FromSocket

data IPAddrSource = FromSocket | FromHeader

instance EnvVarReader RequestLogger.IPAddrSource where
    envStringToValue "FromHeader" = Right RequestLogger.FromHeader
    envStringToValue "FromSocket" = Right RequestLogger.FromSocket
    envStringToValue otherwise    = Left "Expected 'FromHeader' or 'FromSocket'"
```

### Custom Middleware

IHP provides an "escape-hatch" from the framework with the `CustomMiddleware` option.
This can be used to run any WAI middleware after IHP's middleware stack, allowing for possibilities
such as embedding a Servant or Yesod app into an IHP app, adding GZIP compression, or any other
number of possibilities. See [wai-extra](https://hackage.haskell.org/package/wai-extra) for examples
of WAI middleware that could be added.

The following example sets up a custom middleware that infers the real IP using `X-Forwarded-For`
and adds a custom header for every request.

```haskell
module Config where

import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.RealIp (realIp)

config :: ConfigBuilder
config = do
    option $ CustomMiddleware $ addHeaders [("X-My-Header", "Custom WAI Middleware!")] . realIp
```

### Compression Middleware
We can compress assets using gzip or brotli.
First, let's add the required Haskell dependencies:
In your `default.nix` file, add:
```nix
        haskellDeps = p: with p; [
            ...

            # Wai Middleware
            wai-middleware-brotli # <-- Add This Dependency
            wai-extra # <-- And This One
        ];
```

Run `devenv up` to update the environment.
Once that succeeds, we can use it in your `Config/Config.hs`:

Add two imports, one for Gzip compression, another for Brotli compression:
```haskell
module Config where
...
import Network.Wai.Middleware.Brotli -- <-- Add This Import
import Network.Wai.Middleware.Gzip -- <-- And This One
```

And then create a function `compressionMiddleware` that combines (composes) Gzip and Brotli compression middleware's into one middleware:
```haskell
-- | Gzip And Brotli Compression Middleware
compressionMiddleware :: CustomMiddleware
compressionMiddleware =
    let
        -- With `GzipCompress` and `BrotliCompress` options, it will compress per request.
        gzipSettings = def { gzipFiles = GzipCompress }
        brotliSettings = defaultSettings { brotliFilesBehavior = BrotliCompress }
    in
        CustomMiddleware (gzip gzipSettings . brotli brotliSettings)

```
Lastly, we can use it as:
```haskell
config :: ConfigBuilder
config = do
    ...
    option compressionMiddleware -- <-- Here we add our middleware
```

The default behavior for `GzipCompress` and `BrotliCompress` is to compress files on the fly.
You can customize this behavior, take a look at the [brotli config](https://github.com/iand675/hs-brotli/blob/master/wai-middleware-brotli/src/Network/Wai/Middleware/Brotli.hs#L53-L66) and [gzip config](https://github.com/yesodweb/wai/blob/master/wai-extra/Network/Wai/Middleware/Gzip.hs#L62-L73).

Also notice `CustomMiddleware (gzip gzipSettings . brotli brotliSettings)`, It's [important that brotli middleware wraps the gzip middleware](https://github.com/iand675/hs-brotli/blob/master/wai-middleware-brotli/src/Network/Wai/Middleware/Brotli.hs#L15-L17), so the responses are not compressed by both, if the client supports brotli, compress with brotli, otherwise gzip, fallback to no compression.

By default all `text/*` content types will be compressed, including `application/json`, `application/javascript`, `application/ecmascript` and `image/x-icon`.
Simply put, html, text, css, javascript, json and icons.

## Database Connection Pool

IHP uses two database connection pools:

1. **postgresql-simple pool** - Used for inserts, updates, deletes, and transactions
2. **hasql pool** - Used for fetch queries with prepared statements (better performance)

### Hasql Pool Configuration

The hasql pool can be configured using environment variables:

| Variable | Default | Description |
|----------|---------|-------------|
| `HASQL_POOL_SIZE` | 20 | Number of connections in the pool |
| `HASQL_IDLE_TIME` | 600 | Seconds before idle connections are closed |

Example `.env` configuration:

```bash
# Use a single connection for consistent prepared statement caching
export HASQL_POOL_SIZE=1

# Keep connections alive for 30 minutes
export HASQL_IDLE_TIME=1800
```

#### Prepared Statement Caching

PostgreSQL prepared statements are cached per-connection. With multiple connections in the pool, the first query on each connection will re-prepare the statement.

## Configuration Reference

This section provides a comprehensive reference of all configuration options available in IHP. Configuration is set in your `Config/Config.hs` file using the `option` function, or through environment variables.

### How Configuration Works

IHP uses a type-based configuration system. Each configuration option is a distinct Haskell type, and you set options using the `option` function inside `Config/Config.hs`:

```haskell
module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig

config :: ConfigBuilder
config = do
    option Production
    option (AppHostname "myapp.com")
```

The first call to `option` for a given type wins. If your `Config.hs` sets a value and the IHP defaults also set a value for the same type, your value takes priority because `config` runs before `ihpDefaultConfig`.

### Server Settings

#### Environment

Controls whether the app runs in Development or Production mode. Many other settings change their defaults based on this value (logging, caching, error pages, etc.).

| | |
|---|---|
| **Type** | `Environment` |
| **Values** | `Development`, `Production` |
| **Default** | `Development` |
| **Env var** | `IHP_ENV` (set to `"Production"` or `"Development"`) |

```haskell
-- Config.hs
config = do
    option Production
```

The key differences between environments are:

- **Static file caching**: Development disables browser cache (max-age=0). Production caches forever with asset hash invalidation.
- **Logging**: Development uses the default format at Debug level. Production uses Apache-style logging at Info level.
- **Background workers**: Development starts a job worker automatically. Production requires a separate `RunJobs` process.
- **Error pages**: Development shows backtraces and code details. Production hides implementation details.

#### App Port

The port the HTTP server listens on.

| | |
|---|---|
| **Type** | `AppPort` |
| **Default** | `8000` |
| **Env var** | `PORT` |

```haskell
-- Config.hs
config = do
    option (AppPort 3000)
```

#### App Hostname

The hostname used when constructing the base URL.

| | |
|---|---|
| **Type** | `AppHostname` |
| **Default** | `"localhost"` |

```haskell
-- Config.hs
config = do
    option (AppHostname "myapp.com")
```

#### Base URL

The full base URL of the application (e.g. `"https://myapp.com"`). This is normally constructed automatically from `AppHostname` and `AppPort`, but can be overridden.

| | |
|---|---|
| **Type** | `BaseUrl` |
| **Default** | Built from hostname and port, e.g. `"http://localhost:8000"` |
| **Env var** | `IHP_BASEURL` (overrides the computed value) |

```haskell
-- Config.hs
config = do
    option (BaseUrl "https://myapp.com")
```

The `IHP_BASEURL` environment variable is particularly useful in production deployments where the app runs behind a reverse proxy.

### Database

#### Database URL

The PostgreSQL connection string.

| | |
|---|---|
| **Type** | `DatabaseUrl` |
| **Default** | `"postgresql:///app?host=<project-dir>/build/db"` (local Unix socket) |
| **Env var** | `DATABASE_URL` |

```haskell
-- Config.hs
config = do
    option (DatabaseUrl "postgresql://user:pass@host:5432/dbname")
```

In production, set the `DATABASE_URL` environment variable instead of hardcoding credentials in source code.

#### Hasql Connection Pool

The hasql pool is used for fetch queries with prepared statements. Configure it via environment variables:

| Env var | Default | Description |
|---------|---------|-------------|
| `HASQL_POOL_SIZE` | `20` | Number of connections in the pool |
| `HASQL_IDLE_TIME` | Not set (uses hasql default) | Seconds before idle connections are closed |

```bash
# .env
export HASQL_POOL_SIZE=1
export HASQL_IDLE_TIME=1800
```

Setting `HASQL_POOL_SIZE=1` gives consistent prepared statement caching since PostgreSQL caches prepared statements per-connection.

### Session

#### Session Cookie

Controls the session cookie behavior (max age, security flags, same-site policy).

| | |
|---|---|
| **Type** | `SessionCookie` |
| **Default** | Path: `/`, Max-Age: 30 days, SameSite: Lax, HttpOnly: yes, Secure: yes if base URL uses HTTPS |

```haskell
-- Config.hs
import qualified Web.Cookie as Cookie

config = do
    option $ SessionCookie (defaultIHPSessionCookie "https://myapp.com")
        { Cookie.setCookieMaxAge = Just (fromIntegral (60 * 60 * 24 * 90)) -- 90 days
        , Cookie.setCookieSameSite = Just Cookie.sameSiteStrict
        }
```

#### Session Secret

The session encryption key. IHP looks for this in the following order:

1. `IHP_SESSION_SECRET_FILE` env var -- path to a key file
2. `IHP_SESSION_SECRET` env var -- the key value directly
3. `Config/client_session_key.aes` file (auto-generated in development)

| Env var | Description |
|---------|-------------|
| `IHP_SESSION_SECRET_FILE` | Path to a file containing the session encryption key |
| `IHP_SESSION_SECRET` | The session encryption key as a string |

In production, set one of these environment variables. In development, IHP auto-generates and uses the `Config/client_session_key.aes` file.

### Logging

#### Logger

Controls log level, format, and destination.

| | |
|---|---|
| **Type** | `FastLogger` (from `System.Log.FastLogger`) |
| **Default** | Logs to stdout |

IHP uses [fast-logger](https://hackage.haskell.org/package/fast-logger) directly. The logger is a `FastLogger` (`LogStr -> IO ()`) created at startup and available via `?context.logger` in controllers.

Log via `?context.logger (toLogStr "message" <> "\n")`. See the [Logging Guide](logging.html) for details.

#### Request Logger IP Source

Controls how the request logger determines the client IP address.

| | |
|---|---|
| **Type** | `RequestLogger.IPAddrSource` |
| **Default** | `FromSocket` |
| **Env var** | `IHP_REQUEST_LOGGER_IP_ADDR_SOURCE` (set to `"FromHeader"` or `"FromSocket"`) |

Set to `FromHeader` when running behind a reverse proxy that sets `X-Forwarded-For`:

```haskell
-- Config.hs
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger

config = do
    option RequestLogger.FromHeader
```

### Security

#### CORS

Cross-Origin Resource Sharing policy. Disabled (no CORS headers) by default.

| | |
|---|---|
| **Type** | `Maybe Cors.CorsResourcePolicy` |
| **Default** | `Nothing` (CORS middleware not applied) |

```haskell
-- Config.hs
import qualified Network.Wai.Middleware.Cors as Cors

config = do
    option $ Just Cors.simpleCorsResourcePolicy
        { Cors.corsOrigins = Just (["https://frontend.example.com"], True)
        , Cors.corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
        , Cors.corsRequestHeaders = ["Content-Type", "Authorization"]
        }
```

#### Request Body Limits

Controls the maximum size of request bodies, uploaded files, number of headers, etc.

| | |
|---|---|
| **Type** | `WaiParse.ParseRequestBodyOptions` |
| **Default** | `WaiParse.defaultParseRequestBodyOptions` (from wai-extra) |

```haskell
-- Config.hs
import qualified Network.Wai.Parse as WaiParse

config = do
    option $ WaiParse.setMaxRequestFileSize (50 * 1024 * 1024) -- 50 MB
           $ WaiParse.defaultParseRequestBodyOptions
```

### CSS Framework

Controls which CSS framework is used for rendering forms, pagination, flash messages, and other UI components.

| | |
|---|---|
| **Type** | `CSSFramework` |
| **Default** | `bootstrap` (Bootstrap) |

```haskell
-- Config.hs
import IHP.View.CSSFramework.Bootstrap (bootstrap)

config = do
    option bootstrap
```

You can customize the CSS framework by overriding individual rendering functions. See the IHP Guide on CSS Frameworks for details.

### File Storage

File storage is configured using helper functions from `IHP.FileStorage.Config`. You must choose one storage backend.

#### Static Directory Storage

Stores uploaded files in the local `static/` directory.

| | |
|---|---|
| **Env var** | `IHP_STORAGE_DIR` (default: `"static/"`) |

```haskell
-- Config.hs
import IHP.FileStorage.Config

config = do
    initStaticDirStorage
```

#### Amazon S3 Storage

Stores files in an AWS S3 bucket.

| Env var | Description |
|---------|-------------|
| `AWS_ACCESS_KEY_ID` | AWS access key |
| `AWS_SECRET_ACCESS_KEY` | AWS secret key |

```haskell
-- Config.hs
import IHP.FileStorage.Config

config = do
    initS3Storage "eu-central-1" "my-bucket-name"
```

#### Minio Storage

Stores files in a Minio-compatible object storage server.

| Env var | Description |
|---------|-------------|
| `MINIO_ACCESS_KEY` | Minio access key |
| `MINIO_SECRET_KEY` | Minio secret key |

```haskell
-- Config.hs
import IHP.FileStorage.Config

config = do
    initMinioStorage "https://minio.example.com" "my-bucket-name"
```

#### Filebase Storage

Stores files using the Filebase IPFS-backed storage service.

| Env var | Description |
|---------|-------------|
| `FILEBASE_KEY` | Filebase access key |
| `FILEBASE_SECRET` | Filebase secret key |

```haskell
-- Config.hs
import IHP.FileStorage.Config

config = do
    initFilebaseStorage "my-bucket-name"
```

### Mail

Mail server configuration is set using `option` with a `MailServer` value. You must configure a mail server before calling `sendMail`.

#### Sendmail (Local)

Uses the local `sendmail` binary:

```haskell
-- Config.hs
import IHP.Mail.Types

config = do
    option Sendmail
```

#### SMTP

Uses a generic SMTP server:

```haskell
-- Config.hs
import IHP.Mail.Types

config = do
    option SMTP
        { host = "smtp.example.com"
        , port = 587
        , credentials = Just ("username", "password")
        , encryption = STARTTLS -- or TLS, or Unencrypted
        }
```

The `SMTPEncryption` type supports three values: `Unencrypted`, `TLS`, and `STARTTLS`. It can be read from environment variables using `env`.

#### Amazon SES

Uses AWS Simple Email Service:

```haskell
-- Config.hs
import IHP.Mail.Types

config = do
    option SES
        { accessKey = "your-access-key"
        , secretKey = "your-secret-key"
        , region = "us-east-1"
        }
```

#### SendGrid

Uses SendGrid for email delivery:

```haskell
-- Config.hs
import IHP.Mail.Types

config = do
    option SendGrid
        { apiKey = "your-sendgrid-api-key"
        , category = Nothing -- or Just "transactional"
        }
```

### Exception Tracking

Controls how unhandled exceptions are reported. Useful for integrating with services like Sentry.

| | |
|---|---|
| **Type** | `ExceptionTracker` |
| **Default** | Warp's default exception handler |

```haskell
-- Config.hs
config = do
    option $ ExceptionTracker \maybeRequest exception -> do
        putStrLn ("Exception: " <> show exception)
        -- Send to Sentry, Bugsnag, etc.
```

### Asset Versioning

Controls cache-busting for static assets. In production, `assetPath` appends a version hash to file URLs.

| Env var | Description |
|---------|-------------|
| `IHP_ASSET_VERSION` | A version string appended to asset URLs for cache busting |
| `IHP_ASSET_BASEURL` | Base URL prepended to asset paths (e.g. a CDN URL) |

These are typically set in your deployment configuration, not in `Config.hs`.

### DataSync (Real-Time)

Settings for IHP DataSync WebSocket connections.

| | |
|---|---|
| **Env var** | `IHP_DATASYNC_MAX_SUBSCRIPTIONS_PER_CONNECTION` |
| **Default** | `128` |
| **Description** | Maximum number of DataSync subscriptions per WebSocket connection |

| | |
|---|---|
| **Env var** | `IHP_DATASYNC_MAX_TRANSACTIONS_PER_CONNECTION` |
| **Default** | `10` |
| **Description** | Maximum number of concurrent DataSync transactions per WebSocket connection |

```haskell
-- Config.hs
config = do
    option (DataSyncMaxSubscriptionsPerConnection 256)
    option (DataSyncMaxTransactionsPerConnection 20)
```

### Row-Level Security

Controls the PostgreSQL role used for queries with Row Level Security enabled.

| | |
|---|---|
| **Type** | `RLSAuthenticatedRole` |
| **Default** | `"ihp_authenticated"` |
| **Env var** | `IHP_RLS_AUTHENTICATED_ROLE` |

```haskell
-- Config.hs
config = do
    option (RLSAuthenticatedRole "my_app_user")
```

### IDE Integration

The base URL of the IHP IDE (development server UI). Only used in Development mode.

| | |
|---|---|
| **Type** | `IdeBaseUrl` |
| **Default** | `"http://localhost:<port+1>"` |
| **Env var** | `IHP_IDE_BASEURL` |

### Startup Initializers

Run custom IO actions when the app server starts. Initializers run concurrently using `async`.

```haskell
-- Config.hs
config = do
    addInitializer do
        putStrLn "App server started!"
        -- Warm caches, start background tasks, etc.
```

### Other Environment Variables

These environment variables are read by IHP's server infrastructure and cannot be set via `option` in `Config.hs`:

| Env var | Default | Description |
|---------|---------|-------------|
| `IHP_SYSTEMD` | `False` | Enable systemd socket activation and watchdog support |
| `IHP_STATIC` | IHP's built-in static directory | Override the path to IHP's framework static files |
| `APP_STATIC` | `"static/"` | Override the path to the application's static files directory |
| `IHP_SOCKET_FD` | Not set | File descriptor for a pre-opened socket (used by the dev server for seamless restarts) |

### Quick Reference: All Environment Variables

| Variable | Default | Category |
|----------|---------|----------|
| `IHP_ENV` | `Development` | Server |
| `PORT` | `8000` | Server |
| `IHP_BASEURL` | Auto-computed | Server |
| `DATABASE_URL` | Local Unix socket | Database |
| `HASQL_POOL_SIZE` | `20` | Database |
| `HASQL_IDLE_TIME` | hasql default | Database |
| `IHP_SESSION_SECRET` | N/A | Session |
| `IHP_SESSION_SECRET_FILE` | N/A | Session |
| `IHP_REQUEST_LOGGER_IP_ADDR_SOURCE` | `FromSocket` | Logging |
| `IHP_ASSET_VERSION` | N/A | Assets |
| `IHP_ASSET_BASEURL` | N/A | Assets |
| `IHP_RLS_AUTHENTICATED_ROLE` | `ihp_authenticated` | Security |
| `IHP_DATASYNC_MAX_SUBSCRIPTIONS_PER_CONNECTION` | `128` | DataSync |
| `IHP_DATASYNC_MAX_TRANSACTIONS_PER_CONNECTION` | `10` | DataSync |
| `IHP_SYSTEMD` | `False` | Deployment |
| `IHP_STATIC` | Built-in | Static Files |
| `APP_STATIC` | `static/` | Static Files |
| `IHP_STORAGE_DIR` | `static/` | File Storage |
| `AWS_ACCESS_KEY_ID` | N/A | File Storage (S3) |
| `AWS_SECRET_ACCESS_KEY` | N/A | File Storage (S3) |
| `MINIO_ACCESS_KEY` | N/A | File Storage (Minio) |
| `MINIO_SECRET_KEY` | N/A | File Storage (Minio) |
| `FILEBASE_KEY` | N/A | File Storage (Filebase) |
| `FILEBASE_SECRET` | N/A | File Storage (Filebase) |
| `IHP_IDE_BASEURL` | `http://localhost:<port+1>` | IDE |
