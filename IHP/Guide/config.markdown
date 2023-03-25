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

newtype StripePublicKey = StripePublicKey Text
```

We want our new config parameter to be filled from a `STRIPE_PUBLIC_KEY` env variable. Therefore we add this to our Config.hs:

```haskell
module Config where

newtype StripePublicKey = StripePublicKey Text

config :: ConfigBuilder
config = do
    -- ...
    stripePublicKey <- StripePublicKey <$> env @Text "STRIPE_PUBLIC_KEY"
    option stripePublicKey
```

Now the app reads the `STRIPE_PUBLIC_KEY` env variable at startup and makes it available to the app.

Before we proceed we should add a default value for this in dev mode. Open the `start` script and add the following env variables:

```bash
# Add this before the `RunDevServer` call at the end of the file
export STRIPE_PUBLIC_KEY="pk_test_..."

# Finally start the dev server
RunDevServer
```

#### Using Custom Config Parameters

You can now access the `StripePublicKey` parameter by calling `getAppConfig @Config.StripePublicKey`:

```haskell
import qualified Config

action MyAction = do
    let (StripePublicKey stripePublicKey) = getAppConfig @Config.StripePublicKey

    putStrLn ("Stripe public key: " <> stripePublicKey)
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
Run, `nix-shell --run 'make -B .envrc'` to update the environment.
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
