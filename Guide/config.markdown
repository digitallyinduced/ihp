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
````
