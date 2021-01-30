# Integrates Sentry into your IHP application

## Install


1. Add `ihp-sentry` to the `haskellDeps` in your `default.nix`:
    ```nix
    let
        ...
        haskellEnv = import "${ihp}/NixSupport/default.nix" {
            ihp = ihp;
            haskellDeps = p: with p; [
                # ...
                ihp-sentry
            ];
        ...
    ```
2. Run `make -B .envrc`
3. Add `import IHP.Sentry` to your `Config/Config.hs`:
    ```haskell
    module Config where

    -- ...

    import IHP.Sentry
    ```
4. Call `initSentry` inside your `Config/Config.hs`

```haskell
module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.Sentry

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")

    initSentry "YOUR-SENTRY-DSN"
```

Exceptions are only sent to sentry if the application is running in production-mode (so `option Production` is set).