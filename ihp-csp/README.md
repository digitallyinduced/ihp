# IHP Content Security Policy (CSP)

Type-safe Content Security Policy for IHP applications.

## Overview

Content Security Policy (CSP) is a critical security feature that helps prevent cross-site scripting (XSS), clickjacking, and other code injection attacks. This package provides a type-safe Haskell API for managing CSP headers in IHP applications.

## Features

- **Type-safe API**: Define CSP policies using Haskell types instead of error-prone strings
- **Good defaults**: Secure default policies out of the box
- **Nonce support**: Built-in support for nonce-based script and style loading
- **Flexible**: Easy to customize policies for your specific needs
- **IHP integration**: Seamlessly integrates with IHP's controller and view system

## Installation

1. Add `ihp-csp` to your project's dependencies in `default.nix`:

```nix
let
    ihp = ...;
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = p: with p; [
            cabal-install
            base
            wai
            text
            hlint
            p.ihp
            ihp-csp  # Add this line
        ];
        otherDeps = p: with p; [
            # Native dependencies, e.g. imagemagick
        ];
        projectPath = ./.;
    };
in
    haskellEnv
```

2. Import the module in your controllers:

```haskell
import qualified IHP.CSP as CSP
```

## Usage

### Basic Usage with Strict CSP

The recommended approach is to use the `strictCSP` policy with nonces:

```haskell
module Web.Controller.Posts where

import Web.Controller.Prelude
import qualified IHP.CSP as CSP

instance Controller PostsController where
    beforeAction = do
        -- Generate a cryptographically secure nonce
        nonce <- CSP.generateNonce
        putContext (CSP.CSPNonce nonce)
        
        -- Set a strict CSP with the nonce
        CSP.setCSP $ CSP.strictCSP nonce

    action PostsAction = do
        posts <- query @Post |> fetch
        render IndexView { .. }
```

In your view, use the nonce for inline scripts and styles:

```haskell
module Web.View.Posts.Index where

import Web.View.Prelude
import qualified IHP.CSP as CSP

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div>
            {scripts}
        </div>
    |]
      where
        scripts = do
            let CSP.CSPNonce nonce = fromFrozenContext
            [hsx|
                <script nonce={nonce}>
                    console.log("This script is allowed by CSP");
                </script>
            |]
```

### Custom CSP Policies

You can create custom CSP policies by modifying `defaultCSP` or `strictCSP`:

```haskell
import qualified IHP.CSP as CSP

myCustomCSP :: Text -> CSP.CSP
myCustomCSP nonce = CSP.defaultCSP
    { CSP.scriptSrc = Just 
        [ CSP.nonce nonce
        , CSP.strictDynamic
        , CSP.host "https://cdn.example.com"
        ]
    , CSP.styleSrc = Just 
        [ CSP.nonce nonce
        , CSP.self
        ]
    , CSP.imgSrc = Just 
        [ CSP.self
        , CSP.data'
        , CSP.host "https://images.example.com"
        ]
    , CSP.connectSrc = Just 
        [ CSP.self
        , CSP.host "wss://example.com"
        ]
    , CSP.frameAncestors = Just 
        [ CSP.host "https://trusted.com"
        ]
    , CSP.upgradeInsecureRequests = True
    }
```

### Default CSP

The `defaultCSP` provides a safe baseline:

```haskell
CSP.setCSP CSP.defaultCSP
```

This sets:
- `default-src 'self'`
- `object-src 'none'`
- `base-uri 'none'`

### Available CSP Sources

The package provides helper functions for all CSP source values:

- `CSP.self` - `'self'`
- `CSP.none` - `'none'`
- `CSP.unsafeInline` - `'unsafe-inline'` (not recommended)
- `CSP.unsafeEval` - `'unsafe-eval'` (not recommended)
- `CSP.strictDynamic` - `'strict-dynamic'`
- `CSP.nonce "abc123"` - `'nonce-abc123'`
- `CSP.data'` - `data:`
- `CSP.https` - `https:`
- `CSP.http` - `http:`
- `CSP.blob` - `blob:`
- `CSP.mediastream` - `mediastream:`
- `CSP.filesystem` - `filesystem:`
- `CSP.host "example.com"` - `example.com`
- `CSP.scheme "https:"` - `https:`

## Environment-Specific Policies

You can adjust CSP based on your environment:

```haskell
import qualified IHP.EnvVar as EnvVar

instance Controller PostsController where
    beforeAction = do
        nonce <- CSP.generateNonce
        putContext (CSP.CSPNonce nonce)
        
        isDev <- EnvVar.envOrDefault "development" "IHP_ENV" >>= pure . (== "development")
        
        let csp = if isDev
            then devCSP nonce
            else prodCSP nonce
        
        CSP.setCSP csp

devCSP :: Text -> CSP.CSP
devCSP nonce = (CSP.strictCSP nonce)
    { CSP.connectSrc = Just 
        [ CSP.self
        , CSP.host "ws://localhost:*"  -- Allow local websockets
        ]
    , CSP.frameAncestors = Just 
        [ CSP.host "http://localhost:8000"
        ]
    }

prodCSP :: Text -> CSP.CSP
prodCSP nonce = (CSP.strictCSP nonce)
    { CSP.connectSrc = Just 
        [ CSP.self
        , CSP.host "wss://myapp.com"
        ]
    , CSP.frameAncestors = Just 
        [ CSP.host "https://myapp.com"
        ]
    , CSP.upgradeInsecureRequests = True
    }
```

## Comparison with Manual CSP

### Before (Manual String-based CSP):

```haskell
contentSecurityPolicy :: Text -> ByteString
contentSecurityPolicy nonce =
    [plain|default-src 'self';
           script-src 'strict-dynamic' 'nonce-#{nonce}';
           style-src 'nonce-#{nonce}' 'self';
           object-src 'none';
           base-uri 'none';|]
    |> cs

setSecurityHeaders :: _ => IO ()
setSecurityHeaders = do
    nonce <- generateAuthenticationToken
    putContext (CSPNonce nonce)
    setHeader ("Content-Security-Policy", contentSecurityPolicy nonce)
```

### After (Type-safe CSP):

```haskell
import qualified IHP.CSP as CSP

setSecurityHeaders :: _ => IO ()
setSecurityHeaders = do
    nonce <- CSP.generateNonce
    putContext (CSP.CSPNonce nonce)
    CSP.setCSP $ CSP.strictCSP nonce
```

## Benefits

1. **Type Safety**: Catch CSP configuration errors at compile time
2. **Maintainability**: Clear, structured CSP definitions
3. **Reusability**: Easy to create and share CSP policy templates
4. **Documentation**: Self-documenting code with clear types
5. **Security**: Good defaults that follow security best practices

## Advanced: All Available Directives

The `CSP` type supports all standard CSP directives:

```haskell
data CSP = CSP
    { defaultSrc :: Maybe CSPSourceList
    , scriptSrc :: Maybe CSPSourceList
    , styleSrc :: Maybe CSPSourceList
    , imgSrc :: Maybe CSPSourceList
    , connectSrc :: Maybe CSPSourceList
    , fontSrc :: Maybe CSPSourceList
    , objectSrc :: Maybe CSPSourceList
    , mediaSrc :: Maybe CSPSourceList
    , frameSrc :: Maybe CSPSourceList
    , frameAncestors :: Maybe CSPSourceList
    , baseUri :: Maybe CSPSourceList
    , formAction :: Maybe CSPSourceList
    , manifestSrc :: Maybe CSPSourceList
    , workerSrc :: Maybe CSPSourceList
    , childSrc :: Maybe CSPSourceList
    , reportUri :: Maybe Text
    , reportTo :: Maybe Text
    , upgradeInsecureRequests :: Bool
    , blockAllMixedContent :: Bool
    }
```

## Best Practices

1. **Always use nonces**: Prefer nonce-based CSP over `unsafe-inline`
2. **Use strict-dynamic**: For modern browsers, this simplifies script loading
3. **Start strict, then relax**: Begin with `strictCSP` and only add exceptions as needed
4. **Test thoroughly**: Verify your CSP works in all browsers you support
5. **Monitor violations**: Use `reportUri` or `reportTo` to track CSP violations

## Troubleshooting

### Inline scripts blocked

Make sure you're adding the nonce attribute to your script tags:

```haskell
let CSP.CSPNonce nonce = fromFrozenContext
[hsx|<script nonce={nonce}>...</script>|]
```

### External scripts not loading

Add the domain to your `scriptSrc`:

```haskell
csp { CSP.scriptSrc = Just [CSP.nonce nonce, CSP.host "https://cdn.example.com"] }
```

### Styles not loading

Ensure your stylesheet tags have the nonce or add the domain to `styleSrc`.

## References

- [MDN: Content Security Policy](https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP)
- [CSP Evaluator](https://csp-evaluator.withgoogle.com/)
- [Content Security Policy Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Content_Security_Policy_Cheat_Sheet.html)
