# Content Security Policy (CSP)

```toc
```

## Introduction

Content Security Policy (CSP) is a powerful security feature that helps protect your IHP application from cross-site scripting (XSS), clickjacking, and other code injection attacks. By specifying which sources of content are allowed to load and execute, you can significantly reduce the attack surface of your web application.

IHP includes the `wai-csp` package that provides a type-safe WAI middleware for managing CSP headers with good defaults and built-in nonce support.

## Why Use CSP?

Without CSP, if an attacker can inject malicious JavaScript into your application (through XSS vulnerabilities), that script will execute with full privileges. CSP prevents this by:

1. **Restricting script sources**: Only allowing scripts from trusted domains
2. **Blocking inline scripts**: Preventing injected `<script>` tags from executing
3. **Using nonces**: Allowing only scripts with a specific cryptographic nonce
4. **Preventing clickjacking**: Controlling which sites can embed your pages in frames

## Installation

Add `wai-csp` to your project's dependencies in `default.nix`:

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
            wai-csp  # Add this line
        ];
        otherDeps = p: with p; [
            # Native dependencies
        ];
        projectPath = ./.;
    };
in
    haskellEnv
```

## Quick Start

### Step 1: Add CSP Middleware

Add the CSP middleware in your `Config/Config.hs`:

```haskell
module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import qualified Network.Wai.Middleware.ContentSecurityPolicy as CSP

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")
    
    -- Add CSP middleware with nonce support
    option $ CustomMiddleware $ CSP.cspMiddlewareWithNonce CSP.strictCSP
```

This automatically:
- Generates a fresh cryptographic nonce for each request
- Sets a strict Content-Security-Policy header
- Stores the nonce in the request vault

### Step 2: Extract Nonce in Controller

In your controller's `beforeAction`, extract the nonce from the request vault and put it into the context so views can access it:

```haskell
module Web.Controller.Posts where

import Web.Controller.Prelude
import qualified Network.Wai.Middleware.ContentSecurityPolicy as CSP
import qualified Data.Vault.Lazy as Vault

-- Define a newtype for the nonce
newtype CSPNonce = CSPNonce Text

instance Controller PostsController where
    beforeAction = do
        -- Extract nonce from request vault and put in context
        case Vault.lookup CSP.cspNonceKey request.vault of
            Just (CSP.CSPNonce nonce) -> putContext (CSPNonce nonce)
            Nothing -> pure ()  -- No nonce if not using cspMiddlewareWithNonce

    action PostsAction = do
        posts <- query @Post |> fetch
        render IndexView { .. }
```

### Step 3: Use Nonce in Views

Now in your views, retrieve the nonce using `fromFrozenContext`:

```haskell
module Web.View.Posts.Index where

import Web.View.Prelude

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div>
            <h1>Posts</h1>
            {customScript}
        </div>
    |]
      where
        customScript = do
            let CSPNonce nonce = fromFrozenContext
            [hsx|
                <script nonce={nonce}>
                    console.log("This script is allowed by CSP");
                </script>
            |]
```

That's it! Your application now has CSP protection with nonces.

## Using Nonces in Views

Once you've set up the middleware and controller as shown above, you can use nonces in your views for inline scripts and styles.

### Inline Scripts

```haskell
scripts :: Html
scripts = do
    let CSPNonce nonce = fromFrozenContext
    [hsx|
        <script nonce={nonce}>
            console.log("Hello from CSP-protected script!");
        </script>
    |]
```

### Inline Styles

```haskell
styles :: Html
styles = do
    let CSPNonce nonce = fromFrozenContext
    [hsx|
        <style nonce={nonce}>
            .custom-class {
                color: blue;
            }
        </style>
    |]
```

### External Scripts with Nonces

Even external scripts can use nonces for additional security:

```haskell
externalScript :: Html
externalScript = do
    let CSPNonce nonce = fromFrozenContext
    [hsx|
        <script nonce={nonce} src="https://cdn.example.com/script.js"></script>
    |]
```

### Helper Function (Optional)

For convenience, you can add a helper function to `Web/View/Prelude.hs`:

```haskell
module Web.View.Prelude
( module Web.View.Prelude
, module IHP.ViewPrelude
, cspNonce
) where

import IHP.ViewPrelude

-- | Get the CSP nonce from the context
cspNonce :: (?context :: ControllerContext) => Text
cspNonce = 
    let CSPNonce nonce = fromFrozenContext
    in nonce
```

Then use it directly in your views:

```haskell
instance View IndexView where
    html IndexView { .. } = [hsx|
        <div>
            <script nonce={cspNonce}>
                console.log("Hello from CSP-protected script!");
            </script>
            
            <style nonce={cspNonce}>
                .custom-class {
                    color: blue;
                }
            </style>
        </div>
    |]
```

## CSP Policies

### Default CSP

The `defaultCSP` provides a safe baseline that you can customize:

```haskell
import qualified Network.Wai.Middleware.ContentSecurityPolicy as CSP

config :: ConfigBuilder
config = do
    option $ CustomMiddleware $ CSP.cspMiddleware CSP.defaultCSP
```

This sets:
- `default-src 'self'` - Only load resources from the same origin
- `object-src 'none'` - Block all plugins
- `base-uri 'none'` - Prevent base tag hijacking

### Strict CSP

The `strictCSP` policy provides stronger protection using nonces:

```haskell
import qualified Network.Wai.Middleware.ContentSecurityPolicy as CSP

config :: ConfigBuilder
config = do
    option $ CustomMiddleware $ CSP.cspMiddlewareWithNonce CSP.strictCSP
```

This sets:
- `default-src 'self'`
- `script-src 'nonce-xxx' 'strict-dynamic'` - Only nonce-tagged scripts
- `style-src 'nonce-xxx' 'self'` - Only nonce-tagged inline styles
- `img-src 'self' data:` - Images from same origin or data URIs
- `connect-src 'self'` - Only same-origin connections
- `object-src 'none'`
- `base-uri 'none'`
- And more secure defaults...

### Custom CSP

You can create custom CSP policies by modifying the default policies:

```haskell
import qualified Network.Wai.Middleware.ContentSecurityPolicy as CSP
import Data.Text (Text)

config :: ConfigBuilder
config = do
    option $ CustomMiddleware $ CSP.cspMiddlewareWithNonce myCustomCSP

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
        , CSP.host "https://fonts.googleapis.com"
        ]
    , CSP.imgSrc = Just 
        [ CSP.self
        , CSP.data'
        , CSP.host "https://images.example.com"
        ]
    , CSP.fontSrc = Just
        [ CSP.self
        , CSP.host "https://fonts.gstatic.com"
        ]
    , CSP.connectSrc = Just 
        [ CSP.self
        , CSP.host "wss://api.example.com"
        ]
    , CSP.upgradeInsecureRequests = True
    }
```

## CSP Sources

The `wai-csp` package provides helper functions for all CSP source values:

| Helper Function | CSP Source | Description |
|----------------|------------|-------------|
| `CSP.self` | `'self'` | Same origin as the document |
| `CSP.none` | `'none'` | No sources allowed |
| `CSP.unsafeInline` | `'unsafe-inline'` | Allow inline scripts/styles (not recommended) |
| `CSP.unsafeEval` | `'unsafe-eval'` | Allow eval() and similar (not recommended) |
| `CSP.strictDynamic` | `'strict-dynamic'` | Trust scripts loaded by trusted scripts |
| `CSP.nonce "abc"` | `'nonce-abc'` | Allow resources with this nonce |
| `CSP.data'` | `data:` | Allow data: URIs |
| `CSP.https` | `https:` | Allow any HTTPS source |
| `CSP.http` | `http:` | Allow any HTTP source |
| `CSP.blob` | `blob:` | Allow blob: URIs |
| `CSP.host "example.com"` | `example.com` | Allow specific host |
| `CSP.scheme "wss:"` | `wss:` | Allow specific scheme |

## Environment-Specific Policies

You often need different CSP policies for development and production:

```haskell
import qualified Network.Wai.Middleware.ContentSecurityPolicy as CSP
import IHP.Environment
import Data.Text (Text)

config :: ConfigBuilder
config = do
    env <- option Development
    
    let cspPolicy = case env of
            Development -> devCSP
            Production -> prodCSP
    
    option $ CustomMiddleware $ CSP.cspMiddlewareWithNonce cspPolicy

devCSP :: Text -> CSP.CSP
devCSP nonce = (CSP.strictCSP nonce)
    { CSP.connectSrc = Just 
        [ CSP.self
        , CSP.host "ws://localhost:8001"  -- IHP dev server
        , CSP.host "ws://localhost:*"
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

## Working with External Libraries

When using external JavaScript libraries or CDNs, you need to add them to your CSP:

### Loading Scripts from CDNs

```haskell
myCSP :: Text -> CSP.CSP
myCSP nonce = (CSP.strictCSP nonce)
    { CSP.scriptSrc = Just 
        [ CSP.nonce nonce
        , CSP.strictDynamic
        , CSP.host "https://cdn.jsdelivr.net"
        ]
    }
```

### Loading Fonts from Google Fonts

```haskell
myCSP :: Text -> CSP.CSP
myCSP nonce = (CSP.strictCSP nonce)
    { CSP.styleSrc = Just 
        [ CSP.nonce nonce
        , CSP.self
        , CSP.host "https://fonts.googleapis.com"
        ]
    , CSP.fontSrc = Just
        [ CSP.self
        , CSP.host "https://fonts.gstatic.com"
        ]
    }
```

## Advanced Features

### Reporting CSP Violations

You can configure CSP to report violations to a specific endpoint:

```haskell
myCSP :: Text -> CSP.CSP
myCSP nonce = (CSP.strictCSP nonce)
    { CSP.reportUri = Just "/csp-report"
    }
```

Then create a controller to handle the reports:

```haskell
module Web.Controller.CSPReport where

import Web.Controller.Prelude

data CSPReportController = CSPReportAction
    deriving (Eq, Show, Data)

instance Controller CSPReportController where
    action CSPReportAction = do
        body <- getRequestBody
        -- Log or store the CSP violation report
        putStrLn $ "CSP Violation: " <> show body
        
        renderPlain "OK"
```

### Upgrade Insecure Requests

Force all HTTP requests to upgrade to HTTPS:

```haskell
myCSP :: Text -> CSP.CSP
myCSP nonce = (CSP.strictCSP nonce)
    { CSP.upgradeInsecureRequests = True
    }
```

## Troubleshooting

### "Refused to execute inline script"

**Problem**: Your inline scripts are being blocked.

**Solution**: Make sure you:
1. Added the CSP middleware with `cspMiddlewareWithNonce`
2. Extract the nonce in your controller's `beforeAction` and put it in context
3. Use the nonce in your script tags

```haskell
-- In Config/Config.hs
option $ CustomMiddleware $ CSP.cspMiddlewareWithNonce CSP.strictCSP

-- In your controller
beforeAction = do
    case Vault.lookup CSP.cspNonceKey request.vault of
        Just (CSP.CSPNonce n) -> putContext (CSPNonce n)
        Nothing -> pure ()

-- In your view
let CSPNonce nonce = fromFrozenContext
[hsx|<script nonce={nonce}>...</script>|]
```

### External Resources Not Loading

**Problem**: Scripts, styles, or other resources from external domains are blocked.

**Solution**: Add the domains to the appropriate directive in your custom CSP policy:

```haskell
myCSP nonce = CSP.strictCSP nonce
    { CSP.scriptSrc = Just 
        [ CSP.nonce nonce
        , CSP.strictDynamic
        , CSP.host "https://cdn.example.com"
        ]
    }
```

### WebSocket Connections Failing

**Problem**: WebSocket connections are blocked.

**Solution**: Add WebSocket origins to `connectSrc`:

```haskell
myCSP nonce = CSP.strictCSP nonce
    { CSP.connectSrc = Just 
        [ CSP.self
        , CSP.host "wss://api.example.com"
        , CSP.host "ws://localhost:8001"  -- For development
        ]
    }
```

## Best Practices

1. **Start Strict**: Begin with `strictCSP` and relax only as needed
2. **Use Nonces**: Prefer nonces over `unsafe-inline`
3. **Controller Injection**: Always extract nonces in the controller and put them in context, don't access the vault directly in views
4. **Test Thoroughly**: Check your CSP in all browsers you support
5. **Monitor Violations**: Use `reportUri` to track CSP issues
6. **Environment-Specific**: Use different policies for dev vs. prod
7. **Avoid Wildcards**: Be specific about allowed sources
8. **Regular Updates**: Review and update your CSP as your app evolves

## Further Reading

- [MDN: Content Security Policy](https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP)
- [CSP Evaluator Tool](https://csp-evaluator.withgoogle.com/)
- [OWASP CSP Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Content_Security_Policy_Cheat_Sheet.html)
- [Can I Use: CSP Browser Support](https://caniuse.com/contentsecuritypolicy)
