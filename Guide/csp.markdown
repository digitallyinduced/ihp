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

The easiest way to add CSP to your IHP application is to add the middleware in your `Config/Config.hs`:

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
- Generates a fresh nonce for each request
- Sets a strict Content-Security-Policy header
- Stores the nonce in the request vault for use in views

## Using Nonces in Views

To use the nonce in your views for inline scripts or styles, retrieve it from the request:

```haskell
module Web.View.Posts.Index where

import Web.View.Prelude
import qualified Network.Wai.Middleware.ContentSecurityPolicy as CSP
import qualified Data.Vault.Lazy as Vault

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div>
            <h1>Posts</h1>
            {customScript}
        </div>
    |]
      where
        customScript = 
            let Just (CSP.CSPNonce nonceValue) = Vault.lookup CSP.cspNonceKey request.vault
            in [hsx|
                <script nonce={nonceValue}>
                    console.log("This script is allowed by CSP");
                </script>
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

## Using Nonces in Views

When you have inline scripts or styles, you need to add the nonce attribute. The nonce is automatically stored in the request vault by the middleware.

### Helper Function (Recommended)

Create a helper function in `Web/View/Prelude.hs`:

```haskell
module Web.View.Prelude
( module Web.View.Prelude
, module IHP.ViewPrelude
) where

import IHP.ViewPrelude
import qualified Network.Wai.Middleware.ContentSecurityPolicy as CSP
import qualified Data.Vault.Lazy as Vault

-- | Get the CSP nonce from the current request
cspNonce :: (?context :: ControllerContext) => Text
cspNonce = 
    let Just (CSP.CSPNonce nonce) = Vault.lookup CSP.cspNonceKey request.vault
    in nonce
```

Then use it in your views:

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

### Inline Scripts

```haskell
scripts :: Html
scripts = 
    let Just (CSP.CSPNonce nonce) = Vault.lookup CSP.cspNonceKey request.vault
    in [hsx|
        <script nonce={nonce}>
            // Your inline JavaScript
            console.log("Hello from CSP-protected script!");
        </script>
    |]
```

### Inline Styles

```haskell
styles :: Html
styles = 
    let Just (CSP.CSPNonce nonce) = Vault.lookup CSP.cspNonceKey request.vault
    in [hsx|
        <style nonce={nonce}>
            .custom-class {
                color: blue;
            }
        </style>
    |]
```

### External Scripts with Nonces

Even external scripts can use nonces:

```haskell
externalScript :: Html
externalScript = 
    let Just (CSP.CSPNonce nonce) = Vault.lookup CSP.cspNonceKey request.vault
    in [hsx|
        <script nonce={nonce} src="https://cdn.example.com/script.js"></script>
    |]
```

## Advanced Features

### Reporting CSP Violations

You can configure CSP to report violations:

```haskell
myCSP :: Text -> CSP.CSP
myCSP nonce = (CSP.strictCSP nonce)
    { CSP.reportUri = Just "https://myapp.com/csp-report"
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
        logInfo ("CSP Violation: " <> show body)
        
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

**Solution**: Make sure you're:
1. Using `cspMiddlewareWithNonce` in your config
2. Retrieving the nonce from the request vault
3. Adding the nonce attribute to your script tags

```haskell
-- In Config/Config.hs
option $ CustomMiddleware $ CSP.cspMiddlewareWithNonce CSP.strictCSP

-- In your view
let Just (CSP.CSPNonce nonce) = Vault.lookup CSP.cspNonceKey request.vault
[hsx|<script nonce={nonce}>...</script>|]
```

### External Resources Not Loading

**Problem**: Scripts, styles, or other resources from external domains are blocked.

**Solution**: Add the domains to the appropriate directive:

```haskell
csp { CSP.scriptSrc = Just 
        [ CSP.nonce nonce
        , CSP.host "https://cdn.example.com"
        ]
    }
```

### WebSocket Connections Failing

**Problem**: WebSocket connections are blocked.

**Solution**: Add WebSocket origins to `connectSrc`:

```haskell
csp { CSP.connectSrc = Just 
        [ CSP.self
        , CSP.host "wss://api.example.com"
        , CSP.host "ws://localhost:8001"  -- For development
        ]
    }
```

## Best Practices

1. **Start Strict**: Begin with `strictCSP` and relax only as needed
2. **Use Nonces**: Prefer nonces over `unsafe-inline`
3. **Test Thoroughly**: Check your CSP in all browsers you support
4. **Monitor Violations**: Use `reportUri` to track CSP issues
5. **Environment-Specific**: Use different policies for dev vs. prod
6. **Avoid Wildcards**: Be specific about allowed sources
7. **Regular Updates**: Review and update your CSP as your app evolves

## Further Reading

- [MDN: Content Security Policy](https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP)
- [CSP Evaluator Tool](https://csp-evaluator.withgoogle.com/)
- [OWASP CSP Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Content_Security_Policy_Cheat_Sheet.html)
- [Can I Use: CSP Browser Support](https://caniuse.com/contentsecuritypolicy)
