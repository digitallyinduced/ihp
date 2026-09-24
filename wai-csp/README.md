# WAI Content Security Policy (CSP) Middleware

Type-safe Content Security Policy middleware for WAI applications.

## Overview

Content Security Policy (CSP) is a critical security feature that helps prevent cross-site scripting (XSS), clickjacking, and other code injection attacks. This package provides a type-safe WAI middleware for managing CSP headers.

## Features

- **Type-safe API**: Define CSP policies using Haskell types instead of error-prone strings
- **WAI Middleware**: Framework-agnostic, works with any WAI application
- **Nonce support**: Built-in cryptographically secure nonce generation
- **Good defaults**: Secure default policies out of the box
- **Flexible**: Easy to customize policies for your specific needs

## Installation

Add `wai-csp` to your project's dependencies.

For IHP projects, add to your `default.nix`:

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

## Usage

### Basic Usage (Static CSP)

Apply CSP middleware with a static policy:

```haskell
import Network.Wai.Middleware.ContentSecurityPolicy

main :: IO ()
main = do
    let policy = defaultCSP
            { scriptSrc = Just [self, host "https://cdn.example.com"]
            , styleSrc = Just [self]
            }
    
    run 8080 $ cspMiddleware policy $ myApp
```

### With Nonces (Recommended)

For better security, use nonce-based CSP:

```haskell
import Network.Wai.Middleware.ContentSecurityPolicy

main :: IO ()
main = do
    run 8080 $ cspMiddlewareWithNonce strictCSP $ myApp
```

The middleware automatically:
1. Generates a fresh nonce for each request
2. Adds it to the CSP header
3. Stores it in the request vault for use in your views

### IHP Integration

In your IHP `Config/Config.hs`:

```haskell
import qualified Network.Wai.Middleware.ContentSecurityPolicy as CSP
import IHP.FrameworkConfig

config :: ConfigBuilder
config = do
    option $ CustomMiddleware $ CSP.cspMiddlewareWithNonce CSP.strictCSP
```

Extract the nonce in your controller's `beforeAction` and put it in context:

```haskell
import qualified Network.Wai.Middleware.ContentSecurityPolicy as CSP
import qualified Data.Vault.Lazy as Vault

-- Define a newtype for the nonce
newtype CSPNonce = CSPNonce Text

instance Controller MyController where
    beforeAction = do
        -- Extract nonce from request vault and put in context
        case Vault.lookup CSP.cspNonceKey request.vault of
            Just (CSP.CSPNonce nonce) -> putContext (CSPNonce nonce)
            Nothing -> pure ()
```

Then in your views, use `fromFrozenContext` to retrieve it:

```haskell
instance View MyView where
    html MyView { .. } = [hsx|
        <div>
            {scripts}
        </div>
    |]
      where
        scripts = 
            let CSPNonce nonce = fromFrozenContext
            in [hsx|
                <script nonce={nonce}>
                    console.log("This script is allowed by CSP");
                </script>
            |]
```

For more details on IHP integration, see the [IHP CSP Guide](https://ihp.digitallyinduced.com/Guide/csp.html).

## CSP Policies

### Default CSP

The `defaultCSP` provides a safe baseline:

```haskell
CSP.cspMiddleware CSP.defaultCSP app
```

This sets:
- `default-src 'self'` - Only load resources from the same origin
- `object-src 'none'` - Block all plugins
- `base-uri 'none'` - Prevent base tag hijacking

### Strict CSP

The `strictCSP` policy provides stronger protection using nonces:

```haskell
CSP.cspMiddlewareWithNonce CSP.strictCSP app
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

Create custom CSP policies by modifying the defaults:

```haskell
import qualified Network.Wai.Middleware.ContentSecurityPolicy as CSP

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

main = run 8080 $ CSP.cspMiddlewareWithNonce myCustomCSP $ myApp
```

## CSP Sources

The package provides helper functions for all CSP source values:

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

## Retrieving the Nonce

When using `cspMiddlewareWithNonce`, the nonce is stored in the request vault:

```haskell
import qualified Network.Wai.Middleware.ContentSecurityPolicy as CSP
import qualified Data.Vault.Lazy as Vault

getNonce :: Request -> Maybe CSP.CSPNonce
getNonce req = Vault.lookup CSP.cspNonceKey req.vault
```

## Environment-Specific Policies

You can apply different policies based on environment:

```haskell
import qualified Network.Wai.Middleware.ContentSecurityPolicy as CSP
import System.Environment (lookupEnv)

main :: IO ()
main = do
    env <- lookupEnv "APP_ENV"
    let cspPolicy = case env of
            Just "development" -> devCSP
            _ -> prodCSP
    
    run 8080 $ CSP.cspMiddlewareWithNonce cspPolicy $ myApp

devCSP :: Text -> CSP.CSP
devCSP nonce = (CSP.strictCSP nonce)
    { CSP.connectSrc = Just 
        [ CSP.self
        , CSP.host "ws://localhost:*"  -- Local dev server
        ]
    }

prodCSP :: Text -> CSP.CSP
prodCSP nonce = (CSP.strictCSP nonce)
    { CSP.connectSrc = Just 
        [ CSP.self
        , CSP.host "wss://myapp.com"
        ]
    , CSP.upgradeInsecureRequests = True
    }
```

## Advanced Features

### Reporting CSP Violations

Configure CSP to report violations:

```haskell
myCSP :: Text -> CSP.CSP
myCSP nonce = (CSP.strictCSP nonce)
    { CSP.reportUri = Just "https://myapp.com/csp-report"
    }
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

### Inline Scripts Blocked

Make sure you're:
1. Using `cspMiddlewareWithNonce` (not `cspMiddleware`)
2. Retrieving the nonce from the request vault
3. Adding the nonce attribute to your script tags

### External Resources Not Loading

Add the domains to the appropriate directive:

```haskell
csp { CSP.scriptSrc = Just 
        [ CSP.nonce nonce
        , CSP.host "https://cdn.example.com"
        ]
    }
```

### WebSocket Connections Failing

Add WebSocket origins to `connectSrc`:

```haskell
csp { CSP.connectSrc = Just 
        [ CSP.self
        , CSP.host "wss://api.example.com"
        ]
    }
```

## Best Practices

1. **Use Nonces**: Prefer `cspMiddlewareWithNonce` with nonce-based policies
2. **Start Strict**: Begin with `strictCSP` and relax only as needed
3. **Test Thoroughly**: Check your CSP in all browsers you support
4. **Monitor Violations**: Use `reportUri` to track CSP issues
5. **Environment-Specific**: Use different policies for dev vs. prod
6. **Avoid Wildcards**: Be specific about allowed sources

## References

- [MDN: Content Security Policy](https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP)
- [CSP Evaluator Tool](https://csp-evaluator.withgoogle.com/)
- [OWASP CSP Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Content_Security_Policy_Cheat_Sheet.html)
