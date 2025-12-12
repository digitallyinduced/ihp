# Content Security Policy (CSP)

```toc
```

## Introduction

Content Security Policy (CSP) is a powerful security feature that helps protect your IHP application from cross-site scripting (XSS), clickjacking, and other code injection attacks. By specifying which sources of content are allowed to load and execute, you can significantly reduce the attack surface of your web application.

IHP provides the `ihp-csp` package that offers a type-safe API for managing CSP headers with good defaults and built-in nonce support.

## Why Use CSP?

Without CSP, if an attacker can inject malicious JavaScript into your application (through XSS vulnerabilities), that script will execute with full privileges. CSP prevents this by:

1. **Restricting script sources**: Only allowing scripts from trusted domains
2. **Blocking inline scripts**: Preventing injected `<script>` tags from executing
3. **Using nonces**: Allowing only scripts with a specific cryptographic nonce
4. **Preventing clickjacking**: Controlling which sites can embed your pages in frames

## Installation

Add `ihp-csp` to your project's dependencies in `default.nix`:

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
            # Native dependencies
        ];
        projectPath = ./.;
    };
in
    haskellEnv
```

## Quick Start

The easiest way to add CSP to your IHP application is to use the strict CSP policy with nonces in your controller's `beforeAction`:

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

Then, in your view, add the nonce to any inline scripts or styles:

```haskell
module Web.View.Posts.Index where

import Web.View.Prelude
import qualified IHP.CSP as CSP

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div>
            <h1>Posts</h1>
            {customScript}
        </div>
    |]
      where
        customScript = do
            let CSP.CSPNonce nonce = fromFrozenContext
            [hsx|
                <script nonce={nonce}>
                    console.log("This script is allowed by CSP");
                </script>
            |]
```

## CSP Policies

### Default CSP

The `defaultCSP` provides a safe baseline that you can customize:

```haskell
CSP.setCSP CSP.defaultCSP
```

This sets:
- `default-src 'self'` - Only load resources from the same origin
- `object-src 'none'` - Block all plugins
- `base-uri 'none'` - Prevent base tag hijacking

### Strict CSP

The `strictCSP` policy provides stronger protection using nonces:

```haskell
nonce <- CSP.generateNonce
putContext (CSP.CSPNonce nonce)
CSP.setCSP $ CSP.strictCSP nonce
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

The `ihp-csp` package provides helper functions for all CSP source values:

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
import qualified IHP.CSP as CSP
import qualified IHP.EnvVar as EnvVar

instance Controller ApplicationController where
    beforeAction = do
        nonce <- CSP.generateNonce
        putContext (CSP.CSPNonce nonce)
        
        env <- EnvVar.envOrDefault "development" "IHP_ENV"
        
        let csp = case env of
                "development" -> devCSP nonce
                _ -> prodCSP nonce
        
        CSP.setCSP csp

devCSP :: Text -> CSP.CSP
devCSP nonce = (CSP.strictCSP nonce)
    { CSP.connectSrc = Just 
        [ CSP.self
        , CSP.host "ws://localhost:*"  -- IHP dev server
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

When you have inline scripts or styles, you need to add the nonce attribute:

### Inline Scripts

```haskell
scripts :: Html
scripts = do
    let CSP.CSPNonce nonce = fromFrozenContext
    [hsx|
        <script nonce={nonce}>
            // Your inline JavaScript
            console.log("Hello from CSP-protected script!");
        </script>
    |]
```

### Inline Styles

```haskell
styles :: Html
styles = do
    let CSP.CSPNonce nonce = fromFrozenContext
    [hsx|
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
externalScript = do
    let CSP.CSPNonce nonce = fromFrozenContext
    [hsx|
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
1. Generating a nonce and storing it in the context
2. Setting the CSP with the nonce
3. Adding the nonce attribute to your script tags

```haskell
-- In controller
nonce <- CSP.generateNonce
putContext (CSP.CSPNonce nonce)
CSP.setCSP $ CSP.strictCSP nonce

-- In view
let CSP.CSPNonce nonce = fromFrozenContext
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
