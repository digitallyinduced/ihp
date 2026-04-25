# ihp-router

Trie-based routing with a Yesod-style DSL for plain WAI applications.

Originally extracted from [IHP](https://ihp.digitallyinduced.com/). The package has **zero IHP dependencies** — `cabal install ihp-router` and use it standalone in any WAI app.

## Quick start

Add `ihp-router` to your cabal file:

```cabal
build-depends:
    base
  , wai
  , warp
  , ihp-router
```

Declare your routes via the `[routes|…|]` quasi-quoter and dispatch incoming requests through `routeTrieMiddleware`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import IHP.Router.WAI
import qualified Data.Text as Text
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)

data MyRoutes
    = ListPosts
    | ShowPost { postId :: Int }
    | DownloadFile { path :: Text.Text }
    deriving (Eq, Show)

[routes|MyRoutes
GET /                         ListPosts
GET /posts/{postId}           ShowPost
GET /files/{+path}            DownloadFile
|]

dispatch :: MyRoutes -> Application
dispatch ListPosts _req respond =
    respond (responseLBS status200 [] "all posts\n")
dispatch (ShowPost { postId }) _req respond =
    respond (responseLBS status200 [] (cs ("post #" <> show postId)))
dispatch (DownloadFile { path }) _req respond =
    respond (responseLBS status200 [] (cs ("file: " <> Text.unpack path)))

main :: IO ()
main = run 3000 (routeTrieMiddleware (myRoutesTrie dispatch) notFound)
  where
    notFound _ respond = respond (responseLBS status404 [] "Not Found")
```

The splice emits two things per controller type:

1. `instance HasPath MyRoutes` — `pathTo` for URL generation.
2. `myRoutesTrie :: (MyRoutes -> Application) -> RouteTrie` — a `RouteTrie` factory parameterised on a dispatch function.

Plug `myRoutesTrie myDispatch` into `routeTrieMiddleware` and you have a working WAI router.

## DSL syntax

### Path captures

Bind URL segments to record fields with `{name}`:

```
GET /posts/{postId}    ShowPost
```

The capture's Haskell type comes from the matching record field — the `UrlCapture` typeclass handles parsing and rendering. Built-in instances cover `Text`, `Int`, `Integer`, `UUID`, `Bool`, `Day`, and `Segment` (a non-empty `Text` newtype).

For a "rest of path" splat, use `{+name}`:

```
GET /files/{+path}     DownloadFile
```

### Query parameters

After the path, declare query params with `?name1&name2`:

```
GET /search?q&page&tags    SearchAction
```

Field type drives the URL shape:

| Field type | Behaviour |
|---|---|
| `a` | required — missing/unparseable returns 404 |
| `Maybe a` | optional — absent decodes to `Nothing`, `pathTo` omits when `Nothing` |
| `[a]` | collected from repeated `?key=v` pairs; one entry per element |

Every record field on the constructor must be covered by either a path capture or a query param — leftover fields fail at compile time with a pointer to the exact fields not yet bound.

### Field renaming

When the URL key differs from the record field, use `{ field = #captureName }` after the action:

```
GET /ShowPost?id     ShowPostAction { postId = #id }
```

### Methods

Multiple methods on one line via `|`:

```
GET|POST /api/widgets    WidgetsEndpointAction
```

`ANY` expands to every standard method:

```
ANY /api/echo            EchoAction
```

`GET` automatically also accepts `HEAD`.

### Header forms

The line above the first route is the header. Three shapes:

1. **Uppercase identifier** — single-controller. Splice emits one `HasPath` + one `<ctrlLower>Trie` binding for that type.

   ```
   [routes|MyController
   GET /foo    SomeAction
   |]
   ```

2. **Lowercase identifier** — multi-controller, with a binding name. Splice emits per-type `HasPath` + per-type `<ctrlLower>Trie`. Useful when one block describes routes for several controller ADTs.

   ```
   [routes|appRoutes
   GET /posts          ListPosts
   GET /users          ListUsers
   |]
   ```

3. **Omitted** — multi-controller without a binding name. Just instances + per-controller `<ctrlLower>Trie` values.

## Public API

Everything you need is re-exported from `IHP.Router.WAI`:

- `routes` — the quasi-quoter
- `RouteTrie`, `routeTrieMiddleware` — runtime dispatch
- `UrlCapture`, `Segment` — types and class methods used by emitted code
- `HasPath` — URL-generation typeclass

For finer-grained control, the underlying modules are:

| Module | Contents |
|---|---|
| `IHP.Router.Trie` | `RouteTrie`, `lookupTrie`, `mergeTrie` |
| `IHP.Router.Middleware` | `routeTrieMiddleware` |
| `IHP.Router.Capture` | `UrlCapture` typeclass + base instances |
| `IHP.Router.UrlGenerator` | `HasPath` typeclass |
| `IHP.Router.DSL.AST` / `Parser` / `Runtime` / `TH` | DSL internals |

## Performance

- O(path-depth) trie lookup, no backtracking
- Per-request capture parsing happens once via `parseCapture` (no `Dynamic` round-trip)
- The trie is built once at startup as a CAF; merging is structural
- `mkHandler` / `mkHandlerQ` runtime helpers keep the per-route Core small (NOINLINE shared shell)

In IHP's internal benchmark on a ~75-route forum app, dispatch is 10-50× faster than its previous `Data`-reflection-based router.

## License

MIT — see [LICENSE](LICENSE).
