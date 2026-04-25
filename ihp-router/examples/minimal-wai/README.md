# minimal-wai

A minimal WAI app demonstrating `ihp-router` as a standalone library — **no IHP dependency**.

## Build & run

From the repository root, with the IHP nix dev shell:

```sh
cd ihp-router/examples/minimal-wai
cabal run minimal-wai
```

You should see:

```
Listening on http://localhost:3000
```

## Try it

```sh
$ curl http://localhost:3000/
all posts

$ curl http://localhost:3000/posts/42
post #42

$ curl http://localhost:3000/posts/42/comments
comments for #42

$ curl http://localhost:3000/files/some/deeply/nested/path
file at: some/deeply/nested/path

$ curl 'http://localhost:3000/search?q=haskell'
searching for: haskell

$ curl -X POST http://localhost:3000/posts
HTTP/1.1 405 Method Not Allowed
Allow: GET,HEAD

$ curl http://localhost:3000/nope
Not Found
```

## What it shows

`Main.hs` is the whole app — ~50 lines of Haskell:

- A controller ADT (`MyRoutes`) with five constructors covering path captures, splat captures, and query parameters.
- A `[routes|…|]` block declaring URLs, methods, and the `?q` query param.
- A plain `dispatch :: MyRoutes -> Application` function.
- `routeTrieMiddleware (myRoutesTrie dispatch) notFound` plugged into Warp.

The `myRoutesTrie` binding is emitted by the splice — you don't write it.

## Why this exists in CI

The dedicated CI job builds this example from a fresh checkout with only `base`, `bytestring`, `http-types`, `text`, `wai`, `warp`, `ihp-router` in scope. Any future change that introduces an IHP import into the `ihp-router` package fails this build, locking in the standalone-no-IHP guarantee.
