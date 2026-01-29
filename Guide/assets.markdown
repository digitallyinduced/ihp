# Static Assets

```toc

```

## Cache Busting

Sometimes problems are caused when your users still have an old version of your JS or CSS files inside their browser's cache.


To avoid this problem, web applications typically append a hash to the url of your JS or CSS file. For example like this:

```html
<script src="/app.js?v=19319eb"></script>
```

IHP provides an [`assetPath`](https://ihp.digitallyinduced.com/api-docs/IHP-Assets-ViewFunctions.html#v:assetPath) view helper to automatically add these hashes:

```haskell
[hsx|
    <script src={assetPath "/app.js"}/>
    <link rel="stylesheet" href={assetPath "/app.css"}/>
|]
```

This HSX code will produce a HTML like this:

```html
<script src="/static/app.js?v=19319eb"/>
<link rel="stylesheet" href="/static/app.css?v=19319eb"/>
```

### Static Route Shortcut

Requests to `/static/*` are served directly by the static file server, bypassing the full middleware stack (session, CORS, auto-refresh, etc.). Since static files only need to be read from disk, this avoids unnecessary overhead.

The `assetPath` helper automatically generates `/static/`-prefixed URLs, so all assets use this fast path by default. Requests without the prefix (e.g. `/app.css`) still work through the normal middleware pipeline as a fallback.

When a CDN base URL is configured via `IHP_ASSET_BASEURL`, the `/static/` prefix is omitted since the CDN serves files directly.

### Asset Version

IHP will set the asset version from the `IHP_ASSET_VERSION` env variable.

Set the `IHP_ASSET_VERSION` env variable to e.g. your git commit hash when running in production.

In development you don't need to specify this environment variable. It will fall back to `dev` as the default value for the version.
