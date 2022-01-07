# Static Assets

```toc

```

## Cache Busting

Sometimes problems are caused when your users still have an old version of your JS or CSS files inside their browsers cache.


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
<script src="/app.js?v=19319eb"/>
<link rel="stylesheet" href="/app.css?v=19319eb"/>
```

### Asset Version

IHP will set the asset version from the `IHP_ASSET_VERSION` env variable.

Set the `IHP_ASSET_VERSION` env variable to e.g. your git commit hash when running in production.

In development you don't need to specify this environment variable. It will fall back to `dev` as the default value for the version.

If you run on IHP Cloud, it works out of the box and you don't need to manually specify the env variable.
