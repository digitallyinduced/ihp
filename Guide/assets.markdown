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

## Directory Structure

All static assets in an IHP project live in the `static/` directory at the root of your project. This is where you place your CSS files, JavaScript files, images, fonts, and any other files that should be served directly to the browser.

A typical `static/` directory looks like this:

```
static/
    app.css          -- Your application styles
    app.js           -- Your application JavaScript
    logo.png         -- Images
    vendor/          -- Third-party libraries (Bootstrap, jQuery, etc.)
        bootstrap-5.3.8/
            bootstrap.min.css
            bootstrap.min.js
        jquery-4.0.0.slim.min.js
        flatpickr.min.css
        flatpickr.js
```

### How Static Files Are Served

IHP serves files from the `static/` directory using WAI's static file server. The serving behavior differs between development and production:

- **Development:** Browser caching is disabled (`Cache-Control: max-age=0`) so that you always see the latest version of your files without needing to clear the cache.
- **Production:** Files are cached aggressively (`Cache-Control: max-age=forever`). The `assetPath` helper appends a version query parameter (e.g. `?v=af5f389`) to bust the cache when you deploy new code.

When `assetPath` is used, the generated URL starts with `/static/` (e.g. `/static/app.css?v=dev`). Requests to `/static/*` are routed directly to the file server, bypassing the session, CORS, and other middleware. This makes static file delivery faster. If you reference a file without the `/static/` prefix (e.g. `/app.css`), it still works but goes through the full middleware stack.

IHP also ships its own built-in static files (such as `helpers.js`, `ihp-auto-refresh.js`, and the vendor libraries). If a file is not found in your project's `static/` directory, IHP falls back to its own static directory.

### URL Mapping

Files in the `static/` directory are served at the root path. For example:

| File on disk         | URL in the browser                     |
|----------------------|----------------------------------------|
| `static/app.css`     | `/app.css` or `/static/app.css`        |
| `static/app.js`      | `/app.js` or `/static/app.js`          |
| `static/logo.png`    | `/logo.png` or `/static/logo.png`      |
| `static/vendor/bootstrap-5.3.8/bootstrap.min.css` | `/vendor/bootstrap-5.3.8/bootstrap.min.css` or `/static/vendor/bootstrap-5.3.8/bootstrap.min.css` |

When using `assetPath`, the `/static/` prefix is added automatically, so you write `assetPath "/app.css"` and get `/static/app.css?v=...` in the output.

## Adding Custom CSS

### Where to Put Your CSS

Place your CSS files directly in the `static/` directory. The convention is to have a main `static/app.css` file that serves as the entry point for your styles.

For larger projects, you can split your CSS into multiple files and use CSS `@import` statements in `app.css` to pull them together:

```css
/* static/app.css */
@import "/layout.css";
@import "/widget.css";
@import "/form.css";
@import "/button.css";
```

Each imported file also lives in `static/`:

```
static/
    app.css
    layout.css
    widget.css
    form.css
    button.css
```

### Linking CSS in Your Layout

CSS files are included in your application through the `stylesheets` function in `Web/View/Layout.hs`. Use the `assetPath` helper to get cache-busting URLs:

```haskell
stylesheets :: Html
stylesheets = [hsx|
        <link rel="stylesheet" href={assetPath "/vendor/bootstrap-5.3.8/bootstrap.min.css"}/>
        <link rel="stylesheet" href={assetPath "/vendor/flatpickr.min.css"}/>
        <link rel="stylesheet" href={assetPath "/app.css"}/>
    |]
```

To add a new CSS file, simply add another `<link>` tag. For example, to add a `dashboard.css`:

```haskell
stylesheets :: Html
stylesheets = [hsx|
        <link rel="stylesheet" href={assetPath "/vendor/bootstrap-5.3.8/bootstrap.min.css"}/>
        <link rel="stylesheet" href={assetPath "/vendor/flatpickr.min.css"}/>
        <link rel="stylesheet" href={assetPath "/app.css"}/>
        <link rel="stylesheet" href={assetPath "/dashboard.css"}/>
    |]
```

### Page-Specific CSS

For CSS rules that only apply to specific pages, use the [`currentViewId`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewSupport.html#v:currentViewId) helper to scope your styles. Wrap your view content in a div with the view ID:

```haskell
module Web.View.Projects.Show where

render = [hsx|
    <div id={currentViewId}>
        <h1>Project Details</h1>
    </div>
|]
```

This renders as `<div id="projects-show">`. Then in your CSS file (e.g. `static/projects.css`):

```css
#projects-show h1 {
    color: blue;
}
```

This pattern keeps your global styles clean and avoids unintended style conflicts between views.

### Third-Party CSS

CSS files from external libraries should be placed in `static/vendor/`. For example, if you download a CSS library, put it in `static/vendor/my-library.min.css` and reference it in your layout:

```haskell
<link rel="stylesheet" href={assetPath "/vendor/my-library.min.css"}/>
```

## Adding Custom JavaScript

### Where to Put JS Files

Place JavaScript files in the `static/` directory. The default project layout includes a `static/app.js` file for your application-level JavaScript.

### Including JavaScript in Your Layout

JavaScript files are loaded through the `scripts` function in `Web/View/Layout.hs`. Again, use `assetPath` for cache busting:

```haskell
scripts :: Html
scripts = [hsx|
        {when isDevelopment devScripts}
        <script src={assetPath "/vendor/jquery-4.0.0.slim.min.js"}></script>
        <script src={assetPath "/vendor/timeago.js"}></script>
        <script src={assetPath "/vendor/popper-2.11.6.min.js"}></script>
        <script src={assetPath "/vendor/bootstrap-5.3.8/bootstrap.min.js"}></script>
        <script src={assetPath "/vendor/flatpickr.js"}></script>
        <script src={assetPath "/vendor/morphdom-umd.min.js"}></script>
        <script src={assetPath "/vendor/turbolinks.js"}></script>
        <script src={assetPath "/vendor/turbolinksInstantClick.js"}></script>
        <script src={assetPath "/vendor/turbolinksMorphdom.js"}></script>
        <script src={assetPath "/helpers.js"}></script>
        <script src={assetPath "/ihp-auto-refresh.js"}></script>
        <script src={assetPath "/app.js"}></script>
    |]
```

Add your own script files by appending more `<script>` tags at the end, before the closing `|]`.

### Global vs. Page-Specific JavaScript

**Global JavaScript** goes in `static/app.js` and is loaded on every page. Use this for initialization code, navbar effects, or any behavior that should be available site-wide.

**Page-specific JavaScript** should be placed in a separate file (e.g. `static/users.js` for the Users controller) and included only in the views that need it:

```haskell
module Web.View.Users.Index where

html UsersIndexView { .. } = [hsx|
    <h1>Users</h1>
    ...
    <script src={assetPath "/users.js"}></script>
|]
```

### Working with Turbolinks

IHP uses [Turbolinks](https://github.com/turbolinks/turbolinks) in production for faster page transitions. Turbolinks replaces the page body without a full reload, which means that `DOMContentLoaded` only fires once on the initial page load, not on subsequent navigations.

If you need code to run every time a page is displayed, use the `ihp:load` event instead of `DOMContentLoaded`:

```javascript
document.addEventListener('ihp:load', () => {
    // This runs on initial load AND on every Turbolinks page transition
    initializeMyWidget();
});
```

IHP provides two custom events for this purpose:

- `ihp:load` -- fires on `DOMContentLoaded` and on every `turbolinks:load`
- `ihp:unload` -- fires on `beforeunload` and before morphdom patches the page

If you use jQuery, the common pattern is:

```javascript
$(document).on('ready turbolinks:load', function () {
    // Your initialization code here
});
```

Avoid attaching event listeners directly on `DOMContentLoaded` for code that should work across page navigations -- it will only execute once and not re-run when Turbolinks replaces the page content.

### Third-Party JavaScript

JavaScript files from external libraries should be placed in `static/vendor/`. For simple libraries, you can download the JS bundle directly and commit it to your repository instead of using a package manager:

```
static/vendor/
    chart.js
    sortable.min.js
```

For more complex setups with many JavaScript dependencies, consider using NPM. See the [Using NPM Packages](#using-npm-packages) section below.

## Images and Fonts

### Images

Place images in the `static/` directory. You can organize them in a subdirectory if you prefer:

```
static/
    logo.svg
    images/
        hero.jpg
        avatar-placeholder.png
```

Reference images in your HSX templates using `assetPath`:

```haskell
[hsx|
    <img src={assetPath "/logo.svg"} alt="Logo"/>
    <img src={assetPath "/images/hero.jpg"} alt="Hero image"/>
|]
```

We recommend using SVG images where possible, as they scale to any resolution and are typically smaller than raster images.

### Referencing Images in CSS

In your CSS files, reference images with relative paths from the web root:

```css
.hero-section {
    background-image: url('/images/hero.jpg');
}
```

### Fonts

Place custom font files in `static/` (for example, in a `static/fonts/` subdirectory):

```
static/
    fonts/
        my-font.woff2
        my-font.woff
```

Load them using a `@font-face` declaration in your CSS:

```css
@font-face {
    font-family: 'MyFont';
    src: url('/fonts/my-font.woff2') format('woff2'),
         url('/fonts/my-font.woff') format('woff');
    font-weight: normal;
    font-style: normal;
    font-display: swap;
}

body {
    font-family: 'MyFont', sans-serif;
}
```

Using `font-display: swap` ensures that text remains visible while the font is loading, which improves perceived performance.

For web fonts hosted externally (such as Google Fonts), you can link them directly in your layout's `stylesheets` function:

```haskell
stylesheets :: Html
stylesheets = [hsx|
        <link rel="preconnect" href="https://fonts.googleapis.com"/>
        <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin="anonymous"/>
        <link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap"/>
        <link rel="stylesheet" href={assetPath "/vendor/bootstrap-5.3.8/bootstrap.min.css"}/>
        <link rel="stylesheet" href={assetPath "/app.css"}/>
    |]
```

## Using NPM Packages

For projects that need JavaScript libraries from the NPM ecosystem, you can add NodeJS to your project and use NPM to manage dependencies.

### Setup

First, add `nodejs` to your project's dependencies in `default.nix`:

```nix
        otherDeps = p: with p; [
            nodejs
        ];
```

Then rebuild your environment with `devenv up` and initialize NPM:

```bash
npm init
```

This creates a `package.json` file. Add both `package.json` and `package-lock.json` to your git repository.

### Installing Packages

Install packages using `npm add`:

```bash
npm add chart.js
```

The installed files will be in `node_modules/`. You can then either:

1. **Copy the built file** to `static/vendor/` and reference it like any other static file.
2. **Use a bundler** (such as esbuild) to bundle the npm packages into a single file in `static/`.

For most IHP projects, where JavaScript is used sparingly, option 1 is simpler. For projects with significant JavaScript, option 2 is more practical.

For a more detailed walkthrough, see the [NPM Guide](npm.html).

## Asset Fingerprinting and Caching

IHP's caching strategy is built around the `assetPath` helper and the `IHP_ASSET_VERSION` environment variable.

### How It Works

1. In **development**, `assetPath "/app.js"` produces `/static/app.js?v=dev`. Browser caching is disabled (max-age=0), so you always see fresh files.

2. In **production**, `assetPath "/app.js"` produces `/static/app.js?v=af5f389ef7a64a04c9fa275111e4739c0d4a78d0` (where the version string comes from `IHP_ASSET_VERSION`). Files are cached aggressively with `Cache-Control: max-age=forever`, `Last-Modified`, and `ETag` headers. When you deploy new code with a new asset version, the query parameter changes, forcing browsers to fetch the updated file.

3. When building with **Nix**, `IHP_ASSET_VERSION` is automatically set to a unique build hash, so cache busting works out of the box without any manual configuration.

### Using a CDN

If you serve your static files through a CDN, set the `IHP_ASSET_BASEURL` environment variable to your CDN's base URL:

```bash
export IHP_ASSET_BASEURL=https://cdn.example.com
```

With this configured, `assetPath "/app.js"` will produce `https://cdn.example.com/app.js?v=af5f389` instead of `/static/app.js?v=af5f389`. The `/static/` prefix is omitted because the CDN serves files directly.

You will need to configure your CDN to pull files from your application's `static/` directory (or upload them to the CDN as part of your deployment process).

## Production Considerations

### Caching Headers

In production mode, IHP sets the following HTTP cache headers on all static files:

- `Cache-Control` (max-age set to forever)
- `Last-Modified`
- `ETag`

This means that after the first request, browsers and CDNs will serve the file from cache. Combined with the `assetPath` cache-busting mechanism, this gives you both fast repeat loads and reliable cache invalidation on deploys.

### CSS and JS Bundling

For production deployments with many individual CSS and JS files, IHP provides a simple bundling mechanism using `make`. Bundling concatenates multiple files into a single `prod.css` and `prod.js`, reducing the number of HTTP requests on the first page load.

To generate bundles:

```bash
make static/prod.js
make static/prod.css
```

Then update your `Web/View/Layout.hs` to load the bundled files in production and individual files in development:

```haskell
stylesheets :: Html
stylesheets = do
    when isDevelopment [hsx|
        <link rel="stylesheet" href={assetPath "/vendor/bootstrap-5.3.8/bootstrap.min.css"}/>
        <link rel="stylesheet" href={assetPath "/vendor/flatpickr.min.css"}/>
        <link rel="stylesheet" href={assetPath "/app.css"}/>
    |]
    when isProduction [hsx|
        <link rel="stylesheet" href={assetPath "/prod.css"}/>
    |]

scripts :: Html
scripts = do
    when isDevelopment [hsx|
        <script id="livereload-script" src={assetPath "/livereload.js"} data-ws={liveReloadWebsocketUrl}></script>
        <script src={assetPath "/vendor/jquery-4.0.0.slim.min.js"}></script>
        <script src={assetPath "/vendor/timeago.js"}></script>
        <script src={assetPath "/vendor/popper-2.11.6.min.js"}></script>
        <script src={assetPath "/vendor/bootstrap-5.3.8/bootstrap.min.js"}></script>
        <script src={assetPath "/vendor/flatpickr.js"}></script>
        <script src={assetPath "/helpers.js"}></script>
        <script src={assetPath "/vendor/morphdom-umd.min.js"}></script>
        <script src={assetPath "/vendor/turbolinks.js"}></script>
        <script src={assetPath "/vendor/turbolinksInstantClick.js"}></script>
        <script src={assetPath "/vendor/turbolinksMorphdom.js"}></script>
        <script src={assetPath "/app.js"}></script>
    |]
    when isProduction [hsx|
        <script src={assetPath "/prod.js"}></script>
    |]
```

The bundling process concatenates files without minification or transpiling. Configure which files are included in the bundle by editing your project's `Makefile`.

If you are building with Nix, the bundle generation command (`make static/prod.js static/prod.css`) runs automatically during the build.

For full details on configuring bundling, see the [Deployment Guide](deployment.html#css--js-bundling).

### Minification

IHP does not include built-in minification. If you need minified assets, you have several options:

- Use pre-minified versions of third-party libraries (most libraries provide `.min.js` and `.min.css` files).
- Add a minification step to your build process using tools like `esbuild`, `terser` (for JS), or `cssnano` (for CSS).
- For most IHP applications, the combination of bundling, caching headers, and gzip compression at the reverse proxy level (e.g. nginx) is sufficient without explicit minification.

### SASS and Webpack

IHP discourages the use of SASS, Webpack, and similar build tools because they add significant complexity and overhead. The recommended approach is to write plain CSS and use IHP's built-in bundling for production. If you follow the IHP philosophy of keeping most logic on the server and using minimal JavaScript, this approach works well.
