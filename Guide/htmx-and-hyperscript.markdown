# htmx and hyperscript

```toc

```

## Introduction

As IHP itself is a hypermedia based tool, defaulting on hyperlinks, forms and RESTful routes, it works well with [htmx](https://htmx.org/) and [hyperscript](https://hyperscript.org/).

These tools can in many cases entirely replace the need for adding Single Page Applications (SPA) and npm to your workflow.

Instead of the separation of concerns paradigm, htmx and hyperscript favours [Locality of Behaviour](https://htmx.org/essays/locality-of-behaviour/).

To learn more, htmx has an [excellent collection of essays](https://htmx.org/essays/) about using the hypermedia approach instead of the current SPA paradigm.

## Make it play nice Turbolinks and AutoRefresh

To have htmx and hyperscript play well with Turbolinks page transitions, you can add this to your main javascript file.

```javascript
document.addEventListener('turbolinks:load', () => {
    htmx.process(document.body);
    _hyperscript.processNode(document.body);
});
```

This makes sure that htmx and hyperscript code does not stop working after a Turbolinks page transition or an AutoRefresh.

## htmx

htmx gives you access to AJAX, as a way to update views locally with an API conveniently residing in HTML attributes.

Instead of the typical Single Page Application pattern parsing JSON and turning into html in the DOM, the API endpoints simply just return HTML to be patched into the DOM.

### Installation

The recommended way of installing htmx is downloading [htmx.min.js](https://unpkg.com/htmx.org/dist/htmx.min.js) from unpkg.com and save it to `static/vendor/htmx.min.js` in your project directory.

Then add it to your `Web/View/Layout.hs` file before your app.js import.

```haskell
scripts :: Html
scripts = [hsx|
        {when isDevelopment devScripts}
        ...
        <script src={assetPath "/vendor/htmx.min.js"}></script>
        <script src={assetPath "/helpers.js"}></script>
        <script src={assetPath "/ihp-auto-refresh.js"}></script>
        <script src={assetPath "/app.js"}></script>
    |]
```

If you want to replace `helpers.js`, follow the migration section below and swap to `/helpers-htmx.js` + `/ihp-auto-refresh-htmx.js`.

### Replacing `helpers.js` with HTMX + morphdom

You can remove `helpers.js` and use HTMX + `helpers-htmx.js` instead.

For boosted `<a>` and `<form>` navigation, set HTMX attributes on the layout root:

1. Add HTMX and morphdom scripts to your layout.
2. Add IHP's HTMX helper script (`/helpers-htmx.js`).
3. Set `hx-boost`, `hx-target`, `hx-select`, and `hx-swap` on your layout root so boosted links/forms patch only the page content container.

```haskell
defaultLayout :: Html -> Html
defaultLayout inner = [hsx|
<!DOCTYPE html>
<html lang="en">
    <head>
        {metaTags}
        {stylesheets}
        {scripts}
        <title>{pageTitleOrDefault "App"}</title>
    </head>
    <body
        hx-ext="morphdom-swap"
        hx-boost="true"
        hx-target="#page-content"
        hx-select="#page-content"
        hx-swap="morphdom"
    >
        <div id="page-content" class="container mt-4">
            {renderFlashMessages}
            {inner}
        </div>
    </body>
</html>
|]
```

```haskell
scripts :: Html
scripts = [hsx|
        {when isDevelopment devScripts}
        ...
        <script src={assetPath "/vendor/htmx.min.js"}></script>
        <script src={assetPath "/vendor/morphdom-umd.min.js"}></script>
        <script src={assetPath "/helpers-htmx.js"}></script>
        <script src={assetPath "/ihp-auto-refresh-htmx.js"}></script>
        <script src={assetPath "/app.js"}></script>
    |]
```

`/helpers-htmx.js` is designed to be used together with `/ihp-auto-refresh-htmx.js`.
This pair is the HTMX equivalent of `/helpers.js` + `/ihp-auto-refresh.js`:

- `helpers-htmx.js` provides HTMX morphdom swap behavior and helper compatibility hooks
- `ihp-auto-refresh-htmx.js` provides Auto Refresh session management and fragment updates

Do not mix legacy and HTMX variants on the same page:

- Not together: `/helpers.js` and `/helpers-htmx.js`
- Not together: `/ihp-auto-refresh.js` and `/ihp-auto-refresh-htmx.js`

#### Official `morphdom-swap` vs `helpers-htmx.js`

`helpers-htmx.js` is intentionally different from HTMX's official `morphdom-swap` extension.

| Difference | What official extension does | What `helpers-htmx.js` does | Do you need this? |
| --- | --- | --- | --- |
| Input/textarea/option state handling | Relies on default morphdom behavior | Explicitly keeps current client state for `input`, `textarea`, `option` during swaps | Recommended when users can be typing while live updates arrive (Auto Refresh, concurrent updates) |
| File input patch safety | No explicit guard for `input[type=file]` | Skips updating file inputs during morph to avoid losing selected files | Recommended if forms with file uploads can be swapped |
| Node key strategy | No custom `getNodeKey` | Uses stable keys (`id`, script `src`) for better identity matching | Recommended for complex/reordered DOM and script stability |
| HTMX processing after custom swap | Returns swapped nodes to HTMX; no explicit `htmx.process` call in extension source | Calls `htmx.process(target)` explicitly after swap | Defensive behavior; usually safe either way |

Use the official extension if you want minimal behavior and default morphdom semantics.
Use `helpers-htmx.js` if you want a closer replacement for IHP helper behavior with fewer edge-case regressions.
In IHP docs and defaults, `helpers-htmx.js` is the standard morphdom swap path.

The source is available as an IHP built-in static file at `ihp/data/static/helpers-htmx.js`.
If you want to vendor it into your app, copy `${IHP}/static/helpers-htmx.js` into your project's `static/` directory.
If you're using `make static/prod.js` bundling, add `JS_FILES += ${IHP}/static/helpers-htmx.js` to your app `Makefile`.

With this setup, HTMX boost handles `<a>` and `<form>` requests and morphdom keeps DOM identity stable (e.g. input values and cursor position).

If you use Auto Refresh with HTMX, use `/ihp-auto-refresh-htmx.js`.

#### `helpers.js` feature-by-feature mapping

| `helpers.js` feature | HTMX / new script equivalent | Status |
| --- | --- | --- |
| Morphdom page/fragment patching (`transitionToNewPage`) | `hx-swap="morphdom"` + `/helpers-htmx.js` | Covered |
| Stable node matching (`getNodeKey`) | Implemented in `/helpers-htmx.js` (`id`, script `src`) | Covered |
| Preserve input value/cursor/checked/selected during swaps | Implemented inside `/helpers-htmx.js` | Covered |
| HTMX re-processing after morphdom swap | Implemented inside `/helpers-htmx.js` via `htmx.process` | Covered |
| Intercept links/forms and submit via AJAX (`initDisableButtonsOnSubmit` + `submitForm`) | `hx-boost="true"` on layout/body | Covered |
| Per-form opt-out of AJAX (`disableJavascriptSubmission`) | `hx-boost="false"` on the form (or parent scope) | Covered with config |
| Follow submitter semantics (`formAction`, clicked button value) | Standard form submitter behavior with HTMX-boosted forms | Covered |
| File uploads via form submit | Native form multipart behavior (`enctype="multipart/form-data"` or `hx-encoding`) | Covered with config |
| Delete links with confirmation (`.js-delete`, `.js-delete-no-confirm`) | `hx-delete` + `hx-confirm` | Covered |
| Disable submit buttons while request is in-flight | `hx-disabled-elt` | Covered |
| Dismiss `.alert` on submit | Built into `/helpers-htmx.js` (`htmx:beforeRequest`) | Covered |
| Pause auto refresh around requests | `/ihp-auto-refresh-htmx.js` already hooks into `htmx:beforeRequest`/`htmx:afterRequest` | Covered |
| Move/refresh auto-refresh session metadata across HTMX swaps | `/ihp-auto-refresh-htmx.js` handles meta harvesting + target inference | Covered |
| URL/history updates after boosted requests | `hx-push-url="true"` where needed (especially GET filter/search forms) | Covered with config |
| Special modal behavior in body morph (`modal-open` + `#main-row` guard) | Usually avoid by targeting a stable content container (`#page-content`) instead of full-body swaps; use `hx-preserve` for modal roots if needed | App-specific behavior |
| `.js-back` helper | Built into `/helpers-htmx.js` (`.js-back`, `[data-js-back]`) | Covered |
| `[data-toggle]` helper for disabling/enabling other fields | Built into `/helpers-htmx.js` | Covered |
| File upload preview (`input[type="file"][data-preview]`) | Built into `/helpers-htmx.js` | Covered |
| Date/time pretty formatting (`.time-ago`, `.date-time`, `.date`, `.time`) | Built into `/helpers-htmx.js` | Covered |
| Flatpickr auto-init on date fields | Built into `/helpers-htmx.js` | Covered |
| `.js-scroll-into-view` after navigation | Built into `/helpers-htmx.js` | Covered |
| `ihp:load` / `ihp:unload` events | Built into `/helpers-htmx.js` | Covered |
| Timer cleanup utilities (`clearAllIntervals` / `clearAllTimeouts`) | Built into `/helpers-htmx.js` | Covered |

#### Detailed migration recipes

`helpers-htmx.js` now includes built-in compatibility for all "easy custom code" items from the table.
That means you should not need to re-implement these in `app.js`:

1. `.js-back` (and `[data-js-back]`) back button handling
2. `[data-toggle]` dependent field enable/disable behavior
3. `input[type=file][data-preview]` preview behavior
4. Date/time formatting (`.time-ago`, `.date-time`, `.date`, `.time`)
5. Flatpickr auto-initialization on initial load and HTMX swaps
6. `.js-scroll-into-view` behavior after swaps
7. `.alert` dismissal on HTMX form requests
8. `ihp:load` and `ihp:unload` compatibility events

HTMX-native features still use HTMX attributes:

1. Delete links: `hx-delete` (+ `hx-confirm` if needed)
2. Disable submit while requesting: `hx-disabled-elt`
3. Push URL for GET forms: `hx-push-url="true"`
4. Disable boost per form: `hx-boost="false"`
5. Multipart uploads: `enctype="multipart/form-data"` (optionally `hx-encoding="multipart/form-data"`)

#### Notes on remaining gaps

With `helpers-htmx.js` plus HTMX-native attributes (`hx-delete`, `hx-disabled-elt`, `hx-push-url`, etc.), the common `helpers.js` migration surface is covered.

### htmx usage

Assume the simple controller as an example:

```haskell
data CounterController
    = CounterAction
    | IncrementCountAction {counterId :: !(Id Counter)}
    | DecrementCountAction {counterId :: !(Id Counter)}
    deriving (Eq, Show, Data)
```

Add `parseRoute @CounterController` to the list of `instance FrontController WebApplication` in `FrontController.hs` (or you'll get a 404 on calling it), and add an `instance AutoRoute CounterController` to `Routes.hs` (or you'll get a compilation error about it not being an instance of AutoRoute).

For HTMX endpoints, use different render helpers depending on the response:

- Full page responses: use `render`
- Fragment responses: use `respondHtmlFragment` (or `renderFragment` when returning a `View`)

This avoids shipping the layout in fragment responses.

```haskell
module Web.Controller.Counter where
import Web.Controller.Prelude
import Web.View.Counter.Counter

instance Controller CounterController where
    action CounterAction = do
        maybeCounter <- query @Counter |> fetchOneOrNothing
        case maybeCounter of
            Nothing -> do
                counter <- newRecord @Counter |> set #count 0 |> createRecord
                render CounterView {..}
            Just counter ->
                render CounterView {..}

    action IncrementCountAction{counterId} = do
        counter <- fetch counterId
        updatedCounter <- counter |> incrementField #count |> updateRecord
        respondHtmlFragment $ counterView updatedCounter

    action DecrementCountAction{counterId} = do
        counter <- fetch counterId
        updatedCounter <- counter |> decrementField #count |> updateRecord
        respondHtmlFragment $ counterView updatedCounter
```

We define the `CounterView` like this, separating `counterView` into a reusable function for both the initial page and the fragment update routes (`IncrementCountAction` and `DecrementCountAction`).

```haskell
module Web.View.Counter.Counter where
import Web.View.Prelude

data CounterView = CounterView {counter :: Counter}

instance View CounterView where
    html CounterView{..} = [hsx|
    <h2>htmx counter</h2>
    {counterView counter}
    <button
        hx-post={IncrementCountAction counter.id}
        hx-target="#counter"
    >
        Increment
    </button>
    <button
        hx-post={DecrementCountAction counter.id}
        hx-target="#counter"
    >
        Decrement
    </button>
|]

counterView :: Counter -> Html
counterView counter = [hsx|<div id="counter">Count: {counter.count}</div>|]
```

The `#counter` element will then be fully replaced with the response from the increment and decrement routes with the database as the source of truth.

With the help of REST controllers returning the html output of an hsx function, one can make dynamic sites without integrating complicated frontend tooling.

This is what is known as [Hypermedia as the Engine of Application State (HATEOAS)](https://htmx.org/essays/hateoas/).

## hyperscript

hyperscript is commonly used as a companion library for htmx as an alternative to js with a human readable syntax and Locality of behaviour.

### Installation

The recommended way of installing htmx is downloading [\_hyperscript.min.js](https://unpkg.com/hyperscript.org/dist/_hyperscript.min.js) from unpkg.com and save it to `static/vendor/_hyperscript.min.js` in your project directory.

Then add it to your `Web/View/Layout.hs` file before your app.js import.

```haskell
scripts :: Html
scripts = [hsx|
        {when isDevelopment devScripts}
        ...
        <script src={assetPath "/vendor/_hyperscript.min.js"}></script>
        <script src={assetPath "/helpers.js"}></script>
        <script src={assetPath "/ihp-auto-refresh.js"}></script>
        <script src={assetPath "/app.js"}></script>
    |]
```

### hyperscript usage

The default `_` attribute is currently not supported by HSX, but using the `data-script` attribute works equally well.

HSX also supports multi line attribute strings out of the box, giving you more readable formatting.

The following example shows you how to call a js function with `js` and manipulating the innerText of the button itself.

```haskell
clickAlert :: Html
clickAlert = [hsx|
    <button
        class="btn"
        data-script="
            on click
                log 'Button clicked'
            then
                js alert('Thank you for the click')
            end
            then
                set me.innerText to 'Already clicked'
        "
    >
        Not yet clicked
    </button>
|]
```
