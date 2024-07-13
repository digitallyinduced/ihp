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

htmx gives you access to AJAX, as a way to update views locally with an API conviently residing in HTML attributes.

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

Instead of using the `render` function, htmx routes are better used with `respondHtml` to avoid the layout being shipped as part of the response. The same function can be used for initializing the view as well as upating.

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
        respondHtml $ counterView updatedCounter

    action DecrementCountAction{counterId} = do
        counter <- fetch counterId
        updatedCounter <- counter |> decrementField #count |> updateRecord
        respondHtml $ counterView updatedCounter
```

We define the `CounterView` like this, separating the `counterView` function into a function that can be used be the initial view as well as the updater routes (`IncrementCountAction` and `DecrementCountAction`).

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
