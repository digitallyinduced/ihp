# Architecture


```toc
```


This section tries to answer common questions on where to place your code. These are recommendations found by digitally induced to be working well.

In general remember that all specific web app logic should stay in the `Web/` space. The `Application/` space is for sharing code across all your different applications. E.g. code shared between your web application and your admin backend.


## Directory Structure


| File or Directory             | Purpose                                                                     |
|-------------------------------|-----------------------------------------------------------------------------|
| Config/                       |                                                                             |
| Config/Config.hs              | Configuration for the framework and your application                        |
| Config/nix/nixpkgs-config.nix | Configuration for the nix package manager                                   |
| Config/nix/haskell-packages/  | Custom Haskell dependencies can be placed here                              |
| Application/                          | Your domain logic lives here                                           |
| Application/Schema.sql           | Models and database tables are defined here                                 |
| Web/Controller       | Web application controllers                                                             |
| Web/View/            | Web application html template files                                                         |
| Web/Types.hs            | Central place for all web application types                                                         |
| static/                       | Images, css and javascript files                                            |
| .ghci                         | Default config file for the Haskell interpreter                             |
| .gitignore                    | List of files to be ignored by git                                          |
| App.cabal, Setup.hs           | Config for the cabal package manager  |
| default.nix                   | Declares your app dependencies (like package.json for NPM or composer.json for PHP)         |
| Makefile                      | Default config file for the make build system                               |


## FAQ


##### Where to place a function I want to use in all my views?

If the function is only used in a single application and is a building block for your layout, place it in `Web/View/Layout.hs`. The module is already imported in all your views (just don't forget to add the function to the export list).

If the function is used across multiple applications or more like a helper function, place it in `Application/Helper/View.hs`. This module is also already included in your view files.

##### Where to place a function I want to use in all my controllers?

Place it in `Application/Helper/Controller.hs`. This module is already imported into your controllers.

##### Where to place a custom type?

Place it in `Web/Types.hs`.

##### Next to my main web application, I'm building an admin backend application. Where to place it?

A IHP project can consist of multiple applications. Run `new-application admin` inside a `nix-shell` to generate a new admin application. The logic for the new application is located in the `Admin/` directory. On the web you can find it at `http://localhost:8000/admin/` (all actions are prefixed with `/admin/`).

##### How to structure my CSS?

CSS files, as all your other static assets, should be placed in the `static` directory.

Create a `static/app.css`. In there use CSS imports to import your other stylesheets. An example `app.css` could look like this:

```css
@import "/layout.css";
@import "/widget.css";
@import "/form.css";
@import "/button.css";
@import "/users.css";
```

###### Page-specific CSS rules

Place page-specific CSS used by e.g. views of the `Web.Controller.Users` controller in `users.css`. Use [currentViewId](https://ihp.digitallyinduced.com/api-docs/IHP-ViewSupport.html#v:currentViewId) to scope your css rules to the view.

Given the view:

```haskell
module Web.View.Projects.Show where

render = [hsx|
    <div id={currentViewId}>
        <h1>Hello World!</h1>
    </div>
|]
```

This will render like

```html
<div id="projects-show">
    <h1>Hello World!</h1>
</div>`
```

So in your `projects.css` you can just do rules like 

```css
#projects-show h1 { color: blue; }
```

###### SASS & Webpack

We discourage the use of tools like SASS or webpack because they have too much overhead.

###### Library CSS

CSS files from external libraries or components should be placed in `static/vendor/`.

##### How to structure my Javascript Code?

JS files, as all your other static assets, should be place in the `static` directory.

In general we follow an approach where most of the business logic resides on the Haskell server. Only for small interactions, or client-side GUI niceness, we try to use a small isolated bit of javascript.

Your global, non-page specific, javascript code can be placed in `app.js`.

E.g. the `app.js` could look like this:

```javascript
$(function () {
    initNavbarEffects();
});

function initNavbarEffects() {
    // ...
}
```

In your `Web.View.Layout` just import the `app.js`:

```html
<script src="/app.js"></script>
```

###### Page-specific JS

Place page-specific JS used by e.g. views of the `Web.Controller.Users` controller in the `users.js`.

In the views, just import the javascript with `<script src="/users.js"></script>`.

###### Webpack

We discourage the use of webpack or any other bundler because they have too much overhead. Of course this advice only applies if you follow the approach to use as little javascript as possible.

###### Library JS

JS files from external libraries or components should be placed in `static/vendor/`. For simplicity it might make sense to just download the javascript bundle of the library you want to use, and then just commit it into git instead of using NPM.

For more complex use-cases with lots of javascript, you should not follow this advice and just use NPM instead.

##### Where to place static images?

Place your images in the `static` folder. We recommend to use SVG images.
