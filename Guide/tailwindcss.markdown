# [Tailwind CSS](https://tailwindcss.com/)

```toc

```

## Introduction

Yes, while bootstrap is the default CSS framework in IHP, you can use IHP together with TailwindCSS (TW in short). This guide will help you set up the latest Tailwind version in your IHP project. We will be leveraging the TW's [JIT](https://tailwindcss.com/docs/just-in-time-mode) mode.

We will also have PostCSS added as part of the installation. PostCSS is used for nesting CSS, minifying, striping out comments, etc.

## Installing

### Tailwind Standalone CLI

[ihp-tailwind-bootstrapper](https://github.com/ship-nix/ihp-tailwind-bootstrapper) allows you to add the [Tailwind Standalone CLI](https://tailwindcss.com/blog/standalone-cli) to your IHP project without adding npm.

`autoprefixer`, `postcss` and all official Tailwind plugins are all included in the prebuilt Tailwind binaries.

From the root of your ihp project, pull the repository and delete the .git folder in the tailwind folder with this one-liner:

```sh
git clone https://github.com/ship-nix/ihp-tailwind-bootstrapper.git tailwind && rm -rf tailwind/.git
```

This will create a `tailwind` directory in your IHP project with all the files you need to start using Tailwind.

Edit `tailwind/app.css` and `tailwind/tailwind.config.js` to your own liking.

### Add dev and production build scripts

In the `Makefile` in your IHP project, add these scripts:

```sh
tailwind-dev:
	bash tailwind/tailwindcss -c tailwind/tailwind.config.js -i ./tailwind/app.css -o static/app.css --watch

static/app.css:
	NODE_ENV=production bash tailwind/tailwindcss -c tailwind/tailwind.config.js -i ./tailwind/app.css -o static/app.css --minify
```

### Updating the .gitignore

As the `static/app.css` is now generated code, it’s best to put the `static/app.css` into our `.gitignore` in the root of you IHP project.

```sh
git rm -f static/app.css # Remove the existing app.css
printf '\nstatic/app.css' >> .gitignore
git add .gitignore
```

## How to use

### Run in development mode

For running the development version of Tailwind in `watch` mode, you can run this line:

```sh
nix-shell --run 'make tailwind-dev'
```

Optionally, you can replace the `RunDevServer` line in your `./start` script with this to run Tailwind automatically when running the IHP development server.

```sh
# Finally start the dev server
wait & make tailwind-dev & RunDevServer
```

### Removing bootstrap

Open `Web/View/Layout.hs` and remove the following `<link>` and `<script>` elements that load bootstrap:

```html
<link rel="stylesheet" href="/vendor/bootstrap.min.css" />
<script src="/vendor/popper-2.11.6.min.js"></script>
<script src="/vendor/bootstrap.min.js"></script>
```

We don't need to make any additions for Tailwind here. Just get rid of bootstrap.

Bootstrap is also part of the production CSS build, we need to remove that as well. Open `Makefile` and remove these lines:

```makefile
CSS_FILES += ${IHP}/static/vendor/bootstrap.min.css
JS_FILES += ${IHP}/static/vendor/popper-2.11.6.min.js
JS_FILES += ${IHP}/static/vendor/bootstrap.min.js
```

### Switching IHP Styling

Right now, your IHP app will still be rendered with some bootstrap CSS class names. We can switch this to use tailwind classes. Since our configuration uses the JIT mode, it means we would need to copy the `tailwind` CSSFramework from IHP core files, into our custom theme. Otherwise, any css defined by IHP itself will not be caught by Tailwind before purging and keeping only the used CSS classes. This might seem weird initially, having to copy/paste; however, we think it is the best compromise since it's very likely you would like to change the default classes and get your site a unique view.

Create a file at `Web/View/CustomCSSFramework.hs` and copy the `import`s and the contents of the `tailwind` function from [IHP/View/CSSFramework.hs](https://github.com/digitallyinduced/ihp/blob/master/IHP/View/CSSFramework.hs):

```haskell
module Web.View.CustomCSSFramework (customTailwind) where

import IHP.View.CSSFramework -- This is the only import not copied from IHP/View/CSSFramework.hs
import IHP.Prelude
import IHP.FlashMessages.Types
import qualified Text.Blaze.Html5 as Blaze
import Text.Blaze.Html.Renderer.Text (renderHtml)
import IHP.HSX.QQ (hsx)
import IHP.HSX.ToHtml ()
import IHP.View.Types
import IHP.View.Classes

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!), (!?))
import qualified Text.Blaze.Html5.Attributes as A
import IHP.ModelSupport
import IHP.Breadcrumb.Types
import IHP.Pagination.Helpers
import IHP.Pagination.Types
import IHP.View.Types (PaginationView(linkPrevious, pagination))


-- Copying the contents of 'tailwind' function
customTailwind :: CSSFramework
customTailwind = def
    { styledFlashMessage
    , styledSubmitButtonClass
    , styledFormGroupClass
    , styledFormFieldHelp
-- ... Keep copying the rest of the function
```

Now JIT will recognize those classes and not purge them.

Open `Config/Config.hs` and make these changes:

```haskell
module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import Web.View.CustomCSSFramework -- ADD THIS IMPORT

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")
    option customTailwind -- ADD THIS OPTION
```

## Developing with Tailwind

Once everything is installed, you can start your tailwind build by calling:

```bash
make tailwind-dev
```

Whenever you change any CSS file in `tailwind/` it will automatically rebuild your styles and write it to `static/app.css`.

## Building for production

Because we defined the `static/app.css` make task above, the standard process of building IHP CSS applies here as usual:

```bash
make static/prod.css
```

`Make` will automatically detect that `static/app.css` is missing and will run `make static/app.css` to produce that missing file. This will then trigger the tailwind production build.

This means you don't need to make any changes to your existing deployment process.
