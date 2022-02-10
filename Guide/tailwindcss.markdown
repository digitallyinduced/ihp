# [Tailwind CSS](https://tailwindcss.com/)

```toc

```

## Introduction

Yes, while bootstrap is the default CSS framework in IHP, you can use IHP together with TailwindCSS (TW in short). This guide will help you set up the latest Tailwind version in your IHP project. We will be leveraging the TW's [JIT](https://tailwindcss.com/docs/just-in-time-mode) mode.

We will also have PostCSS added as part of the installation. PostCSS is used for nesting CSS, minifying, striping out comments, etc.

## Installing

### NodeJS

First, we need to add NodeJS to our project. **You should also follow this step if you have NodeJS already installed on your system.** Installing the NodeJS version via nix allows all people working on your project to get the same NodeJS version as you're using.

For that open your projects `default.nix` and add `nodejs` to `otherDeps`:

```nix
otherDeps = p: with p; [
    # Native dependencies, e.g. imagemagick
    nodejs
];
```

Now you need to rebuild your local development environment:

```bash
nix-shell --run 'make -B .envrc'
```

After that, you have `node` and `npm` available in your project.

### Installing Tailwind

Install Tailwind along with PostCSS and some handy libraries via NPM:

```bash
npm init
npm add tailwindcss@latest postcss@latest autoprefixer@latest @tailwindcss/forms
```

This will create `package.json` and `package-lock.json`. Make sure to commit both files to your git repository.

### Configuring Tailwind

Create a new directory `tailwind`. We're going to place all the CSS files and the tailwind configuration in that directory:

```bash
mkdir tailwind
```

Create the tailwind configuration file at `tailwind/tailwind.config.js` with the following content:

```javascript
const plugin = require('tailwindcss/plugin');

module.exports = {
    mode: 'jit',
    theme: {
        extend: {
        },
    },
    content: [
        "Web/View/**/*.hs",
    ],
    safelist: [
        // Add custom class names.
        // https://tailwindcss.com/docs/content-configuration#safelisting-classes
    ],
    plugins: [
        require('@tailwindcss/forms'),
    ],
};
```

We also need a CSS entry point for Tailwind. Create a new file at `tailwind/app.css`.

```css
@tailwind base;
@tailwind components;
@tailwind utilities;

@layer components {
  .btn {
    @apply px-4 py-2 bg-blue-600 text-white rounded;
  }
}
```

### Adding the build step

We need to add a new build command for starting a tailwind build process to our `Makefile`. For that append this to the `Makefile` in your project:

```makefile
tailwind-dev:
	node_modules/.bin/tailwind -c tailwind/tailwind.config.js -i ./tailwind/app.css -o static/app.css --watch
```

**Make requires tab characters instead of 4 spaces in the second line. Make sure you're using a tab character when pasting this into the file**

This defines a new command `make tailwind-dev` that runs `npx tailwindcss build` whenever a CSS file inside the `tailwind/` directory changes. The CSS output will be placed at `static/app.css` (the standard main CSS file of IHP apps). It will use the tailwind configuration at `tailwind/tailwind.config.js`.

For production builds, we also need a new make target:

```makefile
node_modules:
    NODE_ENV=production npm ci

static/app.css:
	NODE_ENV=production node_modules/.bin/tailwind -c tailwind/tailwind.config.js -i ./tailwind/app.css -o static/app.css --minify
```

**Make requires tab characters instead of 4 spaces in the second line. Make sure you're using a tab character when pasting this into the file**

You can now execute it with `make -B static/app.css`

### Updating the .gitignore

As the `static/app.css` is now generated code, it's best to put the `static/app.css` into our `.gitignore` file.

```bash
git rm -f static/app.css # Remove the existing app.css
printf '\nstatic/app.css' >> .gitignore
git add .gitignore
```

### Removing bootstrap

Open `Web/View/Layout.hs` and remove the following `<link>` and `<script>` elements that load bootstrap:

```html
<link rel="stylesheet" href="/vendor/bootstrap.min.css" />
<script src="/vendor/popper.min.js"></script>
<script src="/vendor/bootstrap.min.js"></script>
```

We don't need to make any additions for Tailwind here. Just get rid of bootstrap.

Bootstrap is also part of the production CSS build, we need to remove that as well. Open `Makefile` and remove these lines:

```makefile
CSS_FILES += ${IHP}/static/vendor/bootstrap.min.css
JS_FILES += ${IHP}/static/vendor/popper.min.js
JS_FILES += ${IHP}/static/vendor/bootstrap.min.js
```

### Switching IHP Styling

Right now, your IHP app will still be rendered with some bootstrap CSS class names. We can switch this to use tailwind classes. Since our configuration uses the JIT mode, it means we would need to copy the `tailwind` CSSFramework from IHP core files, into our custom theme. Otherwise, any css defined by IHP itself will not be caught by Tailwind before purging and keeping only the used CSS classes. This might seem weird initially, having to copy/paste; however, we think it is the best compromise since it's very likely you would like to change the default classes and get your site a unique view.

Create a file at `Web/View/CustomCSSFramework.hs`  and copy the `import`s and the contents of the `tailwind` function from [IHP/View/CSSFramework.hs](https://github.com/digitallyinduced/ihp/blob/master/IHP/View/CSSFramework.hs):

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

This means you don't need to make any changes to your existing deployment process or your IHP Cloud settings.
