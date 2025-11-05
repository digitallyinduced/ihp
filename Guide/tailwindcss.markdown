# [Tailwind CSS](https://tailwindcss.com/)

```toc

```

## Introduction

Yes, while bootstrap is the default CSS framework in IHP, you can also use IHP together with Tailwind CSS.

## Installing

### Adding package to flake.nix

While it's possible to have `nodejs` installed via nix, and then have npm install Tailwind CSS, we can skip that part and have nix install the CLI directly.

In the `flake.nix`, add the `tailwindcss` package bundled with the most common official plugins.

```nix
...
packages = with pkgs; [
    # Native dependencies, e.g. imagemagick
    (nodePackages.tailwindcss.overrideAttrs
        (_: {
            plugins = [
                nodePackages."@tailwindcss/aspect-ratio"
                nodePackages."@tailwindcss/forms"
                nodePackages."@tailwindcss/language-server"
                nodePackages."@tailwindcss/line-clamp"
                nodePackages."@tailwindcss/typography"
            ];
        })
    )
];
...
```

Rebuild your development environment to fetch the added package:

```bash
direnv reload
```

After that, you should be able to verify that `tailwindcss` CLI is available in your project directory by executing it from your shell.

```
tailwindcss
```

### Configuring Tailwind

Create a new directory `tailwind`. We're going to place all the CSS files and the tailwind configuration in that directory:

```bash
mkdir tailwind
```

Create the tailwind configuration file at `tailwind/tailwind.config.js` with the following content:

```javascript
const plugin = require('tailwindcss/plugin');

module.exports = {
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

### Integrating with `devenv`

To start the TailwindCSS build process with the IHP dev server, add the following to your flake.nix:

```nix
...
ihp = {
    enable = true;
    projectPath = ./.;
    packages = with pkgs; [
        # Native dependencies, e.g. imagemagick
        (nodePackages.tailwindcss.overrideAttrs
            (_: {
                plugins = [
                    nodePackages."@tailwindcss/aspect-ratio"
                    nodePackages."@tailwindcss/forms"
                    nodePackages."@tailwindcss/language-server"
                    nodePackages."@tailwindcss/line-clamp"
                    nodePackages."@tailwindcss/typography"
                ];
            })
        )
    ];
    haskellPackages = p: with p; [
        # Haskell dependencies go here
        ...
     ];
};

# Add TailwindCSS build command here
devenv.shells.default = {
    processes = {
        tailwind.exec = "tailwindcss -c tailwind/tailwind.config.js -i ./tailwind/app.css -o static/app.css --watch=always";
    };
};
...
```

Now when you use `devenv up` to start the IHP dev server, the TailwindCSS build process will be started as well. The `--watch=always` flag forces `tailwindcss` to always stay running and watch for any CSS changes in your project and rebuild the `static/app.css` file as needed.

### Adding additional build step for production

For production builds, we also need a new make target:

```makefile
static/app.css:
	tailwindcss -c tailwind/tailwind.config.js -i ./tailwind/app.css -o static/app.css --minify
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

Create a file at `Web/View/CustomCSSFramework.hs`  and copy the `import`s and the contents of the `tailwind` function from [IHP/View/CSSFramework.hs](https://github.com/digitallyinduced/ihp/blob/master/IHP/View/CSSFramework.hs):

```haskell
module Web.View.CustomCSSFramework (customTailwind) where

import IHP.View.CSSFramework -- This is the only import not copied from IHP/View/CSSFramework.hs
import IHP.Prelude
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
    -- See https://ihp.digitallyinduced.com/Guide/config.html
    -- for what you can do here
    option customTailwind -- ADD THIS OPTION
    pure ()
```

## Building for production

Because we defined the `static/app.css` make task above, the standard process of building IHP CSS applies here as usual:

```bash
make static/prod.css
```

`Make` will automatically detect that `static/app.css` is missing and will run `make static/app.css` to produce that missing file. This will then trigger the tailwind production build.

This means you don't need to make any changes to your existing deployment process.
