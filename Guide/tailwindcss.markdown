# [Tailwind CSS](https://tailwindcss.com/)

```toc
```

## Introduction

Yes, while bootstrap is the default CSS framework in IHP you can use IHP together with Tailwind. This guide will help you set up the latest Tailwind version in your IHP project.

## Installing

### [NodeJS](https://nodejs.org/) and [Entr](https://github.com/eradman/entr)

First, we need to add NodeJS to our project. **You should also follow this step if you have NodeJS already installed on your system.** Installing the NodeJS version via nix allows all people working on your project to get the same NodeJS version as you're using.

We also need a file watcher called `entr` to automatically call tailwind whenever some of our CSS files have changed.

For that open your projects `default.nix` and add `nodejs` and `entr` to `otherDeps`:

```nix
        otherDeps = p: with p; [
            # Native dependencies, e.g. imagemagick
            nodejs
            entr
        ];
```

Now you need to rebuild your local development environment:

```bash
nix-shell --run 'make -B .envrc'
```

After that, you have `node` and `npm` available in your project.

### Installing Tailwind

Install Tailwind via NPM as usual:

```bash
npm init
npm add tailwindcss postcss autoprefixer
```

This will create `package.json` and `package-lock.json`. Make sure to commit both files to your git repository.

### Configuring Tailwind

Create a new directory `tailwind`. We're going to place all the CSS files and the tailwind configuration in that directory:

```bash
mkdir tailwind
```

Create the tailwind configuration file at `tailwind/tailwind.config.js` with the following content:

```javascript
module.exports = {
  purge: { mode: "all", content: ["./Web/View/**/*.hs", "./assets/**/*.css"] },
  darkMode: false, // or 'media' or 'class'
  theme: {
    extend: {},
  },
  variants: {
    extend: {},
  },
  plugins: [],
}
```

We also need a CSS entry point for tailwind. Create a new file at `tailwind/app.css`:

```css
@tailwind base;

@tailwind components;

.btn {
  @apply px-4 py-2 bg-blue-600 text-white rounded;
}

form > div {
  @apply mb-4;
}

form input {
  @apply shadow;
  @apply appearance-none;
  @apply border;
  @apply rounded;
  @apply w-full;
  @apply py-2;
  @apply px-3;
  @apply text-gray-700;
  @apply leading-tight;
}

form label {
  @apply block;
  @apply text-gray-700;
  @apply text-sm;
  @apply font-bold;
  @apply mb-2;
}

@tailwind utilities;
```

### Adding the build step

We need to add a new build command for starting a tailwind build process to our `Makefile`. For that append this to the `Makefile` in your project:

```makefile
tailwind-dev:
    ls tailwind/*.css|NODE_ENV=development entr npx tailwindcss build tailwind/app.css -o static/app.css -c tailwind/tailwind.config.js
```

**Make requires tab characters instead of 4 spaces in the second line. Make sure you're using a tab character when pasting this into the file**

This defines a new command `make tailwind-dev` that runs `npx tailwindcss build` whenever a CSS file inside the `tailwind/` directory changes. The CSS output will be placed at `static/app.css` (the standard main CSS file of IHP apps). It will use the tailwind configuration at `tailwind/tailwind.config.js`.

For production builds we also need a new make target:

```makefile
static/app.css:
    NODE_ENV=production npm ci
    NODE_ENV=production npx tailwindcss build tailwind/app.css -o static/app.css -c tailwind/tailwind.config.js
```

**Make requires tab characters instead of 4 spaces in the second line. Make sure you're using a tab character when pasting this into the file**

### Updating the .gitignore

As the `static/app.css` is now generated code, it's best to put the `static/app.css` into our `.gitignore` file.

```bash
git rm -f static/app.css # Remove the existing app.css
echo static/app.css >> .gitignore
git add .gitignore
```

### Removing bootstrap

Open `Web/View/Layout.hs` and remove the `<link>` element that loads the bootstrap CSS:

```html
<link rel="stylesheet" href="/vendor/bootstrap.min.css" />
```

We don't need to make any additions for tailwind here. Just get rid of bootstrap.

Bootstrap is also part of the production CSS build, we need to remove that as well. Open `Makefile` and remove this line:

```makefile
CSS_FILES += ${IHP}/static/vendor/bootstrap.min.css
```

### Switching IHP Styling

Right now your IHP app will still be rendered with some bootstrap CSS class names. We can switch this to use tailwind classes. Open `Config/Config.hs` and make these changes:

```haskell
module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.View.CSSFramework -- ADD THIS IMPORT

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")
    option tailwind -- ADD THIS OPTION
```

#### Advanced: Customizing the tailwind classes

You can also override the default classes used by the `tailwind` styling. Add an additional `import IHP.View.Types` and then you can do something like:

```haskell
option tailwind { styledSubmitButtonClass = "my-app-button", styledValidationResultClass = "failed" }
```

[You can find a full list of options that you can override in the API Documentation](https://ihp.digitallyinduced.com/api-docs/IHP-View-Types.html#t:CSSFramework).

If your configuration is getting complex, it's best to move this configuration out of `Config.hs` and then just import your custom CSS Framework into your `Config`:

```haskell
module Web.View.CustomCSSFramework (customCSSFramework) where
import IHP.View.CSSFramework
import IHP.View.Types

customCSSFramework = tailwind { styledSubmitButtonClass = "my-app-button", styledValidationResultClass = "failed" }

-- Import that inside the Config.hs
module Config where

import Web.View.CustomCSSFramework

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")
    option customCSSFramework

```

## Developing with Tailwind

Once everything is installed you can start your tailwind build by calling:

```bash
make tailwind-dev
```

You should have this process running next to your terminal that runs `./start`.

Whenever you make a change to any CSS file in `tailwind/` it will automatically rebuild your styles and write it to `static/app.css`.

## Building for production

Because we defined the `static/app.css` make task above, the standard process of building IHP CSS applies here as usual:

```bash
make static/prod.css
```

`Make` will automatically detect that `static/app.css` is missing and will run `make static/app.css` to produce that missing file. This will then trigger the tailwind production build.

This means you don't need to make any changes to your existing deployment process or your IHP Cloud settings.

**If your IHP project has been created before 26.11.2020:** Make sure that the line `include ${IHP}/Makefile.dist` inside your `Makefile` is the last line of the file. It will most likely be somewhere at the top. If it's not the last line, the production CSS will not be generated.
