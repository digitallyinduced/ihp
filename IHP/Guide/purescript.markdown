# [PureScript](https://www.purescript.org/)

```toc

```

## Getting Started with IHP and PureScript + [Halogen](https://purescript-halogen.github.io/purescript-halogen/)

It's as simple as passing the `--purescript-halogen` option to `ihp-new`:

```bash
ihp-new --purescript-halogen my-ihp-purescript-halogen-project
```

This will generate a ready-to-go IHP app with PureScript + Halogen set up. To check that everything is working as expected, simply run `./start` as usual. The welcome page should say both "It's working" and "PureScript + Halogen is also working!", which tells you that the setup works as expected.

### Troubleshooting

If you don't see "PureScript + Halogen is also working!" right away, check the terminal if you see `[info] Success! Waiting for next file change.`, otherwise it could be compiling with logs such as `Compiling Module.Name`. Try refreshing once the PureScript code is done compiling.

## Deploying to IHP Cloud

If you decide to deploy your app to IHP Cloud you will need to make some adjustments to your project files in order to get the app compiling. Specifically, add these packages to `otherDeps` in your `default.nix` file:

```
    otherDeps = p: with p; [
        cacert
        esbuild
        git
        nodejs
        purescript
        spago
    ];
```

Then remove these packages from your `packages.json` file and run this command to update package-lock.json:

```
npm install
```

Run this command to ensure spago plays nicely with the new version of Purescript:

```
spago upgrade-set
```

Finally, add this to the end of your `Makefile` to tell IHP Cloud how to build and bundle your app into the index.js file:

```
static/halogen/index.js:
	HOME=/tmp npm ci
	HOME=/tmp spago build --purs-args "--output static/halogen/output" --source-maps
	esbuild static/halogen/main.js --bundle --outfile=static/halogen/index.js --minify --sourcemap
```

You may need to remove your local versions of Purescript and Spago so that they don't conflict with the ones provided by nix.

## Where everything is

You can find all PureScript related in `./halogen` folder. The `static/halogen/main.js` file also loads the compiled output of `halogen/Main.purs`, which is the entry-point into your PureScript app.

## Where to go from here

Checkout Halogen [documentation](https://purescript-halogen.github.io/purescript-halogen/)
