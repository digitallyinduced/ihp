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

## Where everything is

You can find all PureScript related in `./halogen` folder. The `static/halogen/main.js` file also loads the compiled output of `halogen/Main.purs`, which is the entry-point into your PureScript app.

## Where to go from here

Checkout Halogen [documentation](https://purescript-halogen.github.io/purescript-halogen/)
