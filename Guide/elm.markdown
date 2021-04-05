# [Elm](https://elm-lang.org/)

```toc

```

## Introduction

While we recommend using IHP without any frontend frameworks, it may sometimes be useful to have a frontend framework for highly interactive parts of a webapp.

We think that Elm can be a great fit for this, and have therefor made getting started with Elm in a new ihp app as simple as possible

Credits to Lars Lillo Ulvestad, who wrote [a series of excellent articles](https://driftercode.com/blog/ihp-with-elm/) about the setup, which our implementation is based off of.

## Getting Started with IHP and Elm

It's as simple as passing the `--elm` option to `ihp-new`:

```bash
ihp-new --elm my-ihp-elm-project
```

This will generate a ready-to-go IHP app with Elm set up. To check that everything is working as expected, simply run `./start` as usual. The welcome page should say both "It's working" and "Elm is working!", which tells you that the setup works as expected.

### Troubleshooting

If you don't see "Elm is working!" right away, try refreshing after a couple of seconds - the first time the Elm code might take just those few seconds more to compile.

## Where everything is

You can find all Elm related code in `./elm`. Use `index.js` to instantiate your Elm widgets. The file also loads `Main.elm`, which can be your entry-point into your Elm app.

## Where to go from here

We recommend reading Lars' [series of blog articles](https://driftercode.com/blog/ihp-with-elm/) to see how to take this setup further and when to use it. The setup you currently get is mostly equivalent to part 1 of his series.

## Elm Code Is Not Compiling (Reloading)

Some text editors use safe-write when saving a file to disk, check the [parcel docs](https://parceljs.org/hmr.html#safe-write) on how to disable it.
