# IHP: Integrated Haskell Platform

*IHP is a modern batteries-included Web Framework, built on top of Haskell and Nix.*

![](https://raw.githubusercontent.com/digitallyinduced/haskellframework/836e5f30ff95ab22104e5020ebbd01c2f278c83d/Guide/images/ihp-logo.png?token=AAPZ46LZHKVIAKRIY775AGC6ZZ5O6)

## Getting Started

[Follow the Guide to build your first project!](https://ihp.digitallyinduced.com/Guide/intro.html) 🚀

## Why?

We believe that functional programing is the future of software development and want to make functional programing with haskell and nix available to anyone. We try to offer a solution which can be used by developers who have not worked with haskell yet. IHP comes with everything you need to build great web applications with haskell and nix. We have made a lot of pragmatic decision to get you started faster. This way you can just pick up haskell along the way :-)

## Contributing

We are happy to merge your pull requests!

### Running an application against a local copy of the framework

To work on the framework in your application, you need to clone this repository inside your application directory:

```
git clone git@github.com:digitallyinduced/haskellframework.git IHP
```

The best workflow is to use `ghci` to load your application together with the framework version in `IHP`:

```
ghci
:l Main
```

This will now load your application and all the haskell files in `IHP`.

As we are not using the development tooling for the framework development process, we need to manually start the postgres by running `postgres -D build/db/state -k $PWD/build/db -c "listen_addresses="` in another terminal window.

After postgres is started, you can now start the application with the local framework version by running:

```
main
```

After you have made modifications to files inside `IHP`, you need to press `CTRL + C` to stop the process running in `ghci` and then type `:r` to refresh the haskell modules. Now type `main` to start the server again.

### Working on the documentation

To work on the documentation locally, open a nix shell inside the framework directory:

```
cd IHP
nix-shell NixSupport/shell.nix
```

Then switch to the `Guide` directory:

```
cd Guide
```

To rebuild the html files from the markdown files on every file change, run:

```
make watch
```

This will automatically open a browser window with the compiled html of the documentation. You can now make changes to the markdown files and they are reflected in the browser after a page refresh.

When adding a new markdown page, also add it to the `Makefile`, otherwise it will not be built.

### Code Guidelines

- Please use `pure` instead of `return`, as `return` might confuse people coming from other programing languages.
