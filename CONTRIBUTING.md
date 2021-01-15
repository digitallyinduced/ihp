# Contributing

We are happy to merge your pull requests!

## Running an application against a local copy of the framework

To work on the framework in your application, you need to clone this repository inside your application directory.
If you don't have a project, first make sure you have the latest version of `ihp-new`, update it by running: `nix-env -f https://ihp.digitallyinduced.com/ihp-new.tar.gz -i ihp-new`. Use `ihp-new` to create one. Make sure to run `./start` in your project once. Also make sure that [`direnv`](https://direnv.net/docs/hook.html) is set up in your shell.

```
# Set up a local IHP project
ihp-new myproject
cd myproject
./start
```

Running `./start` is necessary, even if you don't intend to run it this way (e.g. you intend to do development and don't intend to run it "normally"), to do some initial setup like creating the database. When it starts normally, just CTRL+C to exit.

Clone the IHP repository into the project directory. The `IHP` directory is added to the GHC search path in applicationGhciConfig. Therefore when the `IHP` directory exists, GHC will load all IHP modules from there.

```
git clone git@github.com:digitallyinduced/ihp.git IHP
# only needs to be run once (do not run inside the IHP directory)
make -B build/ihp-lib
```

The best workflow is to use `ghci` to load your application together with the framework located in `IHP`. In a `nix-shell`:

```
ghci
$ghci> :l Main
```

This will now load your application and all the haskell files in `IHP`.

As we are not using the development tooling for the framework development process we need to manually start the postgres process by running `postgres -D build/db/state -k $PWD/build/db -c "listen_addresses="` in another terminal window.

After postgres is started you can now start the application with the local framework version by running:

```
main
```

After you have made modifications to files inside `IHP`, you need to press `CTRL + C` to stop the process running in `ghci` and then type `:r` to refresh the haskell modules. Now type `main` to start the server again.

### Running the development server

When making changes to the development tooling, follow the setup above, except don't start postgres (the IDE starts it automatically).

Instead of starting your application, start the development server:

```
ghci
:l IHP/exe/IHP/IDE/DevServer.hs
main
```

#### Debugging the development server

You can enable additonal debug logging for the development server by setting the env variable `DEBUG=1`. Like this:

```
export DEBUG=1
ghci
:l IHP/exe/IHP/IDE/DevServer.hs
main
```

## Working on the documentation

To work on the documentation locally open a nix shell inside the framework directory:

```
cd IHP
nix-shell
```

Then switch to the `Guide` directory:

```
cd Guide
```

To rebuild the html files from the markdown files on every file change run:

```
make watch
```

This will automatically open a browser window with the compiled html of the documentation. You can now make changes to the markdown files and they are reflected in the browser after a page refresh.

When adding a new markdown page also add it to the `Makefile`. Otherwise it will not be built.

The documentation reads a bit like a tutorial, but should still be kept somewhat complete. Still, refer the reader to the API docs for comprehensive explanations, technical details or less-used functionality. After all, the target audience is coders.

## Code Guidelines

-   Please use `pure`. `return` might confuse people coming from other programming languages.

-   Please add Haddock-comments to new methods intended to be used by directly when developing using IHP.

-   Please consider carefully before adding new packages as requirements to IHP itself. Make sure the packages are actively maintained.

## Running Tests

When inside the IHP directory, you can run the Test Suite by loading it into a `ghci` like this:

```bash
nix-shell
ghci
:l Test/Main.hs
main
```

Note that it is necessary to start the nix-shell from within the IHP directory to pick up an extra package used in testing.

When doing changes to the test files, use this to reload and rerun the tests:

```
:r
main
```

After creating a new test you need to still call it from the `Main` module by adding it to `IHP/Test/Main.hs`.

## Troubleshooting

### `can't satisify package ihp`

If you get an error like `can't satisify package ihp` or all other IHP packages when running `ghci` most likely the symlink in `build/ihp-lib` is not set up as expected. IHP uses the symlink `build/ihp-lib` in your application's `.ghci` file to access [`IHP/lib/IHP/applicationGhciConfig`](https://github.com/digitallyinduced/ihp/blob/master/lib/IHP/applicationGhciConfig#L39). This `applicationGhciConfig` sets up all the required options for `ghci`.

Try to run `make -B build/ihp-lib` to create the symlink.

### `direnv: error .envrc file not found`

In the project directory, try `make -B .envrc`

### Error messages about missing packages in `ghci`

Perhaps a package was added to IHP recently. Start a `nix-shell` in the IHP directory to fetch missing packages.

### Trouble adding packages to IHP

Either add the package to your project's `default.nix` as well, or change the section `ihp = builtins.fetchGit ...` to `ihp = ./IHP;`in your project's `default.nix`. Then the `IHP/ihp.nix` will be used to fetch packages.

### Reverting back to running with stock IHP library

If you want to go back and use the standard IHP library instead of the cloned source, for instance when your much needed, contributed and approved feature becomes part of the release, then in your project directory:

```
rm -rf IHP
rm build/ihp-lib
./start
```

## Resources

Practical resources to get you up to speed with tools IHP uses.

### Haskell

-   [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/)

### Nix

-   [Scrive Nix Workshop](https://scrive.github.io/nix-workshop/)
