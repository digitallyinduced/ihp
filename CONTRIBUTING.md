# Contributing

We are happy to merge your pull requests!

## devenv

The IHP framework itself uses devenv.sh to set up the development environment. It is a wrapper around `nix-shell` and `direnv`.

Follow the [devenv documentation](https://devenv.sh/getting-started/) to set it up.

## Running an application against a local copy of the framework

To work on the framework in your application, you need to clone this repository inside your application directory.
If you don't have a project, first make sure you have the latest version of `ihp-new`, update it by running: `nix-env -f https://downloads.digitallyinduced.com/ihp-new.tar.gz -i ihp-new`. Use `ihp-new` to create one. Make sure to run `devenv up` in your project once. Also make sure that [`direnv`](https://direnv.net/docs/hook.html) is set up in your shell.

```
# Set up a local IHP project
ihp-new myproject
cd myproject
devenv up
```

Running `devenv up` is necessary, even if you don't intend to run it this way (e.g. you intend to do development and don't intend to run it "normally"), to do some initial setup like creating the database. You can keep `devenv up` running, as it will pick up any changes to the custom app or the IHP, and save you the hassle of manually recompiling from ghci on every change.

Clone the IHP repository into the project directory. The `IHP` directory is added to the GHC search path in applicationGhciConfig. Therefore when the `IHP` directory exists, GHC will load all IHP modules from there.

```
git clone git@github.com:digitallyinduced/ihp.git IHP
# Enable direnv
cd IHP
direnv allow
# Go back to the project root (do not run inside the IHP directory)
cd -
make -B build/ihp-lib
```

Uncomment the `configureFlags = [ "--flag FastBuild" ];` and `doHaddock = false;` lines in the `IHP/ihp.nix` for fast rebuilds, otherwise you could up waiting up to half an hour while IHP builds itself.

We need to make sure not to commit the changes of `IHP/ihp.nix`
To help us with that, you can run this from the root of your project `cd IHP && git update-index --assume-unchanged ihp.nix`, so git will ignore your changes.

It is important to update your custom `default.nix` file and set the `rev` to the latest commit every time you perform a `git pull` from within IHP. This is because certain components continue to use the version defined in `default.nix`, even if you have a local IHP.

### Alternative method

Another workflow, instead of the simpler `devenv up`, is to use `make console` to load your application together with the framework located in `IHP`. In a `nix-shell`:

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

To work on the documentation locally, switch to the `IHP` directory. When you're doing this the first time, you need to run `direnv allow` to install necessary dependencies:

```
cd IHP
direnv allow
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
ghci
:l Test/Main.hs
main
```

Note that if it's the first time you switch to the `IHP` directory, you need to run `direnv allow`. This will then download all dependencies needed for IHP development automatically.

When doing changes to the test files, use this to reload and rerun the tests:

```
:r
main
```

After creating a new test you need to still call it from the `Main` module by adding it to `IHP/Test/Main.hs`.

## Troubleshooting

### `can't satisfy package ihp`

If you get an error like `can't satisfy package ihp` or all other IHP packages when running `ghci` most likely the symlink in `build/ihp-lib` is not set up as expected. IHP uses the symlink `build/ihp-lib` in your application's `.ghci` file to access [`IHP/lib/IHP/applicationGhciConfig`](https://github.com/digitallyinduced/ihp/blob/master/lib/IHP/applicationGhciConfig#L39). This `applicationGhciConfig` sets up all the required options for `ghci`.

Try to run `make -B build/ihp-lib` to create the symlink.

### `error: Could not find module 'Generated.Types'`

You may receive this error when trying to load modules into GHCI like `:l Main`. Run `chmod go-w .` to change the permissions so the `.ghci` file could be loaded properly.

### Error messages about missing packages in `ghci`

Perhaps a package was added to IHP recently. Start a `nix-shell` in the IHP directory to fetch missing packages.

### Trouble adding packages to IHP

Either add the package to your project's `default.nix` as well, or change the section `ihp = builtins.fetchGit ...` to `ihp = ./IHP;`in your project's `default.nix`. Then the `IHP/ihp.nix` will be used to fetch packages.

### Type errors in `build/Generated/Types.hs`

The errors comes from changes to the Schema Compiler, so `build/Generated/Types.hs` is outdated. The error only happens when we have backwards compatibility breaks in some of the interfaces, which doesn't happen very often. If you do get them you should execute from your appication's root directory (not the `IHP` directory)

```
nix-shell
ghci
:l IHP/IHP/SchemaCompiler.hs
compile
```

After this the error should be gone, and you can go back to `:l Main`

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
-   [Implicit Parameters](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#implicit-parameters)

### Nix

-   [Scrive Nix Workshop](https://scrive.github.io/nix-workshop/)
