# Contributing

We are happy to merge your pull requests!

## Running an application against a local copy of the framework

To work on the framework in your application, you need to clone this repository inside your application directory. If you don't have a project, use `ihp-new` to create one. Make sure to run `./start` in your project once. Also make sure that [`direnv`](https://direnv.net/docs/hook.html) is set up in your shell.

```
# Set up a local IHP project
ihp-new ihp-test-project
cd ihp-test-project
./start

# Clone the IHP repository into the project directory
# The `IHP` directory is added to the GHC search path in applicationGhciConfig
# Therefore when the `IHP` directory exists, GHC will load all IHP modules from there
git clone git@github.com:digitallyinduced/ihp.git IHP
```

The best workflow is to use `ghci` to load your application together with the framework version in `IHP`. Then, in your app directory (NOT the IHP directory):

```
make -B build/ihp-lib # only needs to be run once
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

When making changes to the development tooling, follow the setup above. Instead of starting your application, start the development server:

```
ghci
:l IHP/exe/IDE/DevServer.hs
main
```

#### Debugging the development server

You can enable additonal debug logging for the development server by setting the env variable `DEBUG=1`. Like this:

```
export DEBUG=1
ghci
:l IHP/exe/IDE/DevServer.hs
main
```

## Working on the documentation

To work on the documentation locally open a nix shell inside the framework directory:

```
cd IHP
nix-shell NixSupport/shell.nix
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

## Code Guidelines

- Please use `pure`. `return` might confuse people coming from other programing languages.

## Troubleshooting

### `can't satisify package ihp`

If you get an error like `can't satisify package ihp` or all other IHP packages when running `ghci` most likely the symlink in `build/ihp-lib` is not set up as expected. IHP uses the symlink `build/ihp-lib` in your application's `.ghci` file to access [`IHP/lib/IHP/applicationGhciConfig`](https://github.com/digitallyinduced/ihp/blob/master/lib/IHP/applicationGhciConfig#L39). This `applicationGhciConfig` sets up all the required options for `ghci`.

Try to run `make -B build/ihp-lib` to create the symlink.
