# IHP

# Contributing

We are happy to merge your pull requests!

## Running an application against a local copy of the framework

To work on the framework in your application, you need to clone this repository inside your application directory:

```
git clone git@github.com:digitallyinduced/haskellframework.git IHP
```

The best workflow is to use `ghci` to load your application together with the framework version in `IHP`:

```
make ghci
:l Main
```

This will now load your application and all the haskell files in `IHP`.

As we are not using the development tooling for the framework development process, we need to manually start the postgres by running `postgres -D build/db/state -k $PWD/build/db -c "listen_addresses="` in another terminal window.

After postgres is started, you can now start the application with the local framework version by running:

```
main
```

After you have made modifications to files inside `IHP`, you need to press `CTRL + C` to stop the process running in `ghci` and then type `:r` to refresh the haskell modules. Now type `main` to start the server again.

## Code Guidelines

- Please use `pure` instead of `return`, as `return` might confuse people coming from other programing languages.