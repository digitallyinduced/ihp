# Contributing

We are happy to merge your pull requests!

## devenv

The IHP framework itself uses devenv.sh to set up the development environment. It is a wrapper around `nix develop` and `direnv`.

Follow the [devenv documentation](https://devenv.sh/getting-started/) to set it up.

## Running an application against a local copy of the framework

To work on the framework in your application, you need to clone this repository inside your application directory.
If you don't have a project, first make sure you have the latest version of `ihp-new`, update it by running: `nix-env -f https://downloads.digitallyinduced.com/ihp-new.tar.gz -i ihp-new`. Use `ihp-new` to create one. Make sure to run `devenv up` in your project once. Also make sure that [`direnv`](https://direnv.net/docs/hook.html) is set up in your shell.

```
# Set up a local IHP project
ihp-new myproject
cd myproject
```

Clone the IHP repository into the project directory. The `IHP` directory is added to the GHC search path in `applicationGhciConfig`. Therefore when the `IHP` directory exists, GHC will load all IHP modules from there.

```
git clone git@github.com:digitallyinduced/ihp.git IHP
cd IHP

# Patch flake.nix in the parent directory to switch
# from "ihp.url=..." to use the local IHP path as an input to your flake
sed -i "s|ihp.url = .*|ihp.url = \"path://$(pwd)\";|" ../flake.nix

# Note if you are on mac and direnv has not loaded you may have to alter the above patch :
# sed -i '.bak' -E "s|^([[:space:]]*)ihp\.url = \".*\";|\1ihp.url = \"path://$PWD\";|" ../flake.nix


# Enable direnv
direnv allow

# Switch back to the host project directory
cd -

# update your flake lock to make sure your flake references the local version of ihp.
nix flake update
```

### Running the development server

You can now run `devenv up` in the host project directory to start the development server. However, if you'd like to change the DevServer or JS files, we have to start the server differently, without `devenv up`. 
We need to run the following commands (assuming you are on the host project):

```
cd IHP

# Optional - you can set the `DEBUG` environment variable to enable debug logging
export DEBUG=1

ghci
:l ihp-ide/exe/IHP/IDE/DevServer.hs

# Start the server
mainInParentDirectory
```

We don't need to start postgres as the IDE starts it automatically.

If you ever switch back to using the upstream version, restore the original line in flake.nix (e.g., ihp.url = "github:digitallyinduced/ihp";), and then run from the host directory:

```
nix flake lock --update-input ihp
```

### Faster Haskell Builds

Uncomment the `configureFlags = [ "--flag FastBuild" ];` and `doHaddock = false;` lines in the `IHP/ihp.nix` for fast rebuilds, otherwise you could up waiting up to half an hour while IHP builds itself.

We need to make sure not to commit the changes of `IHP/ihp.nix`
To help us with that, you can run this from the root of your project `cd IHP && git update-index --assume-unchanged ihp.nix`, so git will ignore your changes.


## Working on the documentation

To work on the documentation locally, switch to the `IHP` directory. When you're doing this the first time, you need to run `direnv allow` to install necessary dependencies:

```
cd IHP
direnv allow
```

Then switch to the `Guide` directory and run `direnv allow` again:

```
cd Guide
direnv allow
```

To rebuild the html files from the markdown files on every file change run:

```
devenv up
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

## Branches

Since the switch to nix flakes with IHP v1.1 we're using release branches, to make it easy to upgrade IHP versions using `nix flake update`.

E.g. there's a [branch named `v1.1`](https://github.com/digitallyinduced/ihp/tree/v1.1) that contains the latest IHP v1.1.x release. When a new IHP v1.1.x release is made, we'll update the `v1.1` branch to point to the new release commit.

IHP apps have a `flake.nix` like this:

```nix
{
    inputs.ihp.url = "github:digitallyinduced/ihp/v1.1";
}
```

This means that whenever someone runs `nix flake update`, they'll get the latest commit from the IHP v1.1 branch.

This will also lead to a change of the `flake.lock` file in the project. This file ensures that IHP is consistently reproduced across machines, so users should also make sure to check this change into git.

To upgrade to a newer minor version, the URL can be changed to use IHP from the e.g. `v1.2` branch:

```nix
{
    inputs.ihp.url = "github:digitallyinduced/ihp/v1.2";
}
```

In the same way as with updating, running `nix flake update` after declaring a new version will update the `flake.lock` file to ensure consistency in all environments.

### New Releases

When we're preparing a new release, e.g. the IHP v1.2.0 release, we'll create a new branch `v1.2` and merge the current master into that branch.

Additionally every released version of IHP is tagged. [You can see a list of all tags on GitHub.](https://github.com/digitallyinduced/ihp/tags)


## Troubleshooting

### `error: Could not find module 'Generated.Types'`

You may receive this error when trying to load modules into GHCI like `:l Main`. Run `chmod go-w .` to change the permissions so the `.ghci` file could be loaded properly.

### Type errors in `build/Generated/Types.hs`

The errors comes from changes to the Schema Compiler, so `build/Generated/Types.hs` is outdated. The error only happens when we have backwards compatibility breaks in some of the interfaces, which doesn't happen very often. If you do get them you should execute from your appication's root directory (not the `IHP` directory)

```
ghci
:l IHP/IHP/SchemaCompiler.hs
compile
```

After this the error should be gone, and you can go back to `:l Main`

### Reverting back to running with stock IHP library

If you want to go back and use the standard IHP library instead of the cloned source, for instance when your much needed, contributed and approved feature becomes part of the release, then in your project directory:

```
rm -rf IHP
devenv up
```

## Resources

Practical resources to get you up to speed with tools IHP uses.

### Haskell

-   [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/)
-   [Implicit Parameters](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#implicit-parameters)

### Nix

-   [Scrive Nix Workshop](https://scrive.github.io/nix-workshop/)
