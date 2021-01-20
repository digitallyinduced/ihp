## Troubleshooting

This page helps you to solve a few common issues with IHP. In the long term, we should find solutions for these issues so that they don't appear every again. Until then, we have this page so that you can find a solution when you paste the error message into Google.

In case your problem cannot be solved with the steps on this page, [join our awesome Gitter community](https://gitter.im/digitallyinduced/ihp?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge). We're happy to help!

## Auto Troubleshooting

Run this to automatically check for the most common IHP issues:

```bash
curl --silent https://raw.githubusercontent.com/digitallyinduced/ihp/master/Troubleshoot/ihp-troubleshoot | python3
```

It will tell you what is wrong and the steps you need to do to fix it.

## Common Issues

### Makefile:7: /../lib/IHP/Makefile.dist: No such file or directory

**Problem:**

Somehow the project was not correctly linked with IHP and now the project Makefile is unable to access some IHP files it needs.

**Solution:**

Run this command to fix the missing link:

```bash
nix-shell --run 'make build/ihp-lib'
```

**If this is not working:** Please run `which RunDevServer`. If this returns `RunDevServer not found`, this indicates that `direnv` (which has been installed by `ihp-new`) is not loading the `.envrc` file of the

To solve this, run the following commands:

**Bash Users**

```bash
echo 'eval "$(direnv hook bash)"' >> ~/.bashrc
```

**ZSH Users (default on macOS)**

```bash
echo 'eval "$(direnv hook zsh)"' >> ~/.zshrc
```

Other shells: [take a look at the direnv documentation](https://direnv.net/#README).

After that restart your terminal. Then run the following command again to fix the missing link:

```bash
nix-shell --run 'make build/ihp-lib'
```

Now, this should succeed.

### Running `ihp-new` builds a lot of Haskell packages and takes hours to complete

**Problem:**

When you try to start a new IHP project by running `ihp-new someproject`, nix seems to start compiling a lot of Haskell packages with GHC.

**Solution:**

This is most likely caused by a change we did to our binary cache. Run `cachix use digitallyinduced` to fix this.

### warning: substituter 'https://digitallyinduced.cachix.org' does not have a valid signature for path ...

**Problem:**

When running `ihp-new someproject` to create a new project, nix seems to not use the binary cache with the above warning. Instead, it tries to build all the packages itself, which takes hours.

**Solution:**

This is caused by a change we did to our binary cache. Open `~/.config/nix/nix.conf`. There take a look at the `trusted-public-keys =` property. You should have two entries there (the public key of our old binary cache, and the new one). Remove the old one `digitallyinduced.cachix.org-1:3mGU1b6u5obFp2VUfI55Xe8/+mawl7y9Eztu3rb94PI= ` (should be first of the two digitallyinduced keys). Then save the file and the problem is fixed.

### error: file '/build/db/.s.PGSQL.5432' has an unsupported type

**Problem:**

When you try to start a `nix-shell`, an error like this happens:

```bash
$ ./start
error: file '/build/db/.s.PGSQL.5432' has an unsupported type
```

**Solution:**

This happens because nix cannot use the Postgres Unix socket which is in `build/db`. Usually, this happens when the development server of IHP is still running. Stop your IHP development server and then try again.

When the development server is not running, check that you have no Postgres processes belonging to this Unix socket running anymore. In case there are no more processes running, try to remove the file via `rm -f`.

### `Unbound implicit parameter (?modelContext::ModelContext)`

The full error message looks like this:

```bash
MyModule.hs:7:29: error:
    * Unbound implicit parameter (?modelContext::ModelContext)
        arising from a use of `fetch'
    * In the second argument of `(|>)', namely `fetch'
      In a stmt of a 'do' block: users <- query @User |> fetch
      In the expression:
        do users <- query @User |> fetch
           return users
  |
7 |     users <- query @User |> fetch
```

**Problem:**

IHP uses [an implicit parameter](https://ocharles.org.uk/posts/2014-12-11-implicit-params.html) called `?modelContext` to pass around the database connection. The `?` question mark is part of the variable name. When you do a database query from a helper function outside of your controller actions the database connection needs to be passed to your function. This works automatically because it's an implicit parameter but needs to be specified inside your type signature.

**Solution:**

Add `(?modelContext :: ModelContext) =>` to the start of your type signature.

```haskell
-- OLD:
fetchUser :: Text -> IO [User]
fetchUser name = do
    users <- query @User |> fetch
    return users

-- NEW:
fetchUser :: (?modelContext :: ModelContext) => Text -> IO [User]
fetchUser name = do
    users <- query @User |> fetch
    return users
```

### Creating a new IHP project with ihp-new yields git error

When you followed the steps as described on the getting started page (i.e. installing Nix and IHP) but one of the steps
erred during installation (e.g. connection issues, privileges not set correctly) the cache utilized by nix might not
be properly initialized. This can manifest itself as a git error (output below) when running `ihp-new <project-name>`.

```
We will now create your project. This may take up to 30 seconds.
fatal: not a git repository (or any of the parent directories): .git
fatal: not a git repository (or any of the parent directories): .git
error: program 'git' failed with exit code 128
```

If you have a "fresh" nix install, this can be solved by removing your `~/.cache/nix` directory completely.

```
rm -rf ~/.cache/nix
```

After removing your nix cache, running `ihp-new <project-name>` will remain silent for a while since it will
re-cache a substantial number of nix dependencies.

### Ubuntu on Windows Subsystem for Linux (WSL): `ghc-pkg: Couldn't open database /nix/store/gsbpadkxlbswrfva70i8g8lpp2yw967z-ghc-8.10.3-with-packages/lib/ghc-8.10.3/package.conf.d for modification: {handle: /nix/store/gsbpadkxlbswrfva70i8g8lpp2yw967z-ghc-8.10.3-with-packages/lib/ghc-8.10.3/package.conf.d/package.cache.lock}: hLock: invalid argument (Invalid argument)`

**Problem:**

When you run `ihp-new blog` to create a new project on a system using Ubuntu running on Windows Subsystem for Linux 1, you get the above error.

**Solution:**

Switch to WSL 2.
