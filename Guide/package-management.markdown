# Package Management

```toc

```

IHP uses the Nix Package Manager for managing its dependencies, such as Haskell packages, compilers, and even Postgres. You can find more about [the motivation on using nix on the IHP blog](https://ihp.digitallyinduced.com/blog/2020-07-22-why-ihp-is-using-nix.html).

Internally IHP uses [devenv.sh](https://devenv.sh/) together with [Nix flakes](https://devenv.sh/guides/using-with-flakes/).

## Using a Haskell Package

To install a Haskell package from Hackage (the standard Haskell package registry), open the `flake.nix` file and append the package name.

Let's say we want to use [mmark](https://hackage.haskell.org/package/mmark) for rendering markdown in our project. The mmark library is not bundled with IHP, so we need to add this package as a dependency to our project. For that open the `flake.nix`. The file will look like this:

```nix
{
    inputs = {
        ihp.url = "github:digitallyinduced/ihp/1.1";
        nixpkgs.follows = "ihp/nixpkgs";
        flake-parts.follows = "ihp/flake-parts";
        devenv.follows = "ihp/devenv";
        systems.follows = "ihp/systems";
    };

    outputs = inputs@{ ihp, flake-parts, systems, ... }:
        flake-parts.lib.mkFlake { inherit inputs; } {

            systems = import systems;
            imports = [ ihp.flakeModules.default ];

            perSystem = { pkgs, ... }: {
                ihp = {
                    enable = true;
                    projectPath = ./.;
                    packages = with pkgs; [
                        # Native dependencies, e.g. imagemagick
                    ];
                    haskellPackages = p: with p; [
                        # Haskell dependencies go here
                        p.ihp
                        cabal-install
                        base
                        wai
                        text
                        hlint
                    ];
                };
            };

        };
}
```

In the list following `haskellPackages` we can see a few haskell dependencies already. We have to append `mmark` to the list:

```nix
{
    inputs = {
        ihp.url = "github:digitallyinduced/ihp/1.1";
        nixpkgs.follows = "ihp/nixpkgs";
        flake-parts.follows = "ihp/flake-parts";
        devenv.follows = "ihp/devenv";
        systems.follows = "ihp/systems";
    };

    outputs = inputs@{ ihp, flake-parts, systems, ... }:
        flake-parts.lib.mkFlake { inherit inputs; } {

            systems = import systems;
            imports = [ ihp.flakeModules.default ];

            perSystem = { pkgs, ... }: {
                ihp = {
                    enable = true;
                    projectPath = ./.;
                    packages = with pkgs; [
                        # Native dependencies, e.g. imagemagick
                    ];
                    haskellPackages = p: with p; [
                        # Haskell dependencies go here
                        p.ihp
                        cabal-install
                        base
                        wai
                        text
                        hlint
                        mmark
                    ];
                };
            };

        };
}
```

Stop the development server by hitting CTRL + C. Your terminal should now automatically start rebuilding the development environment. This is triggered by `direnv` detecting that the `flake.nix` has changed.

Run `devenv up` again to start the development server, and `mmark` should now be used as expected.


## Using a Native Dependency

Sometimes your project uses some other software tool that is not included with IHP by default. Because we're using nix, we can easily manage that dependency for our project.

Let's say we want to add [ImageMagick](https://imagemagick.org/) to transform and resize images uploaded by the users of our application.

All dependencies of our project are listed in `flake.nix` at the root of the project directory. The file looks like this:

```nix
{
{
    inputs = {
        ihp.url = "github:digitallyinduced/ihp/1.1";
        nixpkgs.follows = "ihp/nixpkgs";
        flake-parts.follows = "ihp/flake-parts";
        devenv.follows = "ihp/devenv";
        systems.follows = "ihp/systems";
    };

    outputs = inputs@{ ihp, flake-parts, systems, ... }:
        flake-parts.lib.mkFlake { inherit inputs; } {

            systems = import systems;
            imports = [ ihp.flakeModules.default ];

            perSystem = { pkgs, ... }: {
                ihp = {
                    enable = true;
                    projectPath = ./.;
                    packages = with pkgs; [
                        # Native dependencies, e.g. imagemagick
                    ];
                    haskellPackages = p: with p; [
                        # Haskell dependencies go here
                        p.ihp
                        cabal-install
                        base
                        wai
                        text
                        hlint
                    ];
                };
            };

        };
}
}
```

We now just have to add `imagemagick` to `packages`:

```nix
{
    inputs = {
        ihp.url = "github:digitallyinduced/ihp/1.1";
        nixpkgs.follows = "ihp/nixpkgs";
        flake-parts.follows = "ihp/flake-parts";
        devenv.follows = "ihp/devenv";
        systems.follows = "ihp/systems";
    };

    outputs = inputs@{ ihp, flake-parts, systems, ... }:
        flake-parts.lib.mkFlake { inherit inputs; } {

            systems = import systems;
            imports = [ ihp.flakeModules.default ];

            perSystem = { pkgs, ... }: {
                ihp = {
                    enable = true;
                    projectPath = ./.;
                    packages = with pkgs; [
                        # Native dependencies, e.g. imagemagick
                        imagemagick
                    ];
                    haskellPackages = p: with p; [
                        # Haskell dependencies go here
                        p.ihp
                        cabal-install
                        base
                        wai
                        text
                        hlint
                    ];
                };
            };

        };
}
```

Stop the development server by hitting CTRL + C. Your terminal should now automatically start rebuilding the development environment and install ImageMagick. This is triggered by `direnv` detecting that the `flake.nix` has changed.

When you are inside the project with your terminal, you can also call `imagemagick` to see that it's available.

You can look up the package name for the software you depend on inside the nixpkgs repository. [Just open it on GitHub](https://github.com/NixOS/nixpkgs) and use the GitHub search to look up the package name.

## Advanced

### Using a Different Version of a Haskell Package

Let's say we want to use [the google-oauth2 package from hackage](https://hackage.haskell.org/package/google-oauth2) to add Google OAuth to our application. We can install the package in our project by adding it to `haskellPackages` in our `flake.nix`:

```nix
{
    inputs = {
        ihp.url = "github:digitallyinduced/ihp/1.1";
        nixpkgs.follows = "ihp/nixpkgs";
        flake-parts.follows = "ihp/flake-parts";
        devenv.follows = "ihp/devenv";
        systems.follows = "ihp/systems";
    };

    outputs = inputs@{ ihp, flake-parts, systems, ... }:
        flake-parts.lib.mkFlake { inherit inputs; } {

            systems = import systems;
            imports = [ ihp.flakeModules.default ];

            perSystem = { pkgs, ... }: {
                ihp = {
                    enable = true;
                    projectPath = ./.;
                    packages = with pkgs; [
                        # Native dependencies, e.g. imagemagick
                    ];
                    haskellPackages = p: with p; [
                        # Haskell dependencies go here
                        p.ihp
                        cabal-install
                        base
                        wai
                        text
                        hlint
                        google-oauth2
                    ];
                };
            };

        };
}
```

This will install version 0.3.0.0 of the google-oauth2 package, as this is the latest version available in the package set used by IHP. For our specific application requirements, we want to use version 0.2.2, an older version of this package.

The package definition comes from nixpkgs. To override it, we will point our `flake.nix` to our own fork of nixpkgs. First (with the default nixpkgs in `flake.nix`) we find the *current* nixpkgs revision for our version of IHP by running `nix flake metadata --inputs-from . nixpkgs`:

```sh
$ nix flake metadata --inputs-from . nixpkgs
Resolved URL:  github:NixOS/nixpkgs/9cb344e96d5b6918e94e1bca2d9f3ea1e9615545?narHash=sha256-gKlP0LbyJ3qX0KObfIWcp5nbuHSb5EHwIvU6UcNBg2A%3D
Locked URL:    github:NixOS/nixpkgs/9cb344e96d5b6918e94e1bca2d9f3ea1e9615545?narHash=sha256-gKlP0LbyJ3qX0KObfIWcp5nbuHSb5EHwIvU6UcNBg2A%3D
Description:   A collection of packages for the Nix package manager
Revision:      9cb344e96d5b6918e94e1bca2d9f3ea1e9615545
Last modified: 2025-08-20 17:33:59
Fingerprint:   a7682548550237130fe9f60275c2be495005dce98ced24e788bc9b3bf74fc91f
```

This tells us that IHP uses nixpkgs at revision `9cb344e96d5b6918e94e1bca2d9f3ea1e9615545`. We will now create a shallow clone at that revision (nixpkgs is big and shallow clones are faster):

1. Hit fork on https://github.com/NixOS/nixpkgs and open https://github.com/YOURUSER/nixpkgs/tree/9cb344e96d5b6918e94e1bca2d9f3ea1e9615545

2. Click the tag/branch selector and give it a name like `ihp-1.4` and click the "Create branch ihp-1.4 from c6a788f" button

3. Shallow clone your fork at that branch, and give it some new name for the changes you will put on top:

   ```bash
   git clone --depth 3 -b ihp-1.4 https://github.com/YOURUSER/nixpkgs
   cd nixpkgs
   git checkout -b downgraded-google-oauth2
   ```

To use the older version of the package we need to override the package definition. To do this, enter a temporary Nix shell with `cabal2nix` and `cabal-install`:

```bash
nix-shell -p cabal2nix -p cabal-install
```

Now inside this shell, we can use `cabal2nix` to get a nix package definition for the older version of google-oauth2:

```bash
cabal2nix cabal://google-oauth2-0.2.2
```

(You may need to run `cabal update` first, so that your local cabal cache of Hackage is updated.)

This will output a new nix package definition like this:

```nix
{ mkDerivation, aeson, base, bytestring, hspec, HTTP, http-conduit
, http-types, load-env, stdenv
}:
mkDerivation {
  pname = "google-oauth2";
  version = "0.2.2";
  sha256 = "0n408kh48d7ky09j9zw9ad4mhbv1v7gq6i3ya4f6fhkjqqgw8c1j";
  libraryHaskellDepends = [
    aeson base bytestring HTTP http-conduit
  ];
  testHaskellDepends = [
    base bytestring hspec http-conduit http-types load-env
  ];
  description = "Google OAuth2 token negotiation";
  license = stdenv.lib.licenses.mit;
}
```

Now open `pkgs/development/haskell-modules/hackage-packages.nix` and look for the line

```nix
  "google-oauth2" = callPackage (
```

Remove the contents of `callPackage(…)` and instead insert the package definition you got from `cabal2nix`, giving e.g.

```nix
  "google-oauth2" = callPackage (
{ mkDerivation, aeson, base, bytestring, hspec, HTTP, http-conduit
, http-types, load-env, stdenv
}:
mkDerivation {
  pname = "google-oauth2";
  version = "0.2.2";
  sha256 = "0n408kh48d7ky09j9zw9ad4mhbv1v7gq6i3ya4f6fhkjqqgw8c1j";
  libraryHaskellDepends = [
    aeson base bytestring HTTP http-conduit
  ];
  testHaskellDepends = [
    base bytestring hspec http-conduit http-types load-env
  ];
  description = "Google OAuth2 token negotiation";
  license = stdenv.lib.licenses.mit;
}
  ) { };
```

Go back to your project directory and run `nix flake update`. This will try to install the new `google-oauth2` package in the expected version `0.2.2`.

This step might fail with an error like `Encountered missing or private dependencies`:

```nix
$ nix-shell
these derivations will be built:
  /nix/store/9a0bnhr5fzj0a40g72j2sb1sdbcpfavj-google-oauth2-0.2.2.drv
  /nix/store/nngc65qyw3bdf6zn84ca0jpxz19mmcv6-ghc-8.8.3-with-packages.drv
building '/nix/store/9a0bnhr5fzj0a40g72j2sb1sdbcpfavj-google-oauth2-0.2.2.drv'...
setupCompilerEnvironmentPhase
Build with /nix/store/102df4ic8qmmc6qqq33wwppizk9pnj1s-ghc-8.8.3.
unpacking sources
unpacking source archive /nix/store/w028wlk7f2q5axsw94f5igdbyfq982pl-google-oauth2-0.2.2.tar.gz
source root is google-oauth2-0.2.2
setting SOURCE_DATE_EPOCH to timestamp 1479590674 of file google-oauth2-0.2.2/test/Spec.hs
patching sources
compileBuildDriverPhase
setupCompileFlags: -package-db=/private/var/folders/27/4cznr1bj5yd0mq5sbf3qtgv40000gn/T/nix-build-google-oauth2-0.2.2.drv-0/setup-package.conf.d -j4 -threaded
[1 of 1] Compiling Main             ( Setup.hs, /private/var/folders/27/4cznr1bj5yd0mq5sbf3qtgv40000gn/T/nix-build-google-oauth2-0.2.2.drv-0/Main.o )
Linking Setup ...
configuring
configureFlags: --verbose --prefix=/nix/store/ba7dpyhns6k6a9q6s9faxg3wc389y22b-google-oauth2-0.2.2 --libdir=$prefix/lib/$compiler --libsubdir=$abi/$libname --docdir=/nix/store/s6xdqq2wbqs006f29xkyhivrg9cjp6v5-google-oauth2-0.2.2-doc/share/doc/google-oauth2-0.2.2 --with-gcc=clang --package-db=/private/var/folders/27/4cznr1bj5yd0mq5sbf3qtgv40000gn/T/nix-build-google-oauth2-0.2.2.drv-0/package.conf.d --ghc-option=-j4 --disable-split-objs --enable-library-profiling --profiling-detail=exported-functions --disable-profiling --enable-shared --disable-coverage --enable-static --disable-executable-dynamic --disable-tests --disable-benchmarks --enable-library-vanilla --disable-library-for-ghci --extra-include-dirs=/nix/store/4i88rry2ckc928by1dzg0q9w98c9n5dr-libc++-7.1.0/include --extra-lib-dirs=/nix/store/4i88rry2ckc928by1dzg0q9w98c9n5dr-libc++-7.1.0/lib --extra-include-dirs=/nix/store/r983c8mry11l7nlnnlyfd67kic2y2dym-libc++abi-7.1.0/include --extra-lib-dirs=/nix/store/r983c8mry11l7nlnnlyfd67kic2y2dym-libc++abi-7.1.0/lib --extra-include-dirs=/nix/store/42k02ckpn4z28fpz327pr2j5h04nm8sy-compiler-rt-7.1.0-dev/include --extra-lib-dirs=/nix/store/3247xyxxpjnv876fxf7py914dd9qhsgn-compiler-rt-7.1.0/lib --extra-lib-dirs=/nix/store/3wiasc9rkx49pijdb85kkys3plhcixdb-ncurses-6.1-20190112/lib --extra-lib-dirs=/nix/store/w1v69v7w418nvsc3rxndp4cq59110bzj-libffi-3.3/lib --extra-lib-dirs=/nix/store/hrmqighslwzk32rmz00k1lw5k53f5jr3-gmp-6.2.0/lib --extra-include-dirs=/nix/store/m3yqw7xcmhvd0cmm3jh3b469kxf3yhfp-libiconv-osx-10.12.6/include --extra-lib-dirs=/nix/store/m3yqw7xcmhvd0cmm3jh3b469kxf3yhfp-libiconv-osx-10.12.6/lib --extra-framework-dirs=/nix/store/jj3pnq7i1ipd5x33xrsii3r26mdvcmir-swift-corefoundation/Library/Frameworks
Using Parsec parser
Configuring google-oauth2-0.2.2...
CallStack (from HasCallStack):
  die', called at libraries/Cabal/Cabal/Distribution/Simple/Configure.hs:1022:20 in Cabal-3.0.1.0:Distribution.Simple.Configure
  configureFinalizedPackage, called at libraries/Cabal/Cabal/Distribution/Simple/Configure.hs:475:12 in Cabal-3.0.1.0:Distribution.Simple.Configure
  configure, called at libraries/Cabal/Cabal/Distribution/Simple.hs:625:20 in Cabal-3.0.1.0:Distribution.Simple
  confHook, called at libraries/Cabal/Cabal/Distribution/Simple/UserHooks.hs:65:5 in Cabal-3.0.1.0:Distribution.Simple.UserHooks
  configureAction, called at libraries/Cabal/Cabal/Distribution/Simple.hs:180:19 in Cabal-3.0.1.0:Distribution.Simple
  defaultMainHelper, called at libraries/Cabal/Cabal/Distribution/Simple.hs:116:27 in Cabal-3.0.1.0:Distribution.Simple
  defaultMain, called at Setup.hs:2:8 in main:Main
Setup: Encountered missing or private dependencies:
aeson >=0.8 && <0.12

builder for '/nix/store/9a0bnhr5fzj0a40g72j2sb1sdbcpfavj-google-oauth2-0.2.2.drv' failed with exit code 1
cannot build derivation '/nix/store/nngc65qyw3bdf6zn84ca0jpxz19mmcv6-ghc-8.8.3-with-packages.drv': 1 dependencies couldn't be built
error: build of '/nix/store/nngc65qyw3bdf6zn84ca0jpxz19mmcv6-ghc-8.8.3-with-packages.drv' failed
```

This is usually caused by a version mismatch between what the package expects and what is given by nix. You can disable version checking by "jailbreaking" the package.

To jailbreak the package open `flake.nix` and append `"google-oauth2"` to the `doJailbreakPackages` list:

```nix
{
    inputs = {
        ihp.url = "github:digitallyinduced/ihp/1.1";
        nixpkgs.follows = "ihp/nixpkgs";
        flake-parts.follows = "ihp/flake-parts";
        devenv.follows = "ihp/devenv";
        systems.follows = "ihp/systems";
    };

    outputs = inputs@{ ihp, flake-parts, systems, ... }:
        flake-parts.lib.mkFlake { inherit inputs; } {

            systems = import systems;
            imports = [ ihp.flakeModules.default ];

            perSystem = { pkgs, ... }: {
                ihp = {
                    enable = true;
                    projectPath = ./.;
                    packages = with pkgs; [
                        # Native dependencies, e.g. imagemagick
                    ];
                    haskellPackages = p: with p; [
                        # Haskell dependencies go here
                        p.ihp
                        cabal-install
                        base
                        wai
                        text
                        hlint
                    ];
                    doJailbreakPackages = [ "google-oauth2" ];
                };
            };

        };
}
```

After that try to run `devenv up`.

### Building Postgres With Extensions

For some applications you may want to install custom postgres extension
libraries and have them available in the nix store.

For example to enable the [postgis](https://postgis.net/) spatial
and geographic objects in PostgreSQL add
`services.postgres.extensions = extensions: [ extensions.postgis ];` to your project's `flake.nix`:


```nix
{
    inputs = {
        ihp.url = "github:digitallyinduced/ihp/v1.4";
        nixpkgs.follows = "ihp/nixpkgs";
        flake-parts.follows = "ihp/flake-parts";
        devenv.follows = "ihp/devenv";
        systems.follows = "ihp/systems";
    };

    outputs = inputs@{ ihp, flake-parts, systems, nixpkgs, ... }:
        flake-parts.lib.mkFlake { inherit inputs; } {
            systems = import systems;
            imports = [ ihp.flakeModules.default ];

            perSystem = { pkgs, ... }: {
                ihp = {
                    inherit appName;
                    enable = true;
                    projectPath = ./.;
                    packages = with pkgs; [];
                    haskellPackages = p: with p; [
                        # ...
                    ];
                };
                devenv.shells.default = {
                    services.postgres.extensions = extensions: [ extensions.postgis ];
                };
            };
        };
}

```

Behind the scenes this will pass a function to the postgres nix expressions `postgresql.withPackages`
function making the extension in your app's nix store postgres package.

After the install you can run `CREATE EXTENSION postgis;` to enable all the features of the
installed extension.

### Stopping Nix From Running Tests for a Haskell Dependency

Nix will try to run a test suite for a package when it's building it from source code. Sometimes the tests fail which will stop you from installing the package. In case you want to ignore the failing tests and use the package anyway follow these steps.

Open `flake.nix` and append the package name to the `dontCheckPackages` list:

```nix
{
    inputs = {
        ihp.url = "github:digitallyinduced/ihp/1.1";
        nixpkgs.follows = "ihp/nixpkgs";
        flake-parts.follows = "ihp/flake-parts";
        devenv.follows = "ihp/devenv";
        systems.follows = "ihp/systems";
    };

    outputs = inputs@{ ihp, flake-parts, systems, ... }:
        flake-parts.lib.mkFlake { inherit inputs; } {

            systems = import systems;
            imports = [ ihp.flakeModules.default ];

            perSystem = { pkgs, ... }: {
                ihp = {
                    enable = true;
                    projectPath = ./.;
                    packages = with pkgs; [
                        # Native dependencies, e.g. imagemagick
                    ];
                    haskellPackages = p: with p; [
                        # Haskell dependencies go here
                        p.ihp
                        cabal-install
                        base
                        wai
                        text
                        hlint
                    ];
                    dontCheckPackages = [ "my-failing-package" ]; # <------- ADD YOUR PACKAGE HERE
                };
            };

        };
}
```

After that, you can do `nix flake update` without running the failing tests.

### Nixpkgs Pinning

By default, all projects use a specific version of nixpkgs pinned by IHP. You can override the nixpkgs version by replacing `nixpkgs.follows` with `nixpkgs.url` and your custom values:

```nix
{
    inputs = {
        ihp.url = "github:digitallyinduced/ihp/1.1";
        nixpkgs.url = "github:NixOS/nixpkgs?rev=PUT YOUR CUSTOM REVISION HERE";
        flake-parts.follows = "ihp/flake-parts";
        devenv.follows = "ihp/devenv";
        systems.follows = "ihp/systems";
    };

    # ...
}
```

Run `nix flake update` to rebuild the development environment.

We highly recommend only using nixpkgs versions which are provided by IHP because these are usually verified to be working well with all the packages used by IHP. Additionally, you will need to build a lot of packages from source code as they will not be available in the digitally induced binary cache.

### Binary Cache

When installing IHP, the `ihp-new` tool will add the `digitallyinduced.cachix.org` binary cache to your nix system. This binary cache provides binaries for all IHP packages and commonly used dependencies for all nixpkgs versions used by IHP.

When you are using packages that are not in the binary cache and thus are compiled from source code very often, we highly recommend setting up your own [cachix binary cache](https://cachix.org/) and use it next to the default `digitallyinduced.cachix.org` cache.
