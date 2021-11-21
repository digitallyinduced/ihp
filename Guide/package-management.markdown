# Package Management

```toc

```

IHP is using the Nix Package Manager for managing its dependencies, such as Haskell packages, compilers, and even Postgres. You can find more about [the motivation on using nix on the IHP blog](https://ihp.digitallyinduced.com/blog/2020-07-22-why-ihp-is-using-nix.html).

## Using a Haskell Package

To install a Haskell package from Hackage (the standard Haskell package registry), open the `default.nix` file and append the package name.

Let's say we want to use [mmark](https://hackage.haskell.org/package/mmark) for rendering markdown in our project. The mmark library is not bundled with IHP, so we need to add this package as a dependency to our project. For that open the `default.nix`. The file will look like this:

```nix
let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        rev = "c6d40612697bb7905802f23b7753702d33b9e2c1";
    };
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        compiler = "ghc865";
        haskellDeps = p: with p; [
            cabal-install
            base
            wai
            text
            hlint
            p.ihp
        ];
        otherDeps = p: with p; [
        ];
        projectPath = ./.;
    };
in
    haskellEnv
```

In the list following `haskellDeps` we can see a few haskell dependencies already. We have to append `mmark` to the list:

```nix
let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        rev = "c6d40612697bb7905802f23b7753702d33b9e2c1";
    };
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        compiler = "ghc865";
        haskellDeps = p: with p; [
            cabal-install
            base
            wai
            text
            hlint
            p.ihp
            mmark
        ];
        otherDeps = p: with p; [
        ];
        projectPath = ./.;
    };
in
    haskellEnv
```

Run `nix-shell` to see that your project still builds fine. After that run `make -B .envrc` to rebuild the development environment used by IHP. Now you can run `./start` again to start the development server. The `mmark` can now be used as expected:

```bash
nix-shell
# All good? Proceed
make -B .envrc
./start
```

## Using a Native Dependency

Sometimes your project uses some other software tool that is not included with IHP by default. Because we're using nix, we can easily manage that dependency for our project.

Let's say we want to add [ImageMagick](https://imagemagick.org/) to transform and resize images uploaded by the users of our application.

All dependencies of our project are listed in `default.nix` at the root of the project directory. The file looks like this:

```nix
let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        rev = "c6d40612697bb7905802f23b7753702d33b9e2c1";
    };
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        compiler = "ghc865";
        haskellDeps = p: with p; [
            cabal-install
            base
            classy-prelude
            wai
            text
            hlint
            ihp
            wreq
        ];
        otherDeps = p: with p; [
        ];
        projectPath = ./.;
    };
in
    haskellEnv
```

We now just have to add `imagemagick` to `otherDeps`:

```nix
let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        rev = "c6d40612697bb7905802f23b7753702d33b9e2c1";
    };
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        compiler = "ghc865";
        haskellDeps = p: with p; [
            cabal-install
            base
            classy-prelude
            wai
            text
            hlint
            ihp
            wreq
        ];
        otherDeps = p: with p; [

            imagemagick # <-----------------------

        ];
        projectPath = ./.;
    };
in
    haskellEnv
```

If running, stop your development server. Now run `make -B .envrc`. This will install ImageMagick locally to your project.

When you are inside the project with your terminal, you can also call `imagemagick` to see that it's available.

You can look up the package name for the software you depend on inside the nixpkgs repository. [Just open it on GitHub](https://github.com/NixOS/nixpkgs) and use the GitHub search to look up the package name.

## Advanced

### Using a Different Version of a Haskell Package

Let's say we want to use [the google-oauth2 package from hackage](https://hackage.haskell.org/package/google-oauth2) to add Google OAuth to our application. We can install the package in our project by adding it to `haskellPackages` in our `default.nix`:

```nix
let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        rev = "c6d40612697bb7905802f23b7753702d33b9e2c1";
    };
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        compiler = "ghc865";
        haskellDeps = p: with p; [
            cabal-install
            base
            wai
            text
            hlint
            p.ihp
            google-oauth2
        ];
        otherDeps = p: with p; [
        ];
        projectPath = ./.;
    };
in
    haskellEnv
```

This will install version 0.3.0.0 of the google-oauth2 package, as this is the latest version available in the package set used by IHP. For our specific application requirements, we want to use version 0.2.2, an older version of this package.

To use the older version of the package we need to override the package definition. To do this you need to install install `cabal2nix` first:

```bash
nix-env -i cabal2nix
```

After cabal2nix is installed we can use it to get a nix package definition for the older version of google-oauth2:

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

Save this package definition code to a new file in `Config/nix/haskell-packages/google-oauth2.nix`. IHP projects are configured to automatically pick up any Haskell package definitions in the `Config/nix/haskell-packages` directory. So this package definition will be used automatically.

Go back to your project directory and run `nix-shell`. This will try to install the new `google-oauth2` package in the expected version `0.2.2`.

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

To jailbreak the package open `Config/nix/nixpkgs-config.nix` and append `"google-oauth2"` to the `doJailbreakPackages` list:

```nix
{ ihp }:
import "${toString ihp}/NixSupport/make-nixpkgs-from-options.nix" {
    ihp = ihp;
    haskellPackagesDir = ./haskell-packages/.;
    doJailbreakPackages = [ "google-oauth2" ];
}
```

After that try to run `nix-shell` again. This will most likely work now.

When the run of `nix-shell` succeeds, you also need to run `make -B .envrc` to rebuild the `.envrc` file. Otherwise, the new package might not be visible to all tools:

```bash
make -B .envrc
```

### Stopping Nix From Running Tests for a Haskell Dependency

Nix will try to run a test suite for a package when it's building it from source code. Sometimes the tests fail which will stop you from installing the package. In case you want to ignore the failing tests and use the package anyway follow these steps.

Open `Config/nix/nixpkgs-config.nix` and append the package name to the `dontCheckPackages` list:

```nix
{ ihp }:
import "${toString ihp}/NixSupport/make-nixpkgs-from-options.nix" {
    ihp = ihp;
    haskellPackagesDir = ./haskell-packages/.;
    dontCheckPackages = [ "my-failing-package" ]; # <------- ADD YOUR PACKAGE HERE
}
```

After that, you can do `nix-shell` without running the failing tests.

### Nixpkgs Pinning

All projects using IHP are using a specific pinned version of nixpkgs. [The specific version used is defined by IHP in `NixSupport/make-nixpkgs-from-options.nix`](https://github.com/digitallyinduced/ihp/blob/master/NixSupport/make-nixpkgs-from-options.nix#L9).

You can override the nixpkgs version by setting the `nixPkgsRev` and `nixPkgsSha256` to your custom values:

```nix
{ ihp }:
import "${toString ihp}/NixSupport/make-nixpkgs-from-options.nix" {
    ihp = ihp;
    haskellPackagesDir = ./haskell-packages/.;
    
    nixPkgsRev = "c7f75838c360473805afcf5fb2fa65e678efd94b"; # <---- SET THIS OPTION
    nixPkgsSha256 = "04vx1j2gybm1693a8wxw6bpcsd4h1jdw541vwic8nfm3n80r4ckm"; # <---- ALSO SET THIS
}
```

Run `make -B .envrc` to rebuild the development environment.

We highly recommend only using nixpkgs versions which are provided by IHP because these are usually verified to be working well with all the packages used by IHP. Additionally, you will need to build a lot of packages from source code as they will not be available in the digitally induced binary cache.

### Binary Cache

When installing IHP, the `ihp-new` tool will add the `digitallyinduced.cachix.org` binary cache to your nix system. This binary cache provides binaries for all IHP packages and commonly used dependencies for all nixpkgs versions used by IHP.

When you are using packages that are not in the binary cache and thus are compiled from source code very often, we highly recommend setting up your own [cachix binary cache](https://cachix.org/) and use it next to the default `digitallyinduced.cachix.org` cache.
