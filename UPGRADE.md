## Upgrading IHP Versions
This document describes breaking changes, as well as how to fix them, that have occured at given releases.
After updating your project, please consult the segments from your current release until now.

# Upgrade to 1.4.0 from 1.3.0

## Switch IHP version

1. **Switch IHP version**

    - **IHP Basic**

        Open `flake.nix` and change the git commit in line 3 to the following:

        ```diff
        -        ihp.url = "github:digitallyinduced/ihp/v1.3";
        +        ihp.url = "github:digitallyinduced/ihp/v1.4";
        ```

    - **IHP Pro & IHP Business**

        Visit https://ihp.digitallyinduced.com/Builds and copy the latest v1.4 URL into your `flake.nix`.

2. **Remake Env**

    Run the following commands:

    ```bash
    nix flake update ihp
    direnv reload
    ```

    Now you can start your project as usual with `devenv up`.

## Use the new `ihp-migrate` package for database migrations

The `migrate` command now has its own package `ihp-migrate`, so it is no longer available by default in the devshell or `nix-shell`. To make it available again, you can add the package to `flake.nix`:
```diff
                     haskellPackages = p: with p; [
                         # Haskell dependencies go here
                         p.ihp
+                        p.ihp-migrate
                         cabal-install
```

However, for running it when deploying from Github Actions, it may be faster to add a devshell containing just that package. Put

```nix
                devShells.ihp-migrate = pkgs.mkShell {
                  buildInputs = [
                    pkgs.ghc.ihp-migrate
                  ];
                };

```
in `flake.nix` in the `perSystem` block (next to the `ihp` block). Then you can run it as `nix develop .#ihp-migrate --command migrate`.

## Use nixpkgs instead of `Config/nix/haskell-packages`

IHP now expects package overrides to be defined in nixpkgs, so if you had any files in `Config/nix/haskell-packages` they will be ignored in 1.4. The procedure is now to fork nixpkgs, do the changes there, and point your `flake.nix` at the fork. But the fork should start from the nixpkgs commit that this IHP version was based off.

IHP 1.4 by default uses nixpkgs revision `9cb344e96d5b6918e94e1bca2d9f3ea1e9615545` (you can find this with `nix flake metadata --inputs-from . nixpkgs` if you have the default `nixpkgs` in your `flake.nix`). So to convert your `Config/nix/haskell-packages` you can

1. Hit fork on https://github.com/NixOS/nixpkgs and open https://github.com/YOURUSER/nixpkgs/tree/9cb344e96d5b6918e94e1bca2d9f3ea1e9615545

2. Click the tag/branch selector and give it a name like `ihp-1.4` and click the "Create branch ihp-1.4 from c6a788f" button

3. Shallow clone your fork at that branch (nixpkgs is big and shallow clone is faster), and give it some new name for the changes you will put on top:

   ```bash
   git clone --depth 3 -b ihp-1.4 https://github.com/YOURUSER/nixpkgs
   cd nixpkgs
   git checkout -b my-changes-to-ihp-1.4
   ```

4. Edit `pkgs/development/haskell-modules/hackage-packages.nix` and insert the contents from your old `haskell-packages` For example, if you wanted to update HPDF, you would I look for `"HPDF"` in `hackage-packages.nix` and replace the existing entry with what you had in `Config/nix/haskell-packages/HPDF.nix` (or use the output of `cabal2nix cabal://HPDF` as described in [package management in the Guide](https://ihp.digitallyinduced.com/Guide/package-management.html#using-a-different-version-of-a-haskell-package).).

5. Commit and push those changes to your fork, and take note of the revision of your fork

6. Put that commit in `nixpkgs.url` in your `flake.nix` and comment out `nixpkgs.follows`, e.g.

   ```diff
   -        nixpkgs.follows = "ihp/nixpkgs"
   +        # nixpkgs.follows = "ihp/nixpkgs"; # Overridden to upgrade HPDF:
   +        nixpkgs.url = "github:YOURUSER/nixpkgs?rev=YOURREVISION";
   ```

7. Delete the old package definitions: `git rm -r Config/nix/haskell-packages`



# Upgrade to 1.3.0 from 1.2.0
1. **Switch IHP version**

    - **IHP Basic**

        Open `flake.nix` and change the git commit in line 3 to the following:

        ```diff
        -        ihp.url = "github:digitallyinduced/ihp/v1.2";
        +        ihp.url = "github:digitallyinduced/ihp/v1.3";
        ```

    - **IHP Pro & IHP Business**

        Visit https://ihp.digitallyinduced.com/Builds and copy the latest v1.3.0 URL into your `flake.nix`.

2. **Remake Env**

    Run the following commands:

    ```bash
    direnv reload
    ```

    Now you can start your project as usual with `devenv up`.

# Upgrade to 1.2.0 from 1.1.0
1. **Switch IHP version**

    - **IHP Basic**

        Open `flake.nix` and change the git commit in line 3 to the following:

        ```diff
        -        ihp.url = "github:digitallyinduced/ihp/v1.1";
        +        ihp.url = "github:digitallyinduced/ihp/v1.2";
        ```

    - **IHP Pro & IHP Business**

        Visit https://ihp.digitallyinduced.com/Builds and copy the latest v1.2.0 URL into your `flake.nix`.

2. **Remake Env**

    Run the following commands:

    ```bash
    direnv reload
    ```

    Now you can start your project as usual with `devenv up`.


# Upgrade to 1.1.0 from 1.0.1

This version switch the IHP development environment to use devenv.sh. devenv.sh is a nix-based development environment that is similar to IHP's one, but is more general (e.g. you can use devenv.sh for non IHP projects as well). It's also a lot faster.

This update process is a bit more complex than normal IHP updates, but it's worth it and shouldn't take more than 10 minutes.

1. **Optional, but recommended: Activate flakes in your system's Nix config**

    This new IHP release makes use of nix flakes. While it's not required to be enabled to work with IHP, we highly recommended that you enable it inside your nix configuration.
    For that, either `~/.config/nix/nix.conf` (to enable flakes just for your user) or `/etc/nix/nix.conf` (to enable flakes globally) must contain the following snippet:

    ```bash
    experimental-features = nix-command flakes
    ```

2. **Add devenv and direnv specific code to `.gitignore`**
    ```
    .devenv*
    devenv.local.nix
    .direnv
    .env
    ```

3. **Edit your `.envrc` and migrate env vars from `./start`**
    Open your `.envrc` and replace it with this:
    ```
    if ! has nix_direnv_version || ! nix_direnv_version 2.3.0; then
        source_url "https://raw.githubusercontent.com/nix-community/nix-direnv/2.3.0/direnvrc" "sha256-Dmd+j63L84wuzgyjITIfSxSD57Tx7v51DMxVZOsiUD8="
    fi
    
    use flake . --impure --accept-flake-config
    
    # Include .env file if it exists locally. Use the .env file to load env vars that you don't want to commit to git
    if [ -f .env ]
    then
        set -o allexport
        source .env
        set +o allexport
    fi
    # Add your env vars here
    #
    # E.g. export AWS_ACCESS_KEY_ID="XXXXX"
    ```

    Does your app have any custom env vars specified in `start`? These now belong to `.envrc`:

    E.g. this `start` script:

    ```bash
    #!/usr/bin/env bash
    # Script to start the local dev server

    # ...

    # You can define custom env vars here:
    # export CUSTOM_ENV_VAR=".."

    export SES_ACCESS_KEY="XXXX"
    export SES_SECRET_KEY="XXXX"
    export SES_REGION="us-east-1"

    # Finally start the dev server
    RunDevServer
    ```

    Needs to be turned into an `.envrc` file like this:

    ```diff
    if ! has nix_direnv_version || ! nix_direnv_version 2.3.0; then
        source_url "https://raw.githubusercontent.com/nix-community/nix-direnv/2.3.0/direnvrc" "sha256-Dmd+j63L84wuzgyjITIfSxSD57Tx7v51DMxVZOsiUD8="
    fi

    use flake . --impure
    if [ -f .env ]
    then
      set -o allexport
      source .env
      set +o allexport
    fi

    ## Add the exports from your start script here:

    +export SES_ACCESS_KEY="XXXX"
    +export SES_SECRET_KEY="XXXX"
    +export SES_REGION="us-east-1"
    ```

    After that, your `start` script is not needed anymore, and you can delete it.

4. **Optional: Delete `start` script**

    The `start` script is not needed anymore, and can be deleted. The new start command is now `devenv up`. You can keep the `start` script if you want to, but it's not required.

5. **Remove `.envrc` from your `.gitignore`**
    ```diff
    .envrc
    ```

    The `.envrc` should now be committed to your git repository, as the file is no longer automatically generated. The `make .envrc` command will no longer work, and is not needed anymore.

6. **Create a `flake.nix`**

    Add a new file `flake.nix` with the following content:

    ```nix
    {
        inputs = {
            # Here you can adjust the IHP version of your project
            # You can find new releases at https://github.com/digitallyinduced/ihp/releases
            ihp.url = "github:digitallyinduced/ihp/v1.1";
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

    **Using IHP Pro / Business / Enterprise?**

    Replace the `ihp.url = "github:digitallyinduced/ihp/v1.1";` inside your new `flake.nix` with `ihp.url = "tarball+<YOUR BUILD URL>;`.

    Open https://ihp.digitallyinduced.com/Builds to retrieve the latest v1.1 build url and replace the `<YOUR BUILD URL>` with your build URL. E.g. `ihp.url = "tarball+https://ihp.digitallyinduced.com/BuildTarball?userId=XXXX&token=YYYY&version=ZZZZ";`

8. **Copy packages from `default.nix` to `flake.nix`:**

    Did you add any Haskell dependencies or native dependencies (e.g. imagemagick) to your `default.nix`? Then you need to add them to the `flake.nix` configuration. If you haven't, you can skip this part.

    E.g. if this is our `default.nix`:

    ```nix
    # default.nix
    let
        ihp = ...;
        haskellEnv = import "${ihp}/NixSupport/default.nix" {
            ihp = ihp;
            haskellDeps = p: with p; [
                cabal-install
                base
                wai
                text
                hlint
                p.ihp

                # <---- Custom packages start here
                http-streams
                ihp-stripe
            ];
            otherDeps = p: with p; [
                # <---- Custom native packages
                nodejs
            ];
            projectPath = ./.;
        };
    in
        haskellEnv
    ```

    We need to adjust the `flake.nix` like this:

    ```nix
    {
        inputs = {
            # Here you can adjust the IHP version of your project
            # You can find new releases at https://github.com/digitallyinduced/ihp/releases
            ihp.url = "github:digitallyinduced/ihp/v1.1";
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
                            nodejs # <-- Added here
                        ];
                        haskellPackages = p: with p; [
                            # Haskell dependencies go here
                            p.ihp
                            cabal-install
                            base
                            wai
                            text
                            hlint

                            # <---- Custom packages start here
                            http-streams
                            ihp-stripe
                        ];
                    };
                };

            };
    }
    ```

    After that adjust the `default.nix` to read it's packages from the `flake.nix`. It's currently there for backwards compatibility reasons, but will be removed in the future:

    ```nix
    # For backwards compatibility using flake.nix
    (import
        (
            fetchTarball {
                url = "https://github.com/edolstra/flake-compat/archive/35bb57c0c8d8b62bbfd284272c928ceb64ddbde9.tar.gz";
                sha256 = "sha256:1prd9b1xx8c0sfwnyzkspplh30m613j42l1k789s521f4kv4c2z2";
            }
        )
    { src = ./.; }).defaultNix
    ```

This means that from now on when adding new packages, you need to do it in a single file - `flake.nix`.

8. **Copy settings from `Config/nix/nixpkgs-config.nix` to `flake.nix`**

    Did you do any changes to `nixpkgs-config.nix` in your project? Likely you haven't, so you can skip this part. For reference, if the file looks like below, you don't need to do anything here:

    ```nix
    # Config/nix/nixpkgs-config.nix
    # Standard nixpkgs-config.nix, nothing to do here
    { ihp, additionalNixpkgsOptions, ... }:
    import "${toString ihp}/NixSupport/make-nixpkgs-from-options.nix" {
        ihp = ihp;
        haskellPackagesDir = ./haskell-packages/.;
        additionalNixpkgsOptions = additionalNixpkgsOptions;
    }
    ```

    If you have done any changes to that file, it might look like this:

    ```nix
    { ihp, additionalNixpkgsOptions, ... }:
    import "${toString ihp}/NixSupport/make-nixpkgs-from-options.nix" {
        ihp = ihp;
        haskellPackagesDir = ./haskell-packages/.;
        additionalNixpkgsOptions = additionalNixpkgsOptions;
        dontCheckPackages = ["my-failing-package"];
        doJailbreakPackages = ["my-jailbreak-package"];
    }
    ```

    The `dontCheckPackages` and `doJailbreakPackages` options need to be moved to `flake.nix`:

    ```diff
    ihp = {
        # ...

    +   dontCheckPackages = [ "my-failing-package" ];
    +   doJailbreakPackages = [ "my-jailbreak-package" ];
    };
    ```

    If you've pinned the IHP app to a specific nixpkgs version in your `nixpkgs-config.nix`, you need to apply that version to the nixpkgs input in `flake.nix` by replacing `nixpkgs.follows` with `nixpkgs.url`.

    ```nix
    {
        inputs = {
            ihp.url = "github:digitallyinduced/ihp/v1.1";
            nixpkgs.url = "github:NixOS/nixpkgs?rev=PUT YOUR CUSTOM REVISION HERE";
            flake-parts.follows = "ihp/flake-parts";
            devenv.follows = "ihp/devenv";
            systems.follows = "ihp/systems";
        };

        # ...
    }
    ```

9. **Update `.ghci`**

    Replace your current `.ghci` with the following:

    ```
    :set -XNoImplicitPrelude
    :def loadFromIHP \file -> (System.Environment.getEnv "IHP_LIB") >>= (\ihpLib -> readFile (ihpLib <> "/" <> file))
    :loadFromIHP applicationGhciConfig
    import IHP.Prelude
    ```

    This will finally solve all the issues that typically happen around IHP's stateful `build/ihp-lib` symlink. This symlink is now replaced with an env variable called `IHP_LIB` that is automatically provided by devenv.

10. **Update `Makefile`**
    
    Open your `Makefile` and remove the following boilerplate code at the top of the file:

    ```Makefile
    ifneq ($(wildcard IHP/.*),)
    IHP = IHP/lib/IHP
    else
    ifneq ($(wildcard build/ihp-lib),)
    IHP = build/ihp-lib
    else
    ifneq ($(shell which RunDevServer),)
    IHP = $(shell dirname $$(which RunDevServer))/../lib/IHP
    else
    IHP = $(error IHP not found! Run the following command to fix this:    nix-shell --run 'make .envrc'    )
    endif
    endif
    endif

    # ...
    ```

11. **Migration finished**

    Finally, approve the new `.envrc`:

    ```bash
    direnv allow
    ```

    You will be asked a few questions like "Do you want to allow configuration setting 'extra-substituters' to be set to 'https://digitallyinduced.cachix.org' (y/N)?"
    You can answer "y" to all of them. This will take some time, as all your packages are now being built.
    Once done, you can commit `flake.lock` to your git repository.

12. **Start project**

    Start your project with `devenv up`.

13. **Update ihp-new**

    We've updated `ihp-new` to the new nix flakes tools. This will speed up the time to create a new project:

    ```bash
    # Run this:
    nix-env -f https://downloads.digitallyinduced.com/ihp-new.tar.gz -i ihp-new

    # Or this if you use nix profile:
    nix profile install -f https://downloads.digitallyinduced.com/ihp-new.tar.gz
    ```


# Upgrade to 1.0.1 from 1.0.0
1. **Switch IHP version**

    - **IHP Basic**

        Open `default.nix` and change the git commit in line 4 to the following:

        ```diff
        -ref = "refs/tags/v1.0.0";
        +ref = "refs/tags/v1.0.1";
        ```

    - **IHP Pro & IHP Business**

        Visit https://ihp.digitallyinduced.com/Builds and copy the latest v1.0.1 URL into your `default.nix`.

2. **Remake Env**

    Run the following commands:

    ```bash
    nix-shell --run 'make -B .envrc'
    nix-shell --run 'make -B build/ihp-lib'
    ```

    Now you can start your project as usual with `./start`.


# Upgrade to 1.0.0 from Beta 0.20.0
1. **Switch IHP version**

    - **IHP Basic**

        Open `default.nix` and change the git commit in line 4 to the following:

        ```diff
        -ref = "refs/tags/v0.19.0";
        +ref = "refs/tags/v1.0.0";
        ```

    - **IHP Pro & IHP Business**

        Visit https://ihp.digitallyinduced.com/Builds and copy the latest v1.0.0-rc1 URL into your `default.nix`.

2. **Remake Env**

    Run the following commands:

    ```bash
    nix-shell --run 'make -B .envrc'
    nix-shell --run 'make -B build/ihp-lib'
    ```

    Now you can start your project as usual with `./start`.

    If you've been using a release candidate before, you need to run `nix-store --gc` before. Otherwise nix might use the cached release candidate.

3. **Fix Type Errors:**
    You might see some type errors after upgrading. Here's how to fix them:

    - The function `getFrameworkConfig` has been removed:

        ```haskell
        -- Old way:
        ?context |> getFrameworkConfig |> get #appConfig

        -- New way:
        ?context.frameworkConfig.appConfig
        ```

    - `Couldn't match type 'CurrentAdminRecord' with 'Admin' arising from a use of 'currentAdminOrNothing'`

      To make `currentAdmin` etc. more consistent with `currentUser` functions, we have removed the explicit type argument passed to these functions.

      E.g. the following:
      ```haskell
      currentAdminOrNothing @Admin
      ```
      Needs to be changed to this:
      ```haskell
      currentAdminOrNothing
      ```
      Additionally you need to specify the `CurrentAdminRecord` inside your `Web/Types.hs`:

      ```haskell
      type instance CurrentAdminRecord = Admin
      ```

# Upgrade to Beta 0.20.0 from Beta 0.19.0
1. **Switch IHP version**

    - **IHP Basic**

        Open `default.nix` and change the git commit in line 4 to the following:

        ```diff
        -ref = "refs/tags/v0.19.0";
        +ref = "refs/tags/v0.20.0";
        ```

    - **IHP Pro & IHP Business**

        Visit https://ihp.digitallyinduced.com/Builds and copy the latest v0.20.0 URL into your `default.nix`.

2. **Remake Env**

    Run the following commands:

    ```bash
    nix-shell --run 'make -B .envrc'
    nix-shell --run 'make -B build/ihp-lib'
    ```

    Now you can start your project as usual with `./start`.

3. **Fix Type Errors:**
    You might see some type errors after upgrading. Here's how to fix them:

    TODO

# Upgrade to Beta 0.19.0 from Beta 0.18.0
1. **Switch IHP version**

    - **IHP Basic**

        Open `default.nix` and change the git commit in line 4 to the following:

        ```diff
        -ref = "refs/tags/v0.18.0";
        +ref = "refs/tags/v0.19.0";
        ```

    - **IHP Pro & IHP Business**

        Visit https://ihp.digitallyinduced.com/Builds and copy the latest v0.19.0 URL into your `default.nix`.

2. **Remake Env**

    Run the following commands:

    ```bash
    nix-shell --run 'make -B .envrc'
    nix-shell --run 'make -B build/ihp-lib'
    ```

    Now you can start your project as usual with `./start`.

# Upgrade to Beta 0.18.0 from Beta 0.17.0
1. **Switch IHP version**

    - **IHP Basic**

        Open `default.nix` and change the git commit in line 4 to the following:

        ```diff
        -ref = "refs/tags/v0.17.0";
        +ref = "refs/tags/v0.18.0";
        ```

    - **IHP Pro & IHP Business**

        Visit https://ihp.digitallyinduced.com/Builds and copy the latest v0.18.0 URL into your `default.nix`.

2. **Remake Env**

    Run the following commands:

    ```bash
    nix-shell --run 'make -B .envrc'
    nix-shell --run 'make -B build/ihp-lib'
    ```

    Now you can start your project as usual with `./start`.

# Upgrade to Beta 0.17.0 from Beta 0.16.0
1. **Switch IHP version**

    - **IHP Basic**

        Open `default.nix` and change the git commit in line 4 to the following:

        ```diff
        -ref = "refs/tags/v0.16.0";
        +ref = "refs/tags/v0.17.0";
        ```

    - **IHP Pro & IHP Business**

        Visit https://ihp.digitallyinduced.com/Builds and copy the latest v0.17.0 URL into your `default.nix`.

2. **Remake Env**

    Run the following commands:

    ```bash
    nix-shell --run 'make -B .envrc'
    nix-shell --run 'make -B build/ihp-lib'
    ```

    Now you can start your project as usual with `./start`.

3. **Forms**

    - If your app's forms have custom attributes using `fieldInput`, you will need to follow this step.
    - If your app has no custom attributes, you can skip this step.
    - If you don't know whether this applies to you, the compiler will tell you as this leads to a type error.

    The `fieldInput` attribute has bee removed completly. But IHP still continues to support custom attributes in form fields.

    Code like this:

    ```haskell
     {(textareaField #content)
         { helpText = "Markdown"
         , fieldInput = (\fieldInput -> H.textarea content ! A.rows "16")
         }
     }
    ```

    needs to be changed to this:

    ```haskell
     {(textareaField #content)
         { helpText = "Markdown"
         , additionalAttributes = [ ("rows", "16") ]
         }
     }
    ```

4. **Newtype Generics**

    We don't use the "Newtype Generics" package as much as expected. To keep IHP lightweight we've removed that package from IHP.

    The `newtype-generics` package provided functions like `pack` and `unpack` to wrap things inside a `newtype` wrapper.

    A common use case is to unpack a `Id Project` to get the underlying `UUID` value. If you've been using code like `unpack projectId`, replace this with the new [`unpackId`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:unpackId) like this: `unpackId projectId`. Same goes for [`packId`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:packId).

    If you use functionality of `newtype-generics` beyond wrapping and unwrapping the Id values, [please add `newtype-generics` to your `default.nix` and run `make -B .envrc` again](https://ihp.digitallyinduced.com/Guide/package-management.html#using-a-haskell-package).

5. **SMTP Mail**

    If you configure a custom SMTP server in your `Config.hs`, you will need to explicitly configure the encryption setting:

    Change code like this:

    ```haskell
    import IHP.Mail

    config :: ConfigBuilder
    config = do
        option $ SMTP
            { host = "smtp.myisp.com"
            , port = 2525
            , credentials = Nothing
            }
    ```

    To this:

    ```haskell
    import IHP.Mail

    config :: ConfigBuilder
    config = do
        option $ SMTP
            { host = "smtp.myisp.com"
            , port = 2525
            , credentials = Nothing
            , encryption = TLS -- <-- NEW, other options: `Unencrypted` or `STARTTLS`
            }
    ```

# Upgrade to Beta 0.16.0 from Beta 0.15.0
1. **Switch IHP version**

    - **IHP Basic**

        Open `default.nix` and change the git commit in line 4 to the following:

        ```diff
        -ref = "refs/tags/v0.15.0";
        +ref = "refs/tags/v0.16.0";
        ```

    - **IHP Pro & IHP Business**

        Visit https://ihp.digitallyinduced.com/Builds and copy the latest v0.16 URL into your `default.nix`.

2. **Remake Env**

    Run the following commands:

    ```bash
    nix-shell --run 'make -B .envrc'
    nix-shell --run 'make -B build/ihp-lib'
    ```

    Now you can start your project as usual with `./start`.

3. **Tests**

    If your app has Hspec tests, you will need to follow this step. If your app has no Hspec tests, you can skip this.

    The `withParams` test helper has been replaced with `callActionWithParams`.

    Test code like this:

    ```haskell
            it "creates a new post" $ withParams [("title", "Post title"), ("body", "Body of post")] do
                response <- callAction CreatePostAction

                let (Just location) = (lookup "Location" (responseHeaders response))
                location `shouldBe` "http://localhost:8000/Posts"
    ```

    needs to be changed to this:

    ```haskell
            it "creates a new post" $ withContext do -- <-- `withContext` here, otherwise it will not work
                response <- callActionWithParams CreatePostAction [("title", "Post title"), ("body", "Body of post")] -- <-- `callAction` turns into `callActionWithParams`

                let (Just location) = (lookup "Location" (responseHeaders response))
                location `shouldBe` "http://localhost:8000/Posts"
    ```


# Upgrade to Beta 0.15.0 from Beta 0.14.0
1. **Switch IHP version**

    - **IHP Basic**

        Open `default.nix` and change the git commit in line 4 to the following:

        ```diff
        -ref = "refs/tags/v0.14.0";
        +ref = "refs/tags/v0.15.0";
        ```

        Please continue the upgrade instructions and don't run any 'make' commands yet.

    - **IHP Pro & IHP Business**

        Visit https://ihp.digitallyinduced.com/Builds and copy the latest v0.15 URL into your `default.nix`.
2. **Patch `Config/nix/nixpkgs-config.nix`**

    Open `Config/nix/nixpkgs-config.nix` and replace it with this:

    ```diff
    # See https://ihp.digitallyinduced.com/Guide/package-management.html
    { ihp, additionalNixpkgsOptions, ... }:
    import "${toString ihp}/NixSupport/make-nixpkgs-from-options.nix" {
        ihp = ihp;
        haskellPackagesDir = ./haskell-packages/.;
        additionalNixpkgsOptions = additionalNixpkgsOptions;
    }
    ```

    **Did you run a recent master version of IHP?**

    Please also apply this patch if you have been using a recent master version of IHP. The `additionalNixpkgsOptions` is likely missing in your file.

3. **Remake Env**

    Run the following commands:

    ```bash
    make clean
    nix-shell -j auto --cores 0 --run 'make -B .envrc'
    nix-shell --run 'make -B build/ihp-lib'
    ```

    Now you can start your project as usual with `./start`.

4. **.gitignore**

    Add the following lines to your `.gitignore` file:

    ```
    # Ignore locally checked out IHP version
    IHP
    ```

5. **`./start` script**

    Open the project's `start` script and append the following after `set -e` in line 4:

    ```bash
    # On macOS the default max count of open files is 256. IHP needs atleast 1024 to run well.
    #
    # The wai-static-middleware sometimes doesn't close it's file handles directly (likely because of it's use of lazy bytestrings)
    # and then we usually hit the file limit of 256 at some point. With 1024 the limit is usually never hit as the GC kicks in earlier
    # and will close the remaining lazy bytestring handles.
    if [[ $OSTYPE == 'darwin'* ]]; then
        ulimit -n 4096
    fi
    ```

    The file should now look like this: https://github.com/digitallyinduced/ihp-boilerplate/blob/62754efc0b7f8c82f36d0bbdf84e68418fc571c7/start

6. **Session Cookies**

    With IHP v0.15 we've switched the encoding of session cookies from a textual encoding to a binary encoding. IHP v0.15 will ignore the old session format.

    After switching v0.15 your users will be logged out and need to log in again.

# Upgrade to Beta 0.14.0 from Beta 0.13.1

## Switch IHP version

Open `default.nix` and change the git commit in line 4 to the following:

```diff
-ref = "refs/tags/v0.13.1";
+ref = "refs/tags/v0.14.0";
```

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.

## IHP Background Jobs

IHP jobs now can be scheduled to run at a specific time with `runAt`. For that every table that acts as a job queue in your application needs to be migration.

1. Create a new migration using `new-migration`.
2. For every table ending with `_jobs` do this:
    ```sql
    alter table $TABLE add column run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL;
    update $TABLE set run_at = created_at;
    ```
    where `$TABLE` should be replaced with the jobs table.

    The line `update $TABLE set run_at = created_at;` sets the right `run_at` value for all existing jobs.

After that apply this migration to all your IHP instances running on `v.0.14.0`.

# Upgrade to Beta 0.13.1 from Beta 0.13.0

## Switch IHP version

Open `default.nix` and change the git commit in line 4 to the following:

```diff
-ref = "refs/tags/v0.13.0";
+ref = "refs/tags/v0.13.1";
```

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.

# Upgrade to Beta 0.13.0 from Beta 0.12.0

## Switch IHP version

Open `default.nix` and change the git commit in line 4 to the following:

```diff
-ref = "refs/tags/v0.12.0";
+ref = "refs/tags/v0.13.0";
```

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.

## Updating nixpkgs

If you have custom nix package definitions in your project, you will likely get an error that `stdenv` doesn't exist in expressions like `stdenv.lib.SOMETHING`.

nixpkgs has moved `stdenv.lib` to just `lib`. So you need to replace all mentions of `stdenv.lib.` with `lib.`. You might also need to change import statements that import `stdenv` to instead import `lib` directly.

[To get a better understanding of this, take a look a the upgrade commit to see what changes we did to the custom package definitions included with IHP.](https://github.com/digitallyinduced/ihp/commit/cfc8ceb4918749e833f79ba3d362082d0010f1b4)

### Postgres: v11 -> v13

With the nixpkgs update the development postgres server has been updated. Make sure that you run `make clean` during the update process. Otherwise the local database might not start as expected. When you get an error like `libpq: failed (could not connect to server: No such file or directory`, likely postgres is not starting up because your database state is still on v11, while the new postgres is v13.

# Upgrade to Beta 0.12.0 from Beta 0.11.0

## Switch IHP version

Open `default.nix` and change the git commit in line 4 to the following:

```diff
-ref = "refs/tags/v0.11.0";
+ref = "refs/tags/v0.12.0";
```

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.

## Updating jQuery

The jQuery version used by IHP has changed. We're switching from `3.2.1` to `3.6.0`. jQuery `3.2.1` has some known security vulnerabilities, so it's recommended that you follow these update steps.

To update your application looks search for the string `/vendor/jquery-3.2.1.slim.min.js` in your code base and replace it with `/vendor/jquery-3.6.0.slim.min.js`. Likely the only mention is inside the `Web/View/Layout.hs`.

# Upgrade to Beta 0.11.0 from Beta 0.10.0

## Switch IHP version

Open `default.nix` and change the git commit in line 4 to the following:

```diff
-ref = "refs/tags/v0.10.0";
+ref = "refs/tags/v0.11.0";
```

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.

## Important Login Changes

Important if you use IHP's Login: IHP's built-in sessions controller used for the built-in login now uses case-insensitive lookup for the email addresses at login. This will improve user experience for users that create their account with `Firstname.Lastname@example.com` and then try to log in using `firstname.lastname@example.com`.


# Upgrade to Beta 0.10.0 from Beta 0.9.0

## Switch IHP version

Open `default.nix` and change the git commit in line 4 to the following:

```diff
-ref = "refs/tags/v0.9.0";
+ref = "refs/tags/v0.10.0";
```

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.

## Upgrade IHP.HtmlSupport

If you got an type error related to `IHP.HtmlSupport`, follow this step:

This error like is related to the rename of all `IHP.HtmlSupport.*` modules to `IHP.HSX.*`. You can fix this error by replacing all mentions of `IHP.HtmlSupport.` with `IHP.HSX.` in your code base.

## Important `data-` attribute changes

Boolean data attributes like `<div data-is-active={True}>` were rendered like `<div data-is-active="data-is-active">` or `<div>` (if `False`) in previous versions of IHP.

These boolean data attributes are now rendered like `<div data-is-active="true">` and `<div data-is-active="false">`. If you have JS code consuming your data attributes, make sure that you update the JS code.

Other non-data attributes like `<input disabled={True}>` are not affected by this change and will continue to render as `<input disabled="disabled"/>`.

# Upgrade to Beta 0.9.0 from Beta 0.8.0

## Switch IHP version

Open `default.nix` and change the git commit in line 4 to the following:

```diff
-ref = "refs/tags/v0.8.0";
+ref = "refs/tags/v0.9.0";
```

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.


## Upgrade AutoRoute

If you got an type error related to AutoRoute's `parseArgument` function, you need to also follow this step.

The `parseArgument` interface has been removed as this works out of the box now. To upgrade remove your definitions of `parseArgument`.

# Upgrade to Beta 0.8.0 from Beta 13.12.2020 (v20201213)

## Update `Config/nix/nixpkgs-config.nix`

Replace the content of that file with this:
```nix
# See https://ihp.digitallyinduced.com/Guide/package-management.html
{ ihp }:
import "${toString ihp}/NixSupport/make-nixpkgs-from-options.nix" {
    ihp = ihp;
    haskellPackagesDir = ./haskell-packages/.;
}
```

## Switch IHP version

Open `default.nix` and change the git commit in line 4 to the following:

```diff
-rev = "67e99ec469d5a215a0b92d8759d5b7a4c7b0e0e1";
+ref = "refs/tags/v0.8.0";
```

**IMPORTANT: `rev` changes to `ref` here. Make sure that you don't miss that. Otherwise nix will complain.**

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.

# Upgrade to Beta 13.12.2020 (v20201213) from Beta 27.11.2020 (v20201127)

## Update `Main.hs`

1. Add import for `import IHP.Job.Types` to your `Main.hs`:

    ```haskell
    import IHP.Job.Types
    ```
2. Add the following instance to your `Main.hs`:
    ```haskell
    instance Worker RootApplication where
        workers _ = []
    ```

## Switch IHP version

First open `default.nix` and change the git commit in line 4 to the following:

```bash
rev = "17c9507e519a7c37ccf001ada050df171e4af8ef";
```

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.


# Upgrade to Beta 27.11.2020 (v20201127) from Beta 13.11.2020 (v20201113)

## Switch IHP version

First open `default.nix` and change the git commit in line 4 to the following:

```bash
rev = "79d4892d6cd531eb2b446a46a2a0e434c8a39895";
```

After that run the following command to update your project:

```bash
make clean
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.


# Upgrade to Beta 13.11.2020 (v20201113) from Beta 30.10.2020 (v20201030)

## Update your `Config/Config.hs`

Old:

```haskell
module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.Mail.Types

instance FrameworkConfig where
    environment = Development
    appHostname = "localhost"
```

New:

```haskell
module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.Mail.Types

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")
```

Do you have a ´baseUrl´ key in your config?

Old:

```haskell
    baseUrl = "https://..."
```

New:

```haskell
    option (BaseUrl "https://...")
```

### MailServer

Do you have a `mailServer` key in your config?

Old:

```haskell
    mailServer = SES { .. }
```

New:

```haskell
    option SES { .. }
```

## Update `Main.hs`

Old:

```haskell
main = IHP.Server.run
```

New:

```haskell
main = IHP.Server.run config
```

## Update `Web/Routes.hs`

Remove all lines like `type instance ModelControllerMap AdminApplication Project = ProjectsController`.

Search for `ModelControllerMap` in your project. If there are still some results for this, remove the found lines.

## Update `Web/Types.hs`

Remove the `data ViewContext = ..`. The View Context is not used anymore in IHP.

## Update all Views `Web/View/*/*.hs`

Open every view file in the `View` directory.

Remove the `ViewContext` from the `instance View`:

```diff
-instance View EditView ViewContext where
+instance View EditView where
```

Does the view have a custom view-specific layout?

```diff
-instance View ShowEnumView ViewContext where
-    beforeRender (context, view) = (context { layout = schemaDesignerLayout }, view)
+instance View ShowEnumView where
+    beforeRender view = setLayout schemaDesignerLayout
```

This is quite common in `View\Sessions\New.hs` if you are using the built-in authentication.

## Update `Web/View/Layouts.hs`

1. Remove:

```haskell
type Html = HtmlWithContext ViewContext
```

2. If you have other applications such as `Admin`, please also remove the `$APP/View/Context.hs` files.

3. Update calls to `isDevelopment`:

```diff
-when (isDevelopment FrameworkConfig.environment) [hsx|
+when isDevelopment [hsx|
```

4. Update calls to `isProduction`:

```diff
-when (isProduction FrameworkConfig.environment) [hsx|
+when isProduction [hsx|
```

5. Add type signatures to all functions in `Layout.hs`:

```diff
-defaultLayout view = [hsx|...|]
+defaultLayout :: Html -> Html
+defaultLayout view = [hsx|...|]
```

```diff
-stylesheets = [hsx|...|]
+stylesheets :: Html
+stylesheets = [hsx|...|]
```

```diff
-scripts = [hsx|...|]
+scripts :: Html
+scripts = [hsx|...|]
```

```diff
-metaTags = [hsx|...|]
+metaTags :: Html
+metaTags = [hsx|...|]
```

## Remove the `Web/View/Context.hs`

```bash
rm Web/View/Context.hs
```

## Update `Web/View/Prelude.hs`

```diff
-, module Web.View.Context
```

```diff
-import Web.View.Context
```


### `?controllerContext` has been renamed to `?context`

In case you use `?controllerContext` somewhere in your code (e.g. in a type signature or as a value) rename it to `?context`.

### `?viewContext` has been renamed to `?context`

In case you use `?viewContext` somewhere in your code (e.g. in a type signature or as a value) rename it to `?context`.

## Update `Web/FrontController.hs`

1. Add an `import Web.View.Layout (defaultLayout)` at the top of the file
2. Make sure there is a `InitControllerContext`. If it does not exist, place this at the bottom of the file:

```haskell
-- Add these imports, most other imports can propably be removed
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
```

**If you miss this step, you will get an error like `Unable to find ViewLayout in controller context`.**

## Switch IHP version

First open `default.nix` and change the git commit in line 4 to the following:

```bash
rev = "d02a0699220a87d32889ff2a7b87ad81f8bc8195";
```

After that run the following command to update your project:

```bash
nix-shell -j auto --cores 0 --run 'make -B .envrc'
make -B build/ihp-lib
```

Now you can start your project as usual with `./start`.
