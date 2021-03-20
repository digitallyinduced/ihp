# Updating IHP

```toc

```

## Releases

During the beta releases, there is a new release every two weeks on Friday. [You can find a list of all releases on GitHub.](https://github.com/digitallyinduced/ihp/releases).

A new version is usually announced first via the [IHP Newsletter](http://eepurl.com/g51zq1) and also on Gitter and Twitter.

IHP version numbers are assigned by the release date. For example, the version `v10072020` stands for the release of the `10.07.2020`.

## Updating to the Latest Release

To update to the current IHP version, follow the instructions [in the release notes](https://github.com/digitallyinduced/ihp/releases). It's recommended to only update a single release version at a time when there are major breaking changes between your current version and the targeted update version.

## Updating to the Latest Git Commit

To test out the latest IHP features even when they are not released yet you can update to the latest IHP git commit. For that, you need [to copy the git commit hash from the latest commit on GitHub](https://github.com/digitallyinduced/ihp/commits/master).

After that open the `default.nix` in your project folder. This will look like this:

```nix
let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        rev = "720a10ddfd500d5930c926c293aea5a9016acb29";
    };

...
```

Now change the git commit hash in line 4 next to `rev = ` to your chosen git commit hash.

Then rebuild everything by running the following:

```bash
nix-shell -j auto --cores 0 --run 'make -B .envrc'
```

After that, you can use the project as usual by using `./start`.

When the commit you're trying out is not merged into the master branch this will fail because nix is doing only a shallow-clone of the repo. Follow the steps in "Updating to a specific Git Commit" below to fix this.

## Updating to a specific Git Commit

When the commit you're trying out is not yet merged into the master branch, follow these steps.

Open the `default.nix` in your project folder. This will look like this:

```nix
let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        rev = "720a10ddfd500d5930c926c293aea5a9016acb29";
    };

...
```

Now change the git commit hash in line 4 next to `rev = ` to your chosen git commit hash. Also add a `ref = "*"` otherwise nix will only shallow-clone the master branch:

```nix
let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        rev = "... my new commit hash ...";
        ref = "*";
    };

...
```

Then rebuild everything by running the following:

```bash
nix-shell -j auto --cores 0 --run 'make -B .envrc'
```

After that, you can use the project as usual by using `./start`.
