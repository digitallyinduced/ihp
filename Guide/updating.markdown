# Updating IHP

```toc

```

## Releases

[You can find a list of all releases on GitHub.](https://github.com/digitallyinduced/ihp/releases).

A new version is usually announced first via the [IHP Newsletter](http://eepurl.com/g51zq1) and also on X.

## Upgrade Instructions

[See UPGRADE.md](https://github.com/digitallyinduced/ihp/blob/master/UPGRADE.md) for intructions on how to upgrade your IHP project to a newer version of IHP.

## Updating to the Latest Release

To update to the current IHP version, follow the instructions [in the release notes](https://github.com/digitallyinduced/ihp/releases). It's recommended to only update a single release version at a time when there are major breaking changes between your current version and the targeted update version.

## Updating to the Latest Git Commit

To test out the latest IHP features even when they are not released yet you can update to the latest IHP git commit:

Open `flake.nix`. It should look like this:

```nix
{
    inputs = {
        ihp.url = "github:digitallyinduced/ihp/v1.4";
...
```

Now drop the `/v1.4` from the `ihp.url = "github:digitallyinduced/ihp/v1.4";` such that the file looks like this:
```nix
{
    inputs = {
        ihp.url = "github:digitallyinduced/ihp";
...
```

This will fetch the latest master branch from IHP.

Then rebuild everything by running the following:

```bash
direnv allow
```

## Updating to a specific Git Commit

When the commit you're trying out is not yet merged into the master branch, follow these steps.

Open the `flake.nix` in your project folder. This will look like this:

```nix
{
    inputs = {
        ihp.url = "github:digitallyinduced/ihp/v1.4";
...
```

Now change the `/v1.4` to your chosen git commit hash:


```nix
{
    inputs = {
        ihp.url = "github:digitallyinduced/ihp/ff63977c178a39d4e22dae82d6f3d8ba1647039e";
...
```

Then rebuild everything by running the following:

```bash
direnv allow
```
