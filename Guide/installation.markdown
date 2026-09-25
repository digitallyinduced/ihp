# Installing IHP

```toc

```

## 1. Dependency: Nix Package Manager

The framework uses the Nix package manager to manage the whole set of dependencies of your application. Nix is the equivalent of `npm`, but for Haskell and much more.

For example, PostgreSQL and the Haskell compiler are both dependencies of your app, as well as all the Haskell or JavaScript packages you want to use. We use Nix to make sure that these dependencies are available to the app - in development, as well as in production.

That's why we first need to make sure that you have Nix installed.

### Installing Nix

We recommend using the [Determinate Nix Installer](https://docs.determinate.systems/). Follow the installation instructions on their website.

Determinate Nix comes with Flakes and lazy-trees enabled by default, so no additional configuration is needed.

## 2. Installing IHP

You can now install IHP by running:

```bash
nix profile install nixpkgs#ihp-new
```

## 3. Install `direnv`

IHP uses [direnv](https://direnv.net/) to automatically load the environment variables defined in `.env` files. This is very useful for development, as it allows you to set up your environment without having to manually export variables every time you start a new terminal session.

```
nix profile add nixpkgs#direnv
```

Next you need to enable `direnv` in your shell. Follow the instructions from the [direnv website](https://direnv.net/docs/hook.html) to do this.

## 4. Setting up your editor

Also, check this out if your editor is already set up. You might miss a plugin that's recommended for IHP to work well.

-   [VS Code](https://ihp.digitallyinduced.com/Guide/editors.html#using-ihp-with-visual-studio-code-vscode) **Don't have the direnv extension installed? Read this link**
-   [Sublime Text](https://ihp.digitallyinduced.com/Guide/editors.html#using-ihp-with-sublime-text)
-   [Emacs](https://ihp.digitallyinduced.com/Guide/editors.html#using-ihp-with-emacs)
-   [Vim / NeoVim](https://ihp.digitallyinduced.com/Guide/editors.html#using-ihp-with-vim-neovim)

It's time to start your first "Hello World" project now!

[Next: Your First Project](https://ihp.digitallyinduced.com/Guide/your-first-project.html)

## System Requirements

IHP is supported on the following platforms:

- **macOS** (Intel and Apple Silicon): Fully supported. On Apple Silicon Macs, make sure Rosetta 2 is installed (`softwareupdate --install-rosetta`), as some Nix packages still require it.
- **Linux** (NixOS, Ubuntu, Fedora, Arch, etc.): Fully supported. Any distribution that can run the Nix package manager will work.
- **Windows**: Use [WSL2](https://learn.microsoft.com/en-us/windows/wsl/install) (Windows Subsystem for Linux). Install a Linux distribution through WSL2, then follow the Linux instructions inside your WSL2 environment. Native Windows is not supported.

### Hardware Recommendations

- **Disk space**: Nix stores all packages in `/nix/store/`, and the initial setup for an IHP project downloads GHC, PostgreSQL, and many libraries. Expect the first project to use roughly 10-20 GB of disk space. This is normal. Subsequent projects reuse cached packages, so they add much less.
- **RAM**: 8 GB minimum recommended. The Haskell compiler can be memory-intensive during builds.
- **Internet**: A stable internet connection is needed for the initial setup. After the first build, most work happens offline.

## What Nix Does

If you have not used Nix before, here is a short explanation of what it does and why IHP uses it.

Nix is a package manager, similar in purpose to Homebrew, apt, or npm, but with one important difference: it guarantees that every developer working on a project has the exact same versions of every dependency. There are no "works on my machine" problems.

When you create an IHP project, Nix takes care of the following:

- **Downloading GHC** (the Haskell compiler) at the exact version IHP requires.
- **Setting up PostgreSQL** so you have a database ready for development.
- **Installing all Haskell and JavaScript libraries** your app depends on.
- **Providing development tools** like the IHP development server and code generators.

All packages are stored in `/nix/store/`. Each package version gets its own unique directory, which is why disk usage is higher than with traditional package managers. The benefit is that nothing ever conflicts, and you can always reproduce the exact same environment.

The first time you build an IHP project, Nix downloads everything from scratch (or from a binary cache). This can take 10-15 minutes depending on your internet connection. After that, subsequent builds are fast because packages are already cached on your machine.

## What direnv Does

direnv is a small tool that automatically sets up your shell environment when you enter a project directory. Without it, you would need to run `nix develop` manually every time you open a new terminal to work on your project.

Here is how it works:

1. Each IHP project contains an `.envrc` file that tells direnv how to set up the environment.
2. When you `cd` into the project directory, direnv detects the `.envrc` file and loads the Nix environment automatically.
3. When you `cd` out of the project directory, direnv unloads the environment, keeping your system clean.

The first time you enter a project directory, you need to run `direnv allow` to tell direnv that you trust the `.envrc` file in that directory. This is a one-time step per project (unless the `.envrc` file changes).

If you choose not to install direnv, you can always activate the project environment manually by running `nix develop` from inside the project directory. direnv simply automates this step.

## Verifying Your Installation

After completing the installation steps above, you can verify that everything is working correctly.

### Before Creating a Project

Check that Nix is installed:

```bash
nix --version
```

This should print something like `nix (Nix) 2.x.x`. If you get "command not found", Nix is not installed or not on your PATH. Try opening a new terminal window.

Check that direnv is installed:

```bash
direnv --version
```

This should print a version number. If not, revisit step 3 above.

Check that `ihp-new` is available:

```bash
ihp-new --help
```

If this prints usage information, you are ready to create your first project.

### After Creating a Project

Once you have created a project with `ihp-new` and entered the project directory:

```bash
cd my-project
direnv allow
```

Wait for the environment to finish loading. The first time, this takes a while as Nix downloads dependencies. You will see direnv output in your terminal. When it finishes, verify the tools are available:

```bash
ghc --version
```

This should show the GHC version IHP uses.

```bash
psql --version
```

This should show the PostgreSQL version.

If both commands produce version output, your development environment is correctly set up.

## Troubleshooting Installation

### Nix Installation Fails on macOS

If the Nix installer fails, try the following:

- Make sure you are using the [Determinate Nix Installer](https://docs.determinate.systems/) as recommended above. It handles macOS-specific setup automatically.
- On Apple Silicon Macs (M1/M2/M3/M4), install Rosetta 2 first: `softwareupdate --install-rosetta`.
- If you see errors about disk permissions or creating `/nix/store`, you may need to enable full disk access for your terminal app in System Settings > Privacy & Security > Full Disk Access.

### Download Is Very Slow

Nix downloads pre-built packages from `cache.nixos.org`. If downloads are slow:

- Check your internet connection.
- Try again at a different time. The cache servers can occasionally be slow.
- If you are behind a corporate proxy or firewall, it may be blocking Nix downloads. Talk to your network administrator.

### Permission Denied Errors

Nix needs to create and manage `/nix/store`. If you see permission errors:

- On macOS, the Determinate Nix Installer should handle disk setup automatically. If it did not, try running the installer again.
- On Linux, make sure you ran the installer with appropriate permissions. The installer typically needs `sudo` access to create the `/nix` directory.

### direnv Not Activating

If direnv does not automatically load the environment when you enter a project directory:

1. Make sure you added the direnv hook to your shell configuration file. For zsh (the default on macOS), add this to your `~/.zshrc`:

    ```bash
    eval "$(direnv hook zsh)"
    ```

    For bash, add this to your `~/.bashrc`:

    ```bash
    eval "$(direnv hook bash)"
    ```

2. Open a new terminal window after editing your shell configuration.
3. Run `direnv allow` inside the project directory.
4. Check `direnv status` to see if direnv is detecting the `.envrc` file.

### "Command Not Found: ghc" After direnv allow

This usually means the Nix environment is still building. When you first run `direnv allow` in a new project, Nix needs to download and set up all dependencies. This can take several minutes.

- Watch the terminal output. direnv will show progress as Nix works.
- Run `direnv status` to check whether the environment is loaded.
- If the environment fails to load, look for error messages in the direnv output and check the [IHP troubleshooting guide](https://ihp.digitallyinduced.com/Guide/troubleshooting.html).

### Getting Help

If you run into problems not covered here:

- Check the [Troubleshooting Guide](https://ihp.digitallyinduced.com/Guide/troubleshooting.html) for solutions to common issues.
- Join the [IHP Slack community](https://ihp.digitallyinduced.com/Slack) to ask questions. The community is active and happy to help.

## Next Steps

You are now ready to build your first IHP application.

- Follow the [Your First Project](your-first-project.html) guide to create a small blog application and learn the basics of IHP.
- Set up your [editor](editors.html) with the recommended plugins for the best development experience.
