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
