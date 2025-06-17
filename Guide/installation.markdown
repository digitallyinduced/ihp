# Installing IHP

```toc

```

## 1. Dependency: Nix Package Manager

The framework uses the nix package manager to manage the whole set of dependencies of your application. Nix is the equivalent of `npm`, but for Haskell and much more.

For example, PostgreSQL and the Haskell compiler are both dependencies of your app, as well as all the Haskell or JavaScript packages you want to use. We use nix to make sure that these dependencies are available to the app - in development, as well as in production.

That's why we first need to make sure that you have nix installed. Follow the installation instructions from https://docs.determinate.systems/

### Enabling Flakes

Flakes is an experimental, yet very popular feature of Nix, so the installation steps above would not have enabled this useful feature.
We also recommend enabling the [lazy-trees](https://determinate.systems/posts/changelog-determinate-nix-352/) features that will result in faster downloads.
Furhermore, we need to define the current user as a "trusted user". This script will add this to `/etc/nix/nix.custom.conf`:

```bash
# This script:
# - Backs up ~/.config/nix/nix.conf if it exists
# - Removes conflicting lines from /etc/nix/nix.custom.conf
# - Appends proper trusted-users and experimental-features
# - Restarts the nix-daemon to apply changes

USERNAME=$(whoami)
CONF_FILE="/etc/nix/nix.custom.conf"

# Backup user-level nix config to avoid conflicts
if [ -f ~/.config/nix/nix.conf ]; then
    mv ~/.config/nix/nix.conf ~/.config/nix/nix.conf-bkp
fi

# Remove old settings from the system config
sudo sed -i '/^trusted-users/d;/^experimental-features/d;/^lazy-trees/d' "$CONF_FILE"

# Add correct settings
sudo tee -a "$CONF_FILE" > /dev/null <<EOF
trusted-users = root $USERNAME
experimental-features = nix-command flakes
lazy-trees = true
EOF

# Restart nix to apply the config
sudo systemctl restart nix-daemon.service
```

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


### GitHub Codespaces / VSCode Devcontainers

To get started with IHP on [GitHub Codespaces](https://docs.github.com/en/codespaces/getting-started/quickstart), simply use the [Codespaces IHP Template](https://github.com/rvarun11/codespaces-ihp) to create a new GitHub repo. On the first start up, a new IHP boilerplate will be generated which you can commit.

To try it out before making your own repo, you can simply start a Codespace from the template itself.

If you have Docker installed locally, then you can use this configuration to work with [VSCode Devcontainers](https://code.visualstudio.com/docs/devcontainers/containers) as well.

## 3. Setting up your editor

Also, check this out if your editor is already set up. You might miss a plugin that's recommended for IHP to work well.

-   [VS Code](https://ihp.digitallyinduced.com/Guide/editors.html#using-ihp-with-visual-studio-code-vscode) **Don't have the direnv extension installed? Read this link**
-   [Sublime Text](https://ihp.digitallyinduced.com/Guide/editors.html#using-ihp-with-sublime-text)
-   [Emacs](https://ihp.digitallyinduced.com/Guide/editors.html#using-ihp-with-emacs)
-   [Vim / NeoVim](https://ihp.digitallyinduced.com/Guide/editors.html#using-ihp-with-vim-neovim)

It's time to start your first "Hello World" project now!

[Next: Your First Project](https://ihp.digitallyinduced.com/Guide/your-first-project.html)
