# Installing IHP

```toc

```

## 1. Dependency: Nix Package Manager

The framework uses the nix package manager to manage the whole set of dependencies of your application. Nix is the equivalent of `npm`, but for Haskell and much more.

For example, PostgreSQL and the Haskell compiler are both dependencies of your app, as well as all the Haskell or JavaScript packages you want to use. We use nix to make sure that these dependencies are available to the app - in development, as well as in production.

That's why we first need to make sure that you have nix installed.

### Mac

Run this command in your terminal to install nix on your machine:

```bash
sh <(curl -L https://nixos.org/nix/install)
```

After this restart your terminal.

### Linux

Before installing nix and IHP, you need `curl` and `git` if you don't have them already. If you are unsure, run this:

```bash
sudo apt update
sudo apt upgrade
sudo apt install git curl make -y
```

**For NixOS Users:** If you're on NixOS, of course you don't need to install nix anymore :-) Just skip this step.

Install nix by running the following command in your shell and follow the instructions on the screen:

```bash
curl -L https://nixos.org/nix/install | sh
```

Due to Linux not loading the `.profile` file, nix will not be loaded. To fix that, we need to add this line to the rc file of your shell (usually `.bashrc`). Open it, and add this line

```bash
. ~/.nix-profile/etc/profile.d/nix.sh
```

There are also other ways to install nix, [take a look at the documentation](https://nixos.org/nix/download.html).

### Windows

Running nix on Windows requires the Windows Subsystem for Linux, which first needs manual activation via **Powershell with Administrator Privileges**:

```bash
dism.exe /online /enable-feature /featurename:Microsoft-Windows-Subsystem-Linux /all /norestart
```

Enabling this Feature needs a restart (even though it won't prompt, and the command-line says /norestart).

To download a Linux Distribution, open the Microsoft Store and search for Ubuntu or Debian. We recommend Ubuntu since it works best with nix on Windows.

Note: You **do not** need a Microsoft account to download. You can simply cancel or close the login forms and the download will continue.

If this is your first time installing WSL and you are encountering problems, rest assured most issues are well-known and solutions can be found online by searching the web for the error messages.

With the Distro downloaded, run it and update it using your package manager. In Ubuntu you would use:

```bash
sudo apt update
sudo apt upgrade
sudo apt install git curl make xdg-utils -y
```

WSL will add your Windows System Paths in your Linux Subsystem. These tend to generate errors due to spaces and brackets in folder names. Also, due to Linux not loading the `.profile`, we need to add the nix.sh manually. To fix these two issues, just add these lines to the end of your `.bashrc`

```bash
PATH=$(/usr/bin/printenv PATH | /usr/bin/perl -ne 'print join(":", grep { !/\/mnt\/[a-z]/ } split(/:/));')
. ~/.nix-profile/etc/profile.d/nix.sh
```

Now, create a folder for nix:

```bash
sudo mkdir -p /etc/nix
```

To make nix usable on Windows, we need to create and add the following lines to the file `/etc/nix/nix.conf` (requires use of sudo again):

```bash
sandbox = false
use-sqlite-wal = false
```

After saving the file, you can now install nix:

```bash
curl -L https://nixos.org/nix/install | sh
```

When the installation finishes successfully, you will be prompted to either reload your environment with the given command, or restart your shell.

If in doubt, just close and reopen Ubuntu/Your Distro.

**NOTES FOR WINDOWS USERS**:

###### Windows Firewall

When using Windows, you will be asked if tasks like ghc-iserv or rundevserver should be allowed by the firewall. This is needed to access the devserver interface and the web application itself.

Installing nix for IHP was done using [this guide](https://nathan.gs/2019/04/12/nix-on-windows/).

Note that nix can gradually grow to use several GB of disk space, especially after upgrading IHP. You can always run `nix-collect-garbage` in a `nix-shell` which will delete older/unused files.

### Enabling flakes

IHP uses Nix flakes. While already widely used, they are still a quite new feature and not yet enabled by default by the Nix install scripts.

If you use IHP normally and with direnv this won't be a problem for you because all the Nix stuff is being handled for you. However, it's a good idea to enable flakes, so you can benefit from their features if you need them or want to try them out.

To enable flakes, either edit `~/.config/nix/nix.conf` (to enable just for your user) or `/etc/nix/nix.conf` (to enable flakes globally) and add the following line:

```bash
experimental-features = nix-command flakes
```

## 2. Installing IHP

You can now install IHP by running:

```bash
nix-env --install ihp-new
```

If you use Home Manager or NixOS, you can also add `ihp-new` to the list of packages.

#### Package not Found?

The package has only been added to nixpkgs recently. In case you're using an older nixpkgs version, the package might not be available yet. In that case you can install `ihp-new` like this:

```bash
nix-env -f https://downloads.digitallyinduced.com/ihp-new.tar.gz -i ihp-new
```

#### NixOS specific

If you get the following error during this step on NixOS:

```bash
MustBeRoot "Run command as root OR execute: $ echo \"trusted-users = root $USER\" | sudo tee -a /etc/nix/nix.conf && sudo pkill nix-daemon"
```

You have to add this to your NixOS configuration:

```bash
nix.settings.trusted-users = [ "root" "USERNAME_HERE" ];
```

[See the documentation for `nix.settings.trusted-users` to learn more about what this is doing](https://search.nixos.org/options?show=nix.settings.trusted-users&query=nix.settings.trusted-users).

#### GitHub Codespaces / VSCode Devcontainers

To get started with IHP on [GitHub Codespaces](https://docs.github.com/en/codespaces/getting-started/quickstart), simply use the [Codespaces IHP Template](https://github.com/rvarun11/codespaces-ihp) to create a new GitHub repo. On the first start up, a new IHP boilerplate will be generated which you can commit.

To try it out before making your own repo, you can simply start a Codespace from the template itself.

If you have Docker installed locally, then you can use this configuration to work with [VSCode Devcontainers](https://code.visualstudio.com/docs/devcontainers/containers) as well.

#### GitPod

If you use GitPod for Development in the Cloud, [you can use IHP GitPod Template](https://github.com/gitpod-io/template-ihp/generate).

On first start up, GitPod will automatically generate a new IHP project for you. You just need to call `git add .` and then commit the boilerplate after first start.

If you want to try it out before making your own repo, use this button below:

[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/gitpod-io/template-ihp)

## 3. Setting up your editor

Also check this out if your editor is already set up. You might miss a plugin that's recommended for IHP to work well.

-   [VS Code](https://ihp.digitallyinduced.com/Guide/editors.html#using-ihp-with-visual-studio-code-vscode) **Don't have the direnv extension installed? Read this link**
-   [Sublime Text](https://ihp.digitallyinduced.com/Guide/editors.html#using-ihp-with-sublime-text)
-   [Emacs](https://ihp.digitallyinduced.com/Guide/editors.html#using-ihp-with-emacs)
-   [Vim / NeoVim](https://ihp.digitallyinduced.com/Guide/editors.html#using-ihp-with-vim-neovim)

It's time to start your first "hello world" project now :)

[Next: Your First Project](https://ihp.digitallyinduced.com/Guide/your-first-project.html)
