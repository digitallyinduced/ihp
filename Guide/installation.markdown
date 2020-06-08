# Installing IHP

```toc
```

## 1. Dependency: Nix Package Manager

The framework uses the nix package manager to manage the whole set of dependencies of your application

For example postgresql and the haskell compiler are both dependencies of your app. as well as all the haskell or javascript packages you want to use. we use nix to make sure that these dependencies are available to the app - in development, as well as in production.

That's why we first need to make sure that you have nix installed.

##### Mac
Run this commands in your terminal to install nix on your machine.

```bash
sh <(curl https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume
```
After this restart your terminal.

If you get an error like `error: refusing to create Nix store volume because the boot volume is FileVault encrypted, but encryption-at-rest is not available.`, follow the steps described [in this GitHub Issue](https://github.com/digitallyinduced/ihp/issues/93#issuecomment-639611313). We're working on improving this step.

##### Linux

Install nix by running the following command in your shell and follow the instructions on the screen:

```bash
curl https://nixos.org/nix/install | sh
```

There are also other ways to install nix, [take a look at the documentation](https://nixos.org/nix/download.html).

##### Windows
Sorry, we don't support windows yet.

## 2. Dependency: Direnv

IHP uses `direnv` to speed up the development shell. As it needs to be hooked into your shell, it needs to be installed manually.

Install it via nix:

```bash
nix-env -i direnv
```

**After that you also need to hook it into your shell:**

##### Bash

If you use bash, run this command to install direnv to bash:

```bash
echo 'eval "$(direnv hook bash)"' >> ~/.bashrc
```

##### ZSH

If you use zsh, run this command to install direnv to zsh:

```bash
echo 'eval "$(direnv hook zsh)"' >> ~/.zshrc
```

##### Other shell

For other shells, [take a look at the direnv documentation](https://direnv.net/#README).

## 3. Installing IHP

You can now install IHP by running:

```bash
nix-env -f https://beta:beta@ihp.digitallyinduced.com/ihp-new.tar.gz -i ihp-new
```
