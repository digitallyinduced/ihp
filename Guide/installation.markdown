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
This will create an extra volume which is then mounted on `/nix`.
We need to use this trick because MacOs Catalina does protect the root directory.

This method is taken from the following Github Comment:
[https://github.com/NixOS/nix/issues/2925#issuecomment-539570232](https://github.com/NixOS/nix/issues/2925#issuecomment-539570232)

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
