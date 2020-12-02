# Installing IHP

```toc
```

## 1. Dependency: Nix Package Manager

The framework uses the nix package manager to manage the whole set of dependencies of your application

For example, postgresql and the Haskell compiler are both dependencies of your app, as well as all the Haskell or javascript packages you want to use. We use nix to make sure that these dependencies are available to the app - in development, as well as in production.

That's why we first need to make sure that you have nix installed.

### Mac
Run this command in your terminal to install nix on your machine.

```bash
sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume
```
After this restart your terminal.

If you get an error like `error: refusing to create Nix store volume because the boot volume is FileVault encrypted, but encryption-at-rest is not available.`, follow the steps described [in this GitHub Issue](https://github.com/digitallyinduced/ihp/issues/93#issuecomment-639611313). We're working on improving this step.

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

To download a Linux Distribution, open the Microsoft Store and search for Ubuntu or Debian. We recommend Ubuntu, since it works best with nix on Windows.

Note: You **do not** need a Microsoft account to download. You can simply cancel or close the login forms and the download will continue.

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

When the installation finishes successfuly, you will be prompted to either reload your environment with the given command, or restart your shell. 

If in doubt, just close and reopen Ubuntu/Your Distro.

**NOTES FOR WINDOWS USERS**:
###### Windows Firewall
When using Windows, you will be asked if tasks like ghc-iserv or rundevserver should be allowed by the firewall. This is needed to access the devserver interface and the webapp itself.

Installing nix for IHP was done using [this guide](https://nathan.gs/2019/04/12/nix-on-windows/).

Note that nix can gradually grow to use several GB of disk space, especially after upgrading IHP. You can always run `nix-collect-garbage` in a `nix-shell` which will delete older/unused files.

## 2. Installing IHP

You can now install IHP by running:

```bash
nix-env -f https://ihp.digitallyinduced.com/ihp-new.tar.gz -i ihp-new
```

If you don't already use cachix, you will be prompted to install it. You don't need it, but it is highly recommended, as it reduces build time dramatically. Learn more about cachix [here](https://cachix.org/).

#### NixOS specific

If you get the following error during this step on NixOS:

```bash
MustBeRoot "Run command as root OR execute: $ echo \"trusted-users = root $USER\" | sudo tee -a /etc/nix/nix.conf && sudo pkill nix-daemon"
```

You have to add this to your NixOS configuration:
```bash
nix.trustedUsers = [ "root" "USERNAME_HERE" ];
```

[See the documentation for `nix.trustedUsers` to learn more about what this is doing](https://github.com/NixOS/nixpkgs/blob/db31e48c5c8d99dcaf4e5883a96181f6ac4ad6f6/nixos/modules/services/misc/nix-daemon.nix#L319).

## 3. Next

It's time to start your first "hello world" project now :)

[Next: Your First Project](https://ihp.digitallyinduced.com/Guide/your-first-project.html)
