# Editors & Tooling

```toc
```

## Introduction

This place describes all the steps needed to get your code editors and other tooling working with IHP. When your favorite code editor is not described here, [feel free to add your setup to this list](https://github.com/digitallyinduced/ihp/tree/master/Guide).

You will also find steps on how to get autocompletion and smart IDE features. This is provided by haskell-language-server. IHP already comes with a bundled haskell-language-server, so you don't need to install it manually.

## Using IHP with Visual Studio Code / VSCode

You need to install the following extensions:

- `direnv`, this loads the project `.envrc` file
- `Haskell`, this get's smart IDE features with haskell-language-server


### VSCode on Windows with Windows Subsystem for Linux

It is important to not access the files within the WSL from Windows itself (however, the other way around is ok). You can seamlessly (including autosave) work on your projects within WSL from VS Code in Windows by adding the `Remote WSL` extension from Microsoft.


## Using IHP with Sublime Text

Works great already out of the box.

Recommended packages:
- `Nix` for syntax highlighting of nix files
- `Direnv` to load the `.envrc` file of the project.
- [`LSP`](https://packagecontrol.io/packages/LSP) for smart IDE features. Use `LSP: Enable Language Server in Project -> Haskell Language Server` to activate.

## Using IHP with Emacs

Install the following packages from [Melpa](https://melpa.org/#/getting-started):
- `dante` – gives IDE features via ghci, see https://github.com/jyp/dante#installation
- `direnv-mode` – lets haskell-mode and dante-mode find the PATH to ghci, see https://github.com/wbolster/emacs-direnv#installation
- `attrap` (optional) – apply fixes at point, see https://github.com/jyp/attrap

and put a `.dir-locals.el` file in your project root with:
```emacs-lisp
((nil
  (dante-repl-command-line . ("ghci"))
  (haskell-process-type . ghci)))
```

## Notes on `haskell-language-server`

IHP currently uses v0.4 of haskell language server.

Because haskell language server is tightly coupled to the GHC version it comes pre-bundled with IHP. In case you also have a local install of haskell language server you need to make sure that the one provided by IHP is used. Ususally this is done automatically when your editor is picking up the `.envrc` file.

When something goes wrong you can also run `haskell-language-server` inside the project directory. This might output some helpful error messages.

If you get an error like `error: attribute 'haskell-language-server' missing`: Make sure your `Config/nix/nixpkgs-config.nix` is using the latest nixpkgs version used by IHP.

In line 21 and 22 it should be configured like this:

```
  nixPkgsRev = "07e5844fdf6fe99f41229d7392ce81cfe191bcfc";
  nixPkgsSha256 = "0p2z6jidm4rlp2yjfl553q234swj1vxl8z0z8ra1hm61lfrlcmb9";
```

In case you have a different `nixPkgsRev` or `nixPkgsSha256` replace it with the above code. After that run `make -B .envrc` and `make hie.yaml`.