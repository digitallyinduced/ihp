# Editors & Tooling

```toc
```

## Introduction

This place describes all the steps needed to get your code editors and other tooling working with IHP. When your favorite code editor is not described here, [feel free to add your setup to this list](https://github.com/digitallyinduced/ihp/tree/master/Guide).

## Using IHP with Visual Studio Code / VSCode

When using VSCode you need to install a plugin which loads the `.envrc` in your project. Otherwise the wrong GHC will be picked up. Take a look at [vscode-direnv](https://github.com/rubymaniac/vscode-direnv).

Most likely you want to use [haskell-language-server](https://github.com/haskell/haskell-language-server) for smart IDE features like autocompletion and jump-to-symbol. This is not working with IHP yet. [There is an active issue to implement support for this.](https://github.com/digitallyinduced/ihp/issues/190)


## Using VSCode on Windows with Windows Subsystem for Linux

It is important to not access the files within the WSL from Windows itself (however, the other way around is ok). You can seamlessly (including autosave) work on your projects within WSL from VS Code in Windows by adding the `Remote WSL` extension from Microsoft.


## Using IHP with Sublime Text

Works great already out of the box.

Recommended packages:
- `Nix` for syntax highlighting of nix files
- `Direnv` to load the `.envrc` file of the project.

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
