# Editors & Tooling

```toc

```

## Introduction

This place describes all the steps needed to get your code editors and other tooling working with IHP. When your favorite code editor is not described here, [feel free to add your setup to this list](https://github.com/digitallyinduced/ihp/tree/master/Guide).

You will also find steps on how to get autocompletion and smart IDE features. This is provided by haskell-language-server. IHP already comes with a bundled haskell-language-server, so you don't need to install it manually.

## Using IHP with Visual Studio Code / VSCode

**Install the following extensions:**

-   [`nix-env-selector`](https://marketplace.visualstudio.com/items?itemName=arrterian.nix-env-selector), this loads the project `default.nix` file, so all the right Haskell packages are available to VSCode
-   [`Haskell`](https://marketplace.visualstudio.com/items?itemName=haskell.haskell), this gets smart IDE features with haskell-language-server
-   [`Haskell HSX`](https://marketplace.visualstudio.com/items?itemName=s0kil.vscode-hsx), provides support for [HSX](https://ihp.digitallyinduced.com/Guide/hsx.html)

To make file paths clickable inside the web browser (e.g. when a type error happens), export this env var in your shell (e.g. in `.bashrc`):

```bash
export IHP_EDITOR="code --goto"
```

### VSCode on Windows with Windows Subsystem for Linux

It is important to not access the files within the WSL from Windows itself (however, the other way around is ok). You can seamlessly (including auto-save) work on your projects within WSL from VS Code in Windows by adding the [`Remote WSL`](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-wsl) extension from Microsoft.

## Using IHP with Sublime Text

Works great already out of the box.

Recommended packages:

-   `Nix` for syntax highlighting of nix files
-   `Direnv` to load the `.envrc` file of the project.
-   [`LSP`](https://packagecontrol.io/packages/LSP) for smart IDE features. Use `LSP: Enable Language Server in Project -> Haskell Language Server` to activate.


To make file paths clickable inside the web browser (e.g. when a type error happens), export this env var in your shell (e.g. in `.bashrc`):

```bash
export IHP_EDITOR="sublime"
```

## Using IHP with Emacs

Install the following packages from [Melpa](https://melpa.org/#/getting-started):

-   `dante` – gives IDE features via ghci, see https://github.com/jyp/dante#installation
-   `direnv-mode` – lets haskell-mode and dante-mode find the PATH to ghci, see https://github.com/wbolster/emacs-direnv#installation
-   `attrap` (optional) – apply fixes at point, see https://github.com/jyp/attrap

and put a `.dir-locals.el` file in your project root with:

```emacs-lisp
((nil
  (dante-repl-command-line . ("ghci"))
  (haskell-process-type . ghci)))
```

## Using IHP with Vim / NeoVim

### Using CoC

Provided you already have CoC setup, just run `:CocConfig` and add the following segment.

```json
{
    "languageserver": {
        "haskell": {
            "command": "haskell-language-server-wrapper",
            "args": ["--lsp"],
            "rootPatterns": [
                "*.cabal",
                "stack.yaml",
                "cabal.project",
                "package.yaml",
                "hie.yaml"
            ],
            "filetypes": ["haskell", "lhaskell"]
        }
    }
}
```

## Notes on `haskell-language-server`

Because haskell-language-server is tightly coupled to the GHC version it comes pre-bundled with IHP. In case you also have a local install of haskell-language-server you need to make sure that the one provided by IHP is used. Usually, this is done automatically when your editor is picking up the `.envrc` file.

When something goes wrong you can also run `haskell-language-server` inside the project directory (within a `nix-shell`). This might output some helpful error messages.

## IHP Dev Server

### Customizing the Web Browser used by IHP

When running `./start` the application will automatically be opened in your default browser. You can manually specificy a browser by setting the env var `IHP_BROWSER`:

```bash
export IHP_BROWSER=firefox
```

You can disable the auto-start of the browser completely using `echo` as your browser:

```bash
export IHP_BROWSER=echo
```


### Running the IHP Dev Server On a Host Different From `localhost`

If you run the IHP dev server on computer different from your local machine (e.g. a second computer in your network or a Cloud Dev Env like GitPod), you need to specify the right base url:

```bash
export IHP_BASEURL=http://some-other-host:8000 # App Url, Default: http://localhost:8000
export IHP_IDE_BASEURL=http://some-other-host:8001 # SchemaDesigner etc., Default: http://localhost:8001
```

Next time you use the dev server via `./start` all links will use the right `IHP_BASEURL` instead of using `localhost:8000`;