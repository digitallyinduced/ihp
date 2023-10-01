# Editors & Tooling

```toc

```

## Introduction

This place describes all the steps needed to get your code editors and other tooling working with IHP. When your favorite code editor is not described here, [feel free to add your setup to this list](https://github.com/digitallyinduced/ihp/tree/master/Guide).

You will also find steps on how to get autocompletion and smart IDE features. This is provided by haskell-language-server. IHP already comes with a bundled haskell-language-server, so you don't need to install it manually.

## Using IHP with Visual Studio Code / VSCode

**Install the following extensions:**

- [`direnv`](https://marketplace.visualstudio.com/items?itemName=mkhl.direnv), this loads the project `.envrc` file, so all the right Haskell packages are available to VSCode
- [`Haskell`](https://marketplace.visualstudio.com/items?itemName=haskell.haskell), this gets smart IDE features with haskell-language-server
- [`Haskell HSX`](https://marketplace.visualstudio.com/items?itemName=s0kil.vscode-hsx), provides support for [HSX](https://ihp.digitallyinduced.com/Guide/hsx.html)

To make file paths clickable inside the web browser (e.g. when a type error happens), export this env var in your shell (e.g. in `.bashrc`):

```bash
export IHP_EDITOR="code --goto"
```

**Video Tutorial:**

You can also [watch "IHP + Visual Studio Code: Autocompletion & Smart IDE Features with Haskell Language Server" on Youtube](https://www.youtube.com/watch?v=_8_8XYO6rgY)

### VSCode + Haskell Language Server Troubleshooting

- **`"Couldn't figure out what GHC version the project is using"`**

    If you get an error `Couldn't figure out what GHC version the project is using` in Visual Studio Code make sure that the Nix Env Selector plugin was started correctly:

    1. Open the project in VS Code
    2. Click `View` -> `Command Palette` -> `Nix-Env: Select Environment` -> `default.nix`
    3. This will restart VS Code. After that Haskell Language Server should be working.

- **`"Failed to get project GHC executable path: CradleError"`**

    If you get an error `Failed to get project GHC executable path` with a `CradleError`, check your Haskell extension's settings and make sure you are using `"haskell.manageHLS": "PATH"`:

    1. Open the project in VS Code
    2. Click `Extensions` (`Ctrl + Shift + X`) -> Right-click `Haskell` -> `Extension Settings` -> `Haskell: Manage HLS` -> `PATH`

    This error happens because the `GHCup` setting ignores all `PATH` variables pointing to `HLS` and instead uses the `/home/$USER/.cache/ghcup` directory. This is contrary to IHP's usage of Nix which places the necessary tooling into your `PATH` whenever your Nix environment is selected.

    A caveat of this fix is that every time you want to use GHCup for tooling version management, you'll need to switch the setting back. The reason for this can be found in [this issue](https://github.com/haskell/vscode-haskell/issues/387).

### VSCode on Windows with Windows Subsystem for Linux

It is important to not access the files within the WSL from Windows itself (however, the other way around is ok). You can seamlessly (including auto-save) work on your projects within WSL from VS Code in Windows by adding the [`Remote WSL`](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-wsl) extension from Microsoft.

## Using IHP with Sublime Text

Works great already out of the box.

Recommended packages:

-   [`Nix`](https://packagecontrol.io/packages/Nix) for syntax highlighting of nix files
-   [`Haskell HSX`](https://packagecontrol.io/packages/Haskell%20HSX) for syntax highlighting of Haskell with HSX
-   [`Direnv`](https://packagecontrol.io/packages/Direnv) to load the `.envrc` file of the project.
-   [`LSP`](https://packagecontrol.io/packages/LSP) for smart IDE features. Use `LSP: Enable Language Server in Project -> Haskell Language Server` to activate.
    In you don't have that option in the menu, select `Open Preferences > Package Settings > LSP > Settings` and add the `"haskell-language-server"` client configuration to the `"clients"`:
    ```json
    {
        "clients": {
            "haskell-language-server": {
                "enabled": true,
                "command": ["haskell-language-server-wrapper", "--lsp"],
                "selector": "source.haskell"
            }
        }
    }
    ```

To make file paths clickable inside the web browser (e.g. when a type error happens), export this env var in your shell (e.g. in `.bashrc`):

```bash
export IHP_EDITOR="sublime"
```

## Using IHP with Emacs

This section describes the minimal setup needed to get syntax
highlighting, goto-definition, type errors and linting of IHP projects
in Emacs with few external dependencies.

If you have Emacs 29 or later, the language server package `eglot` is
already included. If you're stuck on an older Emacs version, install
it from ELPA with `M-x package-install eglot RET`.

Install the following additional packages from [Melpa](https://melpa.org/#/getting-started):

-   `haskell-mode` – enable basic Haskell support, syntax highlighting, ghci interaction
-   `envrc-mode` – lets eglot find the PATH to Haskell Language Server etc.

At the very least you need `(add-hook 'haskell-mode-hook #'eglot-ensure)`
and `(envrc-global-mode +1)` in your `~/.emacs.d/init.el`, but you may also
want to set up some keybindings to common language server functions
(since by default none are included). Here's an example init file:

```emacs-lisp
(use-package envrc
  :config
  (envrc-global-mode +1))

(use-package eglot
  :config
  (add-hook 'haskell-mode-hook #'eglot-ensure)
  ;; Optionally add keybindings to some common functions:
  :bind ((:map eglot-mode-map
               ("C-c C-e r" . eglot-rename)
               ("C-c C-e l" . flymake-show-buffer-diagnostics)
               ("C-c C-e p" . flymake-show-project-diagnostics)
               ("C-c C-e C" . eglot-show-workspace-configuration)
               ("C-c C-e R" . eglot-reconnect)
               ("C-c C-e S" . eglot-shutdown)
               ("C-c C-e A" . eglot-shutdown-all)
               ("C-c C-e a" . eglot-code-actions)
               ("C-c C-e f" . eglot-format))))

;; Optional: Show/pick completions on tab, sane max height:
(setq tab-always-indent 'complete
      completions-max-height 20
      completion-auto-select 'second-tab)

(server-start) ; for emacsclient / quick startup
```

(The built-in completion menu isn't very modern-looking, a good
alternative if you want to add another plugin is
[corfu](https://github.com/minad/corfu).)


To make file paths clickable inside the web browser (e.g. when a type error happens), you'll first need a little helper script to translate file paths from the `file:line:col` format to `+line:col file` which emacs expects. Put this in e.g. `~/bin/emacs-line`:
```bash
#!/bin/sh
path="${1%%:*}"
col="${1##*:}"
line="${1%:*}"; line="${line##*:}"
emacsclient -n +"${line}:${col}" "${path}"
```
and `chmod +x ~/bin/emacs-line`, then export this env var in your shell (e.g. in `.bashrc`):

```bash
export IHP_EDITOR="$HOME/bin/emacs-line"
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

## Haskell Language Server

Because haskell-language-server is tightly coupled to the GHC version it comes pre-bundled with IHP. In case you also have a local install of haskell-language-server you need to make sure that the one provided by IHP is used. Usually, this is done automatically when your editor is picking up the `.envrc` file.

When something goes wrong you can also run `haskell-language-server` inside the project directory (within a `nix-shell`). This might output some helpful error messages.

## IHP Dev Server

### Customizing the Web Browser used by IHP

When running `devenv up` the application will automatically be opened in your default browser. You can manually specify a browser by setting the env var `IHP_BROWSER` in `.envrc`:

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

Next time you use the dev server via `devenv up` all links will use the right `IHP_BASEURL` instead of using `localhost:8000`;

### Hoogle

To quickly look up function type signatures you can use the built-in hoogle server.

To install it:

1. Open `flake.nix`
2. Add `withHoogle = true;` to the `ihp` project block, inside `perSystem` function invocation like this:

```nix
...
outputs = inputs@{ ihp, flake-parts, systems, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {

        systems = import systems;
        imports = [ ihp.flakeModules.default ];

        perSystem = { pkgs, ... }: {
            ihp = {
                enable = true;
                projectPath = ./.;
                packages = with pkgs; [
                    # Native dependencies, e.g. imagemagick
                ];
                haskellPackages = p: with p; [
                    # Haskell dependencies go here
                    p.ihp
                    cabal-install
                    base
                    wai
                    text
                    hlint
                ];
                withHoogle = true; # <-------
            };
        };

    };
```

Run `devenv up` to remake your dev environment.

After that you can use the following command to start hoogle at `localhost:8080`:

```bash
hoogle server --local -p 8080
```
