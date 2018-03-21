Foundation
==========

## Setup

1. Install the nix package manager: `curl https://nixos.org/nix/install | sh`
2. Install direnv via homebrew: `brew install direnv`
3. Run `make` in the project root. This will launch the database server and the webserver. You can visit the app at `http://localhost:8000/`.

# Generators

## Adding a view

To create `src/View/Widgets/Modal.hs` run:
```
$ gen/view Widgets Modal
```

In case there is no controller with the given name, the generator will abort.