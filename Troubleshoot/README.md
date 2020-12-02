# Provides the `ihp-troubleshoot` command

This script detects common error in the dev environment where IHP is running.

The script checks:

- that the current directory looks like an ihp project
- that the `.ghci` file exists and has the right permissions to be loaded
- that `build/ihp-lib` symlink exists and references a valid IHP build
- that `direnv` is loaded correctly into the shell
- that `direnv` can correctly load the .envrc
- that the digitallyinduced cachix cache is installed
- that the old deprecated cachix key is not used
- nix is correctly installed


## Design Goals

The script should be runnable by just downloading a single file. The script should also test that nix is working correctly, so we cannot offer a simple nix package. So this script needs to be a standalone shell script that can be distributed by just downloading the file.