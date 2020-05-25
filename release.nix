let
    pkgs = import ./NixSupport/pkgs.nix;
    compiler = "ghc883";
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/haskellframework.git";
        rev = "26a3ec4096bb2e9e58590bb99afbbe6421de5e66";
    };
in
    pkgs.haskell.packages."${compiler}".callPackage ./ihp.nix { }
