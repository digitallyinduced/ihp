let
    pkgs = import ./NixSupport/pkgs.nix;
    compiler = "ghc883";
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/haskellframework.git";
        rev = "1eadc75cb7d17bb1e5f2961531b1667f2d3d7b8e";
    };
in
    pkgs.haskell.packages."${compiler}".callPackage ./ihp.nix { }
