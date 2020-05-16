let
    pkgs = import ./NixSupport/pkgs.nix;
    compiler = "ghc883";
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/haskellframework.git";
        rev = "ff2db29833d500ae58837d9dc80c267dee5e8b55";
    };
in
    pkgs.haskell.packages."${compiler}".callPackage ./ihp.nix { }
