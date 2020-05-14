let
    pkgs = import ./NixSupport/pkgs.nix;
    compiler = "ghc883";
in
    pkgs.haskell.packages."${compiler}".callPackage ./turbohaskell.nix { }
