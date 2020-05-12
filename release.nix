let
    pkgs = import ./NixSupport/pkgs.nix;
    compiler = "ghc865";
in
    pkgs.haskell.packages."${compiler}".callPackage ./turbohaskell.nix { }
