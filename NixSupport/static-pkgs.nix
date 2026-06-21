{ inputs
, ihp
, system
}:

let
    pkgsOrig = import inputs.nixpkgs {
        inherit system;
        overlays = [ ihp.overlays.default ];
        config = {};
    };

    pkgsMusl = pkgsOrig.pkgsMusl;

    staticDeps = import ./static-deps.nix { pkgs = pkgsMusl; };

    staticGhc = pkgsMusl.haskellPackages.ghc.override {
        enableRelocatedStaticLibs = true;
        enableShared = false;
        enableDwarf = false;
        enableProfiledLibs = false;
        enableDocs = false;
        enableNativeBignum = true;
    };

    disableStaticPackageFeatures = self: super: {
        mkDerivation = args: super.mkDerivation (args // {
            doCheck = false;
            doHaddock = false;
            enableLibraryProfiling = false;
            enableExecutableProfiling = false;
        });
    };

    staticBuildHaskellPackages = pkgsMusl.haskellPackages.buildHaskellPackages.override (old: {
        ghc = staticGhc;
        buildHaskellPackages = staticHaskellPackages;
        overrides = pkgsMusl.lib.composeExtensions
            (old.overrides or (_self: _super: {}))
            disableStaticPackageFeatures;
    });

    staticHaskellPackages = pkgsMusl.haskellPackages.override {
        ghc = staticGhc;
        buildHaskellPackages = staticBuildHaskellPackages;
        overrides = pkgsMusl.lib.composeExtensions
            pkgsOrig.ihpHaskellOverrides
            (pkgsMusl.lib.composeExtensions disableStaticPackageFeatures (self: super: {
                ihp = super.ihp.override { systemdSupport = false; };
            }));
    };

    staticDepsRoot = staticDeps // {
        ghc = staticGhc;
    };

    staticNativeDeps = builtins.attrValues (removeAttrs staticDeps [
        "ghc"
        "cabal2nix-unwrapped"
        "jailbreak-cabal"
    ]);
in
    pkgsMusl // {
        buildPackages = pkgsOrig.buildPackages;
        haskellPackages = staticHaskellPackages;
        ghc = staticHaskellPackages;
        ihpStaticNativeDeps = staticNativeDeps;
        ihpStaticDepsRoot = pkgsMusl.symlinkJoin {
            name = "ihp-static-build-deps";
            paths = builtins.attrValues staticDepsRoot;
        };
    }
