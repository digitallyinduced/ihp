{ pkgs
, ihp
, ghcCompiler
, dontCheckPackages ? []
, doJailbreakPackages ? []
, dontHaddockPackages ? []
, manualOverrides ? _: _: { }
, haskellPackagesDir ? ./haskell-packages
, filter
, ... }:

let
  composeExtensionsList = pkgs.lib.fold pkgs.lib.composeExtensions (_: _: { });

  generatedOverrides = haskellPackagesNew: haskellPackagesOld:
    let
      toPackage = dir: file: _: {
        name = builtins.replaceStrings [ ".nix" ] [ "" ] file;

        value = haskellPackagesNew.callPackage ("${dir}/${file}") { };
      };
      makePackageSet = dir: pkgs.lib.mapAttrs' (toPackage dir) (builtins.readDir dir);
    in {
      "ihp" = ((haskellPackagesNew.callPackage "${toString ihp}/ihp.nix") { inherit filter; });
    } // (makePackageSet haskellPackagesDir) // (makePackageSet "${ihp}/NixSupport/haskell-packages/.");

  makeOverrides =
    function: names: haskellPackagesNew: haskellPackagesOld:
    let
      toPackage = name: {
        inherit name;

        value = function haskellPackagesOld.${name};
      };
    in
    builtins.listToAttrs (map toPackage names);

  ihpDontCheckPackages = [ "mmark" "mmark-ext" "mutable-containers" "hiedb" "hls-fourmolu-plugin" "relude" "inflections" "hls-hlint-plugin" "fourmolu_0_15_0_0" "pcre-heavy"];
  ihpDoJailbreakPackages = [ "haskell-to-elm" "ip" "ghc-syntax-highlighter" "relude" "hs-brotli" "tuples" "singletons-th" "singletons-base" "inflections" "postgresql-simple" "chell" "zigzag" "typerep-map" "relude" "bytebuild" "connection" "microlens" "microlens-th" "hls-hlint-plugin" "hlint" "stripe-concepts" "string-random" "stripe-signature"];
  ihpDontHaddockPackages = [ ];
in ghcCompiler.override {
  overrides = composeExtensionsList [
    generatedOverrides

    # Overrides provided by IHP
    (makeOverrides pkgs.haskell.lib.dontCheck ihpDontCheckPackages)
    (makeOverrides pkgs.haskell.lib.doJailbreak ihpDoJailbreakPackages)
    (makeOverrides pkgs.haskell.lib.dontHaddock ihpDontHaddockPackages)

    # Project specific overrides
    (makeOverrides pkgs.haskell.lib.dontCheck   dontCheckPackages )
    (makeOverrides pkgs.haskell.lib.doJailbreak doJailbreakPackages)
    (makeOverrides pkgs.haskell.lib.dontHaddock dontHaddockPackages)
    manualOverrides

    (self: super: { apply-refact_0_14_0_0 = super.apply-refact_0_14_0_0.override { ghc-exactprint = self.ghc-exactprint_1_8_0_0; }; })
    (self: super: { haskell-language-server = pkgs.haskell.lib.appendConfigureFlag (super.haskell-language-server.override { ghc-exactprint = self.ghc-exactprint_1_8_0_0; fourmolu = self.fourmolu_0_15_0_0; stylish-haskell = self.stylish-haskell_0_14_6_0; apply-refact = self.apply-refact_0_14_0_0; ormolu = self.ormolu_0_7_4_0; hlint = self.hlint_3_8; }) "--enable-executable-dynamic"; })
    (self: super: { ormolu_0_7_4_0 = if pkgs.system == "aarch64-darwin" then pkgs.haskell.lib.overrideCabal super.ormolu_0_7_4_0 (_: { enableSeparateBinOutput = false; }) else super.ormolu_0_7_4_0; })
    (self: super: { ghc-syntax-highlighter = super.ghc-syntax-highlighter_0_0_11_0; })
    (self: super: { retrie = super.retrie.override { ghc-exactprint = self.ghc-exactprint_1_8_0_0; }; })

    (self: super: { websockets = super.websockets_0_13_0_0; })
    (self: super: {
      minio-hs = (pkgs.haskell.lib.overrideCabal super.minio-hs (_: {
        patches = [
          (pkgs.fetchpatch {
            name = "use-crypoton-connection.patch";
            url = "https://github.com/minio/minio-hs/commit/786cf1881f0b62b7539e63547e76afc3c1ade36a.patch";
            sha256 = "sha256-zw0/jhKzShpqV1sUyxWTl73sQOzm6kA/yQOZ9n0L1Ag";
          }) ];
        })).override { connection = super.crypton-connection; };


      smtp-mail = (pkgs.haskell.lib.overrideCabal super.smtp-mail (_: {
        patches = [
          (pkgs.fetchpatch {
            url = "https://github.com/jhickner/smtp-mail/commit/4c724c80814ab1da7c37256a6c10e04c88b9af95.patch";
            sha256 = "sha256-rCyY4rB/wLspeAbLw1jji5BykYFLnmTjLiUyNkiEXmw";
          }) ];
        })).override { connection = super.crypton-connection; };
    })
    
    # (self: super: { fourmolu_0_15_0_0 = super.fourmolu_0_15_0_0.override { hlint = self.ghc-exactprint_1_8_0_0; }; })
  ];
}
