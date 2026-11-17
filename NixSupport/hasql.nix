{ inputs, pkgs }:
self: super:
let
    haskellLib = pkgs.haskell.lib;
    # Upstream driver suites require Docker. IHP's checks exercise the same
    # package set against isolated PostgreSQL instances without containers.
    package = name: haskellLib.dontCheck (self.callPackage
        (./hasql + "/${name}.nix") ({ source = inputs."${name}-source"; }
            // pkgs.lib.optionalAttrs (builtins.elem name [ "pqi-native" "pqi-ffi" ]) {
                pqi-conformance = null;
            }));
in {
    # hasql 2.0.1.0 requires postgresql-binary >=0.15 && <0.16. Snapshots whose
    # default set regressed to 0.14.2 still ship 0.15.0.1 as a versioned
    # attribute; newer snapshots (stackage Nightly 2026-09-13 onwards) have
    # 0.15.0.1 as the default and dropped that attribute, so fall back to it.
    postgresql-binary = super.postgresql-binary_0_15_0_1 or super.postgresql-binary;

    # Same for text-builder: postgresql-simple-postgresql-types needs
    # >=1.0.0.5 && <1.1, which is the default from Nightly 2026-09-13 onwards
    # and only available as a versioned attribute in older snapshots.
    text-builder = super.text-builder_1_0_0_5 or super.text-builder;

    hasql = package "hasql";
    hasql-pool = package "hasql-pool";
    hasql-transaction = package "hasql-transaction";
    hasql-dynamic-statements = package "hasql-dynamic-statements";
    hasql-implicits = package "hasql-implicits";
    hasql-notifications = package "hasql-notifications";
    pqi = package "pqi";
    pqi-ffi = package "pqi-ffi";
    pqi-native = haskellLib.overrideCabal (package "pqi-native") (old: {
        # Hash, HMAC and PBKDF2 also work with IHP's crypton 1.1.4. Keep one
        # provider of Crypto.* modules; SCRAM authentication is tested with it.
        postPatch = (old.postPatch or "") + ''
            substituteInPlace pqi-native.cabal \
                --replace-fail 'crypton >=2.0.1 && <2.1' 'crypton >=1.1.4 && <2.1'
        '';
    });
    # This release uses the unchanged 1.10/2.0 codec API, but its old bound
    # excludes Hasql 2. Keep the codecs used by IHP's PostGIS integration.
    # Newer nixpkgs snapshots also mark this version broken (stale Hasql 2 upper
    # bound), so clear the flag before relaxing the bound.
    hasql-mapping = haskellLib.doJailbreak (haskellLib.markUnbroken super.hasql-mapping);
}
