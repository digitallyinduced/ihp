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
    hasql-mapping = haskellLib.doJailbreak super.hasql-mapping;
}
