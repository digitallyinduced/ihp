{ mkDerivation, base, http-types, ihp, lib, wai, zip-archive }:
mkDerivation {
  pname = "ihp-zip";
  version = "0.1.1";
  sha256 = "1hkx1rf4h297bjjwwf6ckxg6jp7bvr2z92vy4a67n33k8l7mhi18";
  libraryHaskellDepends = [ base http-types ihp wai zip-archive ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Support for making ZIP archives with IHP";
  license = lib.licenses.mit;
}
