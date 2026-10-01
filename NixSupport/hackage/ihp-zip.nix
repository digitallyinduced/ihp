{ mkDerivation, base, http-types, ihp, lib, wai, zip-archive }:
mkDerivation {
  pname = "ihp-zip";
  version = "0.1.2";
  sha256 = "6808e07f4a478a5559f6f3ad8823482c9982dc7ce0a054801d28f47ee8e61d64";
  libraryHaskellDepends = [ base http-types ihp wai zip-archive ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Support for making ZIP archives with IHP";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
