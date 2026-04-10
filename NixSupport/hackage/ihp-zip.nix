{ mkDerivation, base, http-types, ihp, lib, wai, zip-archive }:
mkDerivation {
  pname = "ihp-zip";
  version = "0.1.0";
  sha256 = "3ff75acfca08231d2ea365369a42b4b8f1abf05df64a980116eed193a778d860";
  libraryHaskellDepends = [ base http-types ihp wai zip-archive ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Support for making ZIP archives with IHP";
  license = lib.licenses.mit;
}
