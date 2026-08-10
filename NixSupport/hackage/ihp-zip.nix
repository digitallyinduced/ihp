{ mkDerivation, base, http-types, ihp, lib, wai, zip-archive }:
mkDerivation {
  pname = "ihp-zip";
  version = "0.1.2";
  sha256 = "0r0xwvl7xx183n059870gkf8569c90iqibgkyrcmb2j799zy0238";
  libraryHaskellDepends = [ base http-types ihp wai zip-archive ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Support for making ZIP archives with IHP";
  license = lib.licenses.mit;
}
