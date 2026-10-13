{ mkDerivation, aeson, attoparsec, base, bytestring, data-default, deepseq, directory, hspec
, http-types, lib, resourcet, scientific, string-conversions, text, time, uuid
, vault, vector, wai, wai-extra
}:
mkDerivation {
  pname = "wai-request-params";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring data-default deepseq http-types resourcet scientific
    string-conversions text time uuid vault vector wai wai-extra
  ];
  testHaskellDepends = [
    aeson base bytestring data-default directory hspec http-types scientific
    string-conversions text time uuid vault wai wai-extra
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Generic parameter parsing for WAI requests";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
