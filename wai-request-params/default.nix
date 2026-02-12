{ mkDerivation, aeson, attoparsec, base, bytestring, deepseq, hspec
, http-types, lib, scientific, string-conversions, text, time, uuid
, vault, vector, wai, wai-extra
}:
mkDerivation {
  pname = "wai-request-params";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring deepseq http-types scientific
    string-conversions text time uuid vault vector wai wai-extra
  ];
  testHaskellDepends = [
    aeson base bytestring hspec http-types scientific
    string-conversions text time uuid vault wai wai-extra
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Generic parameter parsing for WAI requests";
  license = lib.licenses.mit;
}
