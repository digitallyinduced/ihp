{ mkDerivation, aeson, attoparsec, base, basic-prelude, blaze-html
, bytestring, fast-logger, ihp, ihp-hsx, lib, megaparsec
, string-conversions, text, wai, wai-request-params, websockets
}:
mkDerivation {
  pname = "ihp-ssc";
  version = "1.6.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base basic-prelude blaze-html bytestring
    fast-logger ihp ihp-hsx megaparsec string-conversions text wai
    wai-request-params websockets
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Server Side Components for IHP";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
