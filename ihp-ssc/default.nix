{ mkDerivation, aeson, attoparsec, base, basic-prelude, blaze-html
, bytestring, ihp, ihp-hsx, ihp-log, lib, megaparsec
, string-conversions, text, wai, wai-request-params, websockets
}:
mkDerivation {
  pname = "ihp-ssc";
  version = "1.3.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base basic-prelude blaze-html bytestring ihp
    ihp-hsx ihp-log megaparsec string-conversions text wai
    wai-request-params websockets
  ];
  description = "Server Side Components for IHP";
  license = lib.licenses.mit;
}
