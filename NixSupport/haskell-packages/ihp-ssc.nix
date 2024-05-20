{ mkDerivation, ihp, lib, aeson, megaparsec, bytestring, wai, websockets, ihp-hsx, base, string-conversions, basic-prelude, text, blaze-html, attoparsec }:
mkDerivation {
  pname = "ihp-ssc";
  version = "1.3.0";
  src = ./../../ihp-ssc;
  libraryHaskellDepends = [ ihp aeson megaparsec bytestring wai websockets ihp-hsx base string-conversions basic-prelude text blaze-html attoparsec ];
  description = "Server Side Components for IHP";
  license = lib.licenses.mit;
}